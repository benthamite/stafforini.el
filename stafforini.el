;;; stafforini.el --- Build commands for stafforini.com -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/stafforini.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (citar "1.4") (paths "0.1") (gptel "0.9") (org-roam "2.0") (transient "0.4"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Emacs commands for building and previewing the stafforini.com Hugo site.
;; Companion package to https://github.com/benthamite/stafforini.com —
;; wraps the project's Python and Elisp build scripts, running them
;; asynchronously via `compile' so output appears in a compilation buffer.
;; See PUBLISHING.md in the Hugo repo for the full workflow documentation.

;;; Code:

(require 'compile)
(require 'paths)
(require 'transient)

;;;; Custom variables

(defgroup stafforini nil
  "Build commands for stafforini.com."
  :group 'tools)

(defcustom stafforini-hugo-dir
  (file-name-concat paths-dir-personal-repos "stafforini.com/")
  "Root directory of the stafforini.com Hugo project."
  :type 'directory
  :group 'stafforini)

(defcustom stafforini-scripts-dir
  (file-name-concat stafforini-hugo-dir "scripts/")
  "Directory containing build scripts."
  :type 'directory
  :group 'stafforini)

;;;; Internal helpers

(defvar-local stafforini--on-success nil
  "Function to call when compilation finishes successfully.")

(defun stafforini--maybe-run-on-success (_buf status)
  "Run the on-success callback if compilation succeeded.
STATUS is the compilation status string."
  (when (and (string-match-p "finished" status)
             stafforini--on-success)
    (funcall stafforini--on-success)))

(defun stafforini--compile (command &optional name on-success)
  "Run COMMAND asynchronously in a compilation buffer.
NAME, if non-nil, is used as the compilation buffer name.
ON-SUCCESS, if non-nil, is called with no arguments when
compilation finishes successfully."
  (let ((default-directory stafforini-hugo-dir)
        (compilation-buffer-name-function
         (when name
           (lambda (_mode) name)))
        (compilation-save-buffers-predicate
         (stafforini--hugo-file-predicate))
        (process-connection-type nil)
        (process-environment
         (stafforini--sanitized-process-environment)))
    (let ((buf (compile command)))
      (when on-success
        (with-current-buffer buf
          (setq stafforini--on-success on-success)
          (add-hook 'compilation-finish-functions
                    #'stafforini--maybe-run-on-success nil t)))
      buf)))

(defun stafforini--hugo-file-predicate ()
  "Return a predicate that matches buffers visiting files under the Hugo dir."
  (lambda ()
    (when-let* ((file (buffer-file-name)))
      (file-in-directory-p file stafforini-hugo-dir))))

(defun stafforini--sanitized-process-environment ()
  "Return `process-environment' with EDITOR/VISUAL neutralized.
Prevents child processes from spawning emacsclient, which can
collide with the running server and produce spurious errors."
  (append '("EDITOR=cat" "VISUAL=cat") process-environment))

(defun stafforini--script-command (script &optional args)
  "Build a shell command to run SCRIPT from `stafforini-scripts-dir'.
The runner (bash or python) is inferred from the file extension.
ARGS, if non-nil, is appended to the command."
  (let ((runner (pcase (file-name-extension script)
                  ("sh" "bash")
                  ("py" "python")
                  (ext (error "Unsupported script extension: .%s" ext)))))
    (concat runner " "
            (shell-quote-argument
             (expand-file-name script stafforini-scripts-dir))
            (if args (concat " " (shell-quote-argument args)) ""))))

(defun stafforini--run-script (script &optional args on-success)
  "Run SCRIPT from `stafforini-scripts-dir' in a compilation buffer.
ARGS, if non-nil, is appended to the command.
ON-SUCCESS, if non-nil, is called when compilation succeeds."
  (stafforini--compile (stafforini--script-command script args)
                       (format "*stafforini-%s*"
                               (file-name-sans-extension script))
                       on-success))

;;;; Export link handler

;; Load the shared export helpers (dotfiles link exporter, relref fixer)
;; from the Hugo project's scripts directory.  In batch exports these are
;; loaded directly by export-notes.el / export-quotes.el via export-common.el;
;; here we load the same file so interactive exports get the same behaviour.
(let ((export-common (file-name-concat stafforini-scripts-dir "export-common.el")))
  (when (file-exists-p export-common)
    (load export-common)))

;;;; Title-drop prevention

;; ox-hugo intermittently drops the `title' field from TOML front matter.
;; The root cause is in `org-export--get-subtree-options': it extracts the
;; heading text via `looking-at' + `match-string-no-properties', but does
;; not check if `looking-at' succeeded.  When the match fails (reason still
;; unknown), `match-string-no-properties' returns nil from stale match data.
;; tomelr then silently drops the nil-valued title key.
;;
;; This :around advice catches the nil *during* export, recovers the title
;; from the heading, and logs a warning for diagnosis.

(defun stafforini--ensure-title (orig-fn info)
  "Ensure `org-hugo--get-sanitized-title' never returns nil.
If ORIG-FN returns nil, extract the heading text directly.
Uses inline expansion of `org-with-wide-buffer' to avoid a
byte-compilation dependency on org-macs at compile time."
  (or (funcall orig-fn info)
      (save-excursion
        (save-restriction
          (widen)
          (org-back-to-heading t)
          (let ((title (or (let ((case-fold-search nil))
                             (when (looking-at org-complex-heading-regexp)
                               (match-string-no-properties 4)))
                           (org-get-heading t t t t))))
            (when (and title (not (string-empty-p (string-trim title))))
              (setq title (string-trim title))
              (display-warning
               'stafforini
               (format "ox-hugo title was nil — recovered: %S (buffer: %s, point: %d)"
                       title (buffer-name) (point))
               :warning)
              title))))))

(with-eval-after-load 'ox-hugo
  (advice-add 'org-hugo--get-sanitized-title :around
              #'stafforini--ensure-title))

;; Keep the post-export fixer as a safety net — it catches any edge cases
;; the :around advice misses (e.g. programmatic exports from other packages).
;; Also inject lastmod (and other front matter fixups) so that interactive
;; single-file exports don't lose metadata that ox-hugo doesn't set.

(defun stafforini--fixup-after-export (&rest _)
  "Apply front matter fixups after an interactive ox-hugo export.
Fixes missing titles (safety net) and injects lastmod, date, and
title markup via `inject-lastmod.py --file'.  Derives the exported
markdown file paths from the current org buffer context."
  (when-let* ((org-file (buffer-file-name))
              ((string-suffix-p ".org" org-file)))
    ;; Title safety net
    (when (fboundp 'export--fix-missing-titles)
      (export--fix-missing-titles org-file stafforini-hugo-dir "notes"))
    ;; Inject lastmod and other front matter fixups for each exported file
    (when (fboundp 'export--heading-export-pairs)
      (let ((script (expand-file-name "inject-lastmod.py" stafforini-scripts-dir)))
        (dolist (pair (export--heading-export-pairs org-file))
          (let ((md-path (expand-file-name
                          (format "content/notes/%s.md" (car pair))
                          stafforini-hugo-dir)))
            (when (file-exists-p md-path)
              (call-process "python3" nil nil nil script "--file" md-path))))))))

(with-eval-after-load 'ox-hugo
  (advice-add 'org-hugo-export-wim-to-md :after
              #'stafforini--fixup-after-export))

;;;; Org-transclusion support for interactive export

(defun stafforini--expand-transclusions-for-export (orig-fn &rest args)
  "Temporarily expand transclusions around ORIG-FN called with ARGS.
When `org-transclusion-mode' is not already active, check whether
the buffer contains #+transclude: directives.  If so, expand them
before export and remove them afterward."
  (let ((needs-expansion (stafforini--buffer-has-transclusions-p)))
    (when needs-expansion
      (stafforini--activate-transclusions))
    (unwind-protect
        (apply orig-fn args)
      (when needs-expansion
        (org-transclusion-remove-all)))))

(defun stafforini--buffer-has-transclusions-p ()
  "Return non-nil if the buffer has unexpanded #+transclude: directives."
  (and (not (bound-and-true-p org-transclusion-mode))
       (save-excursion
         (goto-char (point-min))
         (re-search-forward "^#\\+transclude:" nil t))))

(defun stafforini--activate-transclusions ()
  "Load `org-transclusion' and expand all directives in the buffer."
  (require 'org-transclusion)
  (org-transclusion-add-all))

(with-eval-after-load 'ox-hugo
  (advice-add 'org-hugo-export-wim-to-md :around
              #'stafforini--expand-transclusions-for-export))

;;;; Duplicate-drawer monitor

;; Catch PROPERTIES drawer duplication (a known corruption mode) in real time.

(defun stafforini--check-duplicate-drawers ()
  "Warn if the first heading has consecutive PROPERTIES drawers.
Added to `after-save-hook' in org buffers to catch corruption early."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when (re-search-forward "^\\*" nil t)
          (forward-line 1)
          (when (looking-at "[ \t]*:PROPERTIES:")
            ;; Skip past the first drawer
            (when (re-search-forward "^[ \t]*:END:" nil t)
              (forward-line 1)
              (when (looking-at "[ \t]*:PROPERTIES:")
                (display-warning
                 'stafforini
                 (format "Duplicate PROPERTIES drawer detected in %s at line %d!"
                         (buffer-name) (line-number-at-pos))
                 :error)))))))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'stafforini--check-duplicate-drawers nil t)))

;;;; #+lastmod: keyword stamping

;; Maintains the file-level `#+lastmod:' keyword as the authoritative
;; last-modification date for the stafforini.com pipeline.
;; inject-lastmod.py prefers this keyword over git history, which is
;; noisy: any commit touching a file bumps its date, including bulk
;; mechanical ones (property migrations, merge commits, etc.).

(defcustom stafforini-lastmod-directories
  (list (expand-file-name "~/My Drive/notes/")
        (expand-file-name "~/My Drive/bibliographic-notes/"))
  "Directories whose org files get `#+lastmod:' stamped on save.
Files outside these directories are left untouched."
  :type '(repeat directory)
  :group 'stafforini)

(defcustom stafforini-lastmod-skip-commands '()
  "Commands whose saves should NOT update `#+lastmod:'.
Extend this list to exclude bulk-mechanical operations (e.g.
property drawer migrations) that sweep many files without
representing real content edits."
  :type '(repeat symbol)
  :group 'stafforini)

(defun stafforini-maybe-stamp-lastmod ()
  "Stamp `#+lastmod:' on save when the current buffer qualifies.
Intended for `before-save-hook' in note org buffers.  No-op in
batch mode, outside note directories, or when `this-command' is
listed in `stafforini-lastmod-skip-commands'."
  (when (stafforini--should-stamp-lastmod-p)
    (stafforini--update-lastmod-keyword)))

(defun stafforini--should-stamp-lastmod-p ()
  "Return non-nil if the current save should update `#+lastmod:'."
  (and (not noninteractive)
       (derived-mode-p 'org-mode)
       (stafforini--note-file-p)
       (not (memq this-command stafforini-lastmod-skip-commands))))

(defun stafforini--note-file-p ()
  "Return non-nil if the buffer visits a file under a note directory."
  (when buffer-file-name
    (let ((file (expand-file-name buffer-file-name)))
      (seq-some (lambda (dir) (string-prefix-p (expand-file-name dir) file))
                stafforini-lastmod-directories))))

(defun stafforini--update-lastmod-keyword ()
  "Insert or update the `#+lastmod:' keyword with the current time.
Placed immediately after the first `#+title:' line if present,
otherwise at the top of the buffer."
  (save-excursion
    (save-restriction
      (widen)
      (let ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%S")))
        (goto-char (point-min))
        (cond
         ((re-search-forward "^#\\+lastmod:.*$" nil t)
          (replace-match (format "#+lastmod: %s" timestamp) t t))
         ((progn (goto-char (point-min))
                 (re-search-forward "^#\\+title:.*$" nil t))
          (end-of-line)
          (insert (format "\n#+lastmod: %s" timestamp)))
         (t
          (goto-char (point-min))
          (insert (format "#+lastmod: %s\n" timestamp))))))))

(defun stafforini--enable-lastmod-stamping ()
  "Attach `stafforini-maybe-stamp-lastmod' locally when visiting a note."
  (when (stafforini--note-file-p)
    (add-hook 'before-save-hook #'stafforini-maybe-stamp-lastmod nil t)))

(add-hook 'org-mode-hook #'stafforini--enable-lastmod-stamping)

;;;; Commands

;;;###autoload
(defun stafforini-publish-note ()
  "Add ox-hugo export metadata to the current org note.
Works on the current buffer: adds `#+hugo_base_dir' at the file
level and `:EXPORT_FILE_NAME:', `:EXPORT_HUGO_SECTION:', and
`:EXPORT_DATE:' properties to the first level-1 heading.
If `:EXPORT_FILE_NAME:' is already present, does nothing."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an org-mode buffer"))
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward ":EXPORT_FILE_NAME:" nil t)
        (message "Note is already published.")
      (stafforini--ensure-hugo-base-dir)
      ;; Find first level-1 heading's PROPERTIES drawer
      (goto-char (point-min))
      (unless (re-search-forward "^\\* " nil t)
        (user-error "No level-1 heading found"))
      (unless (re-search-forward "^[ \t]*:PROPERTIES:" nil t)
        (user-error "No PROPERTIES drawer found under the first heading"))
      (unless (re-search-forward "^[ \t]*:END:" nil t)
        (user-error "Unterminated PROPERTIES drawer"))
      ;; Insert export properties before :END:
      (beginning-of-line)
      (let ((slug (file-name-sans-extension
                   (file-name-nondirectory (buffer-file-name))))
            (date (format-time-string "%Y-%m-%d")))
        (insert (format "  :EXPORT_FILE_NAME: %s\n" slug))
        (insert "  :EXPORT_HUGO_SECTION: notes\n")
        (insert (format "  :EXPORT_DATE: %s\n" date)))
      (save-buffer)
      (message "Note published (slug: %s)."
               (file-name-sans-extension
                (file-name-nondirectory (buffer-file-name)))))))

(declare-function org-entry-get "org")
(declare-function org-entry-put "org")
(declare-function org-entry-delete "org")
(declare-function org-back-to-heading "org")
(declare-function org-toggle-tag "org")
(declare-function org-get-heading "org")

;;;; Quote publishing

;;;###autoload
(defun stafforini-publish-quote ()
  "Add ox-hugo export metadata to the heading at point for diary quote export.
Add `:public:' tag and `:EXPORT_FILE_NAME:', `:EXPORT_HUGO_SECTION:',
`:EXPORT_DATE:', and `:EXPORT_HUGO_CUSTOM_FRONT_MATTER:' properties.
If `:EXPORT_FILE_NAME:' is already present, do nothing."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an org-mode buffer"))
  (save-excursion
    (org-back-to-heading t)
    (when (org-entry-get nil "EXPORT_FILE_NAME")
      (user-error "Quote is already published"))
    (stafforini--ensure-hugo-base-dir)
    (let* ((cite-key (stafforini--file-cite-key))
           (author (stafforini--cite-key-author cite-key))
           (work-slug (stafforini--cite-key-to-slug cite-key))
           (title (org-get-heading t t t t))
           (slug (concat author "-" (stafforini--slugify title)))
           (locator (stafforini--heading-quote-locator))
           (date (format-time-string "%Y-%m-%d"))
           (fm-pairs (list (cons "work" (format "\"%s\"" work-slug)))))
      (when locator
        (push (cons "locator" (format "\"%s\"" locator)) fm-pairs)
        (setq fm-pairs (nreverse fm-pairs)))
      (org-toggle-tag "public" 'on)
      (org-entry-put nil "EXPORT_FILE_NAME" slug)
      (org-entry-put nil "EXPORT_HUGO_SECTION" "quotes")
      (org-entry-put nil "EXPORT_DATE" date)
      (org-entry-put nil "EXPORT_HUGO_CUSTOM_FRONT_MATTER"
                     (stafforini--build-hugo-custom-fm fm-pairs))
      (save-buffer)
      (message "Quote published (slug: %s)." slug))))

(defun stafforini--ensure-hugo-base-dir ()
  "Insert `#+hugo_base_dir' at the top of the buffer if missing."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^#\\+hugo_base_dir:" nil t)
      (goto-char (point-min))
      (if (re-search-forward "^#\\+title:" nil t)
          (end-of-line)
        (goto-char (point-min)))
      (insert (format "\n#+hugo_base_dir: %s"
                      (abbreviate-file-name stafforini-hugo-dir))))))

(defun stafforini--file-cite-key ()
  "Return the cite key from ROAM_REFS on the file's level-1 heading."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^\\* " nil t)
      (user-error "No level-1 heading found"))
    (let ((refs (org-entry-get nil "ROAM_REFS")))
      (unless refs
        (user-error "No ROAM_REFS property found"))
      (if (string-match "@\\([A-Za-z0-9]+\\)" refs)
          (match-string 1 refs)
        (user-error "No cite key found in ROAM_REFS")))))

(defun stafforini--cite-key-author (cite-key)
  "Extract the lowercased author surname from CITE-KEY.
E.g. \"Pinker2018EnlightenmentNowCase\" → \"pinker\"."
  (if (string-match "\\`\\([A-Za-z]+?\\)[0-9]" cite-key)
      (downcase (match-string 1 cite-key))
    (user-error "Cannot extract author from cite key: %s" cite-key)))

(defun stafforini--cite-key-to-slug (cite-key)
  "Convert CamelCase CITE-KEY to a kebab-case work slug.
E.g. \"Pinker2018EnlightenmentNowCase\" →
\"pinker-2018-enlightenment-now-case\"."
  (let ((case-fold-search nil))
    (thread-last cite-key
      (replace-regexp-in-string "\\([a-zA-Z]\\)\\([0-9]\\)" "\\1-\\2")
      (replace-regexp-in-string "\\([0-9]\\)\\([a-zA-Z]\\)" "\\1-\\2")
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1-\\2")
      (downcase))))

(defun stafforini--heading-quote-locator ()
  "Return the cite locator from the blockquote under the heading at point.
Search for a [cite:@KEY, LOCATOR] line after #+end_quote.  Return
LOCATOR as a string, or nil if not found."
  (save-excursion
    (org-back-to-heading t)
    (let ((bound (save-excursion
                   (outline-next-heading)
                   (or (point) (point-max)))))
      (when (re-search-forward "#\\+end_quote" bound t)
        (when (re-search-forward "\\[cite:@[^],]+,\\s-*\\(.+?\\)\\]" bound t)
          (match-string 1))))))

;;;; Hugo custom front matter

(defcustom stafforini-hugo-custom-properties
  '(("is_tag" . "true")
    ("unlisted" . "true"))
  "Alist of available Hugo custom front matter properties.
Each entry is (KEY . VALUE) where KEY is the front matter field name
and VALUE is the value to set."
  :type '(alist :key-type string :value-type string)
  :group 'stafforini)

(defun stafforini--parse-hugo-custom-fm ()
  "Parse EXPORT_HUGO_CUSTOM_FRONT_MATTER from the heading at point.
Return an alist of (KEY . VALUE) pairs."
  (when-let* ((raw (org-entry-get nil "EXPORT_HUGO_CUSTOM_FRONT_MATTER")))
    (let ((plist (ignore-errors (car (read-from-string (concat "(" raw ")")))))
          result)
      (while plist
        (when (keywordp (car plist))
          (push (cons (substring (symbol-name (car plist)) 1) ; strip leading :
                      (format "%s" (cadr plist)))
                result))
        (setq plist (cddr plist)))
      (nreverse result))))

(defun stafforini--build-hugo-custom-fm (alist)
  "Build an EXPORT_HUGO_CUSTOM_FRONT_MATTER value string from ALIST."
  (mapconcat (lambda (pair)
               (format ":%s %s" (car pair) (cdr pair)))
             alist " "))

;;;###autoload
(defun stafforini-set-hugo-property ()
  "Toggle a Hugo custom front matter property on the heading at point.
If only one property is defined in `stafforini-hugo-custom-properties',
toggle it directly.  Otherwise, prompt the user to select a property.

When the selected property is already set, remove it; otherwise add it."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an org-mode buffer"))
  (save-excursion
    (org-back-to-heading t)
    (let* ((available stafforini-hugo-custom-properties)
           (current (stafforini--parse-hugo-custom-fm))
           (key (if (length= available 1)
                    (caar available)
                  (completing-read
                   "Property: " (mapcar #'car available) nil t)))
           (value (cdr (assoc key available)))
           (already-set (assoc key current)))
      (if already-set
          (let ((new-alist (assoc-delete-all key (copy-alist current))))
            (if new-alist
                (org-entry-put nil "EXPORT_HUGO_CUSTOM_FRONT_MATTER"
                               (stafforini--build-hugo-custom-fm new-alist))
              (org-entry-delete nil "EXPORT_HUGO_CUSTOM_FRONT_MATTER"))
            (message "Removed `%s' from heading." key))
        (let ((new-alist (append current (list (cons key value)))))
          (org-entry-put nil "EXPORT_HUGO_CUSTOM_FRONT_MATTER"
                         (stafforini--build-hugo-custom-fm new-alist))
          (message "Set `%s = %s' on heading." key value))))))

;;;###autoload
(defun stafforini-export-all-notes ()
  "Export all org notes to Hugo markdown and rebuild the search index.
Stop the Hugo dev server before exporting to avoid overwhelming it
with file-change rebuilds, and restart it on success."
  (interactive)
  (stafforini-stop-server)
  (stafforini--run-script "export-notes.sh" nil #'stafforini-start-server))

;;;###autoload
(defun stafforini-export-all-quotes (&optional full)
  "Export all org quotes to Hugo markdown and rebuild the search index.
With prefix argument FULL, force a complete re-export ignoring the
manifest.  Stop the Hugo dev server before exporting and restart it
on success."
  (interactive "P")
  (stafforini-stop-server)
  (stafforini--compile
   (concat (stafforini--script-command "export-quotes.sh"
                                       (when full "--full"))
           " && " (stafforini--script-command "build-search-index.sh"))
   "*stafforini-export-quotes*"
   #'stafforini-start-server))

;;;###autoload
(defun stafforini-update-works ()
  "Generate or update work pages from BibTeX data.
Restarts the Hugo dev server on success."
  (interactive)
  (stafforini--run-script "generate-work-pages.py" "--skip-postprocess"
                          #'stafforini-start-server))

;;;; Takedown blocklist

(defcustom stafforini-excluded-works-file
  (file-name-concat stafforini-hugo-dir "data/excluded-works.json")
  "Path to the takedown blocklist JSON file.
Entries listed here are suppressed from the published site: no work
page, no quote pages citing the work, no PDF on R2, and no citation
hyperlink anywhere on the site."
  :type 'file
  :group 'stafforini)

(declare-function citar-select-ref "citar")

;;;###autoload
(defun stafforini-exclude-work (cite-key &optional reason)
  "Add CITE-KEY to the site's takedown blocklist with optional REASON.
Appends an entry with the current date under `added' and, if REASON
is a non-empty string, under `reason' to
`stafforini-excluded-works-file'.  After the JSON is written, offers
to run the takedown pipeline so the exclusion takes effect on the
next build.

Interactively, prompts for CITE-KEY via `citar-select-ref' (the same
completing-read interface used by `citar-insert-citation'), filtered
to exclude keys already on the blocklist, then prompts for REASON.
Leaving REASON empty records no reason field."
  (interactive (stafforini--exclude-work-prompt))
  (when (or (null cite-key) (string-empty-p cite-key))
    (user-error "Cite key is required"))
  (let* ((existing (stafforini--read-excluded-works))
         (fields (stafforini--build-exclusion-fields reason))
         (updated (cons (cons cite-key fields)
                        (assoc-delete-all cite-key (copy-sequence existing)))))
    (stafforini--write-excluded-works updated)
    (message "Recorded takedown for %s%s"
             cite-key
             (if reason (format " (%s)" reason) ""))
    (when (y-or-n-p "Run takedown pipeline now (generate-work-pages + verify-site)? ")
      (stafforini--apply-takedowns))))

(defun stafforini--exclude-work-prompt ()
  "Read CITE-KEY and optional REASON for `stafforini-exclude-work'.
Returns a list suitable for `interactive' splicing."
  (require 'citar)
  (let* ((already (stafforini--excluded-cite-keys))
         (key (citar-select-ref
               :filter (lambda (citekey) (not (member citekey already)))))
         (raw (read-string "Reason (optional): "))
         (reason (let ((trimmed (string-trim raw)))
                   (and (not (string-empty-p trimmed)) trimmed))))
    (list key reason)))

(defun stafforini--excluded-cite-keys ()
  "Return the list of cite keys currently on the takedown blocklist."
  (mapcar #'car (stafforini--read-excluded-works)))

(defun stafforini--build-exclusion-fields (reason)
  "Return the alist of JSON fields to store for a new exclusion.
REASON, if non-nil, is added under the `reason' key.  Today's date
is always added under `added'."
  (append (list (cons "added" (format-time-string "%Y-%m-%d")))
          (when reason (list (cons "reason" reason)))))

(defun stafforini--read-excluded-works ()
  "Return the takedown blocklist as an alist.
Each element is (CITE-KEY . FIELDS-ALIST), both using string keys.
Returns nil if the blocklist file is missing or empty."
  (require 'json)
  (let ((file stafforini-excluded-works-file))
    (when (and (file-exists-p file)
               (> (file-attribute-size (file-attributes file)) 0))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((text (string-trim (buffer-string))))
          (unless (string-empty-p text)
            (stafforini--hash-table-to-alist
             (json-parse-string text :object-type 'hash-table
                                :null-object nil :false-object nil))))))))

(defun stafforini--hash-table-to-alist (table)
  "Recursively convert TABLE to an alist with string keys."
  (let (result)
    (maphash (lambda (key value)
               (push (cons key
                           (if (hash-table-p value)
                               (stafforini--hash-table-to-alist value)
                             value))
                     result))
             table)
    result))

(defun stafforini--write-excluded-works (alist)
  "Write ALIST to `stafforini-excluded-works-file' as pretty JSON.
Both top-level and nested entries are serialized with keys sorted
alphabetically so diffs stay stable across runs."
  (require 'json)
  (with-temp-file stafforini-excluded-works-file
    (stafforini--insert-exclusion-json alist)))

(defun stafforini--insert-exclusion-json (alist)
  "Insert ALIST into the current buffer as pretty-printed blocklist JSON."
  (let ((sorted (stafforini--sort-alist-by-key alist)))
    (insert "{\n")
    (stafforini--insert-comma-separated sorted #'stafforini--insert-exclusion-entry)
    (insert "\n}\n")))

(defun stafforini--insert-exclusion-entry (entry)
  "Insert a single top-level blocklist ENTRY (CITE-KEY . FIELDS-ALIST)."
  (let ((fields (stafforini--sort-alist-by-key (cdr entry))))
    (insert "  " (json-encode-string (car entry)) ": {\n")
    (stafforini--insert-comma-separated fields #'stafforini--insert-exclusion-field)
    (insert "\n  }")))

(defun stafforini--insert-exclusion-field (field)
  "Insert one key/value FIELD of a blocklist entry into the current buffer."
  (insert "    " (json-encode-string (car field))
          ": " (json-encode-string (cdr field))))

(defun stafforini--insert-comma-separated (items insert-fn)
  "Apply INSERT-FN to each of ITEMS, separating successive inserts with a comma."
  (let ((first t))
    (dolist (item items)
      (if first (setq first nil) (insert ",\n"))
      (funcall insert-fn item))))

(defun stafforini--sort-alist-by-key (alist)
  "Return a copy of ALIST sorted by car using `string<'."
  (sort (copy-sequence alist)
        (lambda (a b) (string< (car a) (car b)))))

(defun stafforini--apply-takedowns ()
  "Run the local takedown pipeline and offer to deploy on success.
Runs `generate-work-pages.py' (rewrites works.json, removes work
pages, drops quote markdown files whose work is excluded), then
`process-pdfs.py' (removes local PDFs and thumbnails), then
`verify-site.py' against the dev build as a safety gate.  Once the
local state matches the blocklist, prompts to trigger a full deploy
so the removals reach production (Netlify HTML + R2 PDF bucket)."
  (stafforini--compile
   (concat (stafforini--script-command "generate-work-pages.py")
           " && "
           (stafforini--script-command "process-pdfs.py")
           " && "
           (stafforini--script-command "verify-site.py") " --build dev")
   "*stafforini-apply-takedowns*"
   #'stafforini--maybe-deploy-takedown))

(defun stafforini--maybe-deploy-takedown ()
  "Prompt after a successful takedown pipeline to trigger a full deploy."
  (when (y-or-n-p "Takedown applied locally.  Deploy to Netlify + R2 now? ")
    (stafforini-deploy)))

;;;###autoload
(defun stafforini-update-backlinks ()
  "Regenerate backlink data from the org-roam database.
Restarts the Hugo dev server on success."
  (interactive)
  (stafforini--run-script "generate-backlinks.py" nil #'stafforini-start-server))

;;;###autoload
(defun stafforini-process-pdfs ()
  "Strip annotations from PDFs and generate first-page thumbnails."
  (interactive)
  (stafforini--run-script "process-pdfs.py"))

;;;###autoload
(defun stafforini-generate-id-slug-map ()
  "Generate the org-id to Hugo slug JSON mapping.
Writes /tmp/id-slug-map.json, used by quote export to resolve
topic links."
  (interactive)
  (stafforini--run-script "generate-id-slug-map.py"))

;;;###autoload
(defun stafforini-generate-topic-pages ()
  "Generate Hugo content pages for org-roam topic stubs."
  (interactive)
  (stafforini--run-script "generate-topic-pages.py"))

;;;###autoload
(defun stafforini-generate-citing-notes ()
  "Generate the citing-notes reverse index from cite shortcodes."
  (interactive)
  (stafforini--run-script "generate-citing-notes.py"))


;;;###autoload
(defun stafforini-inject-lastmod ()
  "Inject lastmod dates from org file modification times into Hugo markdown."
  (interactive)
  (stafforini--run-script "inject-lastmod.py"))

;;;###autoload
(defun stafforini-deploy (&optional quick)
  "Build the Hugo site and deploy it to Netlify.
With prefix argument QUICK, skip data regeneration and PDF
processing (just clean, build, index, deploy)."
  (interactive "P")
  (stafforini--run-script "deploy.sh" (when quick "--quick")))

;;;###autoload
(defun stafforini-start-server ()
  "Start the Hugo dev server in a dedicated `*hugo-server*' buffer.
Always kills any existing server first to ensure a fresh build.
Hugo's incremental rebuild does not track cross-page shortcode
dependencies, so reusing a running server after content changes
causes stale data."
  (interactive)
  (let ((buf (get-buffer "*hugo-server*")))
    (when buf
      (let ((proc (get-buffer-process buf)))
        (when proc (delete-process proc)))
      (kill-buffer buf)))
  (let ((default-directory stafforini-hugo-dir))
    (async-shell-command "npm run dev" "*hugo-server*")))

;;;###autoload
(defun stafforini-stop-server ()
  "Stop the Hugo dev server."
  (interactive)
  (let* ((buf (get-buffer "*hugo-server*"))
         (proc (and buf (get-buffer-process buf))))
    (if proc
        (progn
          (interrupt-process proc)
          (message "Hugo server stopped."))
      (message "Hugo server is not running."))))

;;;###autoload
(defun stafforini-rebuild-search-index ()
  "Rebuild the Pagefind search index for local development."
  (interactive)
  (stafforini--run-script "build-search-index.sh"))

;;;###autoload
(defun stafforini-full-rebuild ()
  "Run the full build pipeline sequentially.
Steps: export notes (full), export quotes (full), process PDFs,
clean public, hugo build, pagefind index.
The export scripts handle intermediate steps (inject-lastmod,
backlinks, citing-notes, id-slug-map, work-pages, topic-pages)."
  (interactive)
  (let ((default-directory stafforini-hugo-dir))
    (stafforini--compile
     (mapconcat
      #'identity
      (list
       (stafforini--script-command "export-notes.sh" "--full")
       (stafforini--script-command "export-quotes.sh" "--full")
       (stafforini--script-command "process-pdfs.py")
       "find public -mindepth 1 -delete 2>/dev/null || true"
       "hugo --minify"
       "npx pagefind --site public")
      " && ")
     "*stafforini-full-rebuild*")))

;;;; Transient menu

;;;###autoload (autoload 'stafforini-menu "stafforini" nil t)
(transient-define-prefix stafforini-menu ()
  "Stafforini.com build commands."
  [["Export"
    ("N" "Export notes" stafforini-export-all-notes)
    ("Q" "Export quotes" stafforini-export-all-quotes)
    ""
    "Publish"
    ("n" "Publish note" stafforini-publish-note)
    ("q" "Publish quote" stafforini-publish-quote)
    ("x" "Exclude work" stafforini-exclude-work)]
   ["Generate"
    ("F" "Set Hugo property" stafforini-set-hugo-property)
    ("w" "Update works" stafforini-update-works)
    ("b" "Update backlinks" stafforini-update-backlinks)
    ("d" "Process PDFs" stafforini-process-pdfs)]
   ["Auxiliary"
    ("m" "ID-slug map" stafforini-generate-id-slug-map)
    ("t" "Topic pages" stafforini-generate-topic-pages)
    ("c" "Citing notes" stafforini-generate-citing-notes)
    ("l" "Inject lastmod" stafforini-inject-lastmod)
    ]
   ["Build & deploy"
    ("R" "Full rebuild" stafforini-full-rebuild)
    ("i" "Rebuild search index" stafforini-rebuild-search-index)
    ("D" "Deploy to Netlify" stafforini-deploy)]
   ["Server"
    ("s" "Start server" stafforini-start-server)
    ("k" "Stop server" stafforini-stop-server)
    ""
    "Insert"
    ("I" "Insert image" stafforini-insert-image)
    ("T" "Insert topics" stafforini-insert-topics)]])

;;;; Image insertion

(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-context)
(defvar gptel-use-context)
(defvar gptel-tools)
(defvar gptel-use-tools)
(defvar gptel-include-reasoning)
(defvar gptel--known-backends)

(declare-function gptel-request "gptel")
(declare-function gptel--model-capable-p "gptel")
(declare-function mailcap-file-name-to-mime-type "mailcap")
(declare-function org-display-inline-images "org")

(defcustom stafforini-image-description-model
  '("Gemini" . gemini-flash-latest)
  "Model to use for image description.
The value is a cons cell whose car is the backend name and whose cdr is the
model symbol.  See `gptel-extras-ai-models' for available options.
If nil, use the currently active gptel model."
  :type '(choice (const :tag "Use current model" nil)
                 (cons (string :tag "Backend") (symbol :tag "Model")))
  :group 'stafforini)

(defconst stafforini--describe-image-prompt
  "Describe the attached image. Respond with EXACTLY two lines, nothing else:
SHORT: <2-5 word description for filename, lowercase, no punctuation>
ALT: <1-2 sentence alt text, max 50 words>"
  "Prompt for generating short and long image descriptions via AI.")

(defun stafforini--get-article-slug ()
  "Get the article slug for the current org buffer.
The slug is the file name stem, which matches the `:EXPORT_FILE_NAME:' property
used by ox-hugo."
  (if-let ((file (buffer-file-name)))
      (file-name-sans-extension (file-name-nondirectory file))
    (user-error "Buffer is not visiting a file")))

(defun stafforini--slugify (string)
  "Convert STRING to a kebab-case slug suitable for a filename."
  (thread-last string
    (downcase)
    (replace-regexp-in-string "[^a-z0-9 ]" "")
    (string-trim)
    (replace-regexp-in-string " +" "-")))

(defun stafforini--save-clipboard-image ()
  "Save the system clipboard image to a temporary file.
Return the temporary file path."
  (unless (executable-find "pngpaste")
    (user-error "`pngpaste' not found; install it with `brew install pngpaste'"))
  (let ((temp-file (make-temp-file "stafforini-image-" nil ".png")))
    (unless (zerop (call-process "pngpaste" nil nil nil temp-file))
      (delete-file temp-file)
      (user-error "No image found on the clipboard"))
    temp-file))

(defun stafforini--unique-file-path (path)
  "Return PATH if it doesn't exist, otherwise append a numeric suffix."
  (if (not (file-exists-p path))
      path
    (let* ((dir (file-name-directory path))
           (base (file-name-sans-extension (file-name-nondirectory path)))
           (ext (file-name-extension path))
           (counter 1)
           new-path)
      (while (file-exists-p
              (setq new-path (file-name-concat
                              dir (format "%s-%d.%s" base counter ext))))
        (setq counter (1+ counter)))
      new-path)))

(defun stafforini--parse-image-descriptions (response)
  "Parse RESPONSE into (SHORT-NAME . ALT-TEXT).
RESPONSE should contain lines matching \"SHORT: ...\" and \"ALT: ...\"."
  (let (short alt)
    (when (string-match "SHORT:[ \t]*\\([^\n]+\\)" response)
      (setq short (string-trim (match-string 1 response))))
    (when (string-match "ALT:[ \t]*\\([^\n]+\\)" response)
      (setq alt (string-trim (match-string 1 response))))
    (cons short alt)))

(defun stafforini--describe-image (file callback)
  "Describe image in FILE using AI.
CALLBACK is called with two arguments: SHORT-NAME and ALT-TEXT.
Uses `gptel' with the image as context via a let-binding, so the global
`gptel-context' is not disturbed.  The model used is controlled by
`stafforini-image-description-model'."
  (require 'gptel)
  (let* ((model-spec stafforini-image-description-model)
         (gptel-backend (if model-spec
                            (or (alist-get (car model-spec) gptel--known-backends
                                          nil nil #'string=)
                                (user-error "Backend %S not found in gptel"
                                            (car model-spec)))
                          gptel-backend))
         (gptel-model (if model-spec (cdr model-spec) gptel-model)))
    (unless (gptel--model-capable-p 'media)
      (user-error "Model %s does not support images; select a vision-capable model"
                  gptel-model))
    (let ((gptel-context (list (list (expand-file-name file)
                                     :mime (mailcap-file-name-to-mime-type file))))
          (gptel-use-context 'user)
          (gptel-tools nil)
          (gptel-use-tools nil)
          (gptel-include-reasoning nil))
      (gptel-request stafforini--describe-image-prompt
        :callback
        (lambda (response info)
          (if response
              (let* ((parsed (stafforini--parse-image-descriptions response))
                     (short (or (car parsed) "image"))
                     (alt (or (cdr parsed) "Image")))
                (funcall callback short alt))
            (when (and file (string-prefix-p (temporary-file-directory) file))
              (delete-file file t))
            (user-error "Image description failed: %s"
                        (plist-get info :status))))))))

;;;###autoload
(defun stafforini-insert-image (&optional file)
  "Insert an image at point in an org buffer.
The image is stored in `paths-dir-org-images' under a subdirectory named after
the article slug, with an AI-generated descriptive filename.

If FILE is non-nil, use it directly.  Otherwise, prompt the user to choose
between pasting from the clipboard or selecting a file.

Two AI-generated descriptions are produced: a short one for the filename and
a longer one for the alt text.  Both are presented for editing before use."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command must be run in an org-mode buffer"))
  (let* ((source (if file 'file
                   (intern (completing-read "Image source: "
                                           '("clipboard" "file") nil t))))
         (image-file (pcase source
                       ('file (or file (read-file-name "Image file: " nil nil t)))
                       ('clipboard (stafforini--save-clipboard-image))))
         (ext (file-name-extension image-file))
         (slug (stafforini--get-article-slug))
         (buf (current-buffer))
         (pos (point-marker)))
    (stafforini--describe-image
     image-file
     (lambda (short-name alt-text)
       (let* ((short-name (read-string "Filename: " short-name))
              (alt-text (read-string "Alt text: " alt-text))
              (dest-dir (file-name-concat paths-dir-org-images slug))
              (dest-file (stafforini--unique-file-path
                          (file-name-concat
                           dest-dir
                           (concat (stafforini--slugify short-name) "." ext)))))
         (make-directory dest-dir t)
         (copy-file image-file dest-file)
         (when (eq source 'clipboard)
           (delete-file image-file))
         (with-current-buffer buf
           (save-excursion
             (goto-char pos)
             (unless (bolp) (insert "\n"))
             (insert (format "#+attr_html: :alt %s\n[[file:%s]]\n"
                             alt-text dest-file))
             (org-display-inline-images
              nil nil (line-beginning-position -2) (point)))))))))

;;;; Topic insertion

(declare-function org-roam-node-read "org-roam-node")
(declare-function org-roam-node-id "org-roam-node")
(declare-function org-roam-node-title "org-roam-node")

;;;###autoload
(defun stafforini-insert-topics ()
  "Insert a :TOPICS: property with tags selected from known tags.
Reads completion candidates from `stafforini-tags-file' and
prompts with `completing-read-multiple' (comma-separated).
The collected tags are inserted as a middot-separated :TOPICS:
property on the current heading."
  (interactive)
  (let ((tags (stafforini-insert-topics--select)))
    (if (null tags)
        (message "No topics selected.")
      (stafforini-insert-topics--write
       (sort tags #'stafforini-insert-topics--sort-pred)))))

(defcustom stafforini-tags-file
  (expand-file-name
   "~/My Drive/repos/stafforini.com/data/all-tags.json")
  "Path to the JSON file listing all known tags."
  :type 'file
  :group 'stafforini)

(defun stafforini-insert-topics--read-tags ()
  "Return a list of known tag strings from `stafforini-tags-file'."
  (if (file-exists-p stafforini-tags-file)
      (json-parse-string
       (with-temp-buffer
         (insert-file-contents stafforini-tags-file)
         (buffer-string))
       :array-type 'list)
    (user-error "Tags file not found: %s" stafforini-tags-file)))

(defun stafforini-insert-topics--select ()
  "Prompt the user to select tags via `completing-read-multiple'."
  (let ((candidates (stafforini-insert-topics--read-tags)))
    (completing-read-multiple "Select topics (comma-separated): " candidates)))

(defun stafforini-insert-topics--sort-pred (a b)
  "Case-insensitive sort predicate for tag strings A and B."
  (string< (downcase a) (downcase b)))

(defun stafforini-insert-topics--write (tags)
  "Insert TAGS as a :TOPICS: property on the current heading."
  (let ((value (mapconcat #'identity tags " · ")))
    (org-entry-put nil "TOPICS" value)))

(provide 'stafforini)
;;; stafforini.el ends here
