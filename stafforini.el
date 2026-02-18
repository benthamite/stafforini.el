;;; stafforini.el --- Build commands for stafforini.com -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/stafforini.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (paths "0.1") (gptel "0.9"))

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

(defun stafforini--compile (command &optional name)
  "Run COMMAND asynchronously in a compilation buffer.
NAME, if non-nil, is used as the compilation buffer name."
  (let ((default-directory stafforini-hugo-dir)
        (compilation-buffer-name-function
         (when name
           (lambda (_mode) name))))
    (compile command)))

(defun stafforini--reindex-suffix ()
  "Return a shell command fragment that rebuilds the search index."
  (format " && bash %s"
          (shell-quote-argument
           (expand-file-name "build-search-index.sh"
                             stafforini-scripts-dir))))

;;;; Commands

;;;###autoload
(defun stafforini-prepare-notes ()
  "Add ox-hugo metadata to new org note files."
  (interactive)
  (stafforini--compile
   (format "python %s" (shell-quote-argument
                        (expand-file-name "prepare-org-notes.py"
                                          stafforini-scripts-dir)))
   "*stafforini-prepare-notes*"))

;;;###autoload
(defun stafforini-export-all-notes ()
  "Export all org notes to Hugo markdown and rebuild the search index."
  (interactive)
  (stafforini--compile
   (concat
    (format "emacs --batch -l %s" (shell-quote-argument
                                   (expand-file-name "export-notes.el"
                                                     stafforini-scripts-dir)))
    (stafforini--reindex-suffix))
   "*stafforini-export-notes*"))

;;;###autoload
(defun stafforini-export-all-quotes ()
  "Export all org quotes to Hugo markdown and rebuild the search index."
  (interactive)
  (stafforini--compile
   (concat
    (format "bash %s" (shell-quote-argument
                       (expand-file-name "export-quotes.sh"
                                         stafforini-scripts-dir)))
    (stafforini--reindex-suffix))
   "*stafforini-export-quotes*"))

;;;###autoload
(defun stafforini-update-works ()
  "Generate or update work pages from BibTeX data and rebuild the search index."
  (interactive)
  (stafforini--compile
   (concat
    (format "python %s --skip-postprocess" (shell-quote-argument
                                            (expand-file-name "generate-work-pages.py"
                                                              stafforini-scripts-dir)))
    (stafforini--reindex-suffix))
   "*stafforini-update-works*"))

;;;###autoload
(defun stafforini-update-backlinks ()
  "Regenerate backlink data from the org-roam database."
  (interactive)
  (stafforini--compile
   (format "python %s" (shell-quote-argument
                        (expand-file-name "generate-backlinks.py"
                                          stafforini-scripts-dir)))
   "*stafforini-update-backlinks*"))

;;;###autoload
(defun stafforini-start-server ()
  "Start the Hugo dev server, or switch to its buffer if already running.
The server runs in a dedicated `*hugo-server*' buffer."
  (interactive)
  (let ((buf (get-buffer "*hugo-server*")))
    (if (and buf (get-buffer-process buf))
        (display-buffer buf)
      (let ((default-directory stafforini-hugo-dir))
        (async-shell-command "npm run dev" "*hugo-server*")))))

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
  (stafforini--compile
   (format "bash %s" (shell-quote-argument
                      (expand-file-name "build-search-index.sh"
                                        stafforini-scripts-dir)))
   "*stafforini-search-index*"))

;;;###autoload
(defun stafforini-full-rebuild ()
  "Run the full build pipeline sequentially.
Steps: prepare notes, export notes, export quotes, update work
pages, update backlinks, build search index."
  (interactive)
  (let ((default-directory stafforini-hugo-dir))
    (stafforini--compile
     (mapconcat
      #'identity
      (list
       (format "python %s" (shell-quote-argument
                            (expand-file-name "prepare-org-notes.py"
                                              stafforini-scripts-dir)))
       (format "emacs --batch -l %s" (shell-quote-argument
                                      (expand-file-name "export-notes.el"
                                                        stafforini-scripts-dir)))
       (format "bash %s" (shell-quote-argument
                          (expand-file-name "export-quotes.sh"
                                            stafforini-scripts-dir)))
       (format "python %s" (shell-quote-argument
                            (expand-file-name "generate-work-pages.py"
                                              stafforini-scripts-dir)))
       (format "python %s" (shell-quote-argument
                            (expand-file-name "generate-backlinks.py"
                                              stafforini-scripts-dir)))
       (format "bash %s" (shell-quote-argument
                          (expand-file-name "build-search-index.sh"
                                            stafforini-scripts-dir))))
      " && ")
     "*stafforini-full-rebuild*")))

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

(provide 'stafforini)
;;; stafforini.el ends here
