;;; stafforini.el --- Build commands for stafforini.com -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/stafforini.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (paths "0.1"))

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
  "Export all org notes to Hugo markdown in batch mode."
  (interactive)
  (stafforini--compile
   (format "emacs --batch -l %s" (shell-quote-argument
                                  (expand-file-name "export-notes.el"
                                                    stafforini-scripts-dir)))
   "*stafforini-export-notes*"))

;;;###autoload
(defun stafforini-export-all-quotes ()
  "Export all org quotes to Hugo markdown in batch mode."
  (interactive)
  (stafforini--compile
   (format "bash %s" (shell-quote-argument
                      (expand-file-name "export-quotes.sh"
                                        stafforini-scripts-dir)))
   "*stafforini-export-quotes*"))

;;;###autoload
(defun stafforini-update-works ()
  "Generate or update work pages from BibTeX data."
  (interactive)
  (stafforini--compile
   (format "python %s --skip-postprocess" (shell-quote-argument
                                           (expand-file-name "generate-work-pages.py"
                                                             stafforini-scripts-dir)))
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
(defun stafforini-full-rebuild ()
  "Run the full build pipeline sequentially.
Steps: prepare notes, export notes, export quotes, update work
pages, update backlinks, hugo build, pagefind index."
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
       "hugo --minify"
       "npx pagefind --site public")
      " && ")
     "*stafforini-full-rebuild*")))

(provide 'stafforini)
;;; stafforini.el ends here
