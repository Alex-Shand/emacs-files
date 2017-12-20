(provide 'modules-code)

;; Add to language mode hook to auto-wrap comments at 80 characters
(defun comment-auto-fill ()
  ;; Only autofill comments
  (setq-local comment-auto-fill-only-comments t)
  ;; Enable autofill mode
  (auto-fill-mode 1)
  ;; Set the wrap point to 80 characters
  (set-fill-column 80))

;; Setup autoinsert for specific files
(use-package autoinsert
  :init
  ;; Directory where the templates will be stored
  (defvar template-dir "/home/alex/.emacs.d/templates/")
  ;; Don't prompt before insertion
  (setq auto-insert-query nil)
  ;; Tell auto-insert where the templates are
  (setq auto-insert-directory template-dir)
  ;; Run auto-insert when a new file is opened
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)

  :config
  ;; Map discarding the return values of the function
  (mapc
   (lambda (x)
     ;; The template file should end in the same extension as the target file
     ;; type, if this is the case this defines an auto-insert rule matching any
     ;; filename ending in the correct extension
     (define-auto-insert (format "\\.%s$" (file-name-extension x)) x))
   ;; Map but produce a list of the function return values
   (mapcar
    ;; Chop off the directory that directory-files returns with each entry
    'file-name-nondirectory
    ;; List files in the template directory ignoring . and ..
    (directory-files template-dir t directory-files-no-dot-files-regexp))))

;; Enable comment-auto-fill for all modes derived from prog-mode (Should be all
;; programming modes)
(add-hook 'prog-mode-hook 'comment-auto-fill)

;; Perl:

;; Replace perl-mode with cperl-mode
(defalias 'perl-mode 'cperl-mode)

;; Indent with 4 spaces
(setq cperl-indent-level 4)

;; Python:

;; The only way this seems to work is inside a hook
(add-hook 'python-mode-hook '(lambda ()
			       ;; Indent with 4 spaces
			       (setq python-indent 4)
			       ;; Don't attempt to guess indent level in existing python files
			       (setq python-guess-indent nil)))

;; C:

;; Set C style to linux and indent with 4 spaces (Also affects C++)
(setq-default c-default-style "linux"
              c-basic-offset 4)
