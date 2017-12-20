;; General setup, experimental settings and imports for other modules

;; Use the full functionality of common lisp
(require 'cl)

;; Add ~/.emacs.d/modules/ to the load path
(add-to-list 'load-path "/home/alex/.emacs.d/modules/")

;; Packages listed here will be installed when this file is run,
;; removing a package doesn't uninstall it
(setq packages '( use-package
		  auto-package-update
		  nix-mode
		  browse-kill-ring ))

;; Load the builtin package manager
(require 'package)

;; Add the melpa repository
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Initalise the package manager
(package-initialize)

;; Ensure that the packages are installed:

;; Returns a list of packages that need to be installed (nil if
;; nothing needs to be installed)
(defun get-not-installed ()
  (setq-local not-installed ())
  (loop for pkg in packages
        when (not (package-installed-p pkg)) do (push pkg not-installed)
        finally (return not-installed)))

;; If the list of packages that need to be installed is non-empty:
(if (setq not-installed (get-not-installed))
    (progn
      (message "Downloading packages: %s" not-installed)
      (package-refresh-contents)
      (dolist (pkg not-installed)
        (when (not (package-installed-p pkg))
          (package-install pkg)))))

;; Update packages once per week
(require 'auto-package-update)
(setq auto-package-update-interval 7)
;; Prompt before downloading the updates
(setq auto-package-update-prompt-before-update t)
(auto-package-update-maybe)

;; Custom keybindings
(use-package modules-keys)

;; Settings for text mode
(use-package modules-text)

;; Settings for various programming languages
(use-package modules-code)

;; Misc configurations
(use-package modules-misc)

;; Open the scratch buffer in text mode
(setq initial-major-mode 'text-mode)

;; Suppress creation of backup and autosave files
(setq make-backup-files nil
      auto-save-default nil)

;; Indent with spaces not tabs
(setq-default indent-tabs-mode nil)

;; Close all buffers besides the ones emacs uses
(defun close-all ()
  ;; Make the funciton available for interactive use
  (interactive)
  ;; mapc runs a function for it's side affects, disguarding any return value
  (mapc
   (lambda (x)
     ;; Find the name of the buffer
     (let ((name (buffer-name x)))
       ;; Kill the buffer if it's name doesn't start with a space (Emacs
       ;; reserved buffers start with a space)
       (unless (eq ?\s (aref name 0))
	 (kill-buffer x))))
   ;; Produces a list of currently active buffers
   (buffer-list)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(asm-comment-char 35)
 '(package-selected-packages (quote (nix-mode use-package auto-package-update))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
