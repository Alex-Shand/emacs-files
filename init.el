;; General setup, experimental settings and imports for other modules

(require 'cl)

(setq hostname (system-name))
(setq modules-dir (concat (file-name-directory load-file-name) "modules/"))

;; Add ~/.emacs.d/modules/ to the load path
(add-to-list 'load-path modules-dir)

;; Packages listed here will be installed when this file is run,
;; removing a package doesn't uninstall it
(setq packages '( use-package
                  fill-column-indicator
                  nix-mode
                  cython-mode
                  tuareg
                  rust-mode
                  ))

;; Coconut mode (not in melpa)
(load-file (concat (file-name-directory load-file-name) "coconut-mode.el"))

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

;; Custom keybindings
(use-package modules-keys)

;; Settings for text mode
(use-package modules-text)

;; Settings for various programming languages
(use-package modules-code)

;; Misc configurations
(use-package modules-misc)

;; Close all buffers besides the ones emacs uses
(defun close-all ()
  ;; Make the funciton available for interactive use
  (interactive)
  ;; mapc runs a function for its side affects, discarding any return value
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
 '(package-selected-packages
   (quote
    (use-package fill-column-indicator nix-mode cython-mode tuareg rust-mode fsharp-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
