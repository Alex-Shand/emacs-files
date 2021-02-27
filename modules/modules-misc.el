(provide 'modules-misc)

;; Keyboard scroll one line at a time
(setq-default scroll-step 1)

;; Make all searches case sensitive by default
(setq-default case-fold-search nil)

;; Show line and column numbers
(line-number-mode 1)
(column-number-mode 1)

;; Make Emacs work with the system clipboard
(setq-default x-select-enable-clipboard t)

;; Start straight into the scratch buffer and disable the message
(setq-default inhibit-splash-screen t
      initial-scratch-message nil)

;; Open the scratch buffer in python-mode
(setq-default initial-major-mode 'python-mode)

;; Typing over highlighted text deletes it
(delete-selection-mode t)

;; Suppress creation of backup and autosave files
(setq make-backup-files nil
      auto-save-default nil)

;; Indent with 4 spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default indent-line-function 'insert-tab)

;; Change yes/no prompts to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't use dialog boxes
(setq-default use-dialog-box nil)

;; Highlight matching brackets
(show-paren-mode t)
