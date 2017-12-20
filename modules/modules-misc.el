(provide 'modules-misc)

;; Keyboard scroll one line at a time
(setq scroll-step 1)

;; Make all searches case sensitive by default
(setq-default case-fold-search nil)

;; Show line and column numbers
(line-number-mode 1)
(column-number-mode 1)

;; Start straight into the scratch buffer and disable the message
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; Typing over highlighted text deletes it
(delete-selection-mode t)

;; Make Emacs work with the system clipboard
(setq x-select-enable-clipboard t)

;; Indent with 4 spaces instead of tabs (Seems to be overwritten by programming
;; modes)
(setq indent-tabs-mode nil)
(setq tab-width 4)
(setq indent-line-function 'insert-tab)

;; Change yes/no prompts to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't use dialog boxes
(setq use-dialog-box nil)

;; Highlight matching brackets
(show-paren-mode t)
