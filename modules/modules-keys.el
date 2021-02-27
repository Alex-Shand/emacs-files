(provide 'modules-keys)

;; Use C-<direction> to move between windows
(global-set-key (kbd "C-<up>") 'windmove-up)
(global-set-key (kbd "C-<down>") 'windmove-down)
(global-set-key (kbd "C-<right>") 'windmove-right)
(global-set-key (kbd "C-<left>") 'windmove-left)

;; Shift-<Arrow> highlights
(transient-mark-mode t)

;; Throw away any changes to the buffer
(global-set-key (kbd "C-n") 'revert-buffer)
