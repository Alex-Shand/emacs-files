(provide 'modules-keys)

;; Use C-<direction> to move between windows
(global-set-key (kbd "C-<up>") 'windmove-up)
(global-set-key (kbd "C-<down>") 'windmove-down)
(global-set-key (kbd "C-<right>") 'windmove-right)
(global-set-key (kbd "C-<left>") 'windmove-left)

;; Shift-<Arrow> highlights
(transient-mark-mode t)

;; M-y opens browse-kill-ring
(browse-kill-ring-default-keybindings)

(global-set-key (kbd "C-n") 'revert-buffer)
