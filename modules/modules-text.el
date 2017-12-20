(provide 'modules-text)

;; Make text mode the default for new buffers
(setq-default major-mode 'text-mode)
;; Automatically enable auto-fill in text mode
(add-hook 'text-mode-hook 'auto-fill-mode)
