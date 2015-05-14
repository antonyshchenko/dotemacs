;; Cleanup whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)

(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "s-w") 'kill-this-buffer)

(setq ido-auto-merge-work-directories-length -1)

;; show system name and current file path in window title
(setq-default frame-title-format (list (system-name) ": %b (%f)"))

(setq-default vc-follow-symlinks t)
