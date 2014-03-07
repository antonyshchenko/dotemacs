(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(add-to-list 'load-path "~/.emacs.d/vendor")

(push "/usr/local/bin" exec-path)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode t)
(show-paren-mode t)
(column-number-mode t)
(set-fringe-style -1)
(tooltip-mode -1)
(desktop-save-mode 1)

(setq make-backup-files nil)
(setq auto-save-default nil)

;; word moving commands will move cursor into between CamelCaseWords
(global-subword-mode 1)
;; (toggle-frame-maximized)

;; gc tuning
(setq gc-cons-threshold 20000000)

(require 'windmove)
(windmove-default-keybindings 'meta)

;; kill scratch buffer on start
(kill-buffer "*scratch*")

;; user buffer cycling
(defun next-user-buffer ()
  "Switch to the next user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer) )))

(defun previous-user-buffer ()
  "Switch to the previous user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))



; kill open bufer w/o confirmation
(global-set-key (kbd "s-w") 'kill-this-buffer)

;(setq mac-command-modifier 'control)
(setq mac-function-modifier 'hyper)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

(global-unset-key (kbd "s-l"))

(global-unset-key (kbd "s-m"))
(global-set-key (kbd "s-m r") 'kmacro-start-macro)
(global-set-key (kbd "s-m e") 'kmacro-end-or-call-macro)
(global-set-key [f5] 'call-last-kbd-macro)

(global-set-key (kbd "s-{") 'previous-user-buffer)
(global-set-key (kbd "s-}") 'next-user-buffer)

;; (global-set-key (kbd "C-j") 'backward-char)
;; (global-set-key (kbd "C-l") 'forward-char)
;; (global-set-key (kbd "C-k") 'next-line)
;; (global-set-key (kbd "C-i") 'previous-line)

;; (global-set-key (kbd "M-j") 'backward-word)
;; (global-set-key (kbd "M-l") 'forward-word)
(global-set-key (kbd "M-<left>") 'backward-word)
(global-set-key (kbd "M-<right>") 'forward-word)
;; (global-set-key (kbd "s-i") 'beginning-of-buffer)
;; (global-set-key (kbd "s-k") 'end-of-buffer)

(global-set-key (kbd "s-<left>") 'move-beginning-of-line)
(global-set-key (kbd "s-<right>") 'move-end-of-line)
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)

(global-set-key (kbd "M-s-<left>") 'backward-sexp)
(global-set-key (kbd "M-s-<right>") 'forward-sexp)
(global-set-key (kbd "M-s-<up>") 'backward-up-list)
(global-set-key (kbd "M-s-<down>") 'down-list)

(global-set-key (kbd "s-d") 'kill-whole-line)

(global-set-key (kbd "s-/") 'comment-dwim)

;; auto indent new lines
(global-set-key (kbd "RET") 'newline-and-indent)

(require 'linum+)
(setq linum-format "%d ")
(global-linum-mode 1)

;; show system name and current file path in window title
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(Buffer-File-name "%f" (dired-directory dired-directory "%b"))))

(require 'smartscan)
(global-smartscan-mode 1)

;; (require 'sr-speedbar)
;; (setq speedbar-default-position 'left)
;; (setq speedbar-show-unknown-files t)
;; (setq sr-speedbar-right-side nil)
;; (setq speedbar-use-images nil)
;; (global-set-key (kbd "C-c b") 'sr-speedbar-toggle)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(require 'flx-ido)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-down)
(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
  (define-key ido-completion-map (kbd "<up>") 'ido-prev-match))


;; Smooth scrolling
;; From http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq redisplay-dont-pause t
      mouse-wheel-progressive-speed nil
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)


;; AUTOCOMPLETE
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/.cask/24.3.50.1/elpa/auto-complete-20131128.233/dict")
(ac-config-default)
(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'ruby-mode)
(add-to-list 'ac-modes 'web-mode)


;; SMARTPARENS
(require 'smartparens-config)
(require 'smartparens-ruby)
(smartparens-global-mode)
(show-smartparens-global-mode t)
(sp-with-modes '(rhtml-mode)
  (sp-local-pair "<" ">")
  (sp-local-pair "<%" "%>"))


(require 'helm-config)
(define-key global-map [remap list-buffers] 'helm-buffers-list)


;; PROJECTILE
 (projectile-global-mode)
 (setq projectile-enable-caching t)
 (global-set-key (kbd "s-g") 'projectile-grep)
 (global-set-key (kbd "s-r") 'projectile-replace)
 (defun projectile-project-vcs ()
   "Determine the VCS used by the project if any."
   'none)
(require 'helm-projectile)
 (global-set-key (kbd "s-p") 'helm-projectile)


;; RUBY
(require 'ruby-tools)
(require 'rbenv)
(global-rbenv-mode)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook (lambda ()
                            (rbenv-use-corresponding)))

(add-hook 'ruby-mode-hook (lambda ()
                            (local-set-key (kbd "C-c C-{") 'ruby-toggle-block)))

(defun* get-closest-gemfile-root (&optional (file "Gemfile"))
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) ; the win32 builds should translate this correctly
    (loop
     for d = default-directory then (expand-file-name ".." d)
     if (file-exists-p (expand-file-name file d))
     return d
     if (equal d root)
     return nil)))

(require 'compile)

(defun rspec-compile-file ()
  (interactive)
  (compile (format "cd %s;bundle exec rspec %s"
                   (get-closest-gemfile-root)
                   (file-relative-name (buffer-file-name) (get-closest-gemfile-root))
                   ) t))

(defun rspec-compile-on-line ()
  (interactive)
  (compile (format "cd %s;bundle exec rspec %s -l %s"
                   (get-closest-gemfile-root)
                   (file-relative-name (buffer-file-name) (get-closest-gemfile-root))
                   (line-number-at-pos)
                   ) t))

(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c l") 'rspec-compile-on-line)
            (local-set-key (kbd "C-c k") 'rspec-compile-file)
            (rbenv-use-corresponding)
            ))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-code-indent-offset 2)
(setq web-mode-script-padding 2)
(add-hook 'web-mode-hook (lambda()
                           (smartparens-mode -1)
                           (local-set-key (kbd "M-s-<left>") 'web-mode-element-previous)
                           (local-set-key (kbd "M-s-<right>") 'web-mode-element-next)
                           (local-set-key (kbd "M-s-<up>") 'web-mode-element-parent)
                           (local-set-key (kbd "M-s-<down>") 'web-mode-element-child)))


(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.2)
(dolist (hook '(emacs-lisp-mode-hook
                lisp-interaction-mode-hook
                text-mode-hook
                ruby-mode-hook
                web-mode-hook))
    (add-hook hook (lambda() (highlight-symbol-mode 1) )))

(require 'fastnav)
(global-set-key "\M-z" 'fastnav-zap-up-to-char-forward)
(global-set-key "\M-Z" 'fastnav-zap-up-to-char-backward)
(global-set-key "\M-s" 'fastnav-jump-to-char-forward)
(global-set-key "\M-S" 'fastnav-jump-to-char-backward)
(global-set-key "\M-m" 'fastnav-mark-to-char-forward)
(global-set-key "\M-M" 'fastnav-mark-to-char-backward)

(require 'ace-jump-mode)
(define-key global-map (kbd "C-M-j") 'ace-jump-mode)
(setq ace-jump-mode-submode-list
      '(ace-jump-word-mode              ;; C-M-j
        ace-jump-char-mode              ;; C-u C-M-j
        ace-jump-line-mode))            ;; C-u C-u C-M-j

(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'delete-region))
  (yank)
  (call-interactively 'indent-region))

(global-set-key (kbd "s-v") 'yank-and-indent)


(defun zap-to-string (arg str)
  "Same as `zap-to-char' except that it zaps to the given string
instead of a char."
  (interactive "p\nsZap to string: ")
  (kill-region (point) (progn
                         (search-forward str nil nil arg)
                         (point))))

(global-set-key (kbd "C-M-z") 'zap-to-string)

;; (defun forward-word-to-beginning (&optional n)
;;   "Move point forward n words and place cursor at the beginning."
;;   (interactive "p")
;;   (let (myword)
;;     (setq myword
;;           (if (and transient-mark-mode mark-active)
;;               (buffer-substring-no-properties (region-beginning) (region-end))
;;             (thing-at-point 'symbol)))
;;     (if (not (eq myword nil))
;;         (forward-word n))
;;     (forward-word n)
;;     (backward-word n)))

;; (global-set-key (kbd "M-C-f") 'forward-word-to-beginning)


;; Cleanup whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)


(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; ETC

(set-frame-font "Menlo-14")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(fci-rule-color "#383838")
 '(highlight-symbol-on-navigation-p nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2e3436" :foreground "gray100" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo"))))
 '(font-lock-builtin-face ((t (:foreground "brown1"))))
 '(font-lock-comment-face ((t (:foreground "gray43"))))
 '(font-lock-string-face ((t (:foreground "yellow green"))))
 '(helm-selection ((t (:foreground "Green"))))
 '(helm-source-header ((t (:foreground "white" :weight bold))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "gray100"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "orange1"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "purple1"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "OrangeRed1"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "OliveDrab3"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "cyan1"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "SteelBlue1"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "salmon"))))
 '(sp-pair-overlay-face ((t (:inherit default))))
 '(sp-wrap-overlay-face ((t nil)))
 '(sp-wrap-tag-overlay-face ((t nil))))
