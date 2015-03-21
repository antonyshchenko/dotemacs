(defvar env0der-packages
  '(
    helm
    helm-swoop
    ace-jump-mode
    ace-jump-buffer
    helm-projectile
    evil
    evil-nerd-commenter
    color-identifiers-mode
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar env0der-excluded-packages '()
  "List of packages to exclude.")

(defun env0der/init-helm ()
  (use-package helm
    :config
    (progn
      (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
      (global-set-key (kbd "s-f") 'helm-imenu))))

(defun env0der/init-helm-swoop ()
  (use-package helm-swoop
    :config
    (progn
      (setq helm-swoop-speed-or-color t)
      (global-set-key (kbd "C-s") 'helm-swoop))))


(defun env0der/init-ace-jump-mode ()
  (use-package ace-jump-mode
    :config
    (progn
      (define-key global-map (kbd "s-j") 'ace-jump-mode))))

(defun env0der/init-ace-jump-buffer ()
  (use-package ace-jump-buffer
    :config
    (progn
      (global-set-key (kbd "s-b") 'ace-jump-buffer))))

(defun env0der/init-helm-projectile ()
  (use-package helm-projectile
    :config
    (progn
      (global-set-key (kbd "s-o") 'helm-projectile))))

(defun env0der/init-evil ()
  (use-package evil
    :config
    (progn
      ;;;; Clipboard bypass in evil mode
      (defmacro without-evil-mode (&rest do-this)
        ;; Check if evil-mode is on, and disable it temporarily
        `(let ((evil-mode-is-on (evil-mode?)))
           (if evil-mode-is-on
               (disable-evil-mode))
           (ignore-errors
             ,@do-this)
           (if evil-mode-is-on
               (enable-evil-mode))))

      (defmacro evil-mode? ()
        "Checks if evil-mode is active. Uses Evil's state to check."
        `evil-state)

      (defmacro disable-evil-mode ()
        "Disable evil-mode with visual cues."
        `(progn
           (evil-mode 0)
           (message "Evil mode disabled")))

      (defmacro enable-evil-mode ()
        "Enable evil-mode with visual cues."
        `(progn
           (evil-mode 1)
           (message "Evil mode enabled")))


      ;; delete: text object
      (evil-define-operator evil-destroy (beg end type register yank-handler)
        "Vim's 's' without clipboard."
        (evil-delete beg end type ?_ yank-handler))

      (evil-define-operator evil-destroy-replace (beg end type register yank-handler)
        (evil-destroy beg end type register yank-handler)
        (evil-paste-before 1 register))

      (defadvice evil-paste-after (around env0der/evil-paste-after-and-indent activate)
        "Paste and indent"
        (evil-with-single-undo
          ad-do-it
          (call-interactively 'indent-region)))

      (defadvice evil-paste-before (around env0der/evil-paste-before-and-indent activate)
        "Paste and indent"
        (evil-with-single-undo
          ad-do-it
          (call-interactively 'indent-region)))

      (define-key evil-normal-state-map "r" 'evil-destroy-replace)

      (define-key evil-insert-state-map (kbd "C-j") (lambda ()
                                                      (interactive)
                                                      (newline-and-indent)))
      (define-key evil-normal-state-map (kbd "C-j") (lambda ()
                                                      (interactive)
                                                      (newline-and-indent)))
      (define-key evil-normal-state-map (kbd "M-j") (lambda ()
                                                      (interactive)
                                                      (evil-insert-newline-below)))
      (define-key evil-normal-state-map (kbd "M-k") (lambda ()
                                                       (interactive)
                                                       (evil-insert-newline-above)))
      )))

(defun env0der/init-evil-nerd-commenter ()
  (use-package evil-nerd-commenter
    :config
    (progn
      (define-key evil-normal-state-map "," 'evilnc-comment-operator)
      (define-key evil-visual-state-map "," 'evilnc-comment-operator))))

(defun env0der/init-color-identifiers-mode ()
  (use-package color-identifiers-mode
    :init
    (progn
      (global-color-identifiers-mode))))

