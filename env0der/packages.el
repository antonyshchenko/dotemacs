(defvar env0der-packages
  '(
    ace-jump-buffer
    helm-projectile
    evil
    evil-nerd-commenter
    color-identifiers-mode
    projectile
    cider
    tabbar
    tabbar-ruler
    smartparens
    web-mode
    company
    ruby-mode
    cperl-mode
    mo-git-blame
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar env0der-excluded-packages '()
  "List of packages to exclude.")

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
      (define-key evil-normal-state-map (kbd "<RET>") (lambda ()
                                                      (interactive)
                                                      (evil-insert-newline-below)))
      (define-key evil-normal-state-map [(S-return)] (lambda ()
                                                       (interactive)
                                                       (evil-insert-newline-above)))
      )))

(defun env0der/init-evil-nerd-commenter ()
  (use-package evil-nerd-commenter
    :config
    (progn
      (define-key evil-normal-state-map "gc" 'evilnc-comment-operator)
      (define-key evil-visual-state-map "gc" 'evilnc-comment-operator))))

(defun env0der/init-color-identifiers-mode ()
  (use-package color-identifiers-mode
    :init
    (progn
      (global-color-identifiers-mode))))

(defun env0der/init-projectile ()
  (use-package projectile
    :config
    (progn
      (global-set-key (kbd "s-g") 'projectile-ag)

      (defun projectile-ag-with-ignore-files ()
        (interactive)
        (let ((search-term (read-from-minibuffer
                            (projectile-prepend-project-name "Ag search for: ")
                            (projectile-symbol-at-point)))
              (ignore-files (read-from-minibuffer
                             (projectile-prepend-project-name "Ag ignore files: "))))
          (setq tmp ag-arguments)
          (setq ag-arguments (cons (format "--ignore=%s" ignore-files) ag-arguments))
          (ag search-term (projectile-project-root))
          (setq ag-arguments tmp)))
      (global-set-key (kbd "s-G") 'projectile-ag-with-ignore-files)
      )))

(defun env0der/init-cider ()
  (use-package cider
    :config
    (progn
      (defun cider-reset-system ()
        (interactive)
        (spacemacs//cider-eval-in-repl-no-focus "(user/reset)"))
      (define-key clojure-mode-map (kbd "s-r") 'cider-reset-system))))

(defun env0der/init-tabbar-ruler ()
  (use-package tabbar-ruler
    :init
    (setq tabbar-ruler-global-tabbar t)
    (setq tabbar-ruler-global-ruler nil)
    (setq tabbar-ruler-popup-menu nil)
    (setq tabbar-ruler-popup-toolbar nil)
    (setq tabbar-ruler-popup-scrollbar nil)
    (setq tabbar-ruler-movement-timer-delay 1000000)
    (require 'tabbar-ruler)
    (global-set-key (kbd "s-{") 'tabbar-ruler-backward)
    (global-set-key (kbd "s-}") 'tabbar-ruler-forward)

    ;; for now just override and hack this function to remove tab with TAGS file from projectile project tabs list
    (defun tabbar-ruler-projectile-tabbar-buffer-groups ()
      (if tabbar-ruler-projectile-tabbar-buffer-group-calc
          (symbol-value 'tabbar-ruler-projectile-tabbar-buffer-group-calc)
        (set (make-local-variable 'tabbar-ruler-projectile-tabbar-buffer-group-calc)

             (cond
              ((or (get-buffer-process (current-buffer)) (memq major-mode '(comint-mode compilation-mode))) '("Term"))
              ((string-equal "*" (substring (buffer-name) 0 1)) '("Misc"))
              ((string-prefix-p "TAGS" (buffer-name)) '("Misc"))
              ((condition-case err
                   (projectile-project-root)
                 (error nil)) (list (projectile-project-name)))
              ((memq major-mode '(emacs-lisp-mode python-mode emacs-lisp-mode c-mode c++-mode makefile-mode lua-mode vala-mode)) '("Coding"))
              ((memq major-mode '(javascript-mode js-mode nxhtml-mode html-mode css-mode)) '("HTML"))
              ((memq major-mode '(org-mode calendar-mode diary-mode)) '("Org"))
              ((memq major-mode '(dired-mode)) '("Dir"))
              (t '("Main"))))
        (symbol-value 'tabbar-ruler-projectile-tabbar-buffer-group-calc)))


    (tabbar-ruler-group-by-projectile-project)
    ))

(defun env0der/init-smartparens ()
  (use-package smartparens
    :init
    (sp-local-pair 'web-mode "%" "%"
                   :unless '(sp-in-string-or-word-p)
                   :post-handlers '(
                                    (space-and-space-on-each-side "SPC")
                                    (space-on-each-side "=" "#")
                                    ))

    (defun sp-web-mode-is-code-context (id action context)
      (when (and (eq action 'insert)
                 (not (or (get-text-property (point) 'part-side)
                          (get-text-property (point) 'block-side))))

        t))

    (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))
    ))

(defun env0der/init-web-mode ()
  (use-package web-mode
    :init
    ;; (add-to-list 'web-mode-comment-formats '("ruby" . "#"))
    (add-hook 'web-mode-hook (lambda()
                               (setq web-mode-markup-indent-offset 2)
                               (setq web-mode-code-indent-offset 2)
                               (setq web-mode-css-indent-offset 2)
                               (setq web-mode-indent-style 2)
                               (setq web-mode-enable-auto-pairing nil)))))

(defun env0der/init-company ()
  (use-package company
    :config
    (progn
      (define-key company-mode-map (kbd "M-j") 'company-select-next)
      (define-key company-mode-map (kbd "M-k") 'company-select-previous))))

(defun env0der/init-ruby-mode ()
  (when (configuration-layer/layer-usedp 'auto-completion)
    (spacemacs|defvar-company-backends ruby-mode)
    (spacemacs|add-company-hook ruby-mode)

    (defun ruby/post-init-company ()
      (spacemacs|add-company-hook ruby-mode))))

(defun env0der/init-cperl-mode ()
  (when (configuration-layer/layer-usedp 'auto-completion)
    (spacemacs|defvar-company-backends cperl-mode)
    (spacemacs|add-company-hook cperl-mode)

    (defun cperl/post-init-company ()
      (spacemacs|add-company-hook cperl-mode))))

(defun env0der/init-mo-git-blame ()
  (use-package mo-git-blame
    :init
    (progn
      (evil-leader/set-key "gb" 'mo-git-blame-current))
    :config
    (progn
      (dolist (state '(normal visual insert))
        (evil-define-key state mo-git-blame-mode-map (kbd "q") 'mo-git-blame-quit)))))
