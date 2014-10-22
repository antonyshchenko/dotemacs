(setq user-emacs-directory "~/.emacs.d")

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
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(winner-mode 1)
(setq even-window-heights nil)
(require 'windmove)
(windmove-default-keybindings 'meta)

(setq x-select-enable-clipboard t) ;; merge system clipboard and kill-ring

(global-set-key [C-tab] 'other-window)

;; (require 'popwin)
;; (popwin-mode 1)
;; (setq display-buffer-function 'popwin:display-buffer)
;; (push '("\*ag regexp*" :regexp t :height 20 :stick t) popwin:special-display-config)
;; (push '("\*rspec-compilation*" :regexp t :height 20 :stick t) popwin:special-display-config)

;; word moving commands will move cursor into between CamelCaseWords
(global-subword-mode 1)
;; (toggle-frame-maximized)

;; gc tuning
(setq gc-cons-threshold 20000000)


;; kill scratch buffer on start
;;(kill-buffer "*scratch*")
;; never show certain buffers
;; TODO: make ace-jump-buffer use it as well
(setq my-nevershown-buffers '(
                               "^ "
                               "^\\*cycbuf\\*$"
                               "\\` "
                               "\\*helm"
                               "\\*helm-mode"
                               "\\*Echo Area"
                               "\\*Minibuf"
                               "\\TAGS"
                               "\\*Debug Helm Log"
                               "\\*buffer-selection*"
                               "\\*Compile-Log"
                               "\\*Messages"))

;; De-duplicate buffer names by prepending parts of the directory until the name
;; is unique, instead of just appending numbers.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*") ; Do not rename special buffers!

; kill open bufer w/o confirmation
(global-set-key (kbd "s-k") 'kill-this-buffer)

;(setq mac-command-modifier 'control)
(setq mac-function-modifier 'hyper)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "s-/") 'comment-dwim)

;; auto indent new lines
(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(require 'linum+)
(setq linum-format "%d ")
(global-linum-mode 1)

(require 'linum-relative)
(setq linum-relative-format "%3s ")

;; show system name and current file path in window title
(setq-default frame-title-format (list (system-name) ": %b (%f)"))

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
(add-to-list 'ac-modes 'enh-ruby-mode)
(add-to-list 'ac-modes 'web-mode)


(require 'ag)
(setq ag-highlight-search t)
(setq ag-reuse-window nil)

;; PROJECTILE
(projectile-global-mode)
(setq projectile-enable-caching t)
(global-set-key (kbd "s-g") 'projectile-ag)
(global-set-key (kbd "s-r") 'projectile-replace)
(defun projectile-project-vcs ()
  "Determine the VCS used by the project if any."
  'none)

;; HELM
(require 'helm-config)
(define-key global-map [remap list-buffers] 'helm-buffers-list)

(require 'helm-projectile)
(global-set-key (kbd "s-o") 'helm-projectile)

(setq helm-boring-buffer-regexp-list my-nevershown-buffers)
;; (loop for ext in '("\\.swf$" "\\.elc$" "\\.pyc$")
;;       do (add-to-list 'helm-boring-file-regexp-list ext))

(require 'dirtree)
;; (add-hook 'dirtree-mode-hook (lambda()
;;                                (smartparens-mode -1)))

;; RUBY
;; (require 'enh-ruby-mode)
(require 'bundler)
(require 'ruby-tools)
(require 'rbenv)
(global-rbenv-mode)

(add-hook 'enh-ruby-mode-hook (lambda ()
                            (rbenv-use-corresponding)))

(add-hook 'enh-ruby-mode-hook (lambda ()
                            (local-set-key (kbd "C-c C-{") 'ruby-toggle-block)))

(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(setq enh-ruby-bounce-deep-indent t)
(setq enh-ruby-hanging-brace-indent-level 2)

(require 'rspec-mode)

;; better ruby intendation
(setq ruby-deep-indent-paren nil)

(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))


(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)
(global-set-key (kbd "C-0") 'er/clear-history)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-code-indent-offset 2)
(setq web-mode-script-padding 2)
(setq web-mode-disable-auto-pairing t)

(setq js-indent-level 2)


(add-hook 'enh-ruby-mode-hook
      (lambda ()
        (require 'ruby-electric)
        (ruby-electric-mode t)))

(defun ruby-insert-end ()
  "Insert \"end\" at point and reindent current line."
  (interactive)
  (insert "end")
  (ruby-indent-line t)
  (end-of-line))

(require 'autopair)
(autopair-global-mode)

;; ;; SMARTPARENS
;; Bug in Aquamatics when requiring this
;; (require 'smartparens)
;; (require 'smartparens-config)
;; (require 'smartparens-ruby)
;; (smartparens-global-mode)
;; (show-smartparens-global-mode t)
;; (setq sp-autoescape-string-quote nil)
;; ;; (sp-with-modes '(web-mode)
;; ;;   (sp-local-pair "%" "%"
;; ;;                  :unless '(sp-in-string-or-word-p)
;; ;;                  :post-handlers '(
;; ;;                                   (space-and-space-on-each-side "SPC")
;; ;;                                   (space-on-each-side "=" "#")
;; ;;                                   )))


;; (let ((map smartparens-mode-map))
;;     ;; Movement and navigation
;;     (define-key map (kbd "H-l") #'sp-forward-sexp)
;;     (define-key map (kbd "H-h") #'sp-backward-sexp)
;;     (define-key map (kbd "H-j") #'sp-down-sexp)
;;     (define-key map (kbd "H-k") #'sp-up-sexp)
;;     (define-key map (kbd "H-u") #'sp-beginning-of-sexp)
;;     (define-key map (kbd "H-i") #'sp-end-of-sexp)
;;     ;; ;; Deleting and killing
;;     (define-key map (kbd "H-d") #'sp-kill-sexp)
;;     (define-key map (kbd "H-y") #'sp-copy-sexp)
;;     ;; ;; Depth changing
;;     ;; (define-key map (kbd "M-s") #'sp-splice-sexp)
;;     ;; (define-key map (kbd "M-<up>") #'sp-splice-sexp-killing-backward)
;;     ;; (define-key map (kbd "M-<down>") #'sp-splice-sexp-killing-forward)
;;     ;; (define-key map (kbd "M-r") #'sp-splice-sexp-killing-around)
;;     ;; (define-key map (kbd "M-?") #'sp-convolute-sexp)
;;     ;; Barfage & Slurpage
;;     (define-key map (kbd "s-l")  #'sp-forward-slurp-sexp)
;;     (define-key map (kbd "s-h")  #'sp-forward-barf-sexp)
;;     (define-key map (kbd "M-h")  #'sp-backward-slurp-sexp)
;;     (define-key map (kbd "M-l")  #'sp-backward-barf-sexp)
;;     (define-key map (kbd "s-j")  #'sp-raise-sexp)
;;     ;; Miscellaneous commands
;;     ;; (define-key map (kbd "M-S") #'sp-split-sexp)
;;     ;; (define-key map (kbd "M-J") #'sp-join-sexp)
;;     ;; (define-key map (kbd "C-M-t") #'sp-transpose-sexp)
;;     )


(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.2)
(dolist (hook '(emacs-lisp-mode-hook
                lisp-interaction-mode-hook
                text-mode-hook
                enh-ruby-mode-hook
                js-mode-hook
                clojure-mode-hook
                web-mode-hook
                cperl-mode-hook))
  (add-hook hook (lambda()
                   (highlight-symbol-mode 1)
                   (local-set-key (kbd "M-n") 'highlight-symbol-next)
                   (local-set-key (kbd "M-p") 'highlight-symbol-prev)
                   (local-set-key (kbd "M-r") 'highlight-symbol-query-replace))))

(require 'color-identifiers-mode)
(global-color-identifiers-mode)

(require 'multiple-cursors)
(global-set-key (kbd "H-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "H-a") 'mc/mark-all-like-this)


(require 'ace-jump-mode)
(define-key global-map (kbd "C-SPC") 'ace-jump-mode)
(setq ace-jump-mode-submode-list
      '(ace-jump-word-mode              ;; C-M-j
        ace-jump-char-mode              ;; C-u C-M-j
        ace-jump-line-mode))            ;; C-u C-u C-M-j


(require 'ace-jump-buffer)
(global-set-key (kbd "s-b") 'ace-jump-buffer)

(defun switch-to-previous-buffer ()
  "Switch to the previous buffer.

Repeated invocations toggle between the two most recently used
buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) :visible-ok)))

(global-set-key (kbd "H-b") 'switch-to-previous-buffer)


(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)



;; ETAGS stuff
(require 'helm-etags+)
(global-set-key (kbd "H-]") 'helm-etags+-select)
(global-set-key (kbd "H-[") 'helm-etags+-history-go-back)

;; Ruby specific etags stuff
(require 'etags-table)
(defun get-gems-path ()
  (replace-regexp-in-string "\n" "/gems" (shell-command-to-string "rbenv exec gem environment | grep INSTALLATION | cut -d : -f 2 | xargs")))

(add-hook 'helm-etags+-select-hook (lambda()
                                     (setq tags-table-list (list (concat (get-gems-path) "/TAGS")))))

(defun bundle-install ()
  "Run bundle install for the current bundle."
  (interactive)
  (bundle-command (concat "source ~/.zshrc && bundle install && echo 'Generating tags\n' && cd " (get-gems-path) "&& ctags -e -R --extra=+fq --exclude=db --exclude=doc --exclude=log --exclude=tmp --exclude=.git --exclude=public && echo 'Done'")))


(defun projectile-idle-regenerate-tags ()
  "Regenerate the project's tags if in a project"
  (when (projectile-project-p)
    (shell-command-to-string (concat "source ~/.zshrc && cd " (projectile-project-root) " && ctags -R -e --extra=+fq --exclude=log --exclude=tmp --exclude=.git"))))

;; regenerate TAGS file if idle for 30 seconds
(setq projectile-idle-timer (run-with-idle-timer 30 t 'projectile-idle-regenerate-tags))



(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'delete-region))
  (yank)
  (call-interactively 'indent-region))

(global-set-key (kbd "s-v") 'yank-and-indent)


;; Cleanup whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)


(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; current file functions
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key [f6] 'rename-current-buffer-file)

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key [f8] 'delete-current-buffer-file)

;; elisp
;; Elisp go-to-definition with H-] and back again with H-[,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (elisp-slime-nav-mode t)
                                  (local-set-key (kbd "H-]") 'elisp-slime-nav-find-elisp-thing-at-point)
                                  (local-set-key (kbd "H-[") 'pop-tag-mark)))


;; EVIL mode settings
(require 'evil)
(evil-mode 1)
(setq evil-auto-indent t)

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


;; delete: char
(evil-define-operator evil-destroy-char (beg end type register yank-handler)
  :motion evil-forward-char
  (evil-delete-char beg end type ?_))

;; delete: char (backwards)
(evil-define-operator evil-destroy-backward-char (beg end type register yank-handler)
  :motion evil-forward-char
  (evil-delete-backward-char beg end type ?_))

;; delete: text object
(evil-define-operator evil-destroy (beg end type register yank-handler)
  "Vim's 's' without clipboard."
  (evil-delete beg end type ?_ yank-handler))

;; delete: to end of line
(evil-define-operator evil-destroy-line (beg end type register yank-handler)
  :motion nil
  :keep-visual t
  (interactive "<R><x>")
  (evil-delete-line beg end type ?_ yank-handler))

;; delete: whole line
(evil-define-operator evil-destroy-whole-line (beg end type register yank-handler)
  :motion evil-line
  (interactive "<R><x>")
  (evil-delete-whole-line beg end type ?_ yank-handler))

;; change: text object
(evil-define-operator evil-destroy-change (beg end type register yank-handler delete-func)
  (evil-change beg end type ?_ yank-handler delete-func))

;; paste: before
(defun evil-destroy-paste-before ()
  (interactive)
  (without-evil-mode
   (delete-region (point) (mark))
   (evil-paste-before 1)))

;; paste: after
(defun evil-destroy-paste-after ()
  (interactive)
  (without-evil-mode
   (delete-region (point) (mark))
   (evil-paste-after 1)))

;; paste: text object
(evil-define-operator evil-destroy-replace (beg end type register yank-handler)
  (evil-destroy beg end type register yank-handler)
  (evil-paste-before 1 register))



;; Clipboard bypass key rebindings
;; (define-key evil-normal-state-map "s" 'evil-destroy)
;; (define-key evil-normal-state-map "S" 'evil-destroy-line)
;; (define-key evil-normal-state-map "c" 'evil-destroy-change)
;; (define-key evil-normal-state-map "x" 'evil-destroy-char)
;; (define-key evil-normal-state-map "X" 'evil-destroy-whole-line)
;; (define-key evil-normal-state-map "Y" 'evil-copy-to-end-of-line)
;; (define-key evil-visual-state-map "P" 'evil-destroy-paste-before)
;; (define-key evil-visual-state-map "p" 'evil-destroy-paste-after)
(define-key evil-normal-state-map "r" 'evil-destroy-replace)



(evil-define-command evil-paste-and-indent-before
  (count &optional register yank-handler)
  :suppress-operator t
  (interactive "P<x>")
  (evil-with-single-undo
    (evil-paste-before count register yank-handler)
    (call-interactively 'indent-region)))

(evil-define-command evil-paste-and-indent-after
  (count &optional register yank-handler)
  :suppress-operator t
  (interactive "P<x>")
  (evil-with-single-undo
    (evil-paste-after count register yank-handler)
    (call-interactively 'indent-region)))

(define-key evil-normal-state-map "P" 'evil-paste-and-indent-before)
(define-key evil-normal-state-map "p" 'evil-paste-and-indent-after)

(define-key evil-normal-state-map (kbd "RET") (lambda ()
                                                (interactive)
                                                (evil-insert-newline-below)
                                                (evil-normal-state)))
(define-key evil-normal-state-map [(S-return)] (lambda ()
                                                  (interactive)
                                                  (evil-insert-newline-above)
                                                  (evil-normal-state)))
(defun evil-break-line ()
  (interactive)
  (if (eql 'insert evil-state)
      (newline-and-indent)
    (without-evil-mode
     (newline-and-indent))))

(define-key evil-insert-state-map (kbd "C-j") 'evil-break-line)
(define-key evil-normal-state-map (kbd "C-j") 'evil-break-line)

(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

;;; esc quits
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)


(require 'surround)
(global-surround-mode 1)

(require 'evil-matchit)
(global-evil-matchit-mode 1)

(require 'evil-numbers)
(global-set-key (kbd "s-=") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "s--") 'evil-numbers/dec-at-pt)

;; tabbar
(tabbar-define-access-keys '(super))
(global-set-key (kbd "s-t") 'tabbar-new-tab)
(global-set-key (kbd "s-w") 'tabbar-close-tab)
(global-set-key (kbd "s-{") 'previous-tab-or-buffer)
(global-set-key (kbd "s-}") 'next-tab-or-buffer)
