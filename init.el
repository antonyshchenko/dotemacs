(setq user-emacs-directory "~/.aquamacs.d")

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(add-to-list 'load-path "~/.aquamacs.d/vendor")

(push "/usr/local/bin" exec-path)

(setq ns-use-mac-modifier-symbols nil) ;; fix for single-key-description function (and smartparens) in aquamacs

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)
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

(require 'golden-ratio)
(setq golden-ratio-exclude-modes '("ediff-mode"))
(golden-ratio-mode 1)
(setq split-width-threshold nil)

; kill open bufer w/o confirmation
(global-set-key (kbd "s-k") 'kill-this-buffer)

(setq mac-function-modifier 'hyper)

;; auto indent new lines
(global-set-key (kbd "RET") 'newline-and-indent)

(require 'linum+)
(setq linum-format "%d ")
(global-linum-mode 1)

(require 'linum-relative)
(setq linum-relative-format "%3s ")
(setq linum-relative-current-symbol "") ;; show absolute number for the current line

;; show system name and current file path in window title
(setq-default frame-title-format (list (system-name) ": %b (%f)"))

(require 'ido)
(ido-mode t)
(require 'flx-ido)
(setq ido-enable-flex-matching t)
(ido-everywhere t)
(flx-ido-mode t)
(setq ido-use-faces nil) ;; disable ido faces to see flx highlights.
(require 'ido-vertical-mode)
(ido-vertical-mode t)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)

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
(add-to-list 'ac-dictionary-directories "~/.aquamacs.d/.cask/24.3.50.1/elpa/auto-complete-20140208.653/dict")
(ac-config-default)
(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'enh-ruby-mode)
(add-to-list 'ac-modes 'web-mode)


;; PROJECTILE
(projectile-global-mode)
(setq projectile-enable-caching t)
(defun projectile-project-vcs ()
  "Determine the VCS used by the project if any."
  'none)

;; HELM
(require 'helm-config)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-split-window-in-side-p t)

(require 'helm-projectile)
(setq helm-projectile-sources-list
  '(helm-source-projectile-buffers-list
    helm-source-projectile-recentf-list
    helm-source-projectile-files-list
    helm-source-projectile-directories-list
    helm-source-projectile-projects))
(setq projectile-switch-project-action 'helm-projectile)

(global-set-key (kbd "s-o") 'helm-projectile)

(defun open-other-project-in-new-window ()
  (interactive)
  (new-empty-buffer-other-frame)
  (helm-projectile-switch-project))

(global-set-key (kbd "s-p") 'open-other-project-in-new-window)

(setq helm-boring-buffer-regexp-list my-nevershown-buffers)

(global-set-key (kbd "s-f") 'helm-imenu)


(require 'ag)
(setq ag-highlight-search t)
(setq ag-reuse-window nil)

(global-set-key (kbd "s-g") 'projectile-ag)
(global-set-key (kbd "s-G") 'projectile-ag-with-ignore-files)
(global-set-key (kbd "s-r") 'projectile-replace-with-default-value)

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

;; consider visual selection if any
;; TODO: advise would be better
(defun projectile-symbol-at-point ()
  "Get the symbol at point and strip its properties."
  (symbol-at-point-or-selection))

;; ;; TODO: consider visual selection if any
;; (defun projectile-replace-with-default-value (&optional arg)
;;   (interactive "P")
;;   (let* ((old-text (read-string
;;                     (projectile-prepend-project-name "Replace: ")
;;                     (symbol-at-point-or-selection)))
;;          (new-text (read-string
;;                     (projectile-prepend-project-name
;;                      (format "Replace %s with: " old-text)) (projectile-symbol-at-point)))
;;          (directory (if arg
;;                         (read-directory-name "Replace in directory: ")
;;                       (projectile-project-root)))
;;          (files (projectile-files-with-string old-text directory)))
;;     (tags-query-replace old-text new-text nil (cons 'list files))))


;; Fancy query-replace stuff
;; TODO: cleanup this mess
(defun query-replace-wim ()
  (interactive)
  (let ((s (symbol-at-point-or-selection)))
    (let ((r (read-from-minibuffer (format "Replace %s with: " s) s)))
         (setq query-replace-wim-current-search-term s)
         (setq query-replace-wim-replacement r)
         (message "Now select a region where to replace and hit RET")
         (query-replace-wim-region-selection-mode 1)
         (add-hook 'kbd-macro-termination-hook 'query-replace-wim-done))))

(defun symbol-at-point-or-selection ()
  (substring-no-properties (if (use-region-p) (buffer-substring (region-beginning) (region-end)) (or (thing-at-point 'symbol) ""))))

(defvar query-replace-wim-current-search-term nil)
(defvar query-replace-wim-replacement nil)

(defun query-replace-wim-do-replace ()
  (interactive)
  (let ((beg (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (query-replace query-replace-wim-current-search-term query-replace-wim-replacement nil beg end)
    (query-replace-wim-done)))

(defvar query-replace-wim-region-select-map (make-keymap))
(define-key query-replace-wim-region-select-map (kbd "<return>") 'query-replace-wim-do-replace)
(define-key query-replace-wim-region-select-map (kbd "C-g") 'query-replace-wim-done)
(define-key query-replace-wim-region-select-map (kbd "<esc>") 'query-replace-wim-done)

(define-minor-mode query-replace-wim-region-selection-mode
  "Mode to select a region for replacing symbol in"
  nil "Select region for replace" query-replace-wim-region-select-map)

(defun query-replace-wim-done ()
  (interactive)
  (setq query-replace-wim-current-search-term nil)
  (setq query-replace-wim-replacement nil)
  (query-replace-wim-region-selection-mode 0))

;; (defun query-replace-wim-in-defun ()
;;   (interactive)
;;   (let ((s (symbol-at-point-or-selection)))
;;     (narrow-to-defun)
;;     (let ((begin (point-min)) (end (point-max)))
;;       (widen)
;;       (query-replace (read-from-minibuffer "Replace: " s) (read-from-minibuffer "Replace with: " s) nil begin end))))

;; (global-set-key (kbd "M-R") 'query-replace-wim-in-defun)
(global-set-key (kbd "M-r") 'query-replace-wim)



(require 'dirtree)
;; (add-hook 'dirtree-mode-hook (lambda()
;;                                (smartparens-mode -1)))

;; RUBY
;; (require 'enh-ruby-mode)
(require 'bundler)
(require 'ruby-tools)
(require 'rbenv)
(require 'ruby-mode)
(global-rbenv-mode)

(require 'robe)
(setq robe-turn-on-eldoc nil)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'after-save-hook (lambda()
                             (when (and (derived-mode-p 'ruby-mode) (bound-and-true-p robe-mode))
                               (ruby-load-file (buffer-file-name)))))

;; (add-hook 'ruby-mode-hook (lambda ()
;;                             (rbenv-use-corresponding)))

;; (add-hook 'ruby-mode-hook (lambda ()
;;                             (local-set-key (kbd "C-c {") 'ruby-toggle-block)))

;; (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
;; (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
;; (setq enh-ruby-bounce-deep-indent t)
;; (setq enh-ruby-hanging-brace-indent-level 2)

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

(add-hook 'web-mode-hook (lambda()
                           (setq web-mode-enable-auto-pairing nil)))

;; temporary fix for highlight-symbol and web-mode integration issue
(defun web-mode-font-lock-highlight (limit)
  "font-lock matcher"
  ;;(message "font-lock-highlight: point(%S) limit(%S) change-beg(%S) change-end(%S)" (point) limit web-mode-change-beg web-mode-change-end)
  ;;  (when (or (null web-mode-change-beg) (null web-mode-change-end))
  ;;    (message "font-lock-highlight: untouched buffer (%S)" this-command))
  (let ((inhibit-modification-hooks t)
        (buffer-undo-list t)
        (region nil))
    (if (and web-mode-change-beg web-mode-change-end)
        (setq region (web-mode-propertize))
      ;; (message "font-lock-highlight ** untouched buffer (%S) **" this-command)
      (setq region (web-mode-propertize (point) limit)))
    ;;(message "region=%S" region)
    (when (and region (car region))
      (web-mode-highlight-region (car region) (cdr region))
      ))
  nil)


(setq js-indent-level 2)

;; ;; SMARTPARENS
(require 'smartparens-config)
(smartparens-global-mode)
(show-smartparens-global-mode t)
(setq sp-autoescape-string-quote nil)

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



(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.2)
(dolist (hook '(emacs-lisp-mode-hook
                lisp-interaction-mode-hook
                text-mode-hook
                ruby-mode-hook
                js-mode-hook
                clojure-mode-hook
                web-mode-hook
                cperl-mode-hook))
  (add-hook hook (lambda()
                   (highlight-symbol-mode 1)
                   (local-set-key (kbd "M-n") 'highlight-symbol-next)
                   (local-set-key (kbd "M-p") 'highlight-symbol-prev))))

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

;; no IDO please, because it constantly tries to suggest existing file when typing new name
(setq ido-read-file-name-non-ido '(rename-current-buffer-file))
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



;; EVIL mode settings
(require 'evil)
(evil-mode 1)
(setq evil-auto-indent t)
(setq evil-search-module 'evil-search)
(define-key evil-normal-state-map (kbd "C-s") 'evil-search-forward)

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

;; delete: text object without putting to default register
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


(defadvice evil-delete (around evil-delete-dont-yank-empty-strings activate)
  ;; don't yank if deleting empty string (or if it consists only from whitespace chars)
  (if (equal "" (replace-regexp-in-string "[ \t\n]" "" (buffer-substring beg end)))
    (delete-region beg end)
    ad-do-it))


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

(define-key evil-insert-state-map (kbd "M-j") 'evil-join)
(define-key evil-normal-state-map (kbd "M-j") 'evil-join)

(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-visual-state-map (kbd "SPC") 'ace-jump-mode) ;; TODO: fix this - atm SPC advances selection start by 1 char for some reason

;;; esc quits
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(dolist (m (list
            minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map))
  (define-key m [escape] 'minibuffer-keyboard-quit))


;; Homerow motions in minubuffer
(dolist (m (list
             minibuffer-local-map
             minibuffer-local-ns-map
             minibuffer-local-completion-map
             minibuffer-local-must-match-map
             minibuffer-local-isearch-map
             evil-ex-completion-map))
  (define-key m (kbd "S-l") 'forward-char)
  (define-key m (kbd "S-h") 'backward-char)
  (define-key m (kbd "M-l") 'forward-word)
  (define-key m (kbd "M-h") 'backward-word)
  (define-key m (kbd "H-h") 'beginning-of-line)
  (define-key m (kbd "H-l") 'end-of-line))

(define-key evil-visual-state-map [tab] 'align-regexp)

(add-to-list 'load-path "~/.aquamacs.d/vendor/evil-plugins") ;; (require 'evil-textobj-between)
(require 'evil-little-word)

(setq evil-operator-comment-key (kbd ","))
(require 'evil-operator-comment)
(global-evil-operator-comment-mode 1)

(require 'surround)
(global-surround-mode 1)

(require 'evil-matchit)
(global-evil-matchit-mode 1)

(require 'evil-numbers)
(global-set-key (kbd "s-=") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "s--") 'evil-numbers/dec-at-pt)


;; TODO: refactor and move somewhere
(defgroup evil-operator-replace nil
  "Comment/uncomment operator for Evil"
  :prefix "evil-operator-comment-"
  :group 'evil)

(defcustom evil-operator-replace-key (kbd "R")
  "A key for replace operator"
  :type `,(if (get 'key-sequence 'widget-type)
              'key-sequence
            'sexp)
  :group 'evil-operator-replace)

(defun evil-replace-in-region (query beg end)
  "Replace in text from BEG to END with TYPE."
  (query-replace (read-from-minibuffer "Replace: " query) (read-from-minibuffer "Replace with: " query) nil beg end))

(define-minor-mode evil-operator-replace-mode
  :lighter ""
  :keymap (make-sparse-keymap)
  :group 'evil-operator-replace
  (evil-normalize-keymaps))

(defun evil-operator-replace-mode-install () (evil-operator-replace-mode 1))

(define-globalized-minor-mode global-evil-operator-replace-mode
  evil-operator-replace-mode evil-operator-replace-mode-install
  "Global minor mode of replace operator for Evil.")

(evil-define-key 'normal evil-operator-replace-mode-map
  evil-operator-replace-key (lambda ()
                              (interactive)
                              (let ((s (substring-no-properties (or (thing-at-point 'symbol) ""))))
                                (apply 'evil-replace-in-region (cons s (evil-operator-range))))))

(global-evil-operator-replace-mode 1)



(require 'project-explorer)
;; (add-hook 'project-explorer-mode 'turn-off-evil-mode) ;; does not work for some reason

(defun find-project-explorer-window ()
  (cl-find-if
   (lambda (window)
     (and (memq (window-buffer window) (pe/get-project-explorer-buffers))
          (window-parameter window 'window-side)))
   (window-list)))

(defun toggle-project-explorer ()
  ;; TODO: maybe move turn-off-evil-mode out and find another solution, so this function does not depend on evil-mode being installed
  (interactive)
  (let ((pe-window (find-project-explorer-window)))
    (if pe-window
        (progn (select-window pe-window) (pe/quit))
      (progn (project-explorer-open) (turn-off-evil-mode)))))

(global-set-key [f1] 'toggle-project-explorer)

;; ETAGS stuff
(require 'helm-etags+)
(setq tags-add-tables nil) ;; don't ask to keep current tag tables list when switching to other directory/project

(defun hyperjump-to-definiton (arg)
  (interactive "P")
  (let ((thing (hyperjump-to-definiton-thing-at-point )))
    (if (and (derived-mode-p 'ruby-mode) (bound-and-true-p robe-mode))
        (condition-case nil
            (hyperjump-to-definition-via-robe thing)
          (error (hyperjump-to-definition-via-tags thing))) ;; fallback to tag search if robe can't find anything
      (hyperjump-to-definition-via-tags thing))))

(defun hyperjump-to-definiton-thing-at-point ()
  (interactive)
  (let ((thing (thing-at-point 'symbol 1)))
    (when (and (equal 'ruby-mode major-mode) (equal ":" (substring thing 0 1)))
      ;; in ruby mode try to find appropriate method for ruby symbol at point
      (setq thing (substring thing 1)))
    thing))

(defun hyperjump-to-definition-via-tags (thing)
  (interactive)
  (when (derived-mode-p 'ruby-mode 'cperl-mode)
    ;; quick fix for searching full qualified class/module names in ruby and perl
    (setq thing (first (reverse (split-string thing "::")))))
  (helm-etags+-select-internal (concat "\\_<" thing "\\_>")))

(defun hyperjump-to-definition-via-robe (thing)
  (interactive)
  (cond
   ((robe-const-p thing)
    (robe-jump-to-module thing))
   (t
    (robe-jump-to (robe-jump-prompt thing)))))

(global-set-key (kbd "s-]") 'hyperjump-to-definiton)
(global-set-key (kbd "s-[") 'pop-tag-mark)
;; (global-set-key (kbd "s-[") 'helm-etags+-history-go-back)


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

;; elisp
;; Elisp go-to-definition with s-] and back again with s-[,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (elisp-slime-nav-mode t)
                                  (local-set-key (kbd "s-]") 'elisp-slime-nav-find-elisp-thing-at-point)
                                  (local-set-key (kbd "s-[") 'pop-tag-mark)))

;; tabbar
(tabbar-define-access-keys '(super))
(global-set-key (kbd "s-t") 'tabbar-new-tab)
(global-set-key (kbd "s-w") 'tabbar-close-tab)
(global-set-key (kbd "s-{") 'previous-tab-or-buffer)
(global-set-key (kbd "s-}") 'next-tab-or-buffer)


;; (require 'indent-guide)
;; (indent-guide-global-mode)

;; from https://github.com/rejeep/emacs/blob/master/osx.el
(defun reveal-in-finder ()
  "Opens file directory in Finder."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (shell-command
         (format "%s %s" (executable-find "open") (file-name-directory file)))
      (error "Buffer is not attached to any file."))))

(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)


(require 'cider)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(setq cider-popup-stacktraces nil)
(setq cider-repl-popup-stacktraces t)
(setq cider-repl-use-pretty-printing t)

(add-hook 'cider-mode-hook (lambda ()
                             (local-set-key (kbd "s-]") 'cider-jump)
                             (local-set-key (kbd "s-[") 'cider-jump-back)))

(evil-set-initial-state 'cider-repl-mode 'emacs)


(define-minor-mode evil-lisp-mode
  "Minor mode for setting up Evil with smartparens in a single buffer"
  :keymap '()
  (let ((prev-state evil-state))
    (evil-normal-state)
    (evil-change-state prev-state)))

(defun evil-lisp-mode-insert-sexp-after ()
  "Insert sexp after the current one. Inspired by https://github.com/syl20bnr/evil-lisp-state"
  (interactive)
  (if (char-equal (char-after) ?\() (forward-char))
  (sp-up-sexp)
  (evil-insert-state)
  (insert " ")
  (let ((remaining-text-in-line (substring-no-properties (buffer-substring (point) (line-end-position)))))
    (if (or (not (eq nil (string-match "\\`\s+\\'" remaining-text-in-line))) (= 0 (length remaining-text-in-line)))
        (sp-newline)))
  (sp-insert-pair "("))

(defun evil-lisp-mode-insert-sexp-before ()
  "Insert sexp before the current one. Inspired by https://github.com/syl20bnr/evil-lisp-state"
  (interactive)
  (if (char-equal (char-after) ?\() (forward-char))
  (sp-backward-up-sexp)
  (evil-insert-state)
  (if (looking-back "^\\s-*")
      (progn
        (sp-newline)
        (evil-previous-visual-line)
        (evil-end-of-line)
        (insert " ")
        (sp-insert-pair "(")
        (indent-for-tab-command))
    (progn
      (insert " ")
      (backward-char)
      (sp-insert-pair "("))))

(evil-define-key 'normal evil-lisp-mode-map
  (kbd "D") 'sp-kill-hybrid-sexp
  (kbd "C") (lambda ()
              (interactive)
              (let ((current-prefix-arg 4))
                (call-interactively 'sp-kill-hybrid-sexp))
              (evil-insert-state))
  ;; (kbd "S") ' - TODO: change whole line with respect to parens
  ;; foo |bar -- evil-change-whole-line
  ;; (foo |bar) -- ()
  ;; (foo (bar |baz)) -- ()
  ;; (foo -- (
  ;; ((foo -- ((
  (kbd "X") 'sp-backward-delete-char
  (kbd "x") 'sp-delete-char
  (kbd "H") (lambda ()
            (interactive)
            (sp-previous-sexp)
            (sp-backward-sexp))
  (kbd "L") 'sp-next-sexp
  (kbd "J") (lambda ()
            (interactive)
            (sp-down-sexp 2)
            (sp-backward-up-sexp))
  (kbd "K") 'sp-backward-up-sexp
  (kbd "H-r") 'sp-raise-sexp
  (kbd "H-u") 'sp-splice-sexp
  (kbd "H-k") 'sp-splice-sexp-killing-backward
  (kbd "H-j") 'sp-splice-sexp-killing-forward
  (kbd "s-l") 'sp-forward-slurp-sexp
  (kbd "s-h") 'sp-forward-barf-sexp
  (kbd "M-h") 'sp-backward-slurp-sexp
  (kbd "M-l") 'sp-backward-barf-sexp
  (kbd "H-s") 'sp-split-sexp
  (kbd "C-t") 'sp-transpose-sexp
  (kbd "(") 'evil-lisp-mode-insert-sexp-before
  (kbd ")") 'evil-lisp-mode-insert-sexp-after
 )

(dolist (hook '(emacs-lisp-mode-hook
                lisp-interaction-mode-hook
                clojure-mode-hook))
  (add-hook hook (lambda()
                   (evil-lisp-mode))))
