;; init.el --- Load the full configuration -*- lexical-binding: t -*-
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(setq read-process-output-max (* 1024 1024))

(setq debug-on-error nil)
(setq package-check-signature nil)

(setq package-archives
      '(("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
        ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
        ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")))

(package-initialize t)
(unless package--initialized (package-initialize t))

;; set custom file in another place
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(add-to-list 'default-frame-alist
             '(font . "InconsolataGo Nerd Font Mono 15"))

(defun lx/set-font(&optional f)
  (set-fontset-font "fontset-default" 'han (font-spec :family "Microsoft YaHei")))

(add-hook 'after-make-frame-functions 'lx/set-font)
(if (display-graphic-p)
    (lx/set-font))

(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(setq auto-save-default nil)
;; when startup, don't display modeline
(setq mode-line-format nil)
(prefer-coding-system 'utf-8)

(add-hook 'emacs-startup-hook
		  (lambda()
			(set-face-attribute 'mode-line nil
								:box nil
								:overline nil
								:underline nil)
			(set-face-attribute 'mode-line-inactive nil
								:box nil
								:overline nil
								:underline nil)
			))


(recentf-mode)
;; automate install use-pacakge
(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

(setq-default tab-width 4)

(use-package expand-region
  :ensure t
  :bind(("C-=" . er/expand-region)))

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :init
  (setq company-idle-delay 0
        company-tooltip-idle-delay 0
        company-echo-delay 0
		company-minimum-prefix-length 2
		company-tooltip-align-annotations t
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-dabbrev-other-buffers nil
        company-require-match 'never
        company-auto-complete nil
        company-auto-complete-chars nil
        company-frontends '(company-pseudo-tooltip-frontend  ; always show candidates in overlay tooltip
							company-echo-metadata-frontend))  ; show selected candidate docs in echo area)
  
  :bind(:map company-active-map
			 ("C-n" . company-select-next)
			 ("C-p" . company-select-previous)
			 ("C-w" . nil))
  )
(setq company-backends '(company-capf))


(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :ensure t
  :config
  (setq sp-highlight-pair-overlay nil)
  (smartparens-global-mode +1)
  (sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil))

(defun indent-between-pair (&rest _ignored)
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
(sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
(sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET")))

(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode +1))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(use-package exec-path-from-shell
  :ensure t
  :config (setq exec-path-from-shell '("PATH" "GOPATH" "LANG" "LC_CTYPE" "GO111MODULE" "GOFLAGS" "GTK_IM_MODULE" "XMODIFIERS" "QT_IM_MODULE")))

(use-package smex
  :ensure t
  :config (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  :bind ([remap execute-extended-command] . smex))

(use-package ivy
  :ensure t
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t))

(use-package counsel
  :ensure t
  :bind (("C-x b" . counsel-ibuffer)
		 ("C-x C-r" . counsel-recentf)
		 ("M-x" . counsel-M-x)
		 ("C-c C-s" . counsel-rg))
  :config
  (setq counsel-rg-base-command "rg --max-columns 500 --max-columns-preview --with-filename --no-heading --line-number --max-filesize 1M --color never %s"))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
		 ("C-<" . mc/mark-previous-like-this)))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)

  ;; Have magit-status go full screen and quit to previous
  ;; configuration.  Taken from
  ;; http://whattheemacsd.com/setup-magit.el-01.html#comment-748135498
  ;; and http://irreal.org/blog/?p=2253
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defadvice magit-quit-window (after magit-restore-screen activate)
    (jump-to-register :magit-fullscreen))
  :config
  (setq magit-ediff-dwim-show-on-hunks t))

(use-package avy
  :ensure t)

(use-package anzu
  :ensure t
  :bind
  ([remap query-replace] . anzu-query-replace)
  ([remap query-replace-regexp] . anzu-query-replace-regexp))

;; Is will not take effect config in use-package.
(global-anzu-mode +1)

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-diagnostic-package :flycheck)
  (setq lsp-log-io nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-text-document-color nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-gopls-use-placeholders t)
  (setq lsp-gopls-hover-kind "NoDocumentation")
  (setq lsp-completion-provider :none)
  (setq lsp-eldoc-render-all nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-signature-auto-activate t)
  (setq lsp-idle-delay 0.1)
  (setq lsp-completion--no-reordering t)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-enable-dap-auto-configure nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  :config
  (with-eval-after-load 'lsp-mode
    (setq lsp-modeline-diagnostics-scope :workspace))
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.matcher" "CaseInsensitive")
     ("gopls.hoverKind" "NoDocumentation")
     ("gopls.staticcheck" t t)
     ))
  :bind(:map lsp-mode-map
			 ([remap xref-find-definitions] . 'lsp-find-definition)
			 ([remap xref-find-references] . 'lsp-find-references)))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook
          (lambda()
            (lsp-deferred)
            (yas-minor-mode)
            (lsp-go-install-save-hooks)
            (setq compile-command "go build")
            ))


(use-package go-mode
  :ensure t
  :config
  (setq gofmt-command "goimports"))

(use-package yasnippet
  :ensure t
  :commands yas-minor-mode)

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  (setq markdown-css-paths '("http://thomasf.github.io/solarized-css/solarized-light.min.css")))

(use-package avy
  :ensure
  :bind ("C-'" . avy-goto-char))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(setq doom-modeline-env-version t)
(setq doom-modeline-env-enable-go t)
(setq doom-modeline-env-go-executable "go")
(setq doom-modeline-lsp t)
(setq doom-modeline-workspace-name t)
(setq doom-modeline-icon nil)
(setq doom-modeline-buffer-file-name-style 'auto)
(setq doom-modeline-buffer-modification-icon t)
(setq doom-modeline-buffer-encoding t)
(setq doom-modeline-env-load-string "...")
(setq doom-modeline-before-update-env-hook nil)
(setq doom-modeline-after-update-env-hook nil)


(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one	 t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))
  

(use-package crux
  :ensure t
  :bind
  ([remap move-beginning-of-line] . crux-move-beginning-of-line))

(global-set-key (kbd "M-s") 'swiper)
(save-place-mode +1)

(defun window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun scroll-up-half ()
  (interactive)
  (scroll-up (window-half-height)))

(defun scroll-down-half ()         
  (interactive)                    
  (scroll-down (window-half-height)))

(global-set-key (kbd "C-v") 'scroll-up-half)
(global-set-key (kbd "M-v") 'scroll-down-half)

(use-package json-mode
  :ensure t)

;; Switch to the most recently selected buffer other than the current one.
(global-set-key (kbd "C-c <tab>") 'mode-line-other-buffer)
(sp-local-pair 'go-mode "{" nil :post-handlers '(("||\n[i]" "RET")))

(setq js-indent-level 2)

(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2))

(use-package better-defaults
  :ensure t)

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))

(use-package goto-chg
  :ensure t)

(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package gotest
  :ensure t
  :config
  (setq go-test-verbose t))

(setq visible-bell nil)

(use-package yaml-mode
  :ensure t)

(setq org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCEL")))


;; (with-eval-after-load 'org
;;   ()
;;   (add-hook 'org-mode-hook 'org-buffer-face-mode-variable)
;;   (add-hook 'org-agenda-mode-hook 'org-buffer-face-mode-variable))

(global-set-key (kbd "C-c o") 'crux-open-with)


(use-package ob-http
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (http . t)))

(use-package restart-emacs
  :ensure t)


(use-package vterm
  :ensure t)

(use-package vterm-toggle
  :ensure t)

(setq vterm-toggle-fullscreen-p nil)

(global-set-key (kbd "C-`") 'vterm-toggle)

(setq company-tooltip-align-annotations t)

(use-package goto-chg
  :ensure t)

(global-set-key (kbd "C-<tab>") 'goto-last-change)


(use-package so-long
  :ensure nil
  :config (global-so-long-mode 1))

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

(setq dired-listing-switches "-Afhlv")

;; 修改自 https://www.emacswiki.org/emacs/DiredOmitMode
(define-advice dired-do-print (:override (&optional _))
    "Show/hide dotfiles."
    (interactive)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p)
        (progn
          (setq-local dired-dotfiles-show-p nil)
          (dired-mark-files-regexp "^\\.")
          (dired-do-kill-lines))
      (revert-buffer)
      (setq-local dired-dotfiles-show-p t)))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))


(use-package web-mode
  :ensure t
  :config
  (setq web-mode-enable-auto-closing t) ; enable auto close tag in text-mode
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode)))

(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-files (list "~/Dropbox/org/todo.org"))

(setq org-todo-keywords
      '((sequence "TODO(t!)" "NEXT(n!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev                 ; 搜索当前 buffer
        try-expand-dabbrev-all-buffers     ; 搜索所有 buffer
        try-complete-file-name-partially   ; 文件名部分匹配
        try-complete-file-name))           ; 文件名匹配

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))


(setq warning-minimum-level :error)

(use-package go-tag
  :ensure t)

(use-package company-posframe
  :ensure t
  :init
  (setq company-posframe-quickhelp-delay nil)
  (setq company-posframe-show-indicator nil)
  (setq company-posframe-show-metadata nil))
(company-posframe-mode 1)

(add-hook 'c-mode-hook 'lsp-deferred)
(add-hook 'js-mode-hook 'lsp-deferred)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq web-beautify-args '("-f" "-" "-s 2"))

(define-key dired-mode-map "b" 'dired-up-directory)
(define-key dired-mode-map "i" 'ido-find-file)

(define-key go-mode-map (kbd "C-c s") 'lsp-ivy-workspace-symbol)

;; 解决daemon模式下,cursor颜色异常的BUG
(require 'frame)
(defun set-cursor-hook (frame)
(modify-frame-parameters
  frame (list (cons 'cursor-color "DeepSkyBlue"))))
(add-hook 'after-make-frame-functions 'set-cursor-hook)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package beacon
  :ensure t
  :config
  (beacon-mode))

(use-package lsp-ivy
  :ensure t)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(setq org-adapt-indentation t)

;; 禁止bold字体
(defun my/disable-bold-font()
  (set-face-bold-p 'bold nil)
  (mapc
   (lambda (face)
     (set-face-attribute face nil :weight 'normal :underline nil))
   (face-list)))

;; 保证org-mode table中英文字体对齐
(defun my/org-mode-font()
  (make-face 'width-font-face)
  (set-face-attribute 'width-font-face nil :font "Sarasa Mono SC 15")
  (setq buffer-face-mode-face 'width-font-face)
  (buffer-face-mode))

(defun my/org-mode-hook ()
  ;; stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :weight 'normal))
  
  ;; org-mode hide leading starts
  (setq org-hide-leading-stars t))

(add-hook 'org-mode-hook #'my/org-mode-hook)

(add-hook 'org-agenda-mode-hook #'my/org-mode-font)

(with-eval-after-load 'dired
  (my/disable-bold-font))

(use-package blamer
  :ensure t
  :defer 20
  :custom
  (blamer-idle-time 0.2)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 140
                   :italic t)))
  :config
  (setq blamer-commit-formatter " %s"))


(setq org-agenda-files (quote ("~/Dropbox/org/notes.org"
                               "~/Dropbox/org/work.org")))


(add-to-list 'display-buffer-alist
             '("^\\*vterm"
	           (display-buffer-reuse-window
	            display-buffer-in-side-window)
	           (reusable-frames . visible)
	           (side            . bottom)
	           (window-height   . 0.33)))


(add-to-list 'display-buffer-alist
             '("^\\*Flycheck"
	           (display-buffer-reuse-window
	            display-buffer-in-side-window)
	           (reusable-frames . visible)
	           (side            . bottom)
	           (window-height   . 0.33)))

(add-to-list 'display-buffer-alist
             '("^\\*compilation"
	           (display-buffer-reuse-window
	            display-buffer-in-side-window)
	           (reusable-frames . visible)
	           (side            . bottom)
	           (window-height   . 0.33)))
(add-to-list 'display-buffer-alist
             '("^\\*xref"
	           (display-buffer-reuse-window
	            display-buffer-in-side-window)
	           (reusable-frames . visible)
	           (side            . bottom)
	           (window-height   . 0.33)))


(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))

(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(defun my/project-files-in-directory (dir)
  "Use `fd' to list files in DIR."
  (let* ((default-directory dir)
         (localdir (file-local-name (expand-file-name dir)))
         (command (format "fd -H -t f -0 . %s" localdir)))
    (project--remote-file-names
     (sort (split-string (shell-command-to-string command) "\0" t)
           #'string<))))

(cl-defmethod project-files ((project (head local)) &optional dirs)
  "Override `project-files' to use `fd' in local projects."
  (mapcan #'my/project-files-in-directory
          (or dirs (list (project-root project)))))


(with-eval-after-load 'project
  (setq project-switch-commands 'project-find-file)
  )
