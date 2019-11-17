;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;(setq debug-on-error t)
(setq package-check-signature nil)

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)
(unless package--initialized (package-initialize t))

;; set custom file in another place
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; set font
(set-frame-font "Inconsolata 16" nil t)
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
 					charset (font-spec :family "ZhunYuan" :size 22)))

(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(setq auto-save-default nil)
;; when startup, don't display modeline
(setq mode-line-format nil)
(prefer-coding-system 'utf-8)

(setq company-backends
	  '(company-files
		company-keywords
		company-capf))

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

(use-package projectile
  :ensure t
  :config (projectile-mode)
  :bind
  ("C-c p p" . projectile-switch-project)
  ("C-c p s" . projectile-ripgrep)
  ("C-c p f" . projectile-find-file))

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-tooltip-limit 5
		company-idle-delay 0.1
		company-echo-delay 0.1
		company-minimum-prefix-length 2
		company-tooltip-align-annotations t)
  :bind(:map company-active-map
			 ("C-n" . company-select-next)
			 ("C-p" . company-select-previous)
			 ("C-w" . nil)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode +1)
  (sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil))

(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode +1))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(use-package exec-path-from-shell
  :ensure t
  :config (setq exec-path-from-shell '("PATH" "GOPATH" "LANG" "LC_CTYPE" "GO111MODULE")))

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
		 ("C-c s" . counsel-rg))
  :config
  (setq counsel-rg-base-command "rg -S --no-heading --line-number --color never --hidden %s"))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode)
  (setq counsel-projectile-switch-project-action 'dired))

(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t)
  (setq monokai-height-minus-1 1.0
		monokai-height-plus-1 1.0
		monokai-height-plus-2 1.0
		monokai-height-plus-3 1.0
		monokai-height-plus-4 1.0))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
		 ("C-<" . mc/mark-previous-like-this)))

(use-package magit
  :ensure t
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  :bind (("C-x g" . magit-status))
  :config (setq magit-ediff-dwim-show-on-hunks t))

(use-package avy
  :ensure t)

(use-package anzu
  :ensure t
  :bind
  ([remap query-replace] . anzu-query-replace)
  ([remap query-replace-regexp] . anzu-query-replace-regexp))

;; Is will not take effect config in use-package.
(global-anzu-mode +1)

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-sideline-show-symbol nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-peek-fontify 'always)
  :bind(:map lsp-ui-mode-map
			 ([remap xref-find-definitions] . 'lsp-ui-peek-find-definitions)
			 ([remap xref-find-references] . 'lsp-ui-peek-find-references)))

(use-package flycheck
  :ensure t)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook(go-mode . lsp-deferred)
  :config
  (setq lsp-prefer-flymake nil))

(use-package go-mode
  :ensure t
  :config
  (setq gofmt-command "goimports"))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package yasnippet
  :ensure t)

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

(use-package powerline
  :ensure t
  :config
  (powerline-center-theme))

(use-package diminish
  :ensure t)

(diminish 'hungry-delete-mode)
(diminish 'smartparens-mode)
(diminish 'projectile-mode)
(diminish 'eldoc-mode)
(diminish 'ivy-mode)
(diminish 'yas-minor-mode)
(diminish 'anzu-mode)

(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
		((looking-at "\\s)") (forward-char 1) (backward-list 1))
		(t (self-insert-command (or arg 1)))))

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

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.vue\\'" "\\.tmpl\\'")
  :config
  (setq web-mode-markup-indent-offset 2
		web-mode-css-indent-offset 2
		web-mode-code-indent-offset 2
		web-mode-enable-current-element-highlight t
		web-mode-enable-css-colorization t
		web-mode-content-types-alist '(("vue" . "\\.vue\\'"))))

(use-package company-web
  :ensure t)

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'")
  :config (setq js2-mode-show-strict-warnings nil))

(use-package typescript-mode
  :ensure t
  :mode("\\.ts\\'"))

(use-package tide
  :ensure t)

(use-package prettier-js
  :ensure t)

(defun my/js-setup()
  (tide-setup)
  (eldoc-mode +1)
  (company-mode +1))

(defun my/ts-setup()
  (tide-setup)
  (eldoc-mode +1)
  (company-mode +1))

(defun my/web-setup()
  (cond ((equal web-mode-content-type "html")
         (my/web-html-setup))
        ((member web-mode-content-type '("vue"))
         (my/web-vue-setup))))

(defun my/web-vue-setup()
  (tide-setup)
  (eldoc-mode +1)
  (company-mode +1))

(defun my/web-html-setup()
  (tide-setup)
  (eldoc-mode +1)
  (company-mode +1))

(add-hook 'js2-mode-hook 'my/js-setup)
(add-hook 'typescript-mode-hook 'my/ts-setup)
(add-hook 'web-mode-hook 'my/web-setup)

;; Switch to the most recently selected buffer other than the current one.
(global-set-key (kbd "C-c <tab>") 'mode-line-other-buffer)
(sp-local-pair 'go-mode "{" nil :post-handlers '(("||\n[i]" "RET")))

(use-package graphviz-dot-mode
  :ensure t)

(use-package ggtags
  :ensure t)

(add-hook 'c-mode-common-hook
		  (lambda()
			(when (derived-mode-p 'c-mode 'c++-mode)
			  (ggtags-mode 1))))



;; (use-package company-posframe
;;   :ensure t)
;; (company-posframe-mode 1)
