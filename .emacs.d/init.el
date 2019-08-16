(unless (>= emacs-major-version 27)
  (package-initialize))


(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
						 ("melpa" . "http://elpa.emacs-china.org/melpa/")))

;; set custom file in another place
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; set font
(set-frame-font "Inconsolata 16" nil t)
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
 					charset (font-spec :family "WenQuanYi Micro Hei" :size 18)))

(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(setq auto-save-default nil)
;; when startup, don't display modeline
(setq mode-line-format nil)

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
		company-minimum-prefix-length 2)
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
  :config (setq exec-path-from-shell '("PATH" "GOPATH" "LANG" "LC_CTYPE")))

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

(use-package leuven-theme
  :ensure t
  :config
  (load-theme 'leuven t)
  (setq leuven-scale-outline-headlines nil)
  (setq leuven-scale-org-agenda-structure nil))

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
  :bind(:map go-mode-map
			 ("M-?" . lsp-ui-peek-find-references)))

(use-package flycheck
  :ensure t)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-prefer-flymake nil))

(use-package go-mode
  :ensure t
  :config
  (setq gofmt-command "goimports"))

;; put hook out of use-package, because if put it use package is not working.
(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'before-save-hook 'gofmt-before-save)

(use-package yasnippet
  :ensure t)

(use-package markdown-mode
  :ensure t)

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

(global-set-key (kbd "M-s") 'swiper)
(save-place-mode +1)
