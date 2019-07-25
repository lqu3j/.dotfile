(setq package-archives '(("gnu-cn" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
                         ("org-cn" . "http://mirrors.cloud.tencent.com/elpa/org/")
                         ("melpa-cn" . "http://mirrors.cloud.tencent.com/elpa/gnu/")))

;; set custom file in another place
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; set font
(set-frame-font "Inconsolata 18" nil t)
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
 					charset (font-spec :family "WenQuanYi Micro Hei" :size 18)))

(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(setq auto-save-default nil)


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
  :bind (:map projectile-mode-map
			  ("C-c p" . projectile-command-map)))

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-tooltip-limit 5
		company-idle-delay 0.1
		company-echo-delay 0
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
  (smartparens-global-mode)
  (sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil))

(use-package hungry-delete
  :ensure t
  :hook (prog-mode . hungry-delete-mode))

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
		 ("M-x" . counsel-M-x)))

(use-package counsel-projectile
  :ensure t
  :config (counsel-projectile-mode))

(use-package solarized-theme
  :ensure
  :config
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-use-less-bold t)
  (setq solarized-use-more-italic t)
  (setq solarized-emphasize-indicators nil)
  
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  (load-theme 'solarized-light t))


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

(use-package diminish
  :ensure t)

(use-package anzu
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-prefer-flymake nil))

(use-package company-lsp
  :ensure t
  :commands company-lsp)
(use-package go-mode
  :ensure t
  :config (setq gofmt-command "goimports"))

;; put hook out of use-package, because if put it in use package is not working.
(add-hook 'go-mode-hook
	      (lambda()
			(flycheck-mode)
			(lsp-deferred)
			(add-to-list 'flycheck-disabled-checkers 'go-test)
	        (add-to-list 'flycheck-disabled-checkers 'go-unconvert)
	        (add-to-list 'flycheck-disabled-checkers 'go-errcheck)
            (add-to-list 'flycheck-disabled-checkers 'go-staticcheck)
	        (add-to-list 'flycheck-disabled-checkers 'go-vet)
	        (add-to-list 'flycheck-disabled-checkers 'go-gofmt)
	        (add-to-list 'flycheck-disabled-checkers 'go-golint)
	        ))
(add-hook 'before-save-hook 'gofmt-before-save)

(use-package yasnippet
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package avy
  :ensure
  :bind ("C-'" . avy-goto-char))
