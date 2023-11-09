;; init.el --- Load the full configuration -*- lexical-binding: t -*-
(add-hook 'after-init-hook
          #'(lambda ()
              (setq gc-cons-threshold (* 100 1000 1000))))
(add-hook 'focus-out-hook 'garbage-collect)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(run-with-idle-timer 5 t 'garbage-collect)
(setq read-process-output-max (* 50 1024 1024))

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
  (setq company-idle-delay 0.1
        company-tooltip-idle-delay 0.1
        company-echo-delay 0
		company-minimum-prefix-length 2
		company-tooltip-align-annotations t
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-dabbrev-other-buffers nil
        company-require-match 'never
        company-auto-complete nil
        company-auto-complete-chars nil)
        ;; company-frontends '(company-pseudo-tooltip-frontend  ; always show candidates in overlay tooltip
		;; 					company-echo-metadata-frontend))  ; show selected candidate docs in echo area)
  
  :bind(:map company-active-map
			 ("C-n" . company-select-next)
			 ("C-p" . company-select-previous)
			 ("C-w" . nil))
  )

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

(use-package ivy
  :ensure t
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t))

(use-package ivy-prescient
  :ensure t
  :config
  (prescient-persist-mode)
  (ivy-prescient-mode +1))

(use-package counsel
  :ensure t
  :bind (("C-x b" . counsel-switch-buffer)
		 ("C-x C-r" . counsel-recentf)
		 ("M-x" . counsel-M-x)
		 ("C-c C-s" . counsel-rg))
  :config
  (setq counsel-async-command-delay 0.25)
  (setq counsel-rg-base-command `("rg" "-M" "240" "--with-filename" "--no-heading" "--smart-case"  "--line-number" "--color" "never" "%s" ".")))

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


(use-package go-mode
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

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(setq doom-modeline-buffer-file-name-style 'truncate-upto-root)
(setq doom-modeline-icon nil)
(setq doom-modeline-buffer-encoding t)
(setq doom-modeline-before-update-env-hook nil)
(setq doom-modeline-after-update-env-hook nil)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))
  

(use-package crux
  :ensure t
  :bind
  ([remap move-beginning-of-line] . crux-move-beginning-of-line))

(global-set-key (kbd "M-s") 'swiper)
(save-place-mode +1)

;; (defun window-half-height ()
;;   (max 1 (/ (1- (window-height (selected-window))) 2)))

;; (defun scroll-up-half ()
;;   (interactive)
;;   (scroll-up (window-half-height)))

;; (defun scroll-down-half ()         
;;   (interactive)                    
;;   (scroll-down (window-half-height)))

;; (global-set-key (kbd "C-v") 'scroll-up-half)
;; (global-set-key (kbd "M-v") 'scroll-down-half)

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

(global-set-key (kbd "C-t") 'vterm-toggle)
(define-key vterm-mode-map (kbd "C-t")   #'vterm-toggle)

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
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode)))

(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-files (list "~/notes/todo.org"))

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(p!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

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

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq web-beautify-args '("-f" "-" "-s 2"))

(with-eval-after-load 'dired
  (define-key dired-mode-map "b" 'dired-up-directory)
  (define-key dired-mode-map "i" 'ido-find-file)
  (setq dired-listing-switches "-hlv"))

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

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(setq org-adapt-indentation t)

;; 禁止bold字体
;; (defun my/disable-bold-font()
;;   (set-face-bold-p 'bold nil)
;;   (mapc
;;    (lambda (face)
;;      (set-face-attribute face nil :weight 'normal :underline nil))
;;    (face-list)))

;; 保证org-mode table中英文字体对齐
(defun my/org-mode-font()
  (make-face 'width-font-face)
  (set-face-attribute 'width-font-face nil :font "Sarasa Mono SC Nerd 15")
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

;; (with-eval-after-load 'dired
;;   (my/disable-bold-font))

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


(setq org-agenda-files (quote ("~/notes/notes.org"
                               "~/notes/work.org")))


(add-to-list 'display-buffer-alist
             '("^\\*vterm"
	           (display-buffer-reuse-window
	            display-buffer-in-side-window)
	           (reusable-frames . visible)
	           (side            . bottom)
	           (window-height   . 0.33)))


(add-to-list 'display-buffer-alist
             '("^\\*Flymake"
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


(cl-defgeneric project-files (project &optional dirs)
  "Override `project-files' to use `fd' in local projects."
  (mapcan #'my/project-files-in-directory
          (or dirs (list (project-root project)))))

(with-eval-after-load 'project
  (setq project-switch-commands 'project-find-file)
  )


(setq company-backends
      '(company-files
        (company-capf :with company-yasnippet)))

(setq completion-ignore-case t)
(setq org-clock-persist t)
(setq org-clock-persist-query-resume nil)
(setq org-clock-persist-query-save nil)
(setq org-clock-in-resume t)
(setq org-clock-into-drawer t)
(setq org-clock-out-when-done t)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/notes/work.org" "todo list")
         "* TODO %?\n  SCHEDULED: %t")))

(global-set-key (kbd "C-c c") #'org-capture)

(add-hook 'org-after-todo-state-change-hook
          (lambda ()
            (when (string= org-state "IN-PROGRESS")
              (org-clock-in)
              )))

(add-hook 'org-after-todo-state-change-hook
          (lambda ()
            (when (string= org-state "WAIT")
              (org-clock-out)
              )))

(define-key org-mode-map (kbd "C-c C-w") nil)
(global-set-key (kbd "C-c C-w") #'org-refile-to-datetree)


(defun org-refile-to-datetree ()
  "Refile a subtree to a datetree corresponding to it's timestamp.

The current time is used if the entry has no timestamp. If FILE
is nil, refile in the current file."
  (interactive)
  (let* ((file "~/notes/journal.org")
         (datetree-date (or (org-entry-get nil "SCHEDULED" t)
                            (org-read-date t nil "now")))
         (date (org-date-to-gregorian datetree-date))
         )
    (with-current-buffer (current-buffer)
      (save-excursion
        (org-cut-subtree)
        (if file (find-file file))
        (org-datetree-find-date-create date)
        (org-narrow-to-subtree)
        (show-subtree)
        (org-end-of-subtree t)
        (newline)
        (goto-char (point-max))
        (org-paste-subtree 4)
        (widen)
        ))))

(use-package wgrep
  :ensure t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))


(use-package string-inflection
  :ensure t)

(use-package leuven-theme
  :ensure t)

(use-package modus-themes
  :ensure t)

(use-package dracula-theme
  :ensure t)

(load-theme 'doom-gruvbox t)

(defun counsel-rg-project (dir)
  (interactive (list (project-prompt-project-dir)))
  (counsel-rg "" dir)
  )
(global-set-key (kbd "C-x p s") 'counsel-rg-project)


(use-package eglot
  :ensure t
  :config
   (setq eglot-stay-out-of '(company)))


(defun eglot-organize-imports ()
  "Offer to execute code actions `source.organizeImports'."
  (interactive)
  (unless (eglot--server-capable :codeActionProvider)
    (eglot--error "Server can't execute code actions!"))
  (let* ((server (eglot--current-server-or-lose))
         (actions (jsonrpc-request
                   server
                   :textDocument/codeAction
                   (list :textDocument (eglot--TextDocumentIdentifier))))
         (action (cl-find-if
                  (jsonrpc-lambda (&key kind &allow-other-keys)
                    (string-equal kind "source.organizeImports" ))
                  actions)))
    (when action
      (eglot--dcase action
        (((Command) command arguments)
         (eglot-execute-command server (intern command) arguments))
        (((CodeAction) edit command)
         (when edit (eglot--apply-workspace-edit edit))
         (when command
           (eglot--dbind ((Command) command arguments) command
             (eglot-execute-command server (intern command) arguments))))))))

;; (defun project-find-go-module (dir)
;;   (when-let ((root (locate-dominating-file dir "go.mod")))
;;     (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

;; (add-hook 'project-find-functions #'project-find-go-module)

(setq-default eglot-workspace-configuration
              '((:gopls .
                        ((matcher . "CaseInSensitive")
                         (usePlaceholders . t)
                         (hoverKind . "NoDocumentation")
                         ))))

(setq eldoc-echo-area-use-multiline-p nil)
(use-package yasnippet
  :ensure t
  :defer 15 ;; takes a while to load, so do it async
  :diminish yas-minor-mode
  :custom (yas-prompt-functions '(yas-completing-prompt)))

(defun lx/eglot-organize-imports ()
  (eglot-code-actions nil nil "source.organizeImports" t))

(defun lx/go-mode-hook ()
  (yas-minor-mode)
  (eglot-ensure)
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
  (add-hook 'before-save-hook #'lx/eglot-organize-imports 10 t)
  (setq go-ts-mode-indent-offset 4)
  (breadcrumb-local-mode)
  (setq go-test-args "--count=1")
  (setq compile-command "go build"))

(add-hook 'go-ts-mode-hook #'lx/go-mode-hook)
(define-key flymake-mode-map (kbd "C-c d") 'flymake-show-project-diagnostics)
(define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)

(use-package markdown-preview-mode
  :ensure t)
(setq markdown-command "/usr/bin/pandoc")

(setq inhibit-automatic-native-compilation nil)


(defun parse-url (url)
  "convert a git remote location as a HTTP URL"
  (if (string-match "^http" url)
      url
    (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
                              "https://\\2/\\3"
                              url)))
(defun magit-open-repo ()
  "open remote repo URL"
  (interactive)
  (magit-copy-section-value 0)
  (let ((url (concat (magit-get "remote" "origin" "url") "/commit/" (car kill-ring))))
    (progn
      (browse-url (parse-url url))
      (message "opening repo %s" url))))



(add-hook 'magit-log-mode-hook
          (lambda ()
            (local-set-key (kbd "o") 'magit-open-repo)))




;; Sample jar configuration
(setq plantuml-jar-path "/usr/share/plantuml/lib/plantuml.jar")
(setq org-plantuml-jar-path "/usr/share/plantuml/lib/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)
(setq plantuml-output-type "png")
;; Sample executable configuration
(setq plantuml-default-exec-mode 'executable)


(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t))) ; this line activates plantuml

(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-mode))

(ivy-rich-set-columns
 'ivy-switch-buffer
 '(
   (ivy-rich-switch-buffer-indicators (:width 1 :face error :align left))
   (ivy-switch-buffer-transformer (:width 0.3))
   (ivy-rich-switch-buffer-size (:width 7))
   (ivy-rich-switch-buffer-path (:width 0.5))
   ))


(setq ivy-rich-path-style 'abbrev)


(setq dired-dwim-target t)

(use-package yasnippet-snippets
  :ensure t)

(add-to-list 'load-path "~/.emacs.d/plugins/breadcrumb")
(require 'breadcrumb)

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (php "https://github.com/tree-sitter/tree-sitter-php")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))


(setopt treesit-font-lock-level 4)

(setq major-mode-remap-alist
      '((go-mode . go-ts-mode)))

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (go-mode . go-ts-mode)))

(use-package dimmer
  :ensure t
  :config
  (dimmer-mode))

(use-package go-tag
  :load-path "~/.emacs.d/plugins/emacs-go-tag")

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
(add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight))



(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-interpolate-page t)
(defun +pixel-scroll-interpolate-down (&optional lines)
  (interactive)
  (if lines
      (pixel-scroll-precision-interpolate (* -1 lines (pixel-line-height)))
    (pixel-scroll-interpolate-down)))

(defun +pixel-scroll-interpolate-up (&optional lines)
  (interactive)
  (if lines
      (pixel-scroll-precision-interpolate (* lines (pixel-line-height))))
  (pixel-scroll-interpolate-up))

(defalias 'scroll-up-command '+pixel-scroll-interpolate-down)
(defalias 'scroll-down-command '+pixel-scroll-interpolate-up)


(setq display-line-numbers-width 5)


(defun company-yasnippet/disable-after-dot (fun command &optional arg &rest _ignore)
  (if (eq command 'prefix)
      (let ((prefix (funcall fun 'prefix)))
        (when (and prefix (not
                           (eq
                            (char-before (- (point) (length prefix)))
                            ?.)))
          prefix))
    (funcall fun command arg)))

(advice-add #'company-yasnippet :around #'company-yasnippet/disable-after-dot)


(defun lx/company-yasnippet-advice (fun command &optional arg &rest _ignore)
  (if (eq command 'prefix)
      (let ((prefix (funcall fun command)))
        (unless (or (not prefix) (eq (char-before (- (point) (length prefix))) ?.)) prefix))
    (when (and arg (not (get-text-property 0 'yas-annotation-patch arg)))
      (let* ((name (get-text-property 0 'yas-annotation arg))
             (snip (format "%s (Snip)" name))
             (leng (length arg)))
        (put-text-property 0 leng 'yas-annotation snip arg)
        (put-text-property 0 leng 'yas-annotation-patch t arg)))
    (funcall fun command arg)))

(advice-add #'company-yasnippet :around #'lx/company-yasnippet-advice)




(global-set-key (kbd "C-c i") 'string-inflection-cycle)



(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
      (backward-char) (insert "\n") (setq end (1+ end)))
    (indent-region begin end))
  (message "Ah, much better!"))


(use-package apheleia
  :ensure t)

(use-package forge
  :ensure t)

(setq magit-refs-margin '(t "%Y-%m-%d %H:%M:%S" magit-log-margin-width t 18))
(setq magit-log-margin '(t "%Y-%m-%d %H:%M:%S" magit-log-margin-width t 18))

(setq auth-sources '("~/.ssh/authinfo"))

(with-eval-after-load 'forge
  (add-to-list 'forge-alist
               '("git.zx-tech.net"
                 "git.zx-tech.net/api/v1"
                 "git.zx-tech.net"
                 forge-gitea-repository)))



;; we recommend using use-package to organize your init.el
(use-package codeium
  :load-path "~/.emacs.d/plugins/codeium.el"
  ;; if you use straight
  ;; :straight '(:type git :host github :repo "Exafunction/codeium.el")
  ;; otherwise, make sure that the codeium.el file is on load-path

  :init
  ;; use globally
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  ;; or on a hook
  ;; (add-hook 'go-ts-mode-hook
  ;;     (lambda ()
  ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

  ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
  (add-hook 'python-mode-hook
      (lambda ()
          (setq-local completion-at-point-functions
              (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))
  ;; an async company-backend is coming soon!

  ;; codeium-completion-at-point is autoloaded, but you can
  ;; optionally set a timer, which might speed up things as the
  ;; codeium local language server takes ~0.2s to start up
  ;; (add-hook 'emacs-startup-hook
  ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

  ;; :defer t ;; lazy loading, if you want
  :config
  (setq use-dialog-box nil) ;; do not use popup boxes

  ;; if you don't want to use customize to save the api-key
  ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

  ;; get codeium status in the modeline
  (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
  ;; alternatively for a more extensive mode-line
  ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

  ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
  ;; you can also set a config for a single buffer like this:
  ;; (add-hook 'python-mode-hook
  ;;     (lambda ()
  ;;         (setq-local codeium/editor_options/tab_size 4)))

  ;; You can overwrite all the codeium configs!
  ;; for example, we recommend limiting the string sent to codeium for better performance
  (defun my-codeium/document/text ()
    (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
  ;; if you change the text, you should also change the cursor_offset
  ;; warning: this is measured by UTF-8 encoded bytes
  (defun my-codeium/document/cursor_offset ()
    (codeium-utf8-byte-length
     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
  (setq codeium/document/text 'my-codeium/document/text)
  (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))
