;;----------------------------------------------------------------
;; install some dependency package
;;----------------------------------------------------------------
;; Highlight brackets according to their depth
(maybe-require-package 'rainbow-delimiters)
;; Automatic insertion, wrapping and paredit-like navigation with user defined pairs.
(maybe-require-package 'smartparens)
;; hungry delete minor mode
(maybe-require-package 'hungry-delete)
;; Jump to arbitrary positions in visible text and select text quickly.
(maybe-require-package 'avy)
;; Increase selected region by semantic units.
(maybe-require-package 'expand-region)
;; Multiple cursors for Emacs.
(maybe-require-package 'multiple-cursors)
;; Quickly switch windows.
(maybe-require-package 'ace-window)
;; Modeline configuration library for powerline
(maybe-require-package 'spaceline)
;; Show number of matches in mode-line while searching
(maybe-require-package 'anzu)
;; Get environment variables such as $PATH from the shell
(maybe-require-package 'exec-path-from-shell)
;; monokai-theme
(maybe-require-package 'monokai-theme)
;; visually highlight the selected buffer
(maybe-require-package 'dimmer)
;; Manage and navigate projects in Emacs easily
(maybe-require-package 'projectile)
;; Group ibuffer's list by projectile root
(maybe-require-package 'ibuffer-projectile)
;; Diminished modes are minor modes with no modeline display
(maybe-require-package 'diminish)
;;-----------------------------------------------------------------
;;Setting  hook
;;-----------------------------------------------------------------
;; startup rainbow-delimiters, smartparens, hungre-delete mode after prog mode start
(add-hook 'prog-mode-hook
	  (lambda()
	    (rainbow-delimiters-mode)
	    (smartparens-mode)
	    (hungry-delete-mode)
            (setq-default indent-tabs-mode nil)
            (setq tab-width 4)
            (defvaralias 'c-basic-offset 'tab-width)
            (defvaralias 'cperl-indent-level 'tab-width)
            (defvaralias 'default-tab-width 'tab-width)

	    ))
;; setting modeline without box
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
;; start anzu-mode globally
(add-hook 'after-init-hook 'global-anzu-mode)
;; start recentf-mode globally
(add-hook 'after-init-hook 'recentf-mode)
;; setting theme
(add-hook 'after-init-hook 'reapply-themes)
;; start dimmer-mode globally
(add-hook 'after-init-hook 'dimmer-mode)
;; start projectile-mode globally
(add-hook 'after-init-hook 'projectile-mode)
;;---------------------------------------------------------------
;; setting
;;---------------------------------------------------------------
(setq inhibit-splash-screen t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(setq anzu-cons-mode-line-p nil)
(after-load 'hungry-delete
  (diminish 'hungry-delete-mode))
(after-load 'smartparens
  (diminish 'smartparens-mode))
(after-load 'eldoc
  (diminish 'eldoc-mode))
(after-load 'smartparens
  (sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil)
  (sp-with-modes '(go-mode)
    (sp-local-pair "(" nil :post-handlers '(:add gp/sp/await-newline))
    (sp-local-pair "{" nil :post-handlers '(:add gp/sp/await-newline))
    (sp-local-pair "[" nil :post-handlers '(:add gp/sp/await-newline)))
  (set-face-attribute 'sp-pair-overlay-face nil
                      :background "default"))
(after-load 'exec-path-from-shell
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "GOPATH" "HUGO_BASE_DIR"))
    (add-to-list 'exec-path-from-shell-variables var)))
(when (memq window-system '(mac ns x))
  (setq-default exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))
(setq anzu-mode-lighter "")
(set-frame-font "InconsolataGo Nerd Font 16" nil t)
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
		    charset (font-spec :family "ZhunYuan" :size 18)))
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude '("/tmp/" "/ssh:"))
(setq-default custom-enabled-themes '(monokai))
(setq monokai-height-minus-1 1.0
      monokai-height-plus-1 1.0
      monokai-height-plus-2 1.0
      monokai-height-plus-3 1.0
      monokai-height-plus-4 1.0)
(setq-default dimmer-fraction 0.15)
(setq-default projectile-mode-line-prefix " Proj")
(after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
;;---------------------------------------------------------------
;; keybindings
;;---------------------------------------------------------------
(global-set-key (kbd "C-v") 'scroll-half-page-up)
(global-set-key (kbd "M-v") 'scroll-half-page-down)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-'") 'avy-goto-char)
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-x d") 'dired-jump)
(global-set-key (kbd "M-c") 'toggle-letter-case)
(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(after-load 'isearch
  (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)
  (when (fboundp 'isearch-occur)
    (define-key isearch-mode-map (kbd "C-c C-o") 'isearch-occur)))
(define-key isearch-mode-map "\C-\M-w" 'isearch-yank-symbol)
(define-key isearch-mode-map [(control return)] 'sanityinc/isearch-exit-other-end)

(provide 'init-core)
