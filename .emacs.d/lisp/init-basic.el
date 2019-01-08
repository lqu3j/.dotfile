(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;; Setting font
(set-frame-font "InconsolataGo Nerd Font 16" nil t)
(dolist (charset '(kana han symbol cjk-misc bopomofo))
   (set-fontset-font (frame-parameter nil 'font)
					 charset (font-spec :family "ZhunYuan" :size 18)))

;; Seting prog-mode
(add-hook 'prog-mode-hook (lambda()
				(when (maybe-require-package 'rainbow-delimiters)
				  (rainbow-delimiters-mode))
				(when (maybe-require-package 'smartparens)
				  (smartparens-mode)
				  (sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil))
				(when (maybe-require-package 'hungry-delete)
				  (hungry-delete-mode))
				(setq default-tab-width 4)
				(setq tab-width 4)
				))


;;(when (maybe-require-package 'expand-region)
;;  (global-set-key (kbd "C-=") 'er/expand-region))

(provide 'init-basic)
