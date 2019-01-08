;; -*- lexical-binding: t -*-
;;(setq debug-on-error t)

;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(let ((minver "24.4"))
  (when (version< emacs-version minver)
	(error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 20 1024 1024))
	  (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
		(lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-utils)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH


;;----------------------------------------------------------------------------
;; Basic config
;;----------------------------------------------------------------------------
(require 'cl)
(require 'init-basic)


;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------


(require 'init-ivy)
(require 'init-smex)
(require 'init-isearch)
(require 'init-company)
(require 'init-flycheck)
(require 'init-git)
(require 'init-org)
(require 'init-projectile)
(require 'init-recentf)
(require 'init-themes)
(require 'init-whitespace)
(require 'init-go)


;; expand-region
(maybe-require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; multiple-cursors
(maybe-require-package 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; avy
(maybe-require-package 'avy)
(global-set-key (kbd "C-'") 'avy-goto-char)
(global-set-key (kbd "C-;") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g e") 'avy-goto-word-0)

;; ace-window
(maybe-require-package 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(setq mark-ring-max 6)
(setq global-mark-ring-max 6)
(defun xah-pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'.
URL `http://ergoemacs.org/emacs/emacs_jump_to_previous_position.html'
Version 2016-04-04"
  (interactive)
  (set-mark-command t))
(global-set-key (kbd "<f7>") 'pop-global-mark)
(global-set-key (kbd "<f8>") 'xah-pop-local-mark-ring)


(defun scroll-half-page-down ()
  "scroll down half the page"
  (interactive)
  (scroll-down (/ (window-body-height) 2)))

(defun scroll-half-page-up ()
  "scroll up half the page"
  (interactive)
  (scroll-up (/ (window-body-height) 2)))

(global-set-key (kbd "C-v") 'scroll-half-page-up)
(global-set-key (kbd "M-v") 'scroll-half-page-down)

;;----------------------------------------------------------------------------
;; kill line if no region active                                          ;;
;;----------------------------------------------------------------------------
;; http://emacs-fu.blogspot.co.uk/2009/11/copying-lines-without-selecting-them.html
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
	 (list (line-beginning-position)
		   (line-beginning-position 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; change case of letters                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://ergoemacs.org/emacs/modernization_upcase-word.html
(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
	(if (region-active-p)
		(setq p1 (region-beginning) p2 (region-end))
	  (let ((bds (bounds-of-thing-at-point 'word) ) )
		(setq p1 (car bds) p2 (cdr bds)) ) )

	(when (not (eq last-command this-command))
	  (save-excursion
		(goto-char p1)
		(cond
		 ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
		 ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
		 ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
		 ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
		 ((looking-at "[[:upper:]]") (put this-command 'state "all caps") )
		 (t (put this-command 'state "all lower") ) ) )
	  )

	(cond
	 ((string= "all lower" (get this-command 'state))
	  (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
	 ((string= "init caps" (get this-command 'state))
	  (upcase-region p1 p2) (put this-command 'state "all caps"))
	 ((string= "all caps" (get this-command 'state))
	  (downcase-region p1 p2) (put this-command 'state "all lower")) )
	)
  )

;;set this to M-c
(global-set-key "\M-c" 'toggle-letter-case)

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

(setq c-default-style "linux"
	  c-basic-offset 4)

(maybe-require-package 'spaceline)
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(setq anzu-cons-mode-line-p nil)


(add-hook 'hungry-delete-mode-hook
		  (lambda ()
			(diminish 'hungry-delete-mode)))
(add-hook 'smartparens-mode-hook
		  (lambda ()
			(diminish 'smartparens-mode)))
(add-hook 'eldoc-mode-hook
		  (lambda ()
			(diminish 'eldoc-mode)))
(global-set-key (kbd "C-c s s") 'counsel-ag)
(global-set-key (kbd "C-x d") 'dired-jump)
(global-set-key (kbd "C-x b") 'counsel-ibuffer)
(global-set-key (kbd "C-x r") 'counsel-recentf)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:box nil :overline nil :underline nil))))
 '(mode-line-highlight ((t (:box nil))))
 '(mode-line-inactive ((t (:box nil :overline nil :underline nil)))))
;; ;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
