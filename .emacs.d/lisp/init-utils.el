(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))


;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;;----------------------------------------------------------------------------
;; String utilities missing from core emacs
;;----------------------------------------------------------------------------
(defun sanityinc/string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))


;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;;----------------------------------------------------------------------------
;; Browse current HTML file
;;----------------------------------------------------------------------------
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

;;----------------------------------------------------------------------------
;; Toggle letter
;;----------------------------------------------------------------------------
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
;;----------------------------------------------------------------------------
;; scroll up or down
;;----------------------------------------------------------------------------
(defun scroll-half-page-down ()
  "scroll down half the page"
  (interactive)
  (scroll-down (/ (window-body-height) 2)))

(defun scroll-half-page-up ()
  "scroll up half the page"
  (interactive)
  (scroll-up (/ (window-body-height) 2)))

;;----------------------------------------------------------------------------
;; kill line if no region active
;;----------------------------------------------------------------------------
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
	 (list (line-beginning-position)
		   (line-beginning-position 2)))))

;;----------------------------------------------------------------------------
;; Put symbol at current point into search string.
;;----------------------------------------------------------------------------
(defun isearch-yank-symbol ()
  "*Put symbol at current point into search string."
  (interactive)
  (let ((sym (thing-at-point 'symbol)))
	(if sym
		(progn
		  (setq isearch-regexp t
				isearch-string (concat "\\_<" (regexp-quote sym) "\\_>")
				isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
				isearch-yank-flag t))
	  (ding)))
  (isearch-search-and-update))

(defun sanityinc/isearch-exit-other-end ()
  "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))
;;----------------------------------------------------------------------------
;; reapply themes
;;----------------------------------------------------------------------------
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
	(unless (custom-theme-p theme)
	  (load-theme theme t)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))


;;----------------------------------------------------------------------------
;; Newline and indent on appropriate pairs
;;----------------------------------------------------------------------------
(defvar gp/sp/post-command-count 0
  "Number of commands called after a pair has been opened.")

(defun gp/sp/create-newline-and-enter-sexp ()
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defun gp/sp/release-newline-post-command ()
  "Remove the hook and reset the post-command count."
  (remove-hook 'post-command-hook 'gp/sp/await-newline-post-command)
  (setq gp/sp/post-command-count 0))

(defun gp/sp/await-newline-post-command ()
  "If command is newline, indent and enter sexp."
  (if (> gp/sp/post-command-count 1)
      (gp/sp/release-newline-post-command)
    (progn
      (setq gp/sp/post-command-count (+ gp/sp/post-command-count 1))
      (when (memq this-command
                  '(newline newline-and-indent reindent-then-newline-and-indent))
        (gp/sp/release-newline-post-command)
        (gp/sp/create-newline-and-enter-sexp)))))

(defun gp/sp/await-newline (id action context)
  (when (eq action 'insert)
    (add-hook 'post-command-hook 'gp/sp/await-newline-post-command)))

(provide 'init-utils)
