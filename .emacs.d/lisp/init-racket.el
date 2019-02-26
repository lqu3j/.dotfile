(maybe-require-package 'racket-mode)

(add-hook 'racket-mode-hook
          (lambda ()
            (define-key racket-mode-map (kbd "C-c r") 'racket-run)))

(setq tab-always-indent 'complete)
(provide 'init-racket)
