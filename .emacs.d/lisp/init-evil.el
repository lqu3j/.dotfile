(when (maybe-require-package 'evil)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-down)
  )

(when (maybe-require-package 'evil-surround)
  (global-evil-surround-mode 1))
(provide 'init-evil)
