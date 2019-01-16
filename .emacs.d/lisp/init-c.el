(maybe-require-package 'ggtags)

(add-hook 'c-mode-common-hook
          (lambda()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (ggtags-mode 1))
            ))
(after-load 'ggtags
  (set-face-attribute 'ggtags-highlight nil
                      :underline nil))


(provide 'init-c)
