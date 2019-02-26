(maybe-require-package 'anaconda-mode)
(maybe-require-package 'pyvenv)
(maybe-require-package 'company-anaconda)
(add-hook 'python-mode-hook
          (lambda()
            (set (make-local-variable 'company-backends) '(company-anaconda))
            (anaconda-mode)
            (anaconda-eldoc-mode)
            ))

(provide 'init-python)
