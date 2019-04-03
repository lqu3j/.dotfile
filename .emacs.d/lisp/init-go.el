;;----------------------------------------------------------------
;; install some dependency package
;;----------------------------------------------------------------
(maybe-require-package 'go-mode)
(maybe-require-package 'company-go)
(maybe-require-package 'go-eldoc)
(maybe-require-package 'flycheck)
;;----------------------------------------------------------------
;; basic setting
;;----------------------------------------------------------------
(setq flycheck-go-build-install-deps t)
(setq company-go-gocode-args '("-builtin" "-unimported-packages"))
(setq company-go-show-annotation t)
(setq gofmt-command "goimports")

;;-----------------------------------------------------------------
;;Setting  hook
;;-----------------------------------------------------------------
;; setting go-eldoc and comapny-backends, of course start up flycheck
(add-hook 'go-mode-hook
	      (lambda()
	        (set (make-local-variable 'company-backends) '(company-go))
            (set (make-local-variable 'flycheck-display-errors-delay) 0.5)
	        (go-eldoc-setup)
	        (flycheck-mode)
	        (add-to-list 'flycheck-disabled-checkers 'go-test)
	        (add-to-list 'flycheck-disabled-checkers 'go-unconvert)
	        (add-to-list 'flycheck-disabled-checkers 'go-errcheck)
            (add-to-list 'flycheck-disabled-checkers 'go-staticcheck)
            ;;(add-to-list 'flycheck-disabled-checkers 'go-build)
	        (add-to-list 'flycheck-disabled-checkers 'go-vet)
	        (add-to-list 'flycheck-disabled-checkers 'go-gofmt)
	        (add-to-list 'flycheck-disabled-checkers 'go-golint)
	        ))
;; auto format after save
(add-hook 'before-save-hook 'gofmt-before-save)

;;---------------------------------------------------------------
;; keybindings
;;---------------------------------------------------------------
(after-load 'go-mode
  (define-key go-mode-map (kbd "M-.") 'godef-jump))

(provide 'init-go)
