(require-package 'yaml-mode)

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("^spec\\.conf$" . yaml-mode))
(add-to-list 'auto-mode-alist '("^versions\\.conf$" . yaml-mode))

(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)
             (define-key yaml-mode-map [RET] 'newline-and-indent)
             ))

(provide 'init-yaml)
