;;----------------------------------------------------------------------------
;; Misc config - yet to be placed in separate files
;;----------------------------------------------------------------------------
(add-auto-mode 'tcl-mode "Portfile\\'")
(fset 'yes-or-no-p 'y-or-n-p)

(dolist (hook (if (fboundp 'prog-mode)
                  '(prog-mode-hook ruby-mode-hook)
                '(find-file-hooks)))
  (add-hook hook 'goto-address-prog-mode))
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(setq goto-address-mail-face 'link)

(setq-default regex-tool-backend 'perl)

(add-auto-mode 'conf-mode "Procfile")

(setq woman-use-own-frame nil)
(setq woman-fill-frame t)
(setq woman-cache-filename "~/.emacs.d/woman-cache/woman.el")

(global-set-key (kbd "C-c w")
                (lambda ()
                  (interactive)
                  (let ((woman-use-topic-at-point t))
                    (woman))))

(add-hook 'Info-mode-hook
          (lambda ()
            (setq Info-additional-directory-list Info-default-directory-list)))

(require-package 'rst)
(setq auto-mode-alist
      (append '(("\\.rst\\'" . rst-mode)
                ("\\.rest\\'" . rst-mode)) auto-mode-alist))
(add-hook 'rst-adjust-hook 'rst-toc-update)

(add-hook 'find-file-hook (lambda () (linum-mode 1)))


(provide 'init-misc)
