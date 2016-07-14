;; (require-package 'deferred)
;; (require-package 'jedi)
(require-package 'pip-requirements)
;; (require-package 'smartparens-mode)

;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)

(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))


(defun alzuse/python-mode-common-hook()
  (setq mode-name "Python")
  (setq python-indent-offset 4)
  (setq tab-width 4)
  (setq fill-column python-fill-column)
  ;; auto-indent on colon doesn't work well with if statement
  (setq electric-indent-chars (delq ?: electric-indent-chars))
  (require-package 'anaconda-mode)
  (anaconda-mode)
  (local-set-key (kbd "C-j") 'newline-and-indent))

(defun python-setup-shell ()
  (if (executable-find "ipython")
      (progn
        (setq python-shell-interpreter "ipython")
        (when (version< emacs-version "24.4")
          ;; these settings are unnecessary and even counter-productive on emacs 24.4 and newer
          (setq python-shell-prompt-regexp "In \\[[0-9]+\\]: "
                python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
                python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
                python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
                python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))
    (setq python-shell-interpreter "python")))

;; (defun inferior-python-setup-hook ()
;;   (setq indent-tabs-mode t))

;; (add-hook 'inferior-python-mode-hook #'inferior-python-setup-hook)
(add-hook 'python-mode-hook 'python-setup-shell)
(add-hook 'python-mode-hook 'alzuse/python-mode-common-hook)

(provide 'init-python-mode)
