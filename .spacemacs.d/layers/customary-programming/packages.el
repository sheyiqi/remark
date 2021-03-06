(setq customary-programming-packages
      '(
        ansi-color
        cmake-font-lock
        cmake-mode
        racket
        yasnippet
        (cc-mode :location built-in)
        (python :location built-in)
        (emacs-lisp :location built-in)
        ))

(defun customary-programming/post-init-ansi-color ()
  (progn
    (defun customary/colorize-compilation ()
      "Colorize from `compilation-filter-start' to `point'."
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region
         compilation-filter-start (point))))

    (add-hook 'compilation-filter-hook
              #'customary/colorize-compilation)
    )
  )


(defun customary-programming/post-init-racket-mode ()
  (progn
    (eval-after-load 'racket-repl-mode
      '(progn
         (define-key racket-repl-mode-map (kbd "]") nil)
         (define-key racket-repl-mode-map (kbd "[") nil)))
    ))


(defun customary-programming/init-cmake-font-lock ()
  (use-package cmake-font-lock
    :defer t))


(defun customary-programming/post-init-cmake-mode ()
  (progn
    (spacemacs/declare-prefix-for-mode 'cmake-mode
                                       "mh" "docs")
    (spacemacs/set-leader-keys-for-major-mode 'cmake-mode
      "hd" 'cmake-help)
    (defun cmake-rename-buffer ()
      "Renames a CMakeLists.txt buffer to cmake-<directory name>."
      (interactive)
      (when (and (buffer-file-name)
                 (string-match "CMakeLists.txt" (buffer-name)))
        (setq parent-dir (file-name-nondirectory
                          (directory-file-name
                           (file-name-directory (buffer-file-name)))))
        (setq new-buffer-name (concat "cmake-" parent-dir))
        (rename-buffer new-buffer-name t)))

    (add-hook 'cmake-mode-hook (function cmake-rename-buffer)))
  )


(spacemacs|use-package-add-hook emacs-lisp
  :post-config
  (remove-hook 'emacs-lisp-mode-hook 'auto-compile-mode))


(spacemacs|use-package-add-hook python
  :post-config
  (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  ;; if you use pyton3, then you could comment the following line
  (setq python-shell-interpreter "python3"))


(spacemacs|use-package-add-hook yasnippet
    :post-config
  (progn
    (set-face-background 'secondary-selection "gray")
    (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
    (mapc #'(lambda (hook) (remove-hook hook 'spacemacs/load-yasnippet)) '(prog-mode-hook
                                                                      org-mode-hook
                                                                      markdown-mode-hook))
    (defun customary/load-yasnippet ()
      (interactive)
      (unless yas-global-mode
        (progn
          (yas-global-mode 1)
          (setq my-snippet-dir (expand-file-name "~/.spacemacs.d/snippets"))
          (setq yas-snippet-dirs  my-snippet-dir)
          (yas-load-directory my-snippet-dir)
          (setq yas-wrap-around-region t)))
      (yas-minor-mode 1))

    (spacemacs/add-to-hooks 'customary/load-yasnippet '(prog-mode-hook
                                                        org-mode-hook
                                                        markdown-mode-hook
                                                        ))
    ))

(spacemacs|use-package-add-hook c-c++
  :post-config
  (progn
    (require 'company)
    (setq company-backends-c-mode-common '((company-dabbrev-code :with company-keywords)
                                           company-files company-dabbrev))

    (defun my-project-name-contains-substring (REGEX)
      (let ((dir (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   "")))
        (string-match-p REGEX dir)))

    (defun my-create-tags-if-needed (SRC-DIR &optional FORCE)
      "return the full path of tags file"
      (let ((dir (file-name-as-directory (file-truename SRC-DIR)))
            file)
        (setq file (concat dir "TAGS"))
        (when (or FORCE (not (file-exists-p file)))
          (message "Creating TAGS in %s ..." dir)
          (shell-command
           (format "ctags -f %s -e -R %s" file dir)))
        file))

    (defvar my-tags-updated-time nil)

    (defun my-update-tags ()
      (interactive)
      "check the tags in tags-table-list and re-create it"
      (dolist (tag tags-table-list)
        (my-create-tags-if-needed (file-name-directory tag) t)))

    (defun my-auto-update-tags-when-save ()
      (interactive)
      (cond
       ((not my-tags-updated-time)
        (setq my-tags-updated-time (current-time)))
       ((< (- (float-time (current-time)) (float-time my-tags-updated-time)) 300)
        ;; < 300 seconds
        ;; do nothing
        )
       (t
        (setq my-tags-updated-time (current-time))
        (my-update-tags)
        (message "updated tags after %d seconds." (- (float-time (current-time)) (float-time my-tags-updated-time))))))

    ;; http://stackoverflow.com/questions/23553881/emacs-indenting-of-c11-lambda-functions-cc-mode
    (defadvice c-lineup-arglist (around my activate)
      "Improve indentation of continued C++11 lambda function opened as argument."
      (setq ad-return-value
            (if (and (equal major-mode 'c++-mode)
                     (ignore-errors
                       (save-excursion
                         (goto-char (c-langelem-pos langelem))
                         ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                         ;;   and with unclosed brace.
                         (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
                0                       ; no additional indent
              ad-do-it)))               ; default behavior


    (setq c-default-style "linux") ;; set style to "linux"
    (setq c-basic-offset 4)
    (c-set-offset 'substatement-open 0))
    (when (configuration-layer/package-usedp 'spell-checking)
       (remove-hook 'c-mode-hook 'flycheck-mode)
       (remove-hook 'c++-mode-hook 'flycheck-mode)
    )

    ;; return nil to write content to file
    (defun customary/untabify-buffer ()
      (interactive)
      (save-excursion
        (untabify (point-min) (point-max)) nil))

    (add-hook 'c++-mode-hook
              '(lambda ()
                 (add-hook 'write-contents-hooks
                           'customary/untabify-buffer nil t)))
  ;; company backend should be grouped
  )
