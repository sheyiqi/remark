(c-set-offset 'inline-open 0)
(c-set-offset 'friend '-)
(c-set-offset 'substatement-open 0)

;; (defun my-indent-or-complete ()
;; (interactive)
;; (if (looking-at "\\>")
;;     (hippie-expand nil)
;;   (indent-for-tab-command))
;; )

(defun alzuse/c++-mode-common-hook ()
  "Set up c-mode and related modes. Includes support for Qt code (signal, slots and alikes)."

  ;; base-style
  ;; (c-set-style "stroustrup")
  ;; set auto cr mode
  ;; (c-toggle-auto-hungry-state 1)

  ;; qt keywords and stuff ...
  ;; set up indenting correctly for new qt kewords
  (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                                 "\\|protected slot\\|private\\|private slot"
                                 "\\)\\>")
        c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
                                 "\\|public slots\\|protected slots\\|private slots"
                                 "\\)\\>[ \t]*:"))
  (progn
    ;; modify the colour of slots to match public, private, etc ...
    (font-lock-add-keywords 'c++-mode
                            '(("\\<\\(slots\\|signals\\)\\>" . font-lock-type-face)))
    ;; make new font for rest of qt keywords
    (make-face 'qt-keywords-face)
    (set-face-foreground 'qt-keywords-face "BlueViolet")
    ;; qt keywords
    (font-lock-add-keywords 'c++-mode
                            '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)))
    (font-lock-add-keywords 'c++-mode
                            '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
    (font-lock-add-keywords 'c++-mode
                            '(("\\<Q[A-Z][A-Za-z]*" . 'qt-keywords-face)))
    ))

(defun alzuse/c-mode-common-hook()
  ;; (require 'xcscope)

  ;; (require 'gtags)
  ;; (gtags-mode 1)

  ;; (require 'highline)
  ;; (highline-mode 1)
  (setq kept-new-versions 256)
  ;; (define-key c-mode-base-map [(tab)] 'my-indent-or-complete)

  (c-set-style "stroustrup")

  ;;(define-key c-mode-base-map [RET] 'newline-and-indent)

  ;; Do not check for old-style (K&R) function declarations;
  ;; this speeds up indenting a lot.
  (setq c-recognize-knr-p nil)

  (setq tab-width 4 indent-tabs-mode nil)
  (setq c-macro-shrink-window-flag t)
  (setq c-macro-preprocessor "cpp")
  (setq c-macro-cppflags " ")
  (setq c-macro-prompt-flag t)
  ;;  (setq hs-minor-mode t)
  ;;  (setq abbrev-mode t)
  ;;  (setq outline-minor-mode t)
  (modify-syntax-entry ?_ "w" c-mode-syntax-table)

  (define-key c-mode-map (kbd "<f7>") 'smart-compile)
  (define-key c++-mode-map (kbd "<f7>") 'smart-compile)

  ;; (setq ac-auto-start nil)
  ;; (ac-set-trigger-key "<C-return>")

  ;; (define-key c-mode-map [(control tab)] 'semantic-ia-complete-symbol-menu)
  )
(add-hook 'c-mode-common-hook 'alzuse/c-mode-common-hook)
(add-hook 'c++-mode-common-hook 'alzuse/c-mode-common-hook)
(add-hook 'c++-mode-common-hook 'alzuse/c++-mode-common-hook)

(defun smart-compile()
  (interactive)
  ;; ?? Makefile
  (let ((candidate-make-file-name '("makefile" "Makefile" "GNUmakefile"))
        (command nil))
    (if (not (null
              (find t candidate-make-file-name :key
                    '(lambda (f) (file-readable-p f)))))
        (setq command "make -k ")
      ;; ???? Makefile ,???? mode ????????????
      (if (null (buffer-file-name (current-buffer)))
          (message "Buffer not attached to a file, won't compile!")
        (if (eq major-mode 'c-mode)
            (setq command
                  (concat "gcc -Wall -o "
                          (file-name-sans-extension
                           (file-name-nondirectory buffer-file-name))
                          " "
                          (file-name-nondirectory buffer-file-name)
                          " -g -lm "))
          (if (eq major-mode 'c++-mode)
              (setq command
                    (concat "g++ -Wall -o "
                            (file-name-sans-extension
                             (file-name-nondirectory buffer-file-name))
                            " "
                            (file-name-nondirectory buffer-file-name)
                            " -g -lm "))
            (message "Unknow mode, won't compile!")))))
    (if (not (null command))
        (let ((command (read-from-minibuffer "Compile command: " command)))
          (compile command)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doxymacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'doxymacs)
;; (add-hook 'c-mode-common-hook 'doxymacs-mode)
;; (defun my-doxymacs-font-lock-hook ()
;;   (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;;       (doxymacs-font-lock)))
;; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)


;; (require 'flymake nil t)
;; (when (featurep 'flymake)
;;   (set-face-background 'flymake-errline "LightPink")
;;   (set-face-foreground 'flymake-errline "DarkRed")
;;   (set-face-bold-p 'flymake-errline t)
;;   (set-face-background 'flymake-warnline "LightBlue2")
;;   (set-face-foreground 'flymake-warnline "DarkBlue")
;;   (set-face-bold-p 'flymake-warnline t))
;; ;; flymake
;; (defun my-flymake-show-next-error()
;;   (interactive)
;;   (flymake-goto-next-error)
;;   (flymake-display-err-menu-for-current-line)
;;   )

;; (global-set-key (kbd "C-c C-v") 'my-flymake-show-next-error)


;; (setq gtags-mode-hook
;;       '(lambda ()
;;          (setq gtags-path-style 'root)
;;          (define-key gtags-mode-map (kbd "C-c g .") 'gtags-find-tag-from-here)
;;          (define-key gtags-mode-map (kbd "C-c g t") 'gtags-find-tag)
;;          (define-key gtags-mode-map (kbd "C-c g r") 'gtags-find-rtag)
;;          (define-key gtags-mode-map (kbd "C-c g s") 'gtags-find-symbol)
;;          (define-key gtags-mode-map (kbd "C-c g g") 'gtags-find-with-grep)
;;          (define-key gtags-mode-map (kbd "C-c g f") 'gtags-find-file)
;;          (define-key gtags-mode-map (kbd "C-c g 4") 'gtags-find-tag-other-window)
;;          ))

(defun switch-cc-to-h ()
  (interactive)
  (when (string-match "^\\(.*\\)\\.\\([^.]*\\)$" buffer-file-name)
    (let ((name (match-string 1 buffer-file-name))
          (suffix (match-string 2 buffer-file-name)))
      (cond ((string-match suffix "c\\|cc\\|C\\|cpp")
             (cond ((file-exists-p (concat name ".h"))
                    (find-file (concat name ".h"))
                    )
                   ((file-exists-p (concat name ".hh"))
                    (find-file (concat name ".hh"))
                    )
                   ((file-exists-p (concat name ".hpp"))
                    (find-file (concat name ".hpp"))
                    )
                   ))
            ((string-match suffix "h\\|hh\\|hpp")
             (cond ((file-exists-p (concat name ".cc"))
                    (find-file (concat name ".cc"))
                    )
                   ((file-exists-p (concat name ".C"))
                    (find-file (concat name ".C"))
                    )
                   ((file-exists-p (concat name ".cpp"))
                    (find-file (concat name ".cpp"))
                    )
                   ((file-exists-p (concat name ".c"))
                    (find-file (concat name ".c"))
                    )))))))

(setq gdb-many-windows t)
(setq gdb-show-main t)
(setq gud-chdir-before-run nil)
(setq gud-tooltip-mode t)

(provide 'init-c)
