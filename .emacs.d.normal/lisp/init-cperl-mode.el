;; perl mode,include cperl-mode, pod-mode
(autoload 'cperl-mode "cperl-mode" "cperl mode" t)
;; prefer cperl
(defalias 'perl-mode 'cperl-mode)

(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

(add-hook 'cperl-mode-hook 'alzuse/cperl-mode-hook t)
(defun alzuse/cperl-mode-hook ()
  (setq tab-width 4)
  (cperl-set-style "CPerl")
  (define-key cperl-mode-map "\C-j" 'reindent-then-newline-and-indent)
  (setq cperl-indent-level 4)
  (setq cperl-brace-offset 0)
  (setq cperl-continued-brace-offset 0)
  (setq cperl-label-offset -4)
  (setq cperl-continued-statement-offset 4)
  (setq cperl-extra-newline-before-brace nil)
  ;; 修改多行括号内的缩进
  (setq cperl-indent-parens-as-block t)
  ;; 不把空白用下划线显示
  (setq cperl-invalid-face nil)
  ;; windows 下的 pod 高亮有时候有问题，刷新一下就好了。试试这个命令
  (define-key cperl-mode-map (kbd "C-c C-r") 'font-lock-fontify-buffer)
  ;; fill 在写 perl 代码时不常用，所以修改成这个命令，更常用一些。
  (define-key cperl-mode-map (kbd "C-c C-f") 'cperl-perldoc)
  ;;  (set-face-background 'cperl-array-face "blue")
  ;;  (set-face-background 'cperl-hash-face "wheat")
  )

;; windows 下使用这个可以在用 cpan 这样的交互程序。ppm 最好不要在 emacs 内使用。
;; (add-hook 'shell-mode-hook (lambda () (insert "set PERLIO=:unix")
;;                              (call-interactively 'comint-send-input)))

;; 打开 perltidy 生成的文件使用 cperl-mode
(add-to-list 'auto-mode-alist '("\\.tdy$" . cperl-mode))

(provide 'init-cperl-mode)
