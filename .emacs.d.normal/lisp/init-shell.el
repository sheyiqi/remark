;; (defun alzuse/shell-mode-auto-rename-buffer (text)
;;   (if (eq major-mode 'shell-mode)
;;       (rename-buffer  (concat "*shell: " default-directory "*") t)))
;; (add-hook 'comint-output-filter-functions 'alzuse/shell-mode-auto-rename-buffer)

(defadvice shell (around always-new-shell)
  "Always start a new shell."
  (let ((buffer (generate-new-buffer-name "*shell*"))) ad-do-it))

(ad-activate 'shell)

(defun alzuse/shell-mode-kill-buffer-on-exit (process state)
  (shell-write-history-on-exit process state)
  (kill-buffer (process-buffer process)))

(defun alzuse/shell-mode-hook()
  (load-library "ansi-color")
  (ansi-color-for-comint-mode-on)
  (define-key shell-mode-map (kbd "M-p") 'comint-previous-matching-input-from-input)
  (define-key shell-mode-map (kbd "M-n") 'comint-next-matching-input-from-input)
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'alzuse/shell-mode-kill-buffer-on-exit)
  )

(add-hook 'shell-mode-hook 'alzuse/shell-mode-hook)
;; (add-hook 'shell-mode-hook 'set-outline-minor-mode-regexp t )

(require-package 'shell-pop)
(require 'shell-pop)
(global-set-key (kbd "C-c p") 'shell-pop)

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook (lambda () (setq line-spacing 0)))
(defun ansi-term-handle-close ()
  "Close current term buffer when `exit' from term buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)" change)
                              (kill-buffer (process-buffer proc))
                              (when (> (count-windows) 1)
                                (delete-window)))))))
(add-hook 'term-mode-hook 'ansi-term-handle-close)
(add-hook 'term-mode-hook (lambda () (linum-mode -1)))

(require-package 'multi-term)

(defun last-term-buffer (l)
  "Return most recently used term buffer."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
        (car l) (last-term-buffer (cdr l)))))

(defun get-term ()
  "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
  (interactive)
  (let ((b (last-term-buffer (buffer-list))))
    (if (or (not b) (eq 'term-mode major-mode))
        (multi-term)
      (switch-to-buffer b))))


(provide 'init-shell)

;;; init-shell.el ends here
