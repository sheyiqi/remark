(global-set-key (kbd "C-'") 'undo-tree-redo)
(global-set-key (kbd "C-<") 'set-mark-command)

(global-set-key (kbd "C-*") 'customary/isearch-word-star)
(global-set-key (kbd "C-&") 'customary/isearch-region-forward)

(global-set-key (kbd "C-c f") 'customary/go-to-char)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "S-<return>") 'customary/newline-at-end-of-line)

(global-set-key (kbd "C-c w")
                (lambda ()
                  (interactive)
                  (let ((woman-use-topic-at-point t))
                    (woman))))
