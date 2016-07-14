;;----------------------------------------------------------------------------
;; search current word like vim's *
;;----------------------------------------------------------------------------
(defvar customary/isearch-word "")
(defun customary/isearch-word-star ()
  "isearch current word like vim's *"
  (interactive)
  (when (not mark-active)
    (let (word-beg word-end)
      (unless (looking-at "\\<")
        (if (eq (char-syntax (char-after)) ?w)
            (backward-word)
          (and (forward-word) (backward-word)))
        )
      (setq word-beg (point))
      (forward-word)
      (setq word-end (point))
      (setq customary/isearch-word (filter-buffer-substring word-beg word-end nil))
      (backward-word)
      )
    (when (> (length customary/isearch-word) 0)
      (setq customary/isearch-word (concat "\\<" customary/isearch-word "\\>"))
      (isearch-update-ring customary/isearch-word t)
      (add-hook 'isearch-mode-end-hook 'customary/isearch-word-end-hook)
      (isearch-mode t t)
      (isearch-repeat 'forward)
      (message "%s" customary/isearch-word))))

(defun customary/isearch-word-end-hook ()
  (remove-hook 'isearch-mode-end-hook 'customary/isearch-word-end-hook)
  (setq customary/isearch-word ""))

;;----------------------------------------------------------------------------
;; goto char like vim 'f'
;;----------------------------------------------------------------------------
(defun customary/go-to-char (n char)
  "Move forward to Nth occurence of CHAR.
  Typing `my-go-to-char-key' again will move forwad to the next Nth
  occurence of CHAR."
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
                     char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))
