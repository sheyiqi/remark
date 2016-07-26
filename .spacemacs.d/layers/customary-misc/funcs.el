(defun customary/insert-semicolon-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ";")))

(defun customary/delete-semicolon-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (if (looking-back ";")
        (progn
          (backward-char)
          (delete-char 1)))))

(defun customary/insert-comma-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ",")))

(defun customary/delete-comma-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (if (looking-back ",")
        (progn
          (backward-char)
          (delete-char 1)))))

(defmacro customary|toggle-company-backends (backend)
  "Push or delete the backend to company-backends"
  (let ((funsymbol (intern (format "customary/company-toggle-%S" backend))))
    `(defun ,funsymbol ()
       (interactive)
       (if (eq (car company-backends) ',backend)
           (setq-local company-backends (delete ',backend company-backends))
         (push ',backend company-backends)))))

(defun customary/load-my-layout ()
  (interactive)
  (persp-load-state-from-file (concat persp-save-dir "customary")))

(defun customary/save-my-layout ()
  (interactive)
  (persp-save-state-to-file (concat persp-save-dir "customary")))

;; http://blog.binchen.org/posts/use-ivy-mode-to-search-bash-history.html
;; ;FIXME: make it work with zsh
(defun counsel-yank-bash-history ()
  "Yank the bash history"
  (interactive)
  (let (hist-cmd collection val)
    (shell-command "history -r") ; reload history
    (setq collection
          (nreverse
           (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.bash_history"))
                                           (buffer-string))
                         "\n"
                         t)))
    (when (and collection (> (length collection) 0)
               (setq val (if (= 1 (length collection)) (car collection)
                           (ivy-read (format "Bash history:") collection))))
      (kill-new val)
      (message "%s => kill-ring" val))))

  ;; my fix for tab indent
(defun customary/indent-region(numSpaces)
  (progn
                                      ; default to start and end of current line
    (setq regionStart (line-beginning-position))
    (setq regionEnd (line-end-position))

                                      ; if there's a selection, use that instead of the current line
    (when (use-region-p)
      (setq regionStart (region-beginning))
      (setq regionEnd (region-end))
      )

    (save-excursion                          ; restore the position afterwards
      (goto-char regionStart)                ; go to the start of region
      (setq start (line-beginning-position)) ; save the start of the line
      (goto-char regionEnd)                  ; go to the end of region
      (setq end (line-end-position))         ; save the end of the line

      (indent-rigidly start end numSpaces) ; indent between start and end
      (setq deactivate-mark nil)           ; restore the selected region
      )
    )
  )


(defun customary/tab-region (N)
  (interactive "p")
  (if (use-region-p)
      (customary/indent-region 4)               ; region was selected, call indent-region
    (insert "    ")                   ; else insert four spaces as expected
    ))

(defun customary/untab-region (N)
  (interactive "p")
  (customary/indent-region -4))

(defun customary/hack-tab-key ()
  (interactive)
  (local-set-key (kbd "<tab>") 'customary/tab-region)
  (local-set-key (kbd "<S-tab>") 'customary/untab-region)
  )

;; I'm don't like this settings too much.
;; (add-hook 'prog-mode-hook 'customary/hack-tab-key)
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

;; http://blog.binchen.org/posts/new-git-timemachine-ui-based-on-ivy-mode.html
(defun my-git-timemachine-show-selected-revision ()
  "Show last (current) revision of file."
  (interactive)
  (let (collection)
    (setq collection
          (mapcar (lambda (rev)
                    ;; re-shape list for the ivy-read
                    (cons (concat (substring (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
                  (git-timemachine--revisions)))
    (ivy-read "commits:"
              collection
              :action (lambda (rev)
                        (progn (git-timemachine-show-revision rev)
                               (evil-emacs-state))))))

(defun my-git-timemachine ()
  "Open git snapshot with the selected version.  Based on ivy-mode."
  (interactive)
  (unless (featurep 'git-timemachine)
    (require 'git-timemachine))
  (git-timemachine--start #'my-git-timemachine-show-selected-revision))


;; insert date and time
(defun customary/now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%D %-I:%M %p")))

(defun customary/today ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y")))


(defun customary/hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun customary/remove-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))


(defun customary/ash-term-hooks ()
  ;; dabbrev-expand in term
  (define-key term-raw-escape-map "/"
    (lambda ()
      (interactive)
      (let ((beg (point)))
        (dabbrev-expand nil)
        (kill-region beg (point)))
      (term-send-raw-string (substring-no-properties (current-kill 0)))))
  ;; yank in term (bound to C-c C-y)
  (define-key term-raw-escape-map "\C-y"
    (lambda ()
      (interactive)
      (term-send-raw-string (current-kill 0)))))

(defun customary/terminal ()
  "Switch to terminal. Launch if nonexistent."
  (interactive)
  (if (get-buffer "*ansi-term*")
      (switch-to-buffer-other-window "*ansi-term*")
    (progn
      (split-window-right-and-focus)
      (ansi-term "/bin/tcsh")))
  (get-buffer-process "*ansi-term*"))

(defalias 'tt 'customary/terminal)

(defun customary/evil-quick-replace (beg end )
  (interactive "r")
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (let ((selection (regexp-quote (buffer-substring-no-properties beg end))))
      (setq command-string (format "%%s /%s//g" selection))
      (minibuffer-with-setup-hook
          (lambda () (backward-char 2))
        (evil-ex command-string)))))

(defun customary/vcs-project-root ()
  "Return the project root for current buffer."
  (let ((directory default-directory))
    (or (locate-dominating-file directory ".git")
        (locate-dominating-file directory ".svn")
        (locate-dominating-file directory ".hg"))))

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
