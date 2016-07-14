(defun tcl-multiline-options ()
  "spread option/value pairs across multiple lines with continuation characters"
  (interactive)
  (save-excursion
    (tcl-join-continuations)
    (beginning-of-line)
    (while (re-search-forward " -[^ ]+ +"  (line-end-position) t)
      (goto-char (match-beginning 0))
      (insert " \\\n")
      (goto-char (+(match-end 0) 3))
      (indent-according-to-mode)
      (forward-sexp))
    ))

(defun tcl-join-continuations ()
  "join multiple continuation lines into a single physical line"
  (interactive)
  (while (progn (end-of-line) (char-equal (char-before) ?\\))
    (forward-line 1))
  (while (save-excursion (end-of-line 0) (char-equal (char-before) ?\\))
    (end-of-line 0)
    (delete-char -1)
    (delete-char 1)
    (fixup-whitespace)
    ))

(add-hook 'tcl-mode-hook
          '(lambda()

             (setq tcl-application "tclsh8.5")
             ;;(setq tcl-application "/home/engineer/azhang/slash/tclkit_Official/tktars/kit/dummy85-shtc030")
             (local-set-key (kbd "C-c C-\\") 'tcl-multiline-options)
             (local-set-key (kbd "C-c C-/") 'tcl-join-continuations)
             ))

(add-hook 'inferior-tcl-mode-hook
          (lambda ()
            (tcl-send-string (inferior-tcl-proc) "set ::tcl_interactive 1\n")))

(provide 'init-tcl)
