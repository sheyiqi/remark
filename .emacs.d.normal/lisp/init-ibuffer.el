(require-package 'fullframe)
(after-load 'ibuffer
 (fullframe ibuffer ibuffer-quit))

(require-package 'ibuffer-vc)

(defun ibuffer-set-up-preferred-filters ()
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))

;; (add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)


(after-load 'ibuffer
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size))))))


;; Explicitly require ibuffer-vc to get its column definitions, which
;; can't be autoloaded
(after-load 'ibuffer
  (require 'ibuffer-vc))

;; Modify the default ibuffer-formats (toggle with `)
(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)
        (mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))

(setq ibuffer-filter-group-name-face 'font-lock-doc-face)

(global-set-key (kbd "C-x C-b") 'ibuffer)


(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Shell" (or
                         (mode . shell-mode)
                         (mode . eshell-mode)
                         (name . "^\\*terminal\\*$")
                         ))
               ("Dired" (mode . dired-mode))
               ("C/C++" (or
                         (mode . c-mode)
                         (mode . c++-mode)
                         (mode . cc-mode)
                         ))
               ("Python" (mode . python-mode))
               ("Perl" (mode . cperl-mode))
               ("Tcl/Tk" (mode . tcl-mode))
               ("EDA-Views" (or
                             (mode . verilog-mode)
                             (mode . vhdl-mode)
                             (name . ".lib$")
                             (name . ".plib$")
                             (name . ".lef$")
                             (name . ".siz$")
                             (name . ".cdl$")
                             (name . ".cir$")
                             (name . ".cpf$")
                             (name . ".fastscan$")
                             (name . ".mbist$")
                             (name . ".ds$")
                             (name . ".gds$")
                             (name . ".tmax$")
                             ))
               ("Agenda" (or
                          (mode . org-mode)
                          (name . "^\\*Calendar\\*$")
                          (name . "^diary$")
                          (name . "^\\*Agenda")
                          (name . "^\\*org-")
                          (name . "^\\*Org")
                          ))
               ("Web-Dev" (or
                           (name . ".tt$")
                           (mode . html-mode)
                           (mode . css-mode)
                           ))
               ("Other-Dev" (or
                             (mode . postscript-mode)
                             (mode . ps-mode)
                             ))
               ("Emacs-Config" (filename . ".emacs.d")
                )
               ("Elisp" (mode . elisp-mode)
                )
               ("Latex" (or (mode . latex-mode)
                            (mode . LaTeX-mode)
                            (mode . bibtex-mode)
                            (mode . reftex-mode)))
               ("Version-Control" (or (mode . svn-status-mode)
                                      (mode . svn-log-edit-mode)
                                      (name . "^\\*svn-")
                                      (name . "^\\*vc\\*$")
                                      (name . "^\\*Annotate")
                                      (name . "^\\*git-")
                                      (name . "^\\*vc-")))
               ("Gnus" (or
                        (mode . message-mode)
                        (mode . bbdb-mode)
                        (mode . mail-mode)
                        (mode . gnus-group-mode)
                        (mode . gnus-summary-mode)
                        (mode . gnus-article-mode)
                        (name . "^\\.bbdb$")
                        (name . "^\\.newsrc-dribble")
                        ))
               ("Planner" (or
                           (name . "^\\*Calendar\\*$")
                           (name . "^diary$")
                           ))
               ("Text" (or
                        (mode . text-mode)
                        ))
               ("Help" (or
                        (mode . w3m-mode)
                        (mode . man-mode)
                        (mode . woman-mode)
                        (name . "^\\*WoMan-Log\\*$")
                        (mode . docview-mode)
                        (mode . help-mode)
                        (mode . apropos-mode)
                        (name . "\*Apropos\*")
                        (mode . info-mode)
                        (mode . info-edit-mode)
                        (name . "^\\*info\\*$")
                        ))
               ("Emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")
                         (name . "^\\*Completions\\*$")
                         (name . "^\\*grep\\*$")
                         (name . "^\\*Compile-Log\\*$")
                         (name . "^\\*Backtrace\\*$")
                         (name . "^\\*Process List\\*$")
                         (name . "^\\*gud\\*$")
                         (name . "^\\*Kill Ring\\*$")
                         (name . "^\\*Completions\\*$")
                         (name . "^\\*tramp")
                         (name . "^\\*compilation\\*$")
                         (name . "^\\*anything.\*\\*$")
                         (mode . custom-mode)
                         (mode . browse-kill-ring-mode)
                         (mode . anything-mode)
                         ))
               ))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-set-up-preferred-filters)
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))


(provide 'init-ibuffer)
