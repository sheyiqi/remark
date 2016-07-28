(setq customary-better-defaults-packages
      '(
        projectile
        visual-regexp
        visual-regexp-steroids
        command-log
        evil
        flycheck
        discover-my-major
        ace-window
        helm
        tiny
        smartparens
        peep-dired
        markdown-mode
        company
        ))

(defun customary-better-defaults/init-peep-dired ()
  ;;preview files in dired
  (use-package peep-dired
    :defer t
    :commands (peep-dired-next-file
               peep-dired-prev-file)
    :bind (:map dired-mode-map
                ("P" . peep-dired))))


(defun customary-better-defaults/post-init-smartparens ()
  (use-package smartparens
    :defer t
    :init
    (progn
      (defun wrap-sexp-with-new-round-parens ()
        (interactive)
        (insert "()")
        (backward-char)
        (sp-forward-slurp-sexp))

      (global-set-key (kbd "C-(") 'wrap-sexp-with-new-round-parens))
    :config
    (progn
      (setq sp-highlight-pair-overlay nil)

      (evil-define-key 'normal sp-keymap
        (kbd ")>") 'sp-forward-slurp-sexp
        (kbd ")<") 'sp-forward-barf-sexp
        (kbd "(>") 'sp-backward-barf-sexp
        (kbd "(<") 'sp-backward-slurp-sexp))))

(defun customary-better-defaults/post-init-helm ()
  (with-eval-after-load 'helm
    (progn
      ;; limit max number of matches displayed for speed
      (setq helm-candidate-number-limit 100)
      ;; ignore boring files like .o and .a
      (setq helm-ff-skip-boring-files t)
      ;; replace locate with spotlight on Mac
      (setq helm-locate-command "mdfind -name %s %s")
      (push "\\.emlx$" helm-boring-file-regexp-list)
      )
    ))


(defun customary-better-defaults/post-init-command-log ()
  (with-eval-after-load 'global-command-log-mode
    (setq clm/log-command-exceptions* (append clm/log-command-exceptions*
                                              '(evil-next-visual-line
                                                evil-previous-visual-line)))))

(defun customary-better-defaults/init-osx-dictionary ()
  (use-package osx-dictionary
    :init
    (progn
      (evilified-state-evilify osx-dictionary-mode osx-dictionary-mode-map)
      (setq osx-dictionary-use-chinese-text-segmentation t)
      (global-set-key (kbd "C-c d") 'osx-dictionary-search-pointer)
      )))


(defun customary-better-defaults/post-init-ace-window ()
  (global-set-key (kbd "C-x C-o") #'ace-window))

(defun customary-better-defaults/init-discover-my-major ()
  (use-package discover-my-major
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys (kbd "mhm") 'discover-my-major)
      (evilified-state-evilify makey-key-mode makey-key-mode-get-key-map))))


(defun customary-better-defaults/post-init-evil ()
  (progn
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)

    (push "TAGS" spacemacs-useless-buffers-regexp)

    ;; ;; change evil initial mode state
    (loop for (mode . state) in
          '((shell-mode . normal))
          do (evil-set-initial-state mode state))

    (add-hook 'edebug-mode-hook '(lambda () (if edebug-mode
                                            (evil-emacs-state)
                                          (evil-normal-state))))

    ;;mimic "nzz" behaviou in vim
    (defadvice evil-ex-search-next (after advice-for-evil-search-next activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    (defadvice evil-ex-search-previous (after advice-for-evil-search-previous activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
    (define-key evil-normal-state-map (kbd ",te") 'spacemacs/helm-find-files)

    (define-key evil-normal-state-map
      (kbd "Y") 'customary/yank-to-end-of-line)

    ;; rebind g,k to gj and gk
    ;; (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    ;; (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

    (define-key evil-normal-state-map (kbd "[ SPC") (lambda () (interactive) (evil-insert-newline-above) (forward-line)))
    (define-key evil-normal-state-map (kbd "] SPC") (lambda () (interactive) (evil-insert-newline-below) (forward-line -1)))

    (define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
    (define-key evil-normal-state-map (kbd "] b") 'next-buffer)

    (define-key evil-emacs-state-map (kbd "M-f") 'forward-word)
    (define-key evil-insert-state-map (kbd "M-f") 'forward-word)
    (define-key evil-emacs-state-map (kbd "M-b") 'backward-word)
    (define-key evil-insert-state-map (kbd "M-b") 'backward-word)

    (spacemacs/set-leader-keys "bi" 'ibuffer)
    (define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
    (define-key evil-ex-completion-map "\C-b" 'backward-char)
    (define-key evil-ex-completion-map "\C-k" 'kill-line)
    (define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)

    (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
    ;; (define-key evil-visual-state-map (kbd "x") 'er/expand-region)
    ;; (define-key evil-visual-state-map (kbd "X") 'er/contract-region)
    (define-key evil-visual-state-map (kbd "C-r") 'customary/evil-quick-replace)

    ;; in spacemacs, we always use evilify better-defaultsro state
    (evil-add-hjkl-bindings package-menu-mode-map 'emacs)

    ;; (define-key evil-emacs-state-map (kbd "C-w h") 'evil-window-left)
    (define-key evil-emacs-state-map (kbd "C-w") 'evil-delete-backward-word)
    ;; (define-key evil-emacs-state-map (kbd "C-w j") 'evil-window-down)
    ;; (define-key evil-emacs-state-map (kbd "C-w k") 'evil-window-up)
    ;; (define-key evil-emacs-state-map (kbd "C-w l") 'evil-window-right)


    (define-key evil-emacs-state-map (kbd "C-'") 'undo-tree-redo)
    (define-key evil-emacs-state-map (kbd "C-<") 'set-mark-command)
    (define-key evil-emacs-state-map (kbd "C-c f") 'customary/go-to-char)

    ;; for emacs shell mode
    (evil-define-key 'emacs term-raw-map (kbd "C-w")
      'evil-delete-backward-word)

    ;; (setq evil-normal-state-tag   (propertize "[N]" 'face '((:background "DarkGoldenrod2" :foreground "black")))
    ;;       evil-emacs-state-tag    (propertize "[E]" 'face '((:background "SkyBlue2" :foreground "black")))
    ;;       evil-insert-state-tag   (propertize "[I]" 'face '((:background "chartreuse3") :foreground "white"))
    ;;       evil-motion-state-tag   (propertize "[M]" 'face '((:background "plum3") :foreground "white"))
    ;;       evil-visual-state-tag   (propertize "[V]" 'face '((:background "gray" :foreground "black")))
    ;;       evil-operator-state-tag (propertize "[O]" 'face '((:background "purple"))))

    (define-key evil-insert-state-map (kbd "C-z") 'evil-emacs-state)
    ))

(defun customary-better-defaults/init-flycheck-package ()
  (use-package flycheck-package))

(defun customary-better-defaults/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (progn
      ;; (setq flycheck-display-errors-function 'flycheck-display-error-messages)
      (setq flycheck-display-errors-delay 0.2)
      (ispell-change-dictionary "american" t)

      ;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
      ;; if (aspell installed) { use aspell}
      ;; else if (hunspell installed) { use hunspell }
      ;; whatever spell checker I use, I always use English dictionary
      ;; I prefer use aspell because:
      ;; 1. aspell is older
      ;; 2. looks Kevin Atkinson still get some road map for aspell:
      ;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
      (defun flyspell-detect-ispell-args (&optional run-together)
        "if RUN-TOGETHER is true, spell check the CamelCase words."
        (let (args)
          (cond
           ((string-match  "aspell$" ispell-program-name)
            ;; Force the English dictionary for aspell
            ;; Support Camel Case spelling check (tested with aspell 0.6)
            (setq args (list "--sug-mode=ultra" "--lang=en_US"))
            (if run-together
                (setq args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=2")))))
           ((string-match "hunspell$" ispell-program-name)
            ;; Force the English dictionary for hunspell
            (setq args "-d en_US")))
          args))

      (cond
       ((executable-find "aspell")
        ;; you may also need `ispell-extra-args'
        (setq ispell-program-name "aspell"))
       ((executable-find "hunspell")
        (setq ispell-program-name "hunspell")

        ;; Please note that `ispell-local-dictionary` itself will be passed to hunspell cli with "-d"
        ;; it's also used as the key to lookup ispell-local-dictionary-alist
        ;; if we use different dictionary
        (setq ispell-local-dictionary "en_US")
        (setq ispell-local-dictionary-alist
              '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))
       (t (setq ispell-program-name nil)))

      ;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process when "ispell-word" is called.
      ;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
      ;; Please note when you use hunspell, ispell-extra-args will NOT be used.
      ;; Hack ispell-local-dictionary-alist instead.
      (setq-default ispell-extra-args (flyspell-detect-ispell-args t))
      ;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
      (defadvice ispell-word (around my-ispell-word activate)
        (let ((old-ispell-extra-args ispell-extra-args))
          (ispell-kill-ispell t)
          (setq ispell-extra-args (flyspell-detect-ispell-args))
          ad-do-it
          (setq ispell-extra-args old-ispell-extra-args)
          (ispell-kill-ispell t)
          ))

      (defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
        (let ((old-ispell-extra-args ispell-extra-args))
          (ispell-kill-ispell t)
          ;; use emacs original arguments
          (setq ispell-extra-args (flyspell-detect-ispell-args))
          ad-do-it
          ;; restore our own ispell arguments
          (setq ispell-extra-args old-ispell-extra-args)
          (ispell-kill-ispell t)
          ))

      (defun text-mode-hook-setup ()
        ;; Turn off RUN-TOGETHER option when spell check text-mode
        (setq-local ispell-extra-args (flyspell-detect-ispell-args)))
      (add-hook 'text-mode-hook 'text-mode-hook-setup)

      (defun org-mode-hook-setup ()
        "Configure `ispell-skip-region-alist' for `org-mode'."
        (make-local-variable 'ispell-skip-region-alist)
        (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
        (add-to-list 'ispell-skip-region-alist '("~" "~"))
        (add-to-list 'ispell-skip-region-alist '("=" "="))
        (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
      (add-hook 'org-mode-hook #'org-mode-hook-setup)

      )))

(defun customary-better-defaults/init-visual-regexp ()
  (use-package visual-regexp
    :commands (vr/replace vr/query-replace)))

(defun customary-better-defaults/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :commands (vr/select-replace vr/select-query-replace)
    :init
    (progn
      (define-key global-map (kbd "C-c r") 'vr/replace)
      (define-key global-map (kbd "C-c q") 'vr/query-replace))))


(defun customary-better-defaults/init-ag ()
  (use-package ag
    :init))


(defun customary-better-defaults/post-init-company ()
  (progn
    (setq company-minimum-prefix-length 1
          company-idle-delay 0.08)

    (when (configuration-layer/package-usedp 'company)
      (spacemacs|add-company-hook shell-script-mode)
      (spacemacs|add-company-hook makefile-bsdmake-mode)
      (spacemacs|add-company-hook sh-mode)
      (spacemacs|add-company-hook lua-mode)
      (spacemacs|add-company-hook nxml-mode)
      (spacemacs|add-company-hook conf-unix-mode)
      )
    ))
