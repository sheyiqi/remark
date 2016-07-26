(setq customary-misc-packages
      '(
        projectile
        visual-regexp
        visual-regexp-steroids
        command-log
        evil
        discover-my-major
        ace-window
        persp-mode
        helm-github-stars
        helm
        tiny
        smartparens
        peep-dired
        markdown-mode
        magit
        company
        ))

(defun customary-misc/init-peep-dired ()
  ;;preview files in dired
  (use-package peep-dired
    :defer t
    :commands (peep-dired-next-file
               peep-dired-prev-file)
    :bind (:map dired-mode-map
                ("P" . peep-dired))))


(defun customary-misc/post-init-smartparens ()
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

(defun customary-misc/post-init-helm ()
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


(defun customary-misc/post-init-command-log ()
  (with-eval-after-load 'global-command-log-mode
    (setq clm/log-command-exceptions* (append clm/log-command-exceptions*
                                              '(evil-next-visual-line
                                                evil-previous-visual-line)))))

(defun customary-misc/init-osx-dictionary ()
  (use-package osx-dictionary
    :init
    (progn
      (evilified-state-evilify osx-dictionary-mode osx-dictionary-mode-map)
      (setq osx-dictionary-use-chinese-text-segmentation t)
      (global-set-key (kbd "C-c d") 'osx-dictionary-search-pointer)
      )))


(defun customary-misc/post-init-ace-window ()
  (global-set-key (kbd "C-x C-o") #'ace-window))

(defun customary-misc/init-discover-my-major ()
  (use-package discover-my-major
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys (kbd "mhm") 'discover-my-major)
      (evilified-state-evilify makey-key-mode makey-key-mode-get-key-map))))


(defun customary-misc/post-init-evil ()
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

    ;; in spacemacs, we always use evilify miscro state
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
    ;; (define-key evil-emacs-state-map (kbd "s-b") 'ido-switch-buffer)
    ;; (define-key evil-emacs-state-map (kbd "s-f") 'ido-find-file)
    (evil-define-key 'emacs term-raw-map (kbd "C-w")
      'evil-delete-backward-word)

    (setq evil-normal-state-tag   (propertize "[N]" 'face '((:background "DarkGoldenrod2" :foreground "black")))
          evil-emacs-state-tag    (propertize "[E]" 'face '((:background "SkyBlue2" :foreground "black")))
          evil-insert-state-tag   (propertize "[I]" 'face '((:background "chartreuse3") :foreground "white"))
          evil-motion-state-tag   (propertize "[M]" 'face '((:background "plum3") :foreground "white"))
          evil-visual-state-tag   (propertize "[V]" 'face '((:background "gray" :foreground "black")))
          evil-operator-state-tag (propertize "[O]" 'face '((:background "purple"))))

    (define-key evil-insert-state-map (kbd "C-z") 'evil-emacs-state)
    ))

(defun customary-misc/init-visual-regexp ()
  (use-package visual-regexp
    :commands (vr/replace vr/query-replace)))

(defun customary-misc/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :commands (vr/select-replace vr/select-query-replace)
    :init
    (progn
      (define-key global-map (kbd "C-c r") 'vr/replace)
      (define-key global-map (kbd "C-c q") 'vr/query-replace))))


(defun customary-misc/init-ag ()
  (use-package ag
    :init))


(defun customary-misc/post-init-company ()
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


(defun customary-misc/post-init-magit ()
  (progn
    (with-eval-after-load 'magit
      (progn
        (add-to-list 'magit-no-confirm 'stage-all-changes)
        (define-key magit-log-mode-map (kbd "W") 'magit-copy-section-value)
        (define-key magit-status-mode-map (kbd "s-1") 'magit-jump-to-unstaged)
        (define-key magit-status-mode-map (kbd "s-2") 'magit-jump-to-untracked)
        (define-key magit-status-mode-map (kbd "s-3") 'magit-jump-to-staged)
        (define-key magit-status-mode-map (kbd "s-4") 'magit-jump-to-stashes)
        (setq magit-completing-read-function 'magit-builtin-completing-read)

        ;; http://emacs.stackexchange.com/questions/6021/change-a-branchs-upstream-with-magit/6023#6023
        (magit-define-popup-switch 'magit-push-popup ?u
          "Set upstream" "--set-upstream")
        ;; (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
        ;; (add-hook 'magit-section-set-visibility-hook '(lambda (section) (let ((section-type (magit-section-type section)))
        ;;                                                              (if (or (eq 'untracked section-type)
        ;;                                                                      (eq 'stashes section-type))
        ;;                                                                  'hide))))
        ))
    (setq magit-push-always-verify nil)

    (setq magit-process-popup-time 10)))

