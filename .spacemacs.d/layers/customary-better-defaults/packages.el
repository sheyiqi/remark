(setq customary-better-defaults-packages
      '(
        projectile
        peep-dired
        command-log
        visual-regexp
        visual-regexp-steroids
        evil
        flycheck
        discover-my-major
        helm
        tiny
        smartparens
        markdown-mode
        company
        ibuffer
        ))

(defun customary-better-defaults/init-peep-dired ()
  ;;preview files in dired
  (use-package peep-dired
    :defer t
    :commands (peep-dired-next-file
               peep-dired-prev-file)
    :bind (:map dired-mode-map
                ("P" . peep-dired))))

(defun customary-better-defaults/post-init-command-log ()
  (with-eval-after-load 'global-command-log-mode
    (setq clm/log-command-exceptions* (append clm/log-command-exceptions*
                                              '(evil-next-visual-line
                                                evil-previous-visual-line)))))


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


(spacemacs|use-package-add-hook smartparens
  :post-init
  (progn
    (defun wrap-sexp-with-new-round-parens ()
      (interactive)
      (insert "()")
      (backward-char)
      (sp-forward-slurp-sexp))

    (global-set-key (kbd "C-(") 'wrap-sexp-with-new-round-parens))

  :post-config
  (progn
    (setq sp-highlight-pair-overlay nil)

    (evil-define-key 'normal sp-keymap
      (kbd ")>") 'sp-forward-slurp-sexp
      (kbd ")<") 'sp-forward-barf-sexp
      (kbd "(>") 'sp-backward-barf-sexp
      (kbd "(<") 'sp-backward-slurp-sexp)))


(spacemacs|use-package-add-hook helm
  :post-config
  ;; limit max number of matches displayed for speed
  (setq helm-candidate-number-limit 128)
  ;; ignore boring files like .o and .a
  (setq helm-ff-skip-boring-files t)
  ;; replace locate with spotlight on Mac
  (setq helm-locate-command "mdfind -name %s %s")
  (push "\\.emlx$" helm-boring-file-regexp-list)
  )


(spacemacs|use-package-add-hook osx-dictionary
  :post-init
  (progn
    (evilified-state-evilify osx-dictionary-mode osx-dictionary-mode-map)
    (setq osx-dictionary-use-chinese-text-segmentation t)
    (global-set-key (kbd "C-c d") 'osx-dictionary-search-pointer)
    ))


(spacemacs|use-package-add-hook discover-my-major
  :post-config
  (progn
    (spacemacs/set-leader-keys (kbd "mhm") 'discover-my-major)
    (evilified-state-evilify makey-key-mode makey-key-mode-get-key-map)))


(spacemacs|use-package-add-hook evil
  :post-config
  (progn
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)

    (push "TAGS" spacemacs-useless-buffers-regexp)

    ;; ;; change evil initial mode state
    (loop for (mode . state) in
          '((shell-mode . insert))
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


    (evil-define-key 'insert comint-mode-map
      (kbd "C-k") 'kill-line)
    (evil-define-key 'normal comint-mode-map
      (kbd "C-k") 'kill-line)
    ))

(spacemacs|use-package-add-hook flycheck
  :post-config
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
    ))


(spacemacs|use-package-add-hook company
  :post-config
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


;;(defun customary-better-defaults/pre-init-ibuffer ()
;;  (with-eval-after-load 'ibuffer
(spacemacs|use-package-add-hook ibuffer
  :pre-init
  (require 'projectile)
  (require 'f)
  (setq ibuffer-never-show-predicates
        `("^\\*inferior-ensime"
          "^\\*ensime-update"
          "^\\*helm"))

  (setq ibuffer-saved-filter-groups
        (let ((files-by-project
               (--map `(,(f-filename it) (filename . ,(f-expand it))) projectile-known-projects)))
          `(("default"
             ,@files-by-project
             ("dired" (mode . dired-mode))
             ("emacs" (or (name . "\\*Messages\\*")
                          (name . "\\*Compile-Log\\*")
                          (name . "\\*scratch\\*")
                          (name . "\\*Backtrace\\*")
                          (name . "\\*spacemacs\\*")
                          (name . "\\*emacs\\*")))
             ("help" (name . "\\*Help\\*"))))))

  :post-init
  (setq ibuffer-show-empty-filter-groups nil)

  (defun customary/switch-ibuffer-group ()
    (ibuffer-switch-to-saved-filter-groups "default"))

  (add-hook 'ibuffer-mode-hook 'customary/switch-ibuffer-group)
  (add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)


  (defface customary/ibuffer-common-face
    '((default
        :inherit font-lock-keyword-face
        :underline nil
        :background "red"))
    "Face used for ibuffer common buffers")

  (defface customary/ibuffer-common-modified-face
    '((default
        :inherit customary/ibuffer-common-face
        :weight bold
        ))
    "Face used for ibuffer common but modifed buffers")

  (defface customary/ibuffer-star-face
    '((default
        :inherit font-lock-preprocessor-face
        :slant italic))
    "Face used for star-stared buffers")

  (defface customary/ibuffer-readonly-face
    '((default
        :inherit font-lock-constant-face))
    "Face used for readonly buffers")

  (defface customary/ibuffer-compressed-file-face
    '((default
        :inherit font-lock-doc-face))
    "Face used for compressed file buffers")

  (defface customary/ibuffer-null-face
    '((default
        :slant italic))
    "Face used for null buffers")

  (defface customary/ibuffer-help-buffer-mode-face
    '((default
        :inherit font-lock-comment-face
        :foreground "green"
        ))
    "Face used for help buffer modes")

  (defface customary/ibuffer-dired-mode-face
    '((default
        :inherit font-lock-function-name-face
        ))
    "Face used for dired modes")


  (setq ibuffer-fontification-alist
        '(
          ;; (20
          ;;  (and buffer-file-name
          ;;       (buffer-modified-p (buffer-file-name)))
          ;;  customary/ibuffer-common-modified-face)

          (25
           buffer-file-name
           customary/ibuffer-common-face)

          (30
           buffer-read-only
           customary/ibuffer-readonly-face)

          (35
           (and buffer-file-name
                (string-match ibuffer-compressed-file-name-regexp buffer-file-name))
           customary/ibuffer-compressed-file-face)

          (40
           (string-match "^*"
                         (buffer-name))
           customary/ibuffer-star-face)

          (45
           (and
            (string-match "^ "
                          (buffer-name))
            (null buffer-file-name))
           customary/ibuffer-null-face)

          (50
           (memq major-mode ibuffer-help-buffer-modes)
           customary/ibuffer-help-buffer-mode-face)

          (55
           (derived-mode-p
            (quote dired-mode))
           customary/ibuffer-dired-mode-face)))

  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process))))
