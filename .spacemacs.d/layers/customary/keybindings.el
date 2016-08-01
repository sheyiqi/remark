(when (spacemacs/system-is-mac)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq default-input-method "MacOSX")
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (kbd (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (global-set-key (kbd "M-`") 'ns-next-frame)
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "M-˙") 'ns-do-hide-others)
  (after-load 'nxml-mode
    (define-key nxml-mode-map (kbd "M-h") nil))
  (global-set-key (kbd "M-ˍ") 'ns-do-hide-others) ;; what describe-key reports for cmd-option-h
  )


(define-key 'help-command "A" 'apropos)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-i") 'info-display-manual)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(define-key global-map (kbd "<f5>") 'org-capture)
(define-key global-map (kbd "<f9>") 'customary/terminal)


;; http://emacs.stackexchange.com/questions/220/how-to-bind-c-i-as-different-from-tab
;; (define-key input-decode-map [?\C-i] [C-i])
;; (define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward)

(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)
(global-set-key [remap fill-paragraph] #'endless/fill-or-unfill)

;; some easy functions for navigate functions
;; C-M-a beginning-of-defun
;; C-M-e end-of-defun
;; C-M-h mark-defun

(bind-key* "C-c /" 'company-files)
(bind-key* "M-s o" 'occur-dwim)

;; Utility functions
(defun customary/define-key (keymap &rest bindings)
  (declare (indent 1))
  (while bindings
    (define-key keymap (pop bindings) (pop bindings))))

(customary/define-key evil-normal-state-map
  "+" 'evil-numbers/inc-at-pt
  "_" 'evil-numbers/dec-at-pt
  "\\" 'evil-repeat-find-char-reverse
  "[s" (lambda (n) (interactive "p") (dotimes (c n nil) (insert " ")))
  "]s" (lambda (n) (interactive "p")
         (forward-char) (dotimes (c n nil) (insert " ")) (backward-char (1+ n))))

;;(customary/define-key company-active-map
;;  (kbd "C-w") 'evil-delete-backward-word)

;; (customary/define-key company-active-map
;;  (kbd "s-w") 'company-show-location)

(spacemacs/declare-prefix "ot" "Toggle")

(spacemacs/set-leader-keys "fR" 'customary/rename-file-and-buffer)

;;Must set key to nil to prevent error: Key sequence b m s starts with non-prefix key b m
(spacemacs/set-leader-keys "bm" nil)
(spacemacs/declare-prefix "bm" "Bookmark")
(spacemacs/set-leader-keys "bms" 'bookmark-set)
(spacemacs/set-leader-keys "bmr" 'bookmark-rename)
(spacemacs/set-leader-keys "bmd" 'bookmark-delete)

(spacemacs/set-leader-keys "bD" 'spacemacs/kill-other-buffers)

;; tips:  use diminish-undo to toggle mode l
(if (configuration-layer/layer-usedp 'helm)
  (spacemacs/set-leader-keys "rh" 'helm-resume))

(spacemacs/set-leader-keys "gL" 'magit-log-buffer-file)
(spacemacs/set-leader-keys "sj" 'helm-imenu)

;; deal with BOM
(spacemacs/set-leader-keys "fl" 'find-file-literally-at-point)
(spacemacs/set-leader-keys "fh" 'ffap-hexl-mode)
(spacemacs/set-leader-keys "nl" 'spacemacs/evil-search-clear-highlight)
(spacemacs/set-leader-keys "oll" 'customary/load-my-layout)
(spacemacs/set-leader-keys "ols" 'customary/save-my-layout)

(add-hook 'comint-mode-hook (lambda () (define-key comint-mode-map "\t" 'self-insert-command)))
