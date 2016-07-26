;;; Find and load the correct package.el
;;------------------------------------------------------------------------------
;; Patch up annoying package.el quirks
;;------------------------------------------------------------------------------
(defadvice package-generate-autoloads (after close-autoloads (name pkg-dir) activate)
  "Stop package.el from leaving open autoload files lying around."
  (let ((path (expand-file-name (concat
                                 ;; name is string when emacs <= 24.3.1,
                                 (if (symbolp name) (symbol-name name) name)
                                 "-autoloads.el") pkg-dir)))
    (with-current-buffer (find-file-existing path)
      (kill-buffer nil)))) 


;; When switching between Emacs 23 and 24, we always use the bundled package.el in Emacs 24
(let ((package-el-site-lisp-dir
       (expand-file-name "site-lisp/package" user-emacs-directory)))
  (when (and (file-directory-p package-el-site-lisp-dir)
             (> emacs-major-version 23))
    (message "Removing local package.el from load-path to avoid shadowing bundled version")
    (setq load-path (remove package-el-site-lisp-dir load-path))))

(require 'package)



;;; Standard package repositories

;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; We include the org repository for completeness, but don't normally
;; use it.

(add-to-list 'package-archives '("org-cn" . "http://elpa.zilongshanren.com/org/"))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu-cn" . "http://elpa.zilongshanren.com/gnu/")))

;;; Also use Melpa for most packages
(add-to-list 'package-archives '("melpa-cn" . "http://elpa.zilongshanren.com/melpa/"))

;; List of VISIBLE packages from melpa-unstable (http://melpa.org)
;; Feel free to add more packages!

;; (defvar melpa-include-packages
;;   '(bbdb
;;     json-rpc
;;     kv
;;     color-theme
;;     wgrep
;;     robe
;;     groovy-mode
;;     inf-ruby
;;     simple-httpd
;;     dsvn
;;     move-text
;;     string-edit ; looks magnars don't update stable tag frequently
;;     findr
;;     mwe-log-commands
;;     dired-details
;;     yaml-mode
;;     noflet
;;     db
;;     creole
;;     web
;;     sass-mode
;;     idomenu
;;     pointback
;;     buffer-move
;;     regex-tool
;;     csharp-mode
;;     quack
;;     legalese
;;     htmlize
;;     scratch
;;     session
;;     crontab-mode
;;     bookmark+
;;     flymake-lua
;;     multi-term
;;     dired+
;;     inflections
;;     dropdown-list
;;     lua-mode
;;     pomodoro
;;     helm
;;     auto-compile
;;     packed
;;     gitconfig-mode
;;     textile-mode
;;     w3m
;;     fakir
;;     erlang
;;     company-c-headers
;;     company-anaconda
;;     anaconda-mode
;;     ;; make all the color theme packages available
;;     afternoon-theme
;;     define-word
;;     ahungry-theme
;;     alect-themes
;;     ample-theme
;;     ample-zen-theme
;;     anti-zenburn-theme
;;     atom-dark-theme
;;     badger-theme
;;     base16-theme
;;     basic-theme
;;     birds-of-paradise-plus-theme
;;     bliss-theme
;;     boron-theme
;;     bubbleberry-theme
;;     busybee-theme
;;     calmer-forest-theme
;;     cherry-blossom-theme
;;     clues-theme
;;     colonoscopy-theme
;;     color-theme-approximate
;;     color-theme-buffer-local
;;     color-theme-sanityinc-solarized
;;     color-theme-sanityinc-tomorrow
;;     color-theme-solarized
;;     colorsarenice-theme
;;     cyberpunk-theme
;;     dakrone-theme
;;     darcula-theme
;;     dark-krystal-theme
;;     darkburn-theme
;;     darkmine-theme
;;     display-theme
;;     distinguished-theme
;;     django-theme
;;     espresso-theme
;;     firebelly-theme
;;     firecode-theme
;;     flatland-black-theme
;;     pythonic
;;     flatland-theme
;;     flatui-theme
;;     gandalf-theme
;;     gotham-theme
;;     grandshell-theme
;;     gruber-darker-theme
;;     gruvbox-theme
;;     hc-zenburn-theme
;;     helm-themes
;;     hemisu-theme
;;     heroku-theme)
;;   "Don't install any Melpa packages except these packages")
;; 
;; ;; Don't take Melpa versions of certain packages
;; (setq package-filter-function
;;       (lambda (package version archive)
;;         (and
;;          (not (memq package '(eieio)))
;;          (or (and (string-equal archive "melpa") (memq package melpa-include-packages))
;;              (not (string-equal archive "melpa")))
;;          )))
;; 
;; un-comment below code if you prefer use all the package on melpa (unstable) without limitation
;; (setq package-filter-function nil)



;; If gpg cannot be found, signature checking will fail, so we
;; conditionally enable it according to whether gpg is available. We
;; re-run this check once $PATH has been configured
(defun sanityinc/package-maybe-enable-signatures ()
  (setq package-check-signature (when (executable-find "gpg") 'allow-unsigned)))

(sanityinc/package-maybe-enable-signatures)
(after-load 'init-exec-path
  (sanityinc/package-maybe-enable-signatures))



;;; On-demand installation of packages

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install package `%s': %S" package err)
     nil)))


;;; Fire up package.el

(setq package-enable-at-startup nil)
(package-initialize)



(require-package 'fullframe)
(fullframe list-packages quit-window)


(require-package 'cl-lib)
;;(require 'cl-lib)

(defun sanityinc/set-tabulated-list-column-width (col-name width)
  "Set any column with name COL-NAME to the given WIDTH."
  (cl-loop for column across tabulated-list-format
           when (string= col-name (car column))
           do (setf (elt column 1) width)))

(defun sanityinc/maybe-widen-package-menu-columns ()
  "Widen some columns of the package menu table to avoid truncation."
  (when (boundp 'tabulated-list-format)
    (sanityinc/set-tabulated-list-column-width "Version" 13)
    (let ((longest-archive-name (apply 'max (mapcar 'length (mapcar 'car package-archives)))))
      (sanityinc/set-tabulated-list-column-width "Archive" longest-archive-name))))

(add-hook 'package-menu-mode-hook 'sanityinc/maybe-widen-package-menu-columns)


(provide 'init-elpa)
