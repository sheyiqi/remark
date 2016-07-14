(when (< emacs-major-version 24)
  (require-package 'color-theme))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(setq ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])

;; powerline
(require-package 'powerline)
;; (powerline-default-theme)


;; (load-theme 'monokai t)
;;(load-theme 'adam-light t)

(require-package 'moe-theme)
(require 'moe-theme)
(moe-light)

(powerline-moe-theme)

(provide 'init-themes)
