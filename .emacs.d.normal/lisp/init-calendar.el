;; Calender settings

(require-package 'cal-china-x)

;; latitude & longitude & location
(setq calendar-latitude +32.22
      calendar-longitude +126.63
      calendar-location-name "SuZhou")

;; calender
(setq calendar-remove-frame-by-deleting t
      calendar-week-start-day 1
      christian-holidays nil
      hebrew-holidays nil
      islamic-holidays nil
      solar-holidays nil
      mark-holidays-in-calendar t
      view-calendar-holidays-initially nil
      )


(setq other-holidays
      '(
        (holiday-fixed 1   1 "元旦")
        (holiday-fixed 1  14 "毛毛生日")
        (holiday-fixed 11  1 "我生日")
        (holiday-float 5 0 2 "母亲节")
        (holiday-float 6 0 3 "父亲节")
        (holiday-fixed 3  12 "植树节")
        (holiday-fixed 5  4  "青年节")
        (holiday-fixed 9  10 "教师节")
        (holiday-lunar 1  15 "元宵节")
        (holiday-lunar 7  7  "七夕节")
        (holiday-lunar 9  9  "重阳节")
        ))

;;(setq calendar-holidays
;;      (append cal-china-x-priority1-holidays
;;              cal-china-x-priority2-holidays
;;              other-holidays))



;;(add-hook 'calendar-mode-hook '(lambda () (calendar-goto-today)))

(require-package 'calfw)
(require 'calfw)

;; Unicode characters
;; (setq cfw:fchar-junction ?╋
;;       cfw:fchar-vertical-line ?┃
;;       cfw:fchar-horizontal-line ?━
;;       cfw:fchar-left-junction ?┣
;;       cfw:fchar-right-junction ?┫
;;       cfw:fchar-top-junction ?┯
;;       cfw:fchar-top-left-corner ?┏
;;       cfw:fchar-top-right-corner ?┓)

;; Another unicode chars
(setq cfw:fchar-junction ?╬
      cfw:fchar-vertical-line ?║
      cfw:fchar-horizontal-line ?═
      cfw:fchar-left-junction ?╠
      cfw:fchar-right-junction ?╣
      cfw:fchar-top-junction ?╦
      cfw:fchar-top-left-corner ?╔
      cfw:fchar-top-right-corner ?╗)


(provide 'init-calendar)
