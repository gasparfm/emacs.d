;(require-package 'hc-zenburn-theme)
(require-package 'color-theme-modern) 		; color-theme with Emacs24 framework

;(load-theme 'hc-zenburn)
(load-theme 'charcoal-black t)		; last t is NO_CONFIRM param

; Fix linum current-line highlight. Doesn't looks good with this theme
(defface my-linum-hl
  `((t :background "gray30" :foreground "gold"))
  "Face for the currently active Line number"
  :group 'linum)

(myemacs/elapsed-time)
(provide 'init-theme)
