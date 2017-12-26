;;----------------------------------------------------------------------------
;; General for all languages
;;----------------------------------------------------------------------------
;; Useful for editing compressed files transparently
(auto-compression-mode t)
;;----------------------------------------------------------------------------
;; General for C-like languages
;;----------------------------------------------------------------------------

; From https://github.com/purcell/emacs.d/blob/master/lisp/init-markdown.el
;;----------------------------------------------------------------------------
;; Markdown mode
;;----------------------------------------------------------------------------
(when (maybe-require-package 'markdown-mode)
  (after-load 'whitespace-cleanup-mode
    (push 'markdown-mode whitespace-cleanup-mode-ignore-modes)))

;;----------------------------------------------------------------------------
;; CSV Mode
;;----------------------------------------------------------------------------

(require-package 'csv-mode)
(require-package 'csv-nav)

(add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")

(setq csv-separators '("," ";" "|" " "))

;;----------------------------------------------------------------------------
;; PHP Mode
;;----------------------------------------------------------------------------
;; (when (maybe-require-package 'php-mode)
;;   (maybe-require-package 'smarty-mode))

;; (require-package 'ac-php)
;; (add-hook 'php-mode-hook '(lambda ()
;;                            (auto-complete-mode t)
;;                            (require 'ac-php)
;;                            (setq ac-sources  '(ac-source-php ) )
;;                            (yas-global-mode 1)

;;                            (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
;;                            (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back   ) ;go back
;;                            ))

;;----------------------------------------------------------------------------
;; HTML Mode
;;----------------------------------------------------------------------------
(require-package 'tidy)
(add-hook 'html-mode-hook (lambda () (tidy-build-menu html-mode-map)))

(require-package 'tagedit)
(after-load 'sgml-mode
  (tagedit-add-paredit-like-keybindings)
  (add-hook 'sgml-mode-hook (lambda () (tagedit-mode 1))))

(add-auto-mode 'html-mode "\\.\\(jsp\\|tmpl\\)\\'")

;;----------------------------------------------------------------------------
;; CSS Mode
;;----------------------------------------------------------------------------
(setq-default cssm-indent-level 4)
(setq-default cssm-newline-before-closing-bracket t)
(setq-default cssm-indent-function #'cssm-c-style-indenter)
;; (setq-default scss-sass-command '/usr/local/bin/sass)
(setq-default cssm-mirror-mode nil)

;;----------------------------------------------------------------------------
;; Web mode
;;----------------------------------------------------------------------------
(require-package 'web-mode)
(add-hook 'web-mode-hook (lambda () ((fci-mode 0))))

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;----------------------------------------------------------------------------
;; Multiple major modes
;;----------------------------------------------------------------------------
(require-package 'mmm-mode)
(require 'mmm-auto)
(setq mmm-global-mode 'buffers-with-submode-classes)
(setq mmm-submode-decoration-level 2)

;;----------------------------------------------------------------------------
;; Some more mode languages
;;----------------------------------------------------------------------------
(require-package 'gnuplot)
(require-package 'lua-mode)
(require-package 'ansible)
(require-package 'elf-mode)

(provide 'init-langs)
