;;----------------------------------------------------------------------------
;; Modeline configuration
;;----------------------------------------------------------------------------

(require-package 'smart-mode-line)
(require-package 'smart-mode-line-powerline-theme)
(require-package 'sml-modeline)
;; Show number of occurrences when searching
(require-package 'anzu)

(setq sml/theme 'powerline)

(setq sml/no-confirm-load-theme t)
(setq sml/shorten-modes t)
;; Show EOL mode
(setq sml/show-eol t)
;; Show remote buffers
(setq sml/show-remote t)

(sml/setup)
(add-to-list 'sml/replacer-regexp-list '("^~/Proyectos/git/" ":Git:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/www/" ":www:") t)

(sml-modeline-mode t)

(custom-set-variables
 '(anzu-search-threshold 1000)
 '(anzu-replace-threshold 1000)
 '(anzu-deactivate-region t)
 '(anzu-input-idle-delay 0.1)
 '(anzu-replace-to-string-separator " => "))
(global-anzu-mode +1)
(set-face-attribute 'anzu-mode-line nil
                    :foreground "yellow" :weight 'bold)
(after-load 'anzu
	(diminish 'anzu-mode))

;;----------------------------------------------------------------------------
;; Keyboard shortcuts in Anzu Mode
;;----------------------------------------------------------------------------
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "s-<SPC>") 'anzu-query-replace)

(myemacs/elapsed-time)
(provide 'init-modeline)
