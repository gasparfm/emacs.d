;; Auto-complete basic configuration.
;; We'll Find more specific configuration in language files
(require-package 'auto-complete)
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode)
(setq ac-comphist-file (expand-file-name
              "ac-comphist.dat" user-emacs-directory))

;; (add-to-list 'ac-dictionary-directories (expand-file-name
;; 					 "~/.emacs.d_new/elpa/auto-complete-20170124.1845/dict"))

;;a word must be 3 chars long before completition begins
(setq-default ac-auto-start 2)
(set-default 'ac-sources '(ac-source-abbrev ac-source-features ac-source-words-in-same-mode-buffers ac-source-functions ac-source-variables ac-source-symbols ac-source-dictionary ac-source-abbrev ac-source-words-in-buffer))
(setq-default ac-ignore-case 'smart)
;; Do What I Mean mode - Changes TAB behaviour depending on context
(setq-default ac-dwim t)
(setq ac-quick-help-delay 1)
;; Limit candidate limit
(setq ac-candidate-limit 400)
;; Allow searches
(setq ac-use-menu-map t)
(setq ac-candidate-menu-min 0)

(myemacs/elapsed-time)
(provide 'init-auto-complete)
