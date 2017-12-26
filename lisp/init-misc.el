;; Load misc modules and bind keys to them
(setq load-prefer-newer t)

;;(require-package 'neotree)

;;----------------------------------------------------------------------------
;; Uniquify - Provides unique names for files
;;----------------------------------------------------------------------------
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;;----------------------------------------------------------------------------
;; Flycheck - real time code check
;;----------------------------------------------------------------------------
;(when (maybe-require-package 'flycheck)
;  (add-hook 'after-init-hook 'global-flycheck-mode)
;  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

;;----------------------------------------------------------------------------
;; Recent files
;;----------------------------------------------------------------------------

(recentf-mode 1)
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude '("/tmp/"))

;;----------------------------------------------------------------------------
;; Smex - better minibuffer
;;----------------------------------------------------------------------------
(when (maybe-require-package 'smex)
  ;; Change path for ~/.smex-items
  (setq-default smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (global-set-key [remap execute-extended-command] 'smex))

;; Stores command frequency. M-x keyfreq-show
(require-package 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;;----------------------------------------------------------------------------
;; Spell
;;----------------------------------------------------------------------------
;(require-package 'ispell)
;(require-package 'flyspell)
;(require 'init-flyspell)

;;----------------------------------------------------------------------------
;; More things
;;----------------------------------------------------------------------------
(require-package 'htmlize)
(require-package 'regex-tool)
;(require-package 'yasnippet)
;(require 'yasnippet)
;(yas-global-mode 1)
(require-package 'visual-regexp)
(require 'visual-regexp)
(require-package 'annotate)
(require-package 'restclient)
(require-package 'async)                ;Async tasks
(require-package 's)                    ;String manipulation
(require-package 'restart-emacs)         ;Restart emacs from Emacs

;; When Emacs asks yes or no, make it possible to use just y or n
(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'annotate-mode-hook 'my/annotate-mode-stuff)

(defun my/annotate-mode-stuff ()
	(fci-mode 0)
	)

;; We could open annotate mode automatically, but I prefer doing it manually
;; (if (fboundp 'prog-mode)
;;     (add-hook 'prog-mode-hook 'annotate-mode)
;;   (dolist (hook '(lisp-mode-hook
;;                   emacs-lisp-mode-hook
;;                   scheme-mode-hook
;;                   clojure-mode-hook
;;                   ruby-mode-hook
;;                   yaml-mode
;;                   python-mode-hook
;;                   shell-mode-hook
;;                   php-mode-hook
;;                   css-mode-hook
;;                   haskell-mode-hook
;;                   caml-mode-hook
;;                   nxml-mode-hook
;;                   crontab-mode-hook
;;                   perl-mode-hook
;;                   tcl-mode-hook
;;                   javascript-mode-hook))
;;     (add-hook hook 'annotate-mode)))

;; Ido mode to navigate filesystem
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

;; Open eshell in new window
(require-package 'eshell)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
(defun eshell-other-window ()
  "Opens `eshell' in a new window."
  (interactive)
  (let ((buf (eshell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))

;;----------------------------------------------------------------------------
;; Security. Check https://ogbe.net/emacsconfig.html
;;----------------------------------------------------------------------------
(setq tls-checktrust t)
(setq gnutls-verify-error t)
(let ((trustfile "/etc/ssl/cert.pem"))
  (setq tls-program
        `(,(format  "gnutls-cli --x509cafile %s -p %%p %%h" trustfile)
          ,(format "openssl s_client -connect %%h:%%p -CAfile %s -no_ssl2 -ign_eof" trustfile)))
  (setq gnutls-trustfiles (list trustfile)))


;;----------------------------------------------------------------------------
;; Configure keys
;;----------------------------------------------------------------------------
(global-set-key (kbd "s-r") 'recentf-open-files)
(global-set-key (kbd "C-x M-c") 'recentf-open-files)
(define-key global-map "\C-ce" 'eshell-other-window)

;; Define keys for minibuffer history search
(define-key minibuffer-local-map (kbd "<prior>") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "<next>") 'next-complete-history-element)

(myemacs/elapsed-time)
(provide 'init-misc)
