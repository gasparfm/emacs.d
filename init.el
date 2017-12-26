;;; Emacs configuration bootstrap file. This file will call a number
;;; of files, located first in lisp/

(when (version<= emacs-version "24")
  (error "This is made form Emacs >=24"))

(defconst emacs-start-time (current-time))
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;;; Raise garbage collection threshold after init
(add-hook 'after-init-hook
	  (lambda () (setq gc-cons-threshold local/gc-cons-threshold)))

;;; Custom variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;;; Custom settings
(setq settings-file (expand-file-name "settings.el" user-emacs-directory))

;;; Loads settings file
(when (file-exists-p custom-file)
  (load settings-file))


;; (setq byte-compile-error-on-warn t)
(require 'init-utils)
(require 'init-site-lisp)
(require 'init-elpa)
(require 'init-theme)
(require 'init-gui)
(require 'init-modeline)

(require 'init-cedet)										;
(require 'init-editing-utils)						;
(require 'init-auto-complete)						;
(require 'init-misc)
(require 'init-windows)
(require 'init-sessions)

(require 'init-compile)
(require 'init-cmode)
(require 'init-langs)
(require 'init-php)
(require 'init-python)

(require 'init-javascript)
(require 'init-nxml)
(require 'init-ruby)
(require 'init-sql)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))
;;; Loads custom file
(when (file-exists-p custom-file)
  (load custom-file))

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "[STARTUP] Loading %s ... done (%.3fs)" load-file-name elapsed)))

(provide 'init)
