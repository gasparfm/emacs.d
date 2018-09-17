
;;----------------------------------------------------------------------------
;; Some cool functions
;;----------------------------------------------------------------------------
;; These functions are made by me (Twitter: @gaspar_fm; GitHub: gasparfm) or
;; heavily modified by me

;;----------------------------------------------------------------------------
;; Toggles fullscreen
;;----------------------------------------------------------------------------
(defun myemacs/toggle-fullscreen ()
       (interactive)
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

(defun myemacs/elapsed-time ()
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "[STARTUP] Loading %s ... done (%.3fs)" load-file-name elapsed)))

;;----------------------------------------------------------------------------
;; Turns off fci-mode
;;----------------------------------------------------------------------------
(defun myemacs/turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq myemacs/old-fci-mode-status fci-mode)
    (when fci-mode (fci-mode -1))))

;;----------------------------------------------------------------------------
;; Turns on fci-mode
;;----------------------------------------------------------------------------
(defun myemacs/turn-on-fci (&rest ignore)
  (progn
		(setq myemacs/old-fci-mode-status 1)
		(fci-mode 1)))

;;----------------------------------------------------------------------------
;; Turns on if it was enabled fci-mode
;;----------------------------------------------------------------------------
(defun myemacs/maybe-turn-on-fci (&rest ignore)
  (when myemacs/old-fci-mode-status (fci-mode 1)))

;;----------------------------------------------------------------------------
;; Turns off linum-mode
;;----------------------------------------------------------------------------
(defun myemacs/turn-off-linum (&rest ignore)
  (when (boundp 'linum-mode)
    (setq myemacs/old-linum-mode-status linum-mode)
    (when linum-mode (linum-mode -1))))

;;----------------------------------------------------------------------------
;; Turns on fci-mode
;;----------------------------------------------------------------------------
(defun myemacs/turn-on-linum (&rest ignore)
  (when (boundp 'linum-mode)
    (setq myemacs/old-linum-mode-status linum-mode)
    (unless linum-mode (linum-mode 1))))

;;----------------------------------------------------------------------------
;; Turns on if it was enabled fci-mode
;;----------------------------------------------------------------------------
(defun myemacs/linum-old-status (&rest ignore)
  (when myemacs/old-linum-mode-status (linum-mode 1)))
