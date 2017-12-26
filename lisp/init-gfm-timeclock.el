;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(setq my-workday 3600)
(setq timeclock-get-workday-function 'gfm/timeclock-workday-length)
(defun gfm/timeclock-workday-length()
  my-workday
  )

(setq gfm/timeclock-default-project-name "DefaultProject")
(defvar gfm/timeclock-auto-stopped nil)	; gfm/timeclock stopped current task because of idle status
(defvar gfm/timeclock-disabled-buffers ()) ;Buffers disabled by default
(defvar gfm/timeclock-seconds-to-idle 5)   ;Seconds to automatically stop task
(defvar gfm/timeclock-timer-seconds 1)   ; Timer seconds
(defvar gfm/timeclock-disable-timer nil)
(defvar gfm/timeclock-default-project-name "Proyecto")
(defvar gfm/timeclock-current-buffer nil)
(defvar gfm/timeclock-current-project nil)

(defun gfm/timeclock-auto-vars ()
    (setq gfm/timeclock-current-buffer (buffer-name(current-buffer)))
    )
(defun gfm/timeclock-set-project (project-name)
  (gfm/timeclock-auto-vars)
  (setq gfm/timeclock-current-project project-name)
	)

(defun gfm/timeclock-new-log (orig-fun code &optional project)
  ""
  (if (equal (downcase code) "i")
      (progn
	(gfm/timeclock-set-project (gfm/timeclock-get-project-name project project))
	(print (concat "METO UN " project "en un " code))
	(setq project (concat "[" gfm/timeclock-current-project "] " gfm/timeclock-current-buffer))
	)
      )
  (let ((res (funcall orig-fun code project)))
    res))

(advice-add 'timeclock-log :around 'gfm/timeclock-new-log)

;; Idea advise-commands from: https://github.com/bbatsov/
(defmacro advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.
The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
                    ,@body))
               commands)))

(defun gfm/timeclock-get-project-name(full &optional default)
  (setq oldproj full)

  (setq proj (let ((my-string oldproj)
		   (my-regexp "^\\[\\([^][]+\\)\\]"))
	       (and (string-match my-regexp my-string)
		    (match-string 1 my-string) )))
  (if proj
      proj
    (if default
	default
      gfm/timeclock-default-project-name)

  )
)
(defun gfm/timeclock-initialize()
  (timeclock-mode-line-display)
  (add-to-list 'gfm/timeclock-disabled-buffers "*Minibuf-1*")
  (add-to-list 'gfm/timeclock-disabled-buffers "*Messages*")
  (add-to-list 'gfm/timeclock-disabled-buffers "*Backtrace*")
  (add-to-list 'gfm/timeclock-disabled-buffers "*Completions*")
  (if (timeclock-currently-in-p)
      (progn
	(setq proj (gfm/timeclock-get-project-name (nth 0 timeclock-project-list)))
	(if proj
	    (gfm/timeclock-set-project proj)
	  (gfm/timeclock-set-project gfm/timeclock-default-project-name))
	)
    )
  (unless gfm/timeclock-disable-timer
    (gfm/timeclock-auto-timer))
  )

(eval-after-load "timeclock" '(gfm/timeclock-initialize))

(defun gfm/timeclock-update-task()
  (if (timeclock-currently-in-p)
      (if gfm/timeclock-current-project
	  ;; We were analyzing task strings with regex and so.
	  ;; Now we are storing current task and buffer in variables
	  ;; so we can access them directly.
  	  ;; (progn
	  ;;   (setq oldproj (nth 0 timeclock-project-list))
	  ;;   (setq proj (let ((my-string oldproj)
	  ;; 		     (my-regexp "\\[\\([^][]+\\)\\]"))
	  ;; 		 (and (string-match my-regexp my-string)
	  ;; 		      (match-string 1 my-string) )))
	  ;;   (setq newproj (concat "[" proj "] " (buffer-name(current-buffer))))
	  ;;   (unless (equal oldproj newproj)
	  ;;     (timeclock-change newproj)
	  ;;     ;; Almacenar el nombre del buffer para no hcaer muchos cambios
	  ;;     )
	  ;;   )
	  (print (buffer-name(current-buffer)))
	  (progn
	    (setq curbuf (buffer-name(current-buffer)))
	    (if (not (equal gfm/timeclock-current-buffer curbuf))
		(progn
		  (timeclock-change "buffer-changed" (concat "[" gfm/timeclock-current-project "] " (buffer-name(current-buffer))))
		  )
	      (print "NO METO CAMBIO")
		)
	    )
	;; (print "Error getting current project")
	)
  )
  )
(defun gfm/timeclock-auto-out(reason)
  (if (timeclock-currently-in-p)
      (progn
	(setq gfm/timeclock-auto-stopped 't)
	(timeclock-out nil reason)
	)
    )
  )

(defun gfm/timeclock-auto-in(project)
  (setq gfm/timeclock-auto-stopped nil)
  (timeclock-in nil project)
  )

(defun gfm/timeclock-buffer-update()

  (gfm/timeclock-auto-out "gfmtk-auto-buffer-change")
  (setq newbuf (buffer-name (current-buffer)))
  (unless (member gfm/timeclock-disabled-buffers newbuf)
    (gfm/timeclock-auto-in (concat "[" gfm/timeclock-current-project "] " newbuf))
    )
  )
;; Copies from org-float-time: https://github.com/mikesperber/org-mode/blob/e2ab44d9fb51bdb3e4519f069d4856dcdd6a5335/lisp/org-compat.el
(defun gfm/timeclock-float-time (&optional time)
  "Convert time value TIME to a floating point number.
TIME defaults to the current time."
  (if (featurep 'xemacs)
      (time-to-seconds (or time (current-time)))
    (float-time time)))

(defun gfm/timeclock-idle-time ()
  "Returns current Emacs idle time in seconds"
  (let ((idle-time (current-idle-time)))
  (if idle-time
      (gfm/timeclock-float-time idle-time)
    0)
  )
  )


(defun gfm/timeclock-auto-timer()
  (if (> (gfm/timeclock-idle-time) gfm/timeclock-seconds-to-idle)
      (gfm/timeclock-auto-out "gfmtk-auto-idle")
    ;; (progn
      (if gfm/timeclock-auto-stopped
	  (gfm/timeclock-auto-in (concat "[" gfm/timeclock-current-project "] " newbuf))
	(if (and (timeclock-currently-in-p) (not (equal gfm/timeclock-current-buffer (buffer-name(current-buffer)))))
	    (gfm/timeclock-buffer-update)
	  )
	)
      ;; )
    )
  (unless gfm/timeclock-disable-timer (run-at-time (format "%d sec" gfm/timeclock-timer-seconds) nil 'gfm/timeclock-auto-timer))
  )

;; En lugar de esto, hacemos un timer a 1 segundo o mas y preguntamos constantemente por el buffer, o contamos tiempo idle...
;; creo que es lo mejor, y lo que hace wakatiem
;; (advise-commands "auto-tasks"
;;                  (select-window switch-to-buffer)
;;                  after
;;                  (gfm/timeclock-update-task))

;; (defun test()
;;   (print "DISCR"))
;; ;;(advice-remove 'timeclock-find-discrep 'test)
;; (add-hook 'after-change-functions 'test)
