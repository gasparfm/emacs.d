;; From https://github.com/purcell/emacs.d/blob/master/lisp/init-sessions.el
;; Using some settings from https://github.com/alexott/emacs-configs/blob/master/rc/emacs-rc-desktop.el
;; save a list of open files in ~/.emacs.d/.emacs.desktop

(setq-default desktop-missing-file-warning nil)

(setq emacs-sessions-directory (expand-file-name "sessions/" user-emacs-directory))
(setq desktop-path (list emacs-sessions-directory)
      desktop-auto-save-timeout 600)
(desktop-save-mode 1)

(defadvice desktop-read (around time-restore activate)
    (let ((start-time (current-time)))
      (prog1
          ad-do-it
        (message "Desktop restored in %.2fms"
                 (float-time (time-subtract (current-time)
				emacs-start-time))))))

(defadvice desktop-create-buffer (around time-create activate)
  (let ((start-time (current-time))
        (filename (ad-get-arg 1)))
    (prog1
        ad-do-it
      (message "Desktop: %.2fms to restore %s"
               (float-time (time-subtract (current-time)
			      emacs-start-time))
               (when filename
		 (abbreviate-file-name filename))))))
;;----------------------------------------------------------------------------
;; When splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Restore histories and registers after saving
;;----------------------------------------------------------------------------
(setq-default history-length 1000)
(savehist-mode t)

(require-package 'session)

(setq session-save-file (expand-file-name ".session" user-emacs-directory))
(setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
(add-hook 'after-init-hook 'session-initialize)

;;----------------------------------------------------------------------------
;; Histories configuration
;;----------------------------------------------------------------------------
;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((comint-input-ring        . 50)
                (compile-history          . 30)
                desktop-missing-file-warning
                (dired-regexp-history     . 20)
                (extended-command-history . 30)
                (face-name-history        . 20)
                (file-name-history        . 100)
                (grep-find-history        . 30)
                (grep-history             . 30)
                (ido-buffer-history       . 100)
                (ido-last-directory-list  . 100)
                (ido-work-directory-list  . 100)
                (ido-work-file-list       . 100)
                (ivy-history              . 100)
                (magit-read-rev-history   . 50)
                (minibuffer-history       . 50)
                (org-clock-history        . 50)
                (org-refile-history       . 50)
                (org-tags-history         . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                register-alist
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                tags-table-list)))

(when (eval-when-compile (and (>= emacs-major-version 24)
                              (version< emacs-version "24.3.50")
                              ))
  (unless (boundp 'desktop-restore-frames)
    (require-package 'frame-restore)
    (frame-restore)))

;;----------------------------------------------------------------------------
;; Backups and auto-save
;;----------------------------------------------------------------------------
;; Backups management (file~)
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . ".__backups__")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Autosave backups management (#file#) Emacs Wiki, https://www.emacswiki.org/emacs/AutoSave
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq auto-save-file-name-transforms
        `((".*" ,emacs-tmp-dir t)))
    (setq auto-save-list-file-prefix
          emacs-tmp-dir)


(myemacs/elapsed-time)
(provide 'init-sessions)
