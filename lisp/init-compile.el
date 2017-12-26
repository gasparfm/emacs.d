;; From: https://github.com/purcell/emacs.d/blob/master/lisp/init-compile.el
(setq-default compilation-scroll-output t)

(require-package 'alert)

;; Customize `alert-default-style' to get messages after compilation

(after-load 'compile
  (add-hook 'compilation-finish-functions
            'sanityinc/alert-after-compilation-finish))

(after-load 'compile
  (defadvice compilation-start (after sanityinc/save-compilation-buffer activate)
    "Save the compilation buffer to find it later."
    (setq sanityinc/last-compilation-buffer next-error-last-buffer))

  (defadvice recompile (around sanityinc/find-prev-compilation (&optional edit-command) activate)
    "Find the previous compilation buffer, if present, and recompile there."
    (if (and (null edit-command)
             (not (derived-mode-p 'compilation-mode))
             sanityinc/last-compilation-buffer
             (buffer-live-p (get-buffer sanityinc/last-compilation-buffer)))
        (with-current-buffer sanityinc/last-compilation-buffer
          ad-do-it)
      ad-do-it)))

(defadvice shell-command-on-region
    (after sanityinc/shell-command-in-view-mode
           (start end command &optional output-buffer &rest other-args)
           activate)
  "Put \"*Shell Command Output*\" buffers into view-mode."
  (unless output-buffer
    (with-current-buffer "*Shell Command Output*"
      (view-mode 1))))


(after-load 'compile
  (require 'ansi-color)
  (defun sanityinc/colourise-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'sanityinc/colourise-compilation-buffer))

;; Key bindings
(global-set-key [S-f6] 'recompile)
(global-set-key [f6] 'compile)


(myemacs/elapsed-time)
(provide 'init-compile)
