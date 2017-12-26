;; Loads functions from libs
(defun load-directory (dir)
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))
		 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))

;; Load lib functions
(load-directory (expand-file-name "lisp/lib/" user-emacs-directory))

;; This is borrowed from https://github.com/purcell/emacs.d/blob/master/lisp/init-utils.el by Steve Purcell but I have added some stuff.

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

;; Elapsed time
(myemacs/elapsed-time)
(provide 'init-utils)
