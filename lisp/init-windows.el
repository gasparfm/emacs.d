;; From https://github.com/purcell/emacs.d/blob/master/lisp/init-company.el

;;----------------------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;----------------------------------------------------------------------------
(winner-mode 1)

;; Make "C-x o" prompt for a target window when there are more than 2
(require-package 'switch-window)
(require 'switch-window)
(setq-default switch-window-shortcut-style 'alphabet)
(setq-default switch-window-timeout nil)

;;----------------------------------------------------------------------------
;; When splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
(defun split-window-func-with-other-buffer (split-function)
  (lexical-let ((s-f split-function))
    (lambda (&optional arg)
      "Split this window and switch to the new window unless ARG is provided."
      (interactive "P")
      (funcall s-f)
      (let ((target-window (next-window)))
        (set-window-buffer target-window (other-buffer))
        (unless arg
          (select-window target-window))))))


;; windmove keybindings on *nix systems with Alt key
(unless (memq window-system '(nt w32))
  (windmove-default-keybindings 'meta))

;;----------------------------------------------------------------------------
;; Key definitions
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))
(global-set-key (kbd "C-x 1") 'sanityinc/toggle-delete-other-windows)
(global-set-key (kbd "C-x |") 'split-window-horizontally-instead)
(global-set-key (kbd "C-x _") 'split-window-vertically-instead)
(global-set-key (kbd "<f7>") 'sanityinc/split-window)
(global-set-key (kbd "C-c <down>") 'sanityinc/toggle-current-window-dedication)


(myemacs/elapsed-time)
(provide 'init-windows)
