;;----------------------------------------------------------------------------
;; Remove some GUI stuff
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(tool-bar-mode -1)
(set-scroll-bar-mode nil)
(menu-bar-mode 0)

;;----------------------------------------------------------------------------
;; Editor configuration
;;----------------------------------------------------------------------------
(setq indicate-empty-lines t)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

;;----------------------------------------------------------------------------
;; Line numbers
;;----------------------------------------------------------------------------
;; Linum snippets from: https://www.emacswiki.org/emacs/LineNumbers
(require 'linum)
(require 'hl-line)

(defface my-linum-hl
  `((t :inherit linum :background ,(face-background 'hl-line nil t)))
  "Face for the current line number."
  :group 'linum)
(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)

(defun my-linum-get-format-string ()
  (let* ((width (1+ (length (number-to-string
                             (count-lines (point-min) (point-max))))))
         (format (concat "%" (number-to-string width) "d \u2502")))
    (setq my-linum-format-string format)))

(defvar my-linum-current-line-number 0)

(defun my-linum-format (line-number)
  (propertize (format my-linum-format-string line-number) 'face
              (if (eq line-number my-linum-current-line-number)
                  'my-linum-hl
                'linum)))
(setq linum-format 'my-linum-format)

(defadvice linum-update (around my-linum-update)
  (let ((my-linum-current-line-number (line-number-at-pos)))
    ad-do-it))
(ad-activate 'linum-update)

(defvar *linum-mdown-line* nil)

(defun line-at-click ()
  (save-excursion
	(let ((click-y (cdr (cdr (mouse-position))))
		  (line-move-visual-store line-move-visual))
	  (setq line-move-visual t)
	  (goto-char (window-start))
	  (next-line (1- click-y))
	  (setq line-move-visual line-move-visual-store)
	  ;; If you are using tabbar substitute the next line with
	  ;; (line-number-at-pos))))
	  (1+ (line-number-at-pos)))))

(defun md-select-linum ()
  (interactive)
  (goto-line (line-at-click))
  (set-mark (point))
  (setq *linum-mdown-line*
		(line-number-at-pos)))

(defun mu-select-linum ()
  (interactive)
  (when *linum-mdown-line*
	(let (mu-line)
	  ;; (goto-line (line-at-click))
	  (setq mu-line (line-at-click))
	  (goto-line (max *linum-mdown-line* mu-line))
	  (set-mark (line-end-position))
	  (goto-line (min *linum-mdown-line* mu-line))
	  (setq *linum-mdown*
			nil))))

(global-set-key (kbd "<left-margin> <down-mouse-1>") 'md-select-linum)
(global-set-key (kbd "<left-margin> <mouse-1>") 'mu-select-linum)
(global-set-key (kbd "<left-margin> <drag-mouse-1>") 'mu-select-linum)

(add-hook 'find-file-hook (lambda () (linum-mode 1)))
(linum-mode)
(setq global-linum-mode t)

;;----------------------------------------------------------------------------
;; Other settings
;;----------------------------------------------------------------------------
;; Visible bell, but flash only modeline. Thanks to http://www.grantjenks.com/
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;; Semi-bold and italic comments
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-comment-face nil :weight 'semibold)

;;----------------------------------------------------------------------------
;; Configure keys
;;----------------------------------------------------------------------------
(global-unset-key (kbd "C-z"))		; Stops C-z from minimizing window
(global-set-key (kbd "M-<down>") (lambda () (interactive) (sanityinc/adjust-opacity nil -2))) ; M-down less visibility
(global-set-key (kbd "M-<up>") (lambda () (interactive) (sanityinc/adjust-opacity nil 2))) ; M-up more visibility
(global-set-key (kbd "M-0") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100))))) ; M-0 standard visibility
(global-set-key (kbd "M-f") 'myemacs/toggle-fullscreen) ; M-f FullScreen
(global-set-key (kbd "s-C-+") 'sacha/increase-font-size)	; C-+ increase font size
(global-set-key (kbd "s-C--") 'sacha/decrease-font-size)	; C-- decrease font size
(global-set-key (kbd "<f12>") 'revert-buffer-no-confirm)
(global-set-key (kbd "s-h") 'global-hl-line-mode)	; Highlight current line

(setq display-time-day-and-date t)
(display-time)

(myemacs/elapsed-time)
(provide 'init-gui)
