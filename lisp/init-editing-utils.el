;; From : https://github.com/purcell/emacs.d/blob/master/lisp/init-editing-utils.el
(require-package 'unfill)
(require-package 'focus)

;; Automatic pairs open symbols (, {, [...
;; (when (fboundp 'electric-pair-mode)
;;   (electric-pair-mode))
(when (eval-when-compile (version< "24.4" emacs-version))
  (electric-indent-mode 1))

;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode t
 make-backup-files t
 auto-save-interval 180									;Auto save every 180 secs
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 0.5
 truncate-lines nil
 truncate-partial-width-windows nil)

(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(transient-mark-mode t)

;;; Newline behaviour
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(when (eval-when-compile (string< "24.3.1" emacs-version))
  ;; https://github.com/purcell/emacs.d/issues/138
  (after-load 'subword
    (diminish 'subword-mode)))

;; Some bugs with indent-guide-mode and autocomplete-mode
;; (when (maybe-require-package 'indent-guide)
;;   (add-hook 'prog-mode-hook 'indent-guide-mode)
;;   (after-load 'indent-guide
;;     (diminish 'indent-guide-mode)))

;;(require-package 'nlinum)

(when (require-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))

(require-package 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)


(require-package 'highlight-symbol)
(dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
  (add-hook hook 'highlight-symbol-mode)
  (add-hook hook 'highlight-symbol-nav-mode))
(add-hook 'org-mode-hook 'highlight-symbol-nav-mode)
(after-load 'highlight-symbol
  (diminish 'highlight-symbol-mode)
  (defadvice highlight-symbol-temp-highlight (around sanityinc/maybe-suppress activate)
    "Suppress symbol highlighting while isearching."
    (unless (or isearch-mode
                (and (boundp 'multiple-cursors-mode) multiple-cursors-mode))
      ad-do-it)))

;;----------------------------------------------------------------------------
;; Zap *up* to char is a handy pair for zap-to-char
;;----------------------------------------------------------------------------
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;;----------------------------------------------------------------------------
;; Browse kill ring
;;----------------------------------------------------------------------------
(require-package 'browse-kill-ring)
(setq browse-kill-ring-highlight-inserted-item 'pulse)
(setq browse-kill-ring-highlight-current-entry t)
(setq browse-kill-ring-show-preview t)
(setq browse-kill-ring-separator "\f")
(global-set-key (kbd "s-Y") 'browse-kill-ring)
(after-load 'browse-kill-ring
  (define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous)
  (define-key browse-kill-ring-mode-map (kbd "<down>") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "<up>") 'browse-kill-ring-previous)
  (define-key browse-kill-ring-mode-map (kbd "<backtab>") 'browse-kill-ring-previous) ;Shift + TAB
  )
(after-load 'page-break-lines
  (push 'browse-kill-ring-mode page-break-lines-modes))

;; Forward yank
(defun yank-pop-forwards (arg)
      (interactive "p")
      (yank-pop (- arg)))
    (global-set-key "\M-Y" 'yank-pop-forwards) ; M-Y (Meta-Shift-Y)

;;----------------------------------------------------------------------------
;; Don't disable narrowing commands
;;----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;----------------------------------------------------------------------------
;; Show matching parens
;;----------------------------------------------------------------------------
(show-paren-mode 1)

;;----------------------------------------------------------------------------
;; Expand region
;;----------------------------------------------------------------------------
(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;----------------------------------------------------------------------------
;; Don't disable these functions
;;----------------------------------------------------------------------------
(put 'scroll-left 'disabled nil)
(put 'eval-expression 'disabled nil)

;;----------------------------------------------------------------------------
;; Rectangle selections, and overwrite text when the selection is active
;;----------------------------------------------------------------------------
(cua-selection-mode t)                  ; for rectangles, CUA is nice


;; Dont delete selected text when pressing key. C-w
(setq delete-selection-mode nil)

;;----------------------------------------------------------------------------
;; Handy key bindings
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

;; (when (maybe-require-package 'avy)
;;   (global-set-key (kbd "C-;") 'avy-goto-word-or-subword-1))

(require-package 'multiple-cursors)
;; multiple-cursors
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-+") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; From active region to multiple cursors:
(global-set-key (kbd "C-c c r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c c c") 'mc/edit-lines)
(global-set-key (kbd "C-c c e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c c a") 'mc/edit-beginnings-of-lines)


;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])



(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)

;;----------------------------------------------------------------------------
;; Page break lines
;;----------------------------------------------------------------------------
(require-package 'page-break-lines)
(global-page-break-lines-mode)
(diminish 'page-break-lines-mode)

;;----------------------------------------------------------------------------
;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.
;;----------------------------------------------------------------------------
(require-package 'move-dup)
;(global-set-key [M-up] 'md/move-lines-up)
;(global-set-key [M-down] 'md/move-lines-down)
;(global-set-key [M-S-up] 'md/move-lines-up)
;(global-set-key [M-S-down] 'md/move-lines-down)

(global-set-key (kbd "C-c d") 'md/duplicate-down)
(global-set-key (kbd "C-c D") 'md/duplicate-up)

;;----------------------------------------------------------------------------
;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
;;----------------------------------------------------------------------------
(defun backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up


;;----------------------------------------------------------------------------
;; Cut/copy the current line if no region is active
;;----------------------------------------------------------------------------
(require-package 'whole-line-or-region)
(whole-line-or-region-mode t)
(diminish 'whole-line-or-region-mode)
(make-variable-buffer-local 'whole-line-or-region-mode)

(defun suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (let ((flagvar (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
        (advice-name (intern (format "suspend-%s" mode-name))))
    (eval-after-load 'cua-rect
      `(progn
         (defvar ,flagvar nil)
         (make-variable-buffer-local ',flagvar)
         (defadvice cua--activate-rectangle (after ,advice-name activate)
           (setq ,flagvar (and (boundp ',mode-name) ,mode-name))
           (when ,flagvar
             (,mode-name 0)))
         (defadvice cua--deactivate-rectangle (after ,advice-name activate)
           (when ,flagvar
             (,mode-name 1)))))))

(suspend-mode-during-cua-rect-selection 'whole-line-or-region-mode)




(defun sanityinc/open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
	 (do-left-margin (and (bolp) (> (current-left-margin) 0)))
	 (loc (point-marker))
	 ;; Don't expand an abbrev before point.
	 (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
	     (if do-left-margin (indent-to (current-left-margin)))
	     (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

;;----------------------------------------------------------------------------
;; Random line sorting
;;----------------------------------------------------------------------------
(defun sort-lines-random (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))




(require-package 'highlight-escape-sequences)
(hes-mode)

(require-package 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c" "C-x 4" "C-x 5" "C-c ;" "C-c ; f" "C-c ' f" "C-x n" "C-x C-r" "C-x r" "M-s" "C-h"))
(add-hook 'after-init-hook
          (lambda ()
            (guide-key-mode 1)
            (diminish 'guide-key-mode)))

;;----------------------------------------------------------------------------
;; More useful things
;;----------------------------------------------------------------------------
(set-default 'truncate-lines t)
(setq show-trailing-whitespace nil)
(setq site-lisp-path (file-name-as-directory (expand-file-name "site-lisp/" user-emacs-directory)))

(require-package 'fill-column-indicator)

;; ;; Global-FCI-mode can cause problems, like minibuffer with fci or information windows
;; ;; drawing incorrectly.
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(defvar fci-mode nil)
;; (global-fci-mode 1)
(add-hook 'prog-mode-hook 'my/fci-mode-stuff)

(defun my/fci-mode-stuff ()
	(fci-mode 1)
	(setq fci-rule-color "dim gray")
	(setq fci-rule-width 2)
	(setq fci-rule-column 130)
	;; Workaround because of implementation of show-trailing-whitespace https://github.com/alpaker/Fill-Column-Indicator/blob/master/fill-column-indicator.el
	(setq whitespace-style '(face trailing))
	;; fci-mode not compatible with non-nil values of hl-line-sticky-flag
	(setq hl-line-sticky-flag nil))

;; (set-fill-column 130)
;; (global-whitespace-mode)
;;
;; Word Count Mode with Goals. Can be activated, Run in Modeline
(require-package 'wc-mode)
; hideshowvis for hs-mode
;(require-package 'hideshowvis)
(dolist (hook (list 'emacs-lisp-mode-hook
                    'java-mode-hook
                    'lisp-mode-hook
                    'perl-mode-hook
                    'hs-mode-hook
                    'c-mode-common-hook))
  (add-hook hook 'hs-minor-mode))
;; From EmacsWiki expand hidden block when searching for line
(defadvice goto-line (after expand-after-goto-line
                            activate compile)
  "hideshow-expand affected block when using goto-line in a collapsed buffer"
  (save-excursion
    (hs-show-block)))

;; Echo keystrokes
(setq echo-keystrokes 0.1)

;; Copy/Paste behaviour. Use X clipboard.
(setq x-select-enable-primary t)
(setq x-select-enable-clipboard nil)

;; Emacs Scroll like any other editor kaka pedo culo pis
(setq scroll-step            1
      scroll-conservatively  10000)
;; Show line numbers when using go-to line when line numbers are not enabled
;; Stolen from http://whattheemacsd.com/key-bindings.el-01.html
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (myemacs/turn-on-linum)
        (goto-line (read-number "Goto line: ")))
    (myemacs/linum-old-status)))

;; Delete trailing whitespace before saving fil
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;----------------------------------------------------------------------------
;; Key definitions
;;----------------------------------------------------------------------------
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "s-<return>") 'sanityinc/newline-at-end-of-line)

(global-set-key (kbd "C-<up>") 'backward-paragraph)
(global-set-key (kbd "C-<down>") 'forward-paragraph)
(global-set-key (kbd "C-<left>") 'backward-word)
(global-set-key (kbd "C-<right>") 'forward-word)

(global-set-key [(s-backspace)] 'undo)
(global-set-key [(s-S-backspace)] 'redo) ;Meta -Shift-Backspace
(global-set-key (kbd "C-o") 'sanityinc/open-line-with-reindent)

;; Mouse horizontal scrolling (in case your mouse has horizontal scroll)
(global-set-key [mouse-6] 'scroll-right)
(global-set-key [mouse-7] 'scroll-left)

;; Sometimes my laptop can't autoindent
(global-set-key (kbd "C-M-º") 'indent-region)

;; hs-mode key bindings I like
(global-set-key (kbd "M-+") 'hs-show-block)
(global-set-key (kbd "M--") 'hs-hide-block)

;; Join lines on M-j
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; Focus mode toggle
(global-set-key (kbd "s-º") (lambda () (interactive) (focus-mode 'toggle)))

;; Browse kill ring
(global-set-key (kbd "C-c k") 'browse-kill-ring)

;; Smarter move to the beginning of the line
;; remap C-a or to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'sacha/smarter-move-beginning-of-line)

(myemacs/elapsed-time)
(provide 'init-editing-utils)
