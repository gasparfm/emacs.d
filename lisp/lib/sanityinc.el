;;----------------------------------------------------------------------------
;; Some cool functions
;;----------------------------------------------------------------------------
;; These functions borrowed from Steve Purcell's work: https://github.com/purcell

;; MISC FUNCTIONS

;;----------------------------------------------------------------------------
;; Adjust window opacity
;;----------------------------------------------------------------------------
(defun sanityinc/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

;;----------------------------------------------------------------------------
;; String utilities missing from core emacs
;;----------------------------------------------------------------------------
(defun sanityinc/string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

(defun sanityinc/string-rtrim (str)
  "Remove trailing whitespace from `STR'."
  (replace-regexp-in-string "[ \t\n]+$" "" str))


;;----------------------------------------------------------------------------
;; Find the directory containing a given library
;;----------------------------------------------------------------------------
(autoload 'find-library-name "find-func")
(defun sanityinc/directory-of-library (library-name)
  "Return the directory in which the `LIBRARY-NAME' load file is found."
  (file-name-as-directory (file-name-directory (find-library-name library-name))))

;; PACKAGE FUNCTIONS

;;----------------------------------------------------------------------------
;; If gpg cannot be found, signature checking will fail, so we
;; conditionally enable it according to whether gpg is available. We
;; re-run this check once $PATH has been configured
;;----------------------------------------------------------------------------
(defun sanityinc/package-maybe-enable-signatures ()
  (setq package-check-signature (when (executable-find "gpg") 'allow-unsigned)))

(defun sanityinc/set-tabulated-list-column-width (col-name width)
  "Set any column with name COL-NAME to the given WIDTH."
  (when (> width (length col-name))
    (cl-loop for column across tabulated-list-format
             when (string= col-name (car column))
             do (setf (elt column 1) width))))

(defun sanityinc/maybe-widen-package-menu-columns ()
  "Widen some columns of the package menu table to avoid truncation."
  (when (boundp 'tabulated-list-format)
    (sanityinc/set-tabulated-list-column-width "Version" 13)
    (let ((longest-archive-name (apply 'max (mapcar 'length (mapcar 'car package-archives)))))
      (sanityinc/set-tabulated-list-column-width "Archive" longest-archive-name))))

;; Compilations

(defun sanityinc/alert-after-compilation-finish (buf result)
  "Use `alert' to report compilation RESULT if BUF is hidden."
  (unless (catch 'is-visible
            (walk-windows (lambda (w)
                            (when (eq (window-buffer w) buf)
                              (throw 'is-visible t))))
            nil)
    (alert (concat "Compilation " result)
           :buffer buf
           :category 'compilation)))

;;----------------------------------------------------------------------------
;; Delete other windows but current
;;----------------------------------------------------------------------------
(defun sanityinc/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
(defun split-window-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))

(defun split-window-vertically-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-vertically))))


;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))


;;----------------------------------------------------------------------------
;; Window dedication
;;----------------------------------------------------------------------------
(defun sanityinc/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))


(defvar sanityinc/last-compilation-buffer nil
  "The last buffer in which compilation took place.")

;; Workaround to avoid weird behaviour with fci-mode and popup
(defvar sanityinc/fci-mode-suppressed nil)
(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible"
  (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-mode)
  (when fci-mode
    (turn-off-fci-mode)))
(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed"
  (when (and (not popup-instances) sanityinc/fci-mode-suppressed)
    (setq sanityinc/fci-mode-suppressed nil)
    (turn-on-fci-mode)))
