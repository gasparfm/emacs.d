(require 'compile)

;; C Compilation
(add-hook 'c-mode-hook
	  (lambda ()
	    (unless (file-exists-p "Makefile")
	      (set (make-local-variable 'compile-command)
		   ;; emulate make's .c.o implicit pattern rule, but with
		   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
		   ;; variables:
		   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
		   (let ((file (file-name-nondirectory buffer-file-name)))
		     (format "%s -c -o %s.o %s %s %s"
			     (or (getenv "CC") "gcc")
			     (file-name-sans-extension file)
			     (or (getenv "CPPFLAGS") "-DDEBUG=9")
			     (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
			     file))))))

(add-hook 'c++-mode-hook
	  (lambda ()
	    (unless (file-exists-p "Makefile")
	      (set (make-local-variable 'compile-command)
		   ;; emulate make's .c.o implicit pattern rule, but with
		   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
		   ;; variables:
		   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
		   (let ((file (file-name-nondirectory buffer-file-name)))
		     (format "%s -c -o %s.o %s %s %s"
			     (or (getenv "CXX") "g++")
			     (file-name-sans-extension file)
			     (or (getenv "CPPFLAGS") "-DDEBUG=9")
			     (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
			     file))))))

;; Use C++ Mode if a File starts with // or #include
(add-to-list 'magic-fallback-mode-alist '("^// " . c++-mode))
(add-to-list 'magic-fallback-mode-alist '("^#include" . c++-mode))

;;----------------------------------------------------------------------------
;; General for C-like languages
;;----------------------------------------------------------------------------

;; Remove system recursive search (it's veeeeeeeeeery slow)
(if (featurep 'semantic)
    (setq-mode-local c-mode
		     semanticdb-find-default-throttle
		     '(file project local unloaded recursive)))

(setq-default c-basic-offset 2)
(setq-default tab-width 2)
(setq-default indent-tabs-mode 't)

(defun my/c-mode-hook ()
  (require 'ac-c-headers)
	(global-set-key [(shift return)] 'c-indent-new-comment-line)
	(add-to-list 'ac-sources 'ac-source-semantic2)
	;; Sets configuration for current file
	(setq c-basic-offset 2)
	(setq tab-width 2)
	(setq indent-tabs-mode 't)

	;; Indentation correct
	;; (c-set-offset 'arglist-cont-nonempty 'tab-width)
	;; (c-set-offset 'arglist-cont-nonempty tab-width)
	(c-set-offset 'arglist-intro 't)
	(c-set-offset 'arglist-close 0)
	(local-set-key "\C-ch" 'eassist-switch-h-cpp)
	(local-set-key "\C-ce" 'eassist-list-methods)
  ;; References of function
  (local-set-key "\C-c\C-r" 'semantic-symref)

  )
(add-hook 'c++-mode-hook 'my/c-mode-hook)

;; Derive from Alex Ott work
(defun my/ac-semantic-candidates ()
  (let* ((a (semantic-analyze-current-context ac-point))
         (tags (semantic-analyze-possible-completions a)))
    (cl-loop for tag in tags
             for class = (semantic-tag-class tag)
             for name = (semantic-format-tag-name tag nil nil)
             for type = (when (member class '(function variable type))
                          (semantic-format-tag-type tag nil))
             for args = (when (member class '(function type))
                          (semantic--format-tag-arguments
                           (if (eq class 'function)
                               (semantic-tag-function-arguments tag)
                             (list ""))
                           #'semantic-format-tag-prototype
                           nil))
             for summary = (if (eq class 'function)
                               (format "(%s) : %s" args type)
                             (format "%s" type))
             collect
             (popup-make-item name :document summary))))

(ac-define-source semantic2
  '((available . (require 'semantic/ia nil t))
    (candidates . my/ac-semantic-candidates)
    (prefix . cc-member)
    (requires . 0)))

(add-hook 'c++-mode-hook (lambda () (setq comment-start "/* "
					  comment-end   "*/")))

(add-hook 'c-mode-hook (lambda () (setq comment-start "/* "
					comment-end   "*/")))

(myemacs/elapsed-time)
(provide 'init-cmode)
