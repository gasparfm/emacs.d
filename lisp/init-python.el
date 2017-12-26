;;----------------------------------------------------------------------------
;; Python Mode
;;----------------------------------------------------------------------------
(require-package 'jedi)
(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
		("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)
(defun my/python-mode-stuff ()
	;; Jedi makes everything a lot easier for everybody!
	(jedi:setup)
	(define-key python-mode-map (kbd "C-]") 'jedi:goto-definition)   ;goto define
  (local-set-key (kbd "<f1>") 'jedi:show-doc)
	(setq jedi:complete-on-dot t)                 ; optional
	)
(add-hook 'python-mode-hook 'my/python-mode-stuff)

(myemacs/elapsed-time)

(provide 'init-python)
