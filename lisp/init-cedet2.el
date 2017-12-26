;;; minimial-cedet-config.el --- Working configuration for CEDET from bzr

;; Copyright (C) Alex Ott
;;
;; Author: Alex Ott <alexott@gmail.com>
;; Keywords: cedet, C++, Java
;; Requirements: CEDET from bzr (http://cedet.sourceforge.net/bzr-repo.shtml)

;; Do checkout of fresh CEDET, and use this config (don't forget to change path below)

(setq cedet-root-path (file-name-as-directory (expand-file-name "site-lisp/cedet-bzr/" user-emacs-directory)))

(load-file (concat cedet-root-path "cedet-devel-load.el"))
(add-to-list 'load-path (concat cedet-root-path "contrib"))
;; select which submodes we want to activate
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)

;; Activate semantic
(semantic-mode 1)

;; load contrib library
(require 'eassist)

;; customisation of modes
(defun alexott/cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  ;;
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)

  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  )
(add-hook 'c-mode-common-hook 'alexott/cedet-hook)
(add-hook 'lisp-mode-hook 'alexott/cedet-hook)
(add-hook 'scheme-mode-hook 'alexott/cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'alexott/cedet-hook)
(add-hook 'erlang-mode-hook 'alexott/cedet-hook)

(defun alexott/c-mode-cedet-hook ()
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  )
(add-hook 'c-mode-common-hook 'alexott/c-mode-cedet-hook)

(semanticdb-enable-gnu-global-databases 'c-mode t)
(semanticdb-enable-gnu-global-databases 'c++-mode t)

(when (cedet-ectag-version-check t)
  (semantic-load-enable-primary-ectags-support))

;; SRecode
(global-srecode-minor-mode 1)

;; EDE
(global-ede-mode 1)
(ede-enable-generic-projects)


;; Setup JAVA....
(require 'cedet-java)

;; ;; Semantic
;; (global-semantic-idle-completions-mode t)
;; (global-semantic-decoration-mode t)
;; (global-semantic-highlight-func-mode t)
;; (global-semantic-show-unmatched-syntax-mode t)

;; ;; CC-mode
;; (add-hook 'c-mode-hook '(lambda ()
;;         (setq ac-sources (append '(ac-source-semantic) ac-sources))
;;         (local-set-key (kbd "RET") 'newline-and-indent)
;;         (linum-mode t)
;;         (semantic-mode t)))

;; Autocomplete
(require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories (expand-file-name
;; 					 "~/.emacs.d_new/elpa/auto-complete-20170124.1845/dict"))
;; (setq ac-comphist-file (expand-file-name
;;              "ac-comphist.dat" user-emacs-directory))
(ac-config-default)

;; (myemacs/elapsed-time)
;; (provide 'init-cedet)

(defun my/c-mode-hook ()
  (add-to-list 'ac-sources 'ac-source-semantic2))
(add-hook 'c++-mode-hook 'my/c-mode-hook)

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
             (popup-make-item name :summary summary))))

(ac-define-source semantic2
  '((available . (require 'semantic/ia nil t))
    (candidates . my/ac-semantic-candidates)
    (prefix . cc-member)
    (requires . 0)))

(myemacs/elapsed-time)
(provide 'init-cedet)
