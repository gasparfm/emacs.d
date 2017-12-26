;; Based on Alex Ott's minimal-cedet-config : https://gist.github.com/alexott/3930120

;; Use builtin CEDET instead of installing new CEDET version
(require 'cedet)
(add-to-list 'load-path (file-name-as-directory (expand-file-name "site-lisp/cedet-contrib/" user-emacs-directory)) "contrib")

;; Workarounds for Emacs < 25
(when (eval-when-compile (version< "25" emacs-version))
  (setq fixes-root-path (file-name-as-directory (expand-file-name "lisp/fixes/" user-emacs-directory)))
  (load-file (concat fixes-root-path "hideif.el")))

;; SemanticDB installed by default
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode t)
;; Buffer parsing when idle
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode 1)
;; Display current function on header
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode 1)
;; Highlight the declaration of current function
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode 1)
;; Display function info in minibuffer
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
;; srecode!!
(add-to-list 'semantic-default-submodes 'global-srecode-minor-mode t)
;; Show parser status when parsing lasts long
(add-to-list 'semantic-default-submodes 'global-show-parser-state-mode t)
;; Decorates some parts of the file due to the position where we are
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode t)
;; Underline in red everything it cannot parse
(add-to-list 'semantic-default-submodes 'global-semantic-show-unmatched-syntax-mode)
;; Most Recently Used tags/functions fast jump
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)

(semantic-mode)

;; These may be automatically required, but I had them from previous versions
(require 'semantic/bovine/c)
(require 'semantic/bovine/gcc)
(require 'semantic/bovine/grammar)
(require 'semantic/bovine/make)
(require 'semantic/bovine/scm)
(require 'semantic/idle)
(require 'semantic/db)
(require 'semantic/ia)
(require 'semantic/analyze)
(require 'eassist)

;; customisation of modes
(defun gasparfm/cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  ;;
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)
  ;; C-c+j = Fast jump
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  ;; C-c+q = Show doc
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  ;; C-c+s Show Summary
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  ;; C-c+p = Toggle between prototype and function
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  ;; C-c+m = Switch MRU tags
  (local-set-key "\C-cp" 'semantic-mrub-switch-tags)
  )

(add-hook 'c-mode-common-hook 'gasparfm/cedet-hook)
(add-hook 'lisp-mode-hook 'gasparfm/cedet-hook)
(add-hook 'scheme-mode-hook 'gasparfm/cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'gasparfm/cedet-hook)
(add-hook 'erlang-mode-hook 'gasparfm/cedet-hook)

(add-to-list 'semantic-dependency-system-include-path '/usr/include/c++/5/bits)
(semanticdb-enable-gnu-global-databases 'c-mode t)
(semanticdb-enable-gnu-global-databases 'c++-mode t)

;; SRecode
;; Delete ~/.emacs.d/srecode-map.el if any problems loading
(require 'srecode)
(global-srecode-minor-mode 1)

;; EDE
(require 'ede)
(global-ede-mode 1)
(ede-enable-generic-projects)

(require-package 'ecb)
(setq-default ecb-tip-of-the-day nil)

(myemacs/elapsed-time)
(provide 'init-cedet)
