(require-package 'php-mode)

(when (maybe-require-package 'php-mode)
  (maybe-require-package 'smarty-mode))

;; From EmacsWiki: https://www.emacswiki.org/emacs/PhpMode
(defun my/php-symbol-lookup ()
  (interactive)
  (let ((symbol (symbol-at-point)))
    (if (not symbol)
        (message "No symbol at point.")

      (browse-url (concat "http://php.net/manual-lookup.php?pattern="
                          (symbol-name symbol))))))


(defun my/php-function-lookup ()
  (interactive)
  (let* ((function (symbol-name (or (symbol-at-point)
                                    (error "No function at point."))))
         (buf (url-retrieve-synchronously (concat "http://php.net/manual-lookup.php?pattern=" function))))
    (with-current-buffer buf
      (goto-char (point-min))
        (let (desc)
          (when (re-search-forward "<div class=\"methodsynopsis dc-description\">\\(\\(.\\|\n\\)*?\\)</div>" nil t)
            (setq desc
              (replace-regexp-in-string
                " +" " "
                (replace-regexp-in-string
                  "\n" ""
                  (replace-regexp-in-string "<.*?>" "" (match-string-no-properties 1)))))

            (when (re-search-forward "<p class=\"para rdfs-comment\">\\(\\(.\\|\n\\)*?\\)</p>" nil t)
              (setq desc
                    (concat desc "\n\n"
                            (replace-regexp-in-string
                             " +" " "
                             (replace-regexp-in-string
                              "\n" ""
                              (replace-regexp-in-string "<.*?>" "" (match-string-no-properties 1))))))))

          (if desc
              (message desc)
            (message "Could not extract function info. Press C-F1 to go the description."))))
    (kill-buffer buf)))

(require-package 'ac-php)

(add-hook 'php-mode-hook 'my/php-mode-stuff)

(defun my/php-mode-stuff ()
  (local-set-key (kbd "<f1>") 'my/php-function-lookup)
  (local-set-key (kbd "C-<f1>") 'my/php-symbol-lookup)
	;; New versions of PHP have this :)
	(php-enable-psr2-coding-style)
	(fci-mode 0)
	(auto-complete-mode t)
	(require 'ac-php)
	(setq ac-sources  '(ac-source-dictionary ac-source-abbrev ac-source-php ) )
	(ac-php-core-eldoc-setup ) ;enable eldoc
	(define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
	(define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back   ) ;go back
	)

(add-hook 'php-mode-hook 'my/php-mode-stuff)

(myemacs/elapsed-time)
(provide 'init-php)
