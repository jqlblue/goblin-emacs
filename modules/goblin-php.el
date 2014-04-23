;;; goblin-php.el

;;; Copyright © 2014 jqlblue <gaoyuan.blue@gmail.com>

;;; URL: https://github.com/jqlblue/goblin-emacs

;;; Commentary:

;; The package is fork from http://batsov.com/prelude/, enhance in web development.

;;; License:

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 2.1 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301  USA

;;; Code:

(require 'php-mode)
(defun goblin-php-mode-defaults ()

  ;; 设置php-mode

  (add-to-list 'auto-mode-alist
			   '("\\.php[34]?\\'\\|\\.phtml\\'" . php-mode))
  (defun php-run ()
	(interactive)
	(shell-command
   (concat "php -q \"" (buffer-file-name) "\"")))
  (defun php-check ()
	(interactive)
	(let ((compilation-error-regexp-alist '(php))
		  (compilation-error-regexp-alist-alist ()))
	  (pushnew '(php "\\(syntax error.*\\) in \\(.*\\) on line \\([0-9]+\\)$" 2 3 nil nil 1)
			   compilation-error-regexp-alist-alist)
	  (compile (concat "php -l -f \"" (buffer-file-name) "\""))))
  (defun php-check-style ()
	"Performs a PHP code sniffer check on the current file."
	(interactive)
	(let ((compilation-error-regexp-alist '(gnu)))
	  ;; (compile (format "phpcs --standard=PEAR --report=emacs \"%s\""
	  (compile (format "phpcs --standard=ZEND \"%s\""
					   (buffer-file-name)))))

  (define-key php-mode-map
	[menu-bar php php-run]
	'("Run PHP" . php-run)
	)
  (define-key php-mode-map
	[menu-bar php php-check]
	'("PHP Check" . php-check)
	)
  (define-key php-mode-map
	[menu-bar php php-check-style]
	'("PHP Check Style" . php-check-style)
	)

  ;; 设置php-tokens
  (require 'php-tokens)

  (setq php-mode-force-pear 1)
  (setq imenu-auto-rescan 1)

  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)  ;用空格代替tab

  ;; 将回车代替C-j的功能，换行的同时对齐
  (define-key c-mode-map [return] 'newline-and-indent)

  (define-key php-mode-map [(control tab)] 'php-complete-function)
  (define-key php-mode-map [(control c) (r)] 'php-run)
  (define-key php-mode-map [(control c) (c)] 'php-check)
  (define-key php-mode-map [(control c) (l)] 'php-check-style)

  (define-key php-mode-map [(control c) (t)] 'php-tokens)

  (interactive)
  (setq fill-column 78)
  ;; 设置php程序的对齐风格
  (c-set-style "K&R")
  ;; 自动模式，在此种模式下当你键入{时，会自动根据你设置的对齐风格对齐
  (c-toggle-auto-state)
  (c-toggle-electric-state 1)
  ;; 此模式下，当按Backspace时会删除最多的空格
  (c-toggle-hungry-state)
  ;; TAB键的宽度设置为4
  (setq c-basic-offset 4)
  ;; 在菜单中加入当前Buffer的函数索引
  (imenu-add-menubar-index)
  ;; 在状态条上显示当前光标在哪个函数体内部
  (which-function-mode)

)

(defun goblin-php-mode-document ()
  (define-key php-mode-map [(control c) (h)] 'document-php-function-lookup)
  (local-set-key (kbd "<f1>") 'document-php-symbol-lookup))
  ;(local-set-key (kbd "C-<f1>") 'document-php-symbol-lookup))


(defun document-php-symbol-lookup ()
  (interactive)
  (let ((symbol (symbol-at-point)))
    (if (not symbol)
        (message "No symbol at point.")

      (browse-url (concat "http://php.net/manual-lookup.php?pattern="
                          (symbol-name symbol))))))


(defun document-php-function-lookup ()
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


;(setq goblin-php-mode-hook 'goblin-php-mode-defaults)

(add-hook 'php-mode-hook 'goblin-php-mode-defaults)
(add-hook 'php-mode-hook 'goblin-php-mode-document)
;(add-hook 'php-mode-hook (lambda ()
;                              (run-hooks 'goblin-php-mode-hook)))
(provide 'goblin-php)

;;; goblin-php.el ends here
