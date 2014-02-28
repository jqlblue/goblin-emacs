;;; goblin-doxymacs.el

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

(require 'doxymacs)

;(add-to-list 'auto-mode-alist '("\\.py\\'" . c-mode-common))
;(add-to-list 'auto-mode-alist '("\\.el\\'" . lisp-mode))
;(add-hook 'c-mode-common-hook 'doxymacs-mode)
(doxymacs-mode)
;(autoload 'doxymacs-mode "doxymacs-mode" "python editing mode." t)
;(add-hook 'php-mode-hook 'doxymacs-mode)
(add-hook 'php-mode-hook
          '(lambda ()
             (doxymacs-mode 1)
             (doxymacs-font-lock)
             (setq doxymacs-doxygen-style "php")))
(add-hook 'python-mode-hook
          '(lambda ()
             (doxymacs-mode 1)
             (doxymacs-font-lock)
             (setq doxymacs-doxygen-style "php")))

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (doxymacs-mode 1)
             (doxymacs-font-lock)
             (setq doxymacs-doxygen-style "lisp")))

;; php template

(defconst doxymacs-php-file-comment-template
 '(
   "/**" > n
   "*" (if (buffer-file-name)
   " "   (file-name-nondirectory (buffer-file-name))
     "") > n
   "*" " @desc"> n
   "*" > n
   "*" " @package "> n
   "*" " @version $Id$"> n
   "*" " @copyright 2005-" (format-time-string "%Y" (current-time)) " xxx All Rights Reserved"> n
   "*" " @author "> n
 "**/" > n)'
 "Default php-style template for file documentation.")

(defconst doxymacs-php-function-comment-template
 '((let ((next-func (doxymacs-find-next-func)))
     (if next-func
	 (list
	  'l
	  "/** " '> 'n
	  " * " 'p '> 'n
	  (doxymacs-parm-tempo-element (cdr (assoc 'args next-func)))
	  (unless
	      (string-match "^[ \t\n]*void[ \t\n]*$"
			    (cdr (assoc 'return next-func)))
	    '(l " * " > n " * @return " (p "Returns: ") > n))
	  " */" '>)
       (progn
	 (error "Can't find next function declaration.")
	 nil))))
 "Default php-style template for function documentation.")

(defconst doxymacs-php-group-begin-comment-template
  ;; The leading space is a hack to get the indentation to work properly
  '(" /*" > n)
  "Default php-style template for beginning-of-group comment.")

(defconst doxymacs-php-group-end-comment-template
  ;; The leading space is a hack to get the indentation to work properly
  '(n " */" >)
  "Default php-style template for end-of-group comment.")

(defconst doxymacs-php-blank-multiline-comment-template
 '("/**" > n "* " p > n "* " > n "*/")
 "Default php-style template for a blank multiline doxygen comment.")

(defconst doxymacs-php-blank-singleline-comment-template
 '("// " > p)
 "Default php-style template for a blank single line doxygen comment.")


;; lisp template
(defconst doxymacs-lisp-file-comment-template
 '(
   ";;; "
   (if (buffer-file-name)
        (file-name-nondirectory (buffer-file-name))
   "") > n
   ""> n
   ";;; " "Copyright © 2014 jqlblue <gaoyuan.blue@gmail.com>"> n
   ""> n
   ";;; " "URL: https://github.com/jqlblue/goblin-emacs"> n
   ""> n
   ";;; " "Commentary:"> n
   ""> n
   ";; " "The package is fork from http://batsov.com/prelude/, enhance in web development."> n
   ""> n
   ";;; " "License:"> n
   ""> n
   ";; " "This library is free software; you can redistribute it and/or"> n
   ";; " "modify it under the terms of the GNU Lesser General Public"> n
   ";; " "License as published by the Free Software Foundation; either"> n
   ";; " "version 2.1 of the License, or (at your option) any later version."> n
   ";; "> n
   ";; " "This library is distributed in the hope that it will be useful,"> n
   ";; " "but WITHOUT ANY WARRANTY; without even the implied warranty of"> n
   ";; " "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU"> n
   ";; " "Lesser General Public License for more details."> n
   ";; "> n
   ";; " "You should have received a copy of the GNU Lesser General Public"> n
   ";; " "License along with this library; if not, write to the Free Software"> n
   ";; " "Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA"> n
   ";; " "02110-1301  USA"> n)'
 "Default lisp-style template for file documentation.")

(defconst doxymacs-lisp-function-comment-template
  ;; The leading space is a hack to get the indentation to work properly
  '(";;" > n)
  "Default lisp-style template for beginning-of-group comment.")

(defconst doxymacs-lisp-group-begin-comment-template
  ;; The leading space is a hack to get the indentation to work properly
  '(";;" > n)
  "Default lisp-style template for beginning-of-group comment.")

(defconst doxymacs-lisp-group-end-comment-template
  ;; The leading space is a hack to get the indentation to work properly
  '(n ";;" >)
  "Default lisp-style template for end-of-group comment.")

(defconst doxymacs-lisp-blank-multiline-comment-template
 '(";;;" > n ";; " p > n ";; " > n ";;;")
 "Default lisp-style template for a blank multiline doxygen comment.")

(defconst doxymacs-lisp-blank-singleline-comment-template
 '(";; " > p)
 "Default lisp-style template for a blank single line doxygen comment.")



(provide 'goblin-doxymacs)

;;; goblin-doxymacs.el ends here
