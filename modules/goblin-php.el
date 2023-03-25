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
;(require 'php-doc)

;; (eval-after-load 'php-mode
;;   '(require 'php-ext))

;; (add-hook 'php-mode-hook
;;           '(lambda ()
;;              (require 'company-php)
;;              (company-mode t)
;;              (add-to-list 'company-backends 'company-ac-php-backend )))

;; (with-eval-after-load 'company
;;   (define-key company-active-map (kbd "M-n") nil)
;;   (define-key company-active-map (kbd "M-p") nil)
;;   (define-key company-active-map (kbd "C-n") #'company-select-next)
;;   (define-key company-active-map (kbd "C-p") #'company-select-previous))

;; ;; (add-hook 'php-mode-hook 'eglot-ensure)

;; ;; (with-eval-after-load "eglot"
;; ;;   (add-to-list 'eglot-server-programs '(php-mode "/Users/gaoyuan/.nvm/versions/node/v16.16.0/bin/intelephense" "--stdio"))
;; ;;   (add-to-list 'eglot-stay-out-of 'eldoc)
;; ;;   (with-eval-after-load "php-mode"
;; ;;     (define-key php-mode-map (kbd "<f5>") #'eldoc-box-eglot-help-at-point)))

;; (with-eval-after-load "eglot"
;;   (add-to-list 'eglot-server-programs '(php-mode "intelephense" "--stdio"))
;;   (add-hook 'php-mode-hook 'eglot-ensure))


;; (use-package php-mode
;;   :defer t)
;; (require 'lsp-bridge)
(defun init-php-mode ()
  (lsp-bridge-mode +1))

(with-eval-after-load "php-mode"
  (define-key php-mode-map (kbd "C-.") nil)
  (setq lsp-bridge-php-lsp-server "phpactor")

  (add-hook 'php-mode-hook (lambda ()
                            (lsp-bridge-mode +1)
                             (c-toggle-auto-newline 1))))
;; (require 'lsp-bridge)

;; (defun init-php-mode ()
;;   (lsp-bridge-mode +1))

;; (with-eval-after-load 'php-mode
;;   (custom-set-variables '(lsp-bridge-php-lsp-server . "phpactor"))
;;   (add-hook 'php-mode-hook #'init-php-mode))

;; (with-eval-after-load "eglot"
;;   (add-to-list 'eglot-server-programs '(php-mode "intelephense" "--stdio"))
;;   (add-hook 'php-mode-hook 'eglot-ensure))

(provide 'goblin-php)

;;; goblin-php.el ends here
