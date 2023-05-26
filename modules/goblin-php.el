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
;; (require 'phpactor)

(defun init-php-mode ()
  (lsp-deferred))

;; (with-eval-after-load 'lsp-mode
;;   (add-to-list 'lsp-language-id-configuration
;;     '(php-mode . "phpactor"))

;;   (lsp-register-client
;;     (make-lsp-client :new-connection (lsp-stdio-connection "phpactor")
;;                      :activation-fn (lsp-activate-on "php")
;;                      :server-id 'phpactor)))


;; (use-package phpactor :ensure t)
;; (use-package company-phpactor :ensure t)

;; (use-package php-mode
;;   ;;
;;   :hook ((php-mode . (lambda () (set (make-local-variable 'company-backends)
;;        '(;; list of backends
;;          company-phpactor
;;          company-files
;;          ))))))

;; (defun init-php-mode ()
;;   (eglot-ensure))

;; (add-hook 'php-mode-hook 'eglot-ensure)
;; (defun init-php-mode ()
;;   (lsp-deferred))

;; (which-key-mode)
;; ;; (add-hook 'php-mode-hook 'lsp)

;; (setq gc-cons-threshold (* 100 1024 1024)
;;       read-process-output-max (* 1024 1024)
;;       treemacs-space-between-root-nodes nil
;;       company-idle-delay 0.0
;;       company-minimum-prefix-length 1
;;       lsp-idle-delay 0.1)  ;; clangd is fast

;; (with-eval-after-load 'lsp-mode
;;   (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
;;   ;; (require 'dap-php)
;;   (yas-global-mode))



;; (add-hook 'php-mode-hook #'init-php-mode))

;; (with-eval-after-load 'php-mode
;;   ;; If phpactor command is not installed as global, remove next ;; and write the full path
;;   (custom-set-variables '(lsp-phpactor-path "/usr/local/bin/phpactor"))
;;   (add-hook 'php-mode-hook #'init-php-mode))
  

;; (add-hook 'php-mode-hook
;;           (lambda ()
;;             (make-local-variable 'eldoc-documentation-function)
;;             (setq eldoc-documentation-function
;;                   'phpactor-hover)))

;; (with-eval-after-load 'php-mode
;;   (custom-set-variables '(lsp-phpactor-path "/usr/local/bin/phpactor"))
;;   (add-hook 'php-mode-hook #'init-php-mode)
;;   ;; (add-to-list 'eglot-stay-out-of 'eldoc)
;;   (phpactor-smart-jump-register))






  ;; (with-eval-after-load "eglot"
  ;; (add-to-list 'eglot-server-programs '(php-mode "intelephense" "--stdio"))
  ;; (add-hook 'php-mode-hook 'eglot-ensure))
;; (require 'php-doc)

;; (eval-after-load 'php-mode
;;    '(require 'php-ext))

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
;; (defun init-php-mode ()
;;   (lsp-bridge-mode +1))

;; (with-eval-after-load "php-mode"
;;   (define-key php-mode-map (kbd "C-.") nil)
;;   (define-key php-mode-map (kbd "C-c C--") 'php-current-class)
;;   (define-key php-mode-map (kbd "C-c C-=") 'php-current-namespace)
;;   (phpactor-smart-jump-register)
;;   (setq lsp-bridge-php-lsp-server "phpactor")
;;   (defvar phpactor-executable "/usr/local/bin/phpactor")
;;   (set (make-local-variable 'company-backends)
;;        '(;; list of backends
;;          company-phpactor
;;          company-files
;;          ))

;;   (add-hook 'php-mode-hook (lambda ()
;;                             (lsp-bridge-mode +1)
;;                             ;; (php-enable-default-coding-style +1)
;;                             ;; (flymake-phpstan-turn-on +1)
;;                             ;; (subword-mode 1)
;;                             (make-local-variable 'eldoc-documentation-function)
;;                             (setq eldoc-documentation-function
;;                               'phpactor-hover)
;;                              (c-toggle-auto-newline 1))))


;; (add-hook 'php-mode-hook 'php-enable-default-coding-style)
;; (add-hook 'php-mode-hook (lambda () (subword-mode 1)))
;; (with-eval-after-load 'php-mode
;;   (define-key php-mode-map (kbd "C-c C--") 'php-current-class)
;;   (define-key php-mode-map (kbd "C-c C-=") 'php-current-namespace))

;; ;; (add-hook 'php-mode-hook #'flymake-phpstan-turn-on)
;; (use-package php-mode
;;   ;;
;;   :hook ((php-mode . (lambda () (set (make-local-variable 'company-backends)
;;        '(;; list of backends
;;          company-phpactor
;;          company-files
;;          ))))))

;; (add-hook 'php-mode-hook
;;           (lambda ()
;;             (make-local-variable 'eldoc-documentation-function)
;;             (setq eldoc-documentation-function
;;                   'phpactor-hover)))

;; (with-eval-after-load 'php-mode
;;   (phpactor-smart-jump-register))




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
