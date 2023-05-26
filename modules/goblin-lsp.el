;;; goblin-lsp.el --- goblin-lsp setup
;;
;; Copyright © 2011-2023 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov, Ben Alex
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; goblin-lsp config.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; Add lsp or lsp-deferred function call to functions for your php-mode customization
;; (defun init-php-mode ()
;;   (eglot-ensure))

;; (with-eval-after-load 'php-mode
;;   ;; If phpactor command is not installed as global, remove next ;; and write the full path
;;   (custom-set-variables '(lsp-phpactor-path "/Users/gaoyuan/.emacs.d/extensions/lsp-server/phpactor"))
;;   (add-hook 'php-mode-hook #'init-php-mode))
;; (defun init-php-mode ()
;;   (lsp-deferred))

(require 'php-mode)

;; (require 'lsp-mode)
;; (with-eval-after-load 'php-mode
;;   ;; If phpactor command is not installed as global, write the full path
;;   ;; (custom-set-variables '(lsp-phpactor-path "/path/to/phpactor"))
;;   (custom-set-variables '(lsp-phpactor-path "/usr/local/bin/phpactor"))
;;   (custom-set-variables '(lsp-clients-php-server-command "/usr/local/bin/phpactor")))
  ;; (add-hook 'php-mode-hook #'init-php-mode))

;; (defun init-php-mode ()
;;   (lsp-deferred))

;; (defvar lsp-language-id-configuration
;;   '((php-mode . "phpactor")))

;; (with-eval-after-load 'php-mode
;;   ;; If phpactor command is not installed as global, write the full path
;;   ;; (custom-set-variables '(lsp-phpactor-path "/path/to/phpactor"))

;;   (lsp-register-client
;;     (make-lsp-client :new-connection (lsp-stdio-connection "/usr/local/bin/phpactor")
;;                      :activation-fn (lsp-activate-on "php")
;;                      :major-modes '(php-mode)
;;                      :priority -1
;;                      :server-id 'phpactor))

;;   (custom-set-variables '(lsp-phpactor-path "/usr/local/bin/phpactor"))
;;   (custom-set-variables '(lsp-clients-php-server-command "/usr/local/bin/phpactor"))
;;   (add-hook 'php-mode-hook #'init-php-mode))


(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-file-watchers nil)
  
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (php-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

;; (with-eval-after-load 'lsp-mode
;;   ;; (add-to-list 'lsp-language-id-configuration
;;   ;;   '(php-mode . "phpactor"))

;;   (lsp-register-client
;;     (make-lsp-client :new-connection (lsp-stdio-connection "/usr/local/bin/phpactor")
;;                      :activation-fn (lsp-activate-on "php")
;;                      :major-modes '(php-mode)
;;                      :priority -1
;;                      :server-id 'phpactor)))



(provide 'goblin-lsp)
;;; goblin-lsp.el ends here
