;;; goblin-lsp-bridge.el --- goblin-lsp-bridge setup
;;
;; Copyright © 2011-2023 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov, Ben Alex
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; goblin-lsp-bridge config.

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

;; lsp-bridge 依赖 : posframe
;(pkginstall 'posframe)

;; lsp-bridge 依赖 : markdown-mode
;(pkginstall 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
             '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))
(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp-bridge 依赖 : yasnippet
;(pkginstall 'yasnippet)
;(pkginstall 'yasnippet-snippets)

(yas-global-mode 1)
;; (define-key yas-minor-mode-map "\C-j" 'yas-expand)
;; (define-key yas-minor-mode-map "\C-k" 'yas-prev-field)
;; (define-key yas-keymap "\C-j" 'yas-next-field-or-maybe-expand)
;; (define-key yas-keymap "\C-k" 'yas-prev-field)

(dolist (keymap (list yas-minor-mode-map yas-keymap))
		(define-key keymap (kbd "TAB") 'nil)
		(define-key keymap [(tab)] 'nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 如果是在图形界面, 则加载 lsp-bridge
(add-to-list 'load-path "~/.emacs.d/extensions/vendor/lsp-bridge")
(require 'lsp-bridge)
;; (if (display-grayscale-p)
;; 	(global-lsp-bridge-mode))

;; Add lsp or lsp-deferred function call to functions for your php-mode customization
;; (defun init-php-mode ()
;;   (lsp-bridge-mode +1))

;; (with-eval-after-load 'php-mode
;;   (setq lsp-bridge-php-lsp-server  "phpactor")
;;   (add-hook 'php-mode-hook #'init-php-mode))



(setq acm-enable-icon t
	  acm-enable-tabnine nil
	  lsp-bridge-signature-show-function 'lsp-bridge-signature-posframe)
;; 快捷键
(global-set-key (kbd "C-c l d") 'lsp-bridge-diagnostic-list)
(global-set-key (kbd "C-c l n") 'lsp-bridge-diagnostic-jump-next)
(global-set-key (kbd "C-c l p") 'lsp-bridge-diagnostic-jump-prev)

;; (setq lsp-bridge-php-lsp-server "phpactor")

(provide 'goblin-lsp-bridge)
;;; goblin-lsp-bridge.el ends here
