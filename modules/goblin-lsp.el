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
(defun init-php-mode ()
  (lsp-deferred))

(with-eval-after-load 'php-mode
  ;; If phpactor command is not installed as global, write the full path
  ;; (custom-set-variables '(lsp-phpactor-path "/path/to/phpactor"))
  (custom-set-variables '(lsp-phpactor-path "/Users/gaoyuan/.emacs.d/extensions/lsp-server/phpactor"))
  (add-hook 'php-mode-hook #'init-php-mode))

(provide 'goblin-lsp)
;;; goblin-lsp.el ends here
