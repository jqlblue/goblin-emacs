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

                                        ;(global-company-mode)

(setenv "PATH" (concat "/usr/local/go/bin" ":"
                       "~/go/bin" ":" 
                       (getenv "PATH")))
(setq exec-path (append exec-path '("/usr/local/go/bin" "~/go/bin")))

;; Reduce the time after which the company auto completion popup opens

;;;custom go ide
(require 'go-autocomplete)
(require 'go-eldoc)
(require 'go-mode)
(require 'auto-complete-config)
(require 'golint)
(ac-config-default)
(defun go-mode-setup ()
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (setq compile-command "go build -v && go test -v && go vet")
  (define-key (current-local-map) "\C-c\C-c" 'compile)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'go-mode-setup)
(add-hook 'go-mode-hook '(lambda ()
                           (local-set-key (kbd "C-c C-k") 'godoc)))
(electric-pair-mode 1)

(provide 'goblin-go)

;;; goblin-rust.el ends here
