;;; goblin-modules.el

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

;; (require 'goblin-helm)
;; (require 'goblin-helm-everywhere)
;; (require 'goblin-auto-complete)

;; (require 'goblin-php)
;; (require 'goblin-web)
;; (require 'goblin-python)
;; (require 'goblin-flymake)
;; (require 'goblin-doxymacs)
;; (require 'goblin-vc)
;; (require 'goblin-org-mode)
;; (require 'goblin-rust)
;; (require 'goblin-go)

;; (load "goblin-slime")

;; (require 'goblin-vertico) ;; A powerful, yet simple, alternative to ivy
(require 'goblin-helm) ;; Interface for narrowing and search
(require 'goblin-helm-everywhere) ;; Enable Helm everywhere
;; (require 'goblin-company)
;; (require 'goblin-dired) 
(require 'goblin-lsp)
;; (require 'goblin-php)
;; (require 'goblin-eglot)
;; (require 'goblin-flutter)

(require 'goblin-magit)
;; (require 'goblin-projectile) 

(provide 'goblin-modules)
;;; goblin-modules.el ends here