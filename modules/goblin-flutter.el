;;; goblin-flutter.el

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

;; Assuming usage with dart-mode
;; (use-package dart-mode
;;   ;; Optional
;;   :hook (dart-mode . flutter-test-mode))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "/Users/gaoyuan/apps/flutter/3.0.2/flutter/"))

;; (defun init-flutter-mode ()
;;   (lsp-bridge-mode +1))

;; (with-eval-after-load "dart-mode"
;;   ;; (define-key php-mode-map (kbd "C-.") nil)
;;   ;; (define-key php-mode-map (kbd "C-c C--") 'php-current-class)
;;   ;; (define-key php-mode-map (kbd "C-c C-=") 'php-current-namespace)
;;   ;; (phpactor-smart-jump-register)
;;   ;; (setq lsp-bridge-php-lsp-server "phpactor")
;;   ;; (defvar phpactor-executable "/usr/local/bin/phpactor")
;;   ;; (set (make-local-variable 'company-backends)
;;   ;;      '(;; list of backends
;;   ;;        company-phpactor
;;   ;;        company-files
;;   ;;        ))

;;   (add-hook 'dart-mode-hook (lambda ()
;;                             (lsp-bridge-mode +1))))
                            ;; (php-enable-default-coding-style +1)
                            ;; (flymake-phpstan-turn-on +1)
                            ;; (subword-mode 1)
                            ;; (make-local-variable 'eldoc-documentation-function)
                            ;; (setq eldoc-documentation-function
                            ;;   'phpactor-hover)
                            ;;  (c-toggle-auto-newline 1))))

;; (add-hook 'dart-mode-hook 'lsp)

(with-eval-after-load 'dart-mode
  (defun goblin-dart-mode-defaults ()

    (setq dap-launch-configuration-providers  '(dap-debug-template-configurations-provider))

    ;; Add to default dart-mode key bindings
    (lsp-dart-define-key "s o" #'lsp-dart-show-outline)
    (lsp-dart-define-key "s f" #'lsp-dart-show-flutter-outline)）
    ;; (dap-dart-setup))

  (setq goblin-dart-mode-hook 'goblin-dart-mode-defaults)

  (add-hook 'dart-mode-hook (lambda ()
                            (run-hooks 'goblin-dart-mode-hook)))))

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

;; (use-package lsp-dart
;;   :ensure t
;;   :hook (dart-mode . lsp)
;;   :init
;;   (dap-register-debug-template "Flutter :: Custom debug"
;;                                (list :flutterPlatform "x86_64"
;;                                      :program "lib/main_debug.dart"
;;                                      :args '("--flavor" "customer_a"))))

(use-package lsp-mode :ensure t)
(use-package lsp-dart 
  :ensure t 
  :hook (dart-mode . lsp))

;; Optional packages
(use-package projectile :ensure t) ;; project management
(use-package yasnippet
  :ensure t
  :config (yas-global-mode)) ;; snipets
(use-package lsp-ui :ensure t) ;; UI for LSP
(use-package company :ensure t) ;; Auto-complete

;; Optional Flutter packages
(use-package hover :ensure t) ;; run app from desktop without emulator


(provide 'goblin-flutter)

;;; goblin-flutter.el ends here
