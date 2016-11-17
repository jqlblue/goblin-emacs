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

(setenv "PATH" (concat "/usr/local/bin" ":"
                       "~/.cargo/bin" ":" 
                       (getenv "PATH")))
(setq exec-path (append exec-path '("/usr/local/bin" "~/.cargo/bin")))

;; Reduce the time after which the company auto completion popup opens
(setq company-idle-delay 0.1)

;; Reduce the number of characters before company kicks in
(setq company-minimum-prefix-length 2)


;; Load rust-mode when you open `.rs` files
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; (require 'company-racer)
;; (with-eval-after-load 'company
;;   (add-to-list 'company-backends 'company-racer))

(unless (getenv "RUST_SRC_PATH")
  (setenv "RUST_SRC_PATH" "~/.rust/rustc-1.13.0/src"))

(add-hook 'rust-mode-hook  #'racer-mode)
(add-hook 'rust-mode-hook  #'flycheck-mode)
(add-hook 'rust-mode-hook  #'cargo-minor-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'rust-mode-hook  #'rustfmt-enable-on-save)

	;(add-hook 'after-init-hook #'global-flycheck-mode)
;; Setting up configurations when you load rust-mode

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(defun indent-buffer ()
  "Indent current buffer according to major mode."
  (interactive)
  (indent-region (point-min) (point-max)))

(add-hook 'rust-mode-hook
          '(lambda ()
             (define-key rust-mode-map (kbd "C-c C-f") #'rustfmt-format-buffer)

             ;; Set path to racer binary
             (setq racer-cmd (concat (getenv "HOME") "/.cargo/bin/racer"))
             (setq rustfmt-bin (concat (getenv "HOME") "/.cargo/bin/rustfmt"))

             ;; Set path to rust src directory
             (setq racer-rust-src-path (concat (getenv "HOME") "/.rust/rustc-1.13.0/src"))
             
             ;; Use flycheck-rust in rust-mode
             (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
             
             ;; Use company-racer in rust mode
             (set (make-local-variable 'company-backends) '(company-racer))
             
             ;; Key binding to jump to method definition
             (local-set-key (kbd "M-.") #'racer-find-definition)
             
             ;; Key binding to auto complete and indent
             (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
             (electric-pair-mode 1)
             ))

(provide 'goblin-rust)

;;; goblin-rust.el ends here
