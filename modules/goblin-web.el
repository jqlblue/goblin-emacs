;;; goblin-web.el

;;; Copyright © 2016 jqlblue <gaoyuan.blue@gmail.com>

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


;;; Code:
(require 'web-mode)
(require 'company)
(require 'company-web-html)
(require 'company-web-jade)

;; Baisc
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.*tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.*tml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("/\\(views\\|html\\|templates\\)/.*\\.php\\'" . web-mode))

;; Engines
(setq web-mode-engines-alist
      '(
        ("php" . "\\.phtml\\'")
        ("blade" . "\\.blade\\'")
        ("django" . "\\.[sd]tpl\\'")
        ("django" . "\\.[sd]tml\\'")
        ))

;; Settings
(setq-default web-mode-enable-auto-pairing t
              web-mode-enable-auto-opening t
              web-mode-enable-auto-indentation t
              web-mode-enable-block-face t
              web-mode-enable-part-face t
              web-mode-enable-comment-keywords t
              web-mode-enable-css-colorization t
              web-mode-enable-current-element-highlight t
              web-mode-enable-heredoc-fontification t
              web-mode-enable-engine-detection t

              web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2

              web-mode-style-padding 2
              web-mode-script-padding 2
              web-mode-block-padding 0
              web-mode-comment-style 2)

;; Custom web-mode colors
(custom-set-faces
 '(web-mode-html-tag-face
   ((t (:foreground "#729fcf"))))
 '(web-mode-html-tag-bracket-face
   ((t (:foreground "#FFE84B"))))
 '(web-mode-current-element-highlight-face
   ((t (:foreground "#FF8A4B"))))
 '(web-mode-current-element-highlight-face
   ((t (:background "#000000"
                    :foreground "#FF8A4B")))))


;; Company web backend
(define-key web-mode-map (kbd "C-'") 'company-web-html)
(add-to-list 'company-backends 'company-restclient)

;; Hooks
(add-hook 'web-mode-hook
          '(lambda ()
             ;; Company-mode
             (set (make-local-variable 'company-backends) (append company-backends '((company-web-html company-web-jade company-yasnippet))))
             ;; Auto indent
             (local-set-key (kbd "RET") 'newline-and-indent)

             ;; Disabled smartparens in web-mode
             (setq smartparens-mode nil)

             ;; Enable todo
             (todo-highlight)))


(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language
                    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
                   (yas-activate-extra-mode 'php-mode)
                 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil))
               )))


;; Extend Autocomplete web-mode
;; not work default-ac
(setq web-mode-ac-sources-alist
      '(("php" . (ac-source-php-auto-yasnippets))
        ("html" . (ac-source-emmet-html-snippets))
        ("css" . (ac-source-emmet-css-snippets))
        ))


(provide 'goblin-web)

;;; goblin-web.el ends here
