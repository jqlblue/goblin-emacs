;;; goblin-packages.el

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
;; (require 'cl)
;; (require 'package)

;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/") t)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; ;; set package-user-dir to be relative to Goblin install path
;; (setq package-user-dir (expand-file-name "elpa" goblin-dir))
;; (package-initialize)

;; (require 'cl-lib)
;; (require 'package)

;; ;;;; Package setup and additional utility functions

;; ;; accessing a package repo over https on Windows is a no go, so we
;; ;; fallback to http there
;; (if (eq system-type 'windows-nt)
;;     (add-to-list 'package-archives
;;                  '("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/" ) t)
;;     (add-to-list 'package-archives
;;                  '("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/" ) t)
;;     (add-to-list 'package-archives
;;                  '("org-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/" ) t))
 ;   (add-to-list 'package-archives
               ;'("melpa" . "https://melpa.org/packages/") t))

;; load the pinned packages
;; (let ((goblin-pinned-packages-file (expand-file-name "goblin-pinned-packages.el" goblin-dir)))
;;   (if (file-exists-p goblin-pinned-packages-file)
;;       (load goblin-pinned-packages-file)))

;; set package-user-dir to be relative to goblin install path

(require 'package)

;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
;; (setq package-user-dir (expand-file-name *emacs-package-user-dir* user-emacs-directory))

;;; Standard package repositories
;; Internet repositories for new packages.
(setq package-archives '(("gnu"    . "http://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa"  . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")))

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; ================ Setup use-package start ===========================

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.2")
(setq package-signature nil)

(setq package-user-dir (expand-file-name "elpa" goblin-dir))
;; (package-initialize)

;; install & enable use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(defvar goblin-packages
  '(sr-speedbar
    diminish
    browse-kill-ring
    ;; projectile
    avy
    undo-tree
    hl-todo
    diff-hl
    operate-on-number
    smartparens
    volatile-highlights
    solarized-theme
    highlight-parentheses
    posframe
    yasnippet
    yasnippet-snippets
    markdown-mode
    ;; eglot
    ;; auto-complete
    helm
    helm-projectile
    php-mode
    phpactor
    company-phpactor
    smart-jump
    doom-modeline
    valign
    doom-themes
    neotree
    ;; flutter
    dart-mode
    lsp-mode
    lsp-dart
    lsp-treemacs
    flycheck
    company
    lsp-ui
    hover
    all-the-icons
    ;; ivy
    ;; phpstan
    ;; web-mode
    ;; python-mode
    ;; nginx-mode
    ;; ac-slime
    ;; slime
    ;; org
    ;; rust-mode
    ;; racer
    ;; flycheck
    ;; flycheck-rust
    ;; cargo
    ;; company
    ;; company-racer
    ;; company-php
    ;; company-web
    ;; company-restclient
    ;; elpy
    ;; company-web
    ;; ac-php
    ;; rustfmt
    ;; let-alist
    )
  "A list of packages to ensure are installed at launch.")

(defun goblin-packages-installed-p ()
  "Check if all packages in `goblin-packages' are installed."
  (cl-every #'package-installed-p goblin-packages))

(defun goblin-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package goblin-packages)
    (add-to-list 'goblin-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun goblin-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'goblin-require-package packages))

;(define-obsolete-function-alias 'goblin-ensure-module-deps 'goblin-require-packages)

(defun goblin-install-packages ()
  "Install all packages listed in `goblin-packages'."
  (unless (goblin-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs Goblin is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (goblin-require-packages goblin-packages)))

;; run package installation
(goblin-install-packages)

(defun goblin-list-foreign-packages ()
  "Browse third-party packages not bundled with Goblin.

Behaves similarly to `package-list-packages', but shows only the packages that
are installed and are not in `goblin-packages'.  Useful for
removing unwanted packages."
  (interactive)
  (package-show-package-list
   (cl-set-difference package-activated-list goblin-packages)))

(defmacro goblin-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present.  The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

(defvar goblin-auto-install-alist
  '(("\\.adoc\\'" adoc-mode adoc-mode)
    ("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.cljc\\'" clojure-mode clojurec-mode)
    ("\\.cljs\\'" clojure-mode clojurescript-mode)
    ("\\.edn\\'" clojure-mode clojure-mode)
    ("\\.cmake\\'" cmake-mode cmake-mode)
    ("CMakeLists\\.txt\\'" cmake-mode cmake-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("Cask" cask-mode cask-mode)
    ("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.elm\\'" elm-mode elm-mode)
    ("\\.ex\\'" elixir-mode elixir-mode)
    ("\\.exs\\'" elixir-mode elixir-mode)
    ("\\.elixir\\'" elixir-mode elixir-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.graphql\\'" graphql-mode graphql-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.jl\\'" julia-mode julia-mode)
    ("\\.json\\'" json-mode json-mode)
    ("\\.kt\\'" kotlin-mode kotlin-mode)
    ("\\.kv\\'" kivy-mode kivy-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.markdown\\'" markdown-mode markdown-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.ml\\'" tuareg tuareg-mode)
    ("\\.pp\\'" puppet-mode puppet-mode)
    ("\\.php\\'" php-mode php-mode)
    ("\\.proto\\'" protobuf-mode protobuf-mode)
    ("\\.pyd\\'" cython-mode cython-mode)
    ("\\.pyi\\'" cython-mode cython-mode)
    ("\\.pyx\\'" cython-mode cython-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.rkt\\'" racket-mode racket-mode)
    ("\\.rs\\'" rust-mode rust-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.styl\\'" stylus-mode stylus-mode)
    ("\\.swift\\'" swift-mode swift-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.thrift\\'" thrift thrift-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)
    ("\\.yaml\\'" yaml-mode yaml-mode)
    ("Dockerfile\\'" dockerfile-mode dockerfile-mode)))

;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))


;; same with adoc-mode
(when (package-installed-p 'adoc-mode)
  (add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
  (add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . adoc-mode)))

(when (package-installed-p 'pkgbuild-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))

;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (goblin-auto-install extension package mode))))
 goblin-auto-install-alist)

(provide 'goblin-packages)
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; goblin-packages.el ends here
