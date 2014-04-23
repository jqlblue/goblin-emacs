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
(require 'cl)
(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; set package-user-dir to be relative to Goblin install path
(setq package-user-dir (expand-file-name "elpa" goblin-dir))
(package-initialize)

(defvar goblin-packages
  '(ace-jump-mode
    ack-and-a-half
    diminish
    browse-kill-ring
    expand-region
    projectile
    undo-tree
    auto-complete
    color-theme-solarized
    epc
    jedi
    php-mode
    python-mode
    nginx-mode
    starter-kit
    ac-slime
    slime
    org)
  "A list of packages to ensure are installed at launch.")

(defun goblin-packages-installed-p ()
  "Check if all packages in `goblin-packages' are installed."
  (every #'package-installed-p goblin-packages))

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

(define-obsolete-function-alias 'goblin-ensure-module-deps 'goblin-require-packages)

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
   (set-difference package-activated-list goblin-packages)))

(defmacro goblin-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present.  The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

(defvar goblin-auto-install-alist
  '(("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.markdown\\'" markdown-mode markdown-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.ml\\'" tuareg tuareg-mode)
    ("\\.pp\\'" puppet-mode puppet-mode)
    ("\\.php\\'" php-mode php-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode2 scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)))

;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

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
