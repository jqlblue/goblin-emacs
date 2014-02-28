;;; init.el

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
(defvar current-user
      (getenv
       (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Goblin Emacs is powering up... Be patient, Master %s!" current-user)

(when (version< emacs-version "24.1")
  (error "Goblin requires at least GNU Emacs 24.1"))

(defvar goblin-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Goblin distribution.")
(defvar goblin-core-dir (expand-file-name "core" goblin-dir)
  "The home of Goblin's core functionality.")
(defvar goblin-modules-dir (expand-file-name  "modules" goblin-dir)
  "This directory houses all of the built-in Goblin modules.")
(defvar goblin-extensions-dir (expand-file-name  "extensions" goblin-dir)
  "This directory houses packages that not under elpa directory.")
(defvar goblin-personal-dir (expand-file-name "personal" goblin-extensions-dir)
  "This directory is for your personal configuration.
Users of Emacs Goblin are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by Goblin.")
(defvar goblin-preload-dir (expand-file-name "preload" goblin-extensions-dir)
  "This directory is for your personal configuration, that you want loaded before Goblin.")
(defvar goblin-vendor-dir (expand-file-name "vendor" goblin-extensions-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar goblin-savefile-dir (expand-file-name "savefile" goblin-dir)
  "This folder stores all the automatically generated save/history-files.")
(defvar goblin-tmp-dir (expand-file-name "tmp" goblin-dir)
  "This folder stores all the automatically generated temporary files.")
(defvar goblin-modules-file (expand-file-name "goblin-modules.el" goblin-dir)
  "This files contains a list of modules that will be loaded by Goblin.")

(unless (file-exists-p goblin-savefile-dir)
  (make-directory goblin-savefile-dir))
(unless (file-exists-p goblin-tmp-dir)
  (make-directory goblin-tmp-dir))

(defun goblin-add-subfolders-to-load-path (parent-dir)
 "Add all level PARENT-DIR subdirs to the `load-path'."
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
                (not (equal f ".."))
                (not (equal f ".")))
       (add-to-list 'load-path name)
       (goblin-add-subfolders-to-load-path name)))))

;; add Goblin's directories to Emacs's `load-path'
(add-to-list 'load-path goblin-core-dir)
(add-to-list 'load-path goblin-modules-dir)
(add-to-list 'load-path goblin-extensions-dir)
(goblin-add-subfolders-to-load-path goblin-extensions-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; preload the personal settings from `goblin-preload-dir'
(when (file-exists-p goblin-preload-dir)
  (message "Loading preload configuration files in %s..." goblin-preload-dir)
  (mapc 'load (directory-files goblin-preload-dir 't "^[^#].*el$")))

(message "Loading Goblin's core...")

;; the core stuff
(require 'goblin-packages)
(require 'goblin-ui)
(require 'goblin-core)
(require 'goblin-editor)
(require 'goblin-global-keybindings)

;; OSX specific settings
(when (eq system-type 'darwin)
  (require 'goblin-osx))

(message "Loading Goblin's modules...")

;; the modules
(when (file-exists-p goblin-modules-file)
  (load goblin-modules-file))

;; config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" goblin-personal-dir))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p goblin-personal-dir)
  (message "Loading personal configuration files in %s..." goblin-personal-dir)
  (mapc 'load (directory-files goblin-personal-dir 't "^[^#].*el$")))

(message "Goblin is ready to do thy bidding, Master %s!" current-user)

(goblin-eval-after-init
 ;; greet the use with some useful tip
 (run-at-time 5 nil 'goblin-tip-of-the-day))

;;; init.el ends here
