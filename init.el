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

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

(defvar current-user
      (getenv
       (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Goblin Emacs is powering up... Be patient, Master %s!" current-user)

(when (version< emacs-version "28.1")
  (error "Goblin requires at least GNU Emacs 28.1"))

;; Always load newest byte code
(setq load-prefer-newer t)

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
;;(defvar goblin-tmp-dir (expand-file-name "tmp" goblin-dir)
;;  "This folder stores all the automatically generated temporary files.")
(defvar goblin-modules-file (expand-file-name "goblin-modules.el" goblin-dir)
  "This files contains a list of modules that will be loaded by Goblin.")

(unless (file-exists-p goblin-savefile-dir)
  (make-directory goblin-savefile-dir))
;;(unless (file-exists-p goblin-tmp-dir)
;;  (make-directory goblin-tmp-dir))
;;(unless (file-exists-p goblin-personal-dir)
;;  (make-directory goblin-personal-dir))

(defun goblin-add-subfolders-to-load-path (parent-dir)
 "Add all level PARENT-DIR subdirs to the `load-path'."
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
                (not (string-prefix-p "." f)))
       (add-to-list 'load-path name)
       (goblin-add-subfolders-to-load-path name)))))

;; add Goblin's directories to Emacs's `load-path'
(add-to-list 'load-path goblin-core-dir)
(add-to-list 'load-path goblin-modules-dir)
(add-to-list 'load-path goblin-vendor-dir)
(goblin-add-subfolders-to-load-path goblin-vendor-dir)
;;(add-to-list 'load-path goblin-extensions-dir)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;;(goblin-add-subfolders-to-load-path goblin-extensions-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(setq inhibit-startup-message t) ; Don't show the splash screen
(setq visible-bell t)            ; Flash when the bell rings

;; preload the personal settings from `goblin-preload-dir'
;;(when (file-exists-p goblin-preload-dir)
;;  (message "Loading preload configuration files in %s..." goblin-preload-dir)
;;  (mapc 'load (directory-files goblin-preload-dir 't "^[^#].*el$")))

(message "Loading Goblin's core...")

;; the core stuff
(require 'goblin-packages)
(require 'goblin-custom)
(require 'goblin-ui)
(require 'goblin-core)
(require 'goblin-editor)
(require 'goblin-lang)
(require 'goblin-global-keybindings)

;; OSX specific settings
(when (eq system-type 'darwin)
  (require 'goblin-osx))

(message "Loading Goblin's modules...")

;; the modules
(when (file-exists-p goblin-modules-file)
  (load goblin-modules-file))


;; (if (file-exists-p prelude-modules-file)
;;     (load prelude-modules-file)
;;   (message "[Prelude] Missing personal modules file %s" prelude-modules-file)
;;   (message "[Prelude] Falling back to the bundled example file sample/prelude-modules.el")
;;   (message "[Prelude] You should copy this file to your personal configuration folder and tweak it to your liking")
;;   (load (expand-file-name "sample/prelude-modules.el" prelude-dir)))

;; config changes made through the customize UI will be store here
;; (setq custom-file (expand-file-name "custom.el" goblin-personal-dir))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p goblin-personal-dir)
  (message "Loading personal configuration files in %s..." goblin-personal-dir)
  (mapc 'load (directory-files goblin-personal-dir 't "^[^#].*el$")))

(message "Goblin is ready to do thy bidding, Master %s!" current-user)

;(goblin-eval-after-init
 ;; greet the use with some useful tip
; (run-at-time 5 nil 'goblin-tip-of-the-day))


;; Use UTF-8 for all character encoding.
;; (set-language-environment 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (set-locale-environment "en.UTF-8")
;; (prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ENCODING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C-h C RET
;; M-x describe-current-coding-system
;; url https://stackoverflow.com/questions/20723229/how-to-reset-emacs-to-save-files-in-utf-8-unix-character-encoding
(add-to-list 'file-coding-system-alist '("\\.php" . utf-8-unix) )
(add-to-list 'file-coding-system-alist '("\\.js" . utf-8-unix) )
(add-to-list 'file-coding-system-alist '("\\.html" . utf-8-unix) )
;; (add-to-list 'file-coding-system-alist '("\\.tex" . utf-8-unix) )
;; (add-to-list 'file-coding-system-alist '("\\.txt" . utf-8-unix) )
;; (add-to-list 'file-coding-system-alist '("\\.el" . utf-8-unix) )
;; (add-to-list 'file-coding-system-alist '("\\.scratch" . utf-8-unix) )
;; (add-to-list 'file-coding-system-alist '("user_prefs" . utf-8-unix) )

;; (add-to-list 'process-coding-system-alist '("\\.txt" . utf-8-unix) )

;; (add-to-list 'network-coding-system-alist '("\\.txt" . utf-8-unix) )

(add-to-list 'network-coding-system-alist '("\\.php" . utf-8-unix) )
(add-to-list 'network-coding-system-alist '("\\.js" . utf-8-unix) )
(add-to-list 'network-coding-system-alist '("\\.html" . utf-8-unix) )

(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; mnemonic for utf-8 is "U", which is defined in the mule.el
(setq eol-mnemonic-dos ":CRLF")
(setq eol-mnemonic-mac ":CR")
(setq eol-mnemonic-undecided ":?")
(setq eol-mnemonic-unix ":LF")

(defalias 'read-buffer-file-coding-system 'lawlist-read-buffer-file-coding-system)
(defun lawlist-read-buffer-file-coding-system ()
  (let* ((bcss (find-coding-systems-region (point-min) (point-max)))
         (css-table
          (unless (equal bcss '(undecided))
            (append '("dos" "unix" "mac")
                    (delq nil (mapcar (lambda (cs)
                                        (if (memq (coding-system-base cs) bcss)
                                            (symbol-name cs)))
                                      coding-system-list)))))
         (combined-table
          (if css-table
              (completion-table-in-turn css-table coding-system-alist)
            coding-system-alist))
         (auto-cs
          (unless find-file-literally
            (save-excursion
              (save-restriction
                (widen)
                (goto-char (point-min))
                (funcall set-auto-coding-function
                         (or buffer-file-name "") (buffer-size))))))
         (preferred 'utf-8-unix)
         (default 'utf-8-unix)
         (completion-ignore-case t)
         (completion-pcm--delim-wild-regex ; Let "u8" complete to "utf-8".
          (concat completion-pcm--delim-wild-regex
                  "\\|\\([[:alpha:]]\\)[[:digit:]]"))
         (cs (completing-read
              (format "Coding system for saving file (default %s): " default)
              combined-table
              nil t nil 'coding-system-history
              (if default (symbol-name default)))))
    (unless (zerop (length cs)) (intern cs))))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-clients-php-server-command "/usr/local/bin/phpactor")
 '(lsp-phpactor-path "/Users/gaoyuan/.emacs.d/extensions/lsp-server/phpactor")
 '(package-selected-packages
   '(lsp-mode flatbuffers-mode json-mode neotree yasnippet-snippets volatile-highlights  vertico valign use-package undo-tree sr-speedbar solarized-theme smartparens smart-jump posframe phpstan orderless operate-on-number markdown-mode hl-todo highlight-parentheses doom-themes doom-modeline diminish diff-hl dart-mode consult company-phpactor browse-kill-ring avy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
