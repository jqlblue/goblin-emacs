;;; goblin-ui.el

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

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(menu-bar-mode -1)

;; hidden scroll bar
(set-scroll-bar-mode nil)
(mouse-wheel-mode 1)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

(setq inhibit-startup-message t) ; Don't show the splash screen
(setq visible-bell t)            ; Flash when the bell rings

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(global-linum-mode t)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; show line numbers at the beginning of each line
(unless goblin-minimalistic-ui
  ;; there's a built-in linum-mode, but we're using
  ;; display-line-numbers-mode or nlinum-mode,
  ;; as it's supposedly faster
  (if (fboundp 'global-display-line-numbers-mode)
      (global-display-line-numbers-mode)
    (global-nlinum-mode t)))

;; make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " Goblin - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; use solarized as the default theme
(load-theme 'solarized-dark t)
;; (load-theme 'material t)
;(load-theme 'solarized-light t)

;; font
;; set font
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
    nil )
  )
 ((string-equal system-type "darwin")   ; Mac OS X
  (progn
    (add-to-list 'default-frame-alist '(font . "Courier New-15"))
    (set-fontset-font "fontset-default"
	 'gb18030 '("Microsoft YaHei" . "unicode-bmp"))
    )
  )
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (add-to-list 'default-frame-alist '(font . "Courier\ New-13")) )
    (set-fontset-font "fontset-default"
	 'gb18030 '("Microsoft YaHei" . "unicode-bmp"))
  )
 )

(provide 'goblin-ui)
;;; goblin-ui.el ends here
