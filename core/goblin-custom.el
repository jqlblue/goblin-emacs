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

;; customize
(defgroup goblin nil
  "Emacs goblin configuration."
  :prefix "goblin-"
  :group 'convenience)

(defcustom goblin-minimalistic-ui nil
  "Controls whether to display the menu-bar and line numbers.
Note that the toolbar is always hidden regardless of this setting."
  :type 'boolean
  :group 'goblin
  :package-version '(goblin . "1.1"))

(defcustom goblin-super-keybindings t
  "Controls whether to use the Super key in keybindings.
They can be problematic in some operating systems (e.g. Windows)
or desktop environments that make heavy use of them."
  :type 'boolean
  :group 'goblin
  :package-version '(goblin . "1.1"))

(defcustom goblin-auto-save t
  "Non-nil values enable goblin's auto save."
  :type 'boolean
  :group 'goblin)

(defcustom goblin-guru t
  "Non-nil values enable `guru-mode'."
  :type 'boolean
  :group 'goblin)

(defcustom goblin-whitespace t
  "Non-nil values enable goblin's whitespace visualization."
  :type 'boolean
  :group 'goblin)

(defcustom goblin-clean-whitespace-on-save t
  "Cleanup whitespace from file before it's saved.
Will only occur if `goblin-whitespace' is also enabled."
  :type 'boolean
  :group 'goblin)

(defcustom goblin-flyspell t
  "Non-nil values enable Prelude's flyspell support."
  :type 'boolean
  :group 'goblin)

(defcustom goblin-user-init-file (expand-file-name "personal/"
                                                    user-emacs-directory)
  "Path to your personal customization file.
goblin recommends you only put personal customizations in the
personal folder.  This variable allows you to specify a specific
folder as the one that should be visited when running
`crux-find-user-init-file'.  This can be easily set to the desired buffer
in Lisp by putting `(setq goblin-user-init-file load-file-name)'
in the desired elisp file."
  :type 'string
  :group 'goblin)

(defcustom goblin-indent-sensitive-modes
  '(conf-mode coffee-mode haml-mode python-mode slim-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list
  :group 'goblin)

(defcustom goblin-format-on-save t
  "Run mode specific format on file before it's saved.
Currently only applies to tide-mode."
  :type 'boolean
  :group 'goblin)

(defcustom goblin-yank-indent-modes '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here."
  :type 'list
  :group 'prelgoblinude)

(defcustom goblin-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  :type 'number
  :group 'goblin)

(defcustom goblin-theme 'zenburn
  "The default color theme, change this in your /personal/preload config."
  :type 'symbol
  :group 'goblin)

(provide 'goblin-custom)
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; goblin-custom.el ends here
