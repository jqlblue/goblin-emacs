;;; goblin-editor.el

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

;; stop creating those backup~ files 
(setq make-backup-files nil)

;; stop creating those #
; files
(setq auto-save-default nil) 

;; none auto-save
(setq auto-save-mode nil)

;; customize
(defgroup goblin nil
  "Emacs Goblin configuration."
  :prefix "goblin-"
  :group 'convenience)

(defcustom goblin-auto-save t
  "Non-nil values enable Goblin's auto save."
  :type 'boolean
  :group 'goblin)

(defcustom goblin-guru t
  "Non-nil values enable guru-mode."
  :type 'boolean
  :group 'goblin)

(defcustom goblin-whitespace t
  "Non-nil values enable Goblin's whitespace visualization."
  :type 'boolean
  :group 'goblin)

(defcustom goblin-clean-whitespace-on-save t
  "Cleanup whitespace from file before it's saved.
Will only occur if goblin-whitespace is also enabled."
  :type 'boolean
  :group 'goblin)

(defcustom goblin-flyspell t
  "Non-nil values enable Goblin's flyspell support."
  :type 'boolean
  :group 'goblin)

;; Death to the tabs!  However, tabs historically indent to the next
;; 8-character offset; specifying anything else will cause *mass*
;; confusion, as it will change the appearance of every existing file.
;; In some cases (python), even worse -- it will change the semantics
;; (meaning) of the program.
;;
;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 4)            ;; but maintain correct appearance

;; delete the selection with a keypress
(delete-selection-mode t)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)


(define-key prog-mode-map (kbd "M-(") (goblin-wrap-with "("))
;; FIXME: pick terminal friendly binding
;; (define-key prog-mode-map (kbd "M-[") (goblin-wrap-with "["))
(define-key prog-mode-map (kbd "M-\"") (goblin-wrap-with "\""))

;; disable annoying blink-matching-paren
(setq blink-matching-paren nil)

;; diminish keeps the modeline tidy
;; (require 'diminish)

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(global-hl-line-mode +1)

;; note - this should be after volatile-highlights is required
;; add the ability to copy and cut the current line, without marking it
(defadvice kill-ring-save (before smart-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-end-position)))))

(defadvice kill-region (before smart-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; tramp, for sudo access
(require 'tramp)
(setq tramp-default-user "gaoyuan")
(setq tramp-default-method "ssh")
(setq password-cache-expiry nil)

(set-default 'imenu-auto-rescan t)

(defun goblin-cleanup-maybe ()
  "Invoke `whitespace-cleanup' if `goblin-clean-whitespace-on-save' is not nil."
  (when goblin-clean-whitespace-on-save
    (whitespace-cleanup)))

(defun goblin-enable-whitespace ()
  "Enable `whitespace-mode' if `goblin-whitespace' is not nil."
  (when goblin-whitespace
    ;; keep the whitespace decent all the time (in this buffer)
    (add-hook 'before-save-hook 'goblin-cleanup-maybe nil t)
    (whitespace-mode +1)))

;(add-hook 'text-mode-hook 'goblin-enable-flyspell)
(add-hook 'text-mode-hook 'goblin-enable-whitespace)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

;(require 'expand-region)

;; projectile is a project management mode
;; (require 'projectile)
;; (setq projectile-cache-file (expand-file-name  "projectile.cache" goblin-savefile-dir))
;; (add-hook 'php-mode-hook #'(lambda () (projectile-mode)))
;; (add-hook 'python-mode-hook #'(lambda () (projectile-mode)))
;; (diminish 'projectile-mode "Prjl")

;; shorter aliases for ack-and-a-half commands
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

;; enable some really cool extensions like C-x C-j(dired-jump)
(require 'dired-x)

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers automatically
(require 'midnight)

;; smarter kill-ring navigation
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(global-set-key (kbd "s-y") 'browse-kill-ring)

;; automatically indenting yanked text if in programming-modes
(defvar yank-indent-modes
  '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here.")

(defvar yank-indent-blacklisted-modes
  '(python-mode slim-mode haml-mode)
  "Modes for which auto-indenting is suppressed.")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (not (member major-mode yank-indent-blacklisted-modes))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
      (let ((transient-mark-mode nil))
    (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of `yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
  (when (and (not (ad-get-arg 0))
             (not (member major-mode yank-indent-blacklisted-modes))
             (or (derived-mode-p 'prog-mode)
                 (member major-mode yank-indent-modes)))
    (let ((transient-mark-mode nil))
      (yank-advised-indent-function (region-beginning) (region-end)))))

;; abbrev config
(add-hook 'text-mode-hook 'abbrev-mode)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; whitespace-mode config
(require 'whitespace)
(setq whitespace-line-column 120) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

(require 'eshell)
(setq eshell-directory-name (expand-file-name "eshell" goblin-savefile-dir))

(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" goblin-savefile-dir))

;; enable Goblin's keybindings
;; (goblin-global-mode t)

;; sensible undo
(global-undo-tree-mode)
 (diminish 'undo-tree-mode)

;(require 'iedit)

;; enable winner-mode to manage window configurations
(winner-mode +1)

(defun visit-emacs-init ()
  "visit emacs init.el file"
  (interactive)
  (find-file (concat "~/.emacs.d/" "init.el")))
(global-set-key (kbd "C-x E") 'visit-emacs-init)

;;当指针到一个括号时，自动显示所匹配的另一个括号
(show-paren-mode 1) 
;;在文档最后自动插入空白一行，好像某些系统配置文件是需要这样的
(setq require-final-newline t) 
(setq track-eol t) 
(setq default-indicate-empty-lines 't)     ;显示文件末尾的空行
(setq x-select-enable-clipboard t) ;支持emacs和外部程序的粘贴 


;;;; 显示时间
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

;; sr speedbar

(setq speedbar-use-images nil)
(global-set-key (kbd "<f2>") (lambda()
                               (interactive)
                               (sr-speedbar-toggle)))


(provide 'goblin-editor)

;;; goblin-editor.el ends here
