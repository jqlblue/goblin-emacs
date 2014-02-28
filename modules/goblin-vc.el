;;; goblin-vc.el

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


;; `C-c C-s'
;;      Split the hunk at point (`diff-split-hunk').  This is for manually
;;      editing patches, and only works with the "unified diff format"
;;      produced by the `-u' or `--unified' options to the `diff' program.
;;      If you need to split a hunk in the "context diff format" produced
;;      by the `-c' or `--context' options to `diff', first convert the
;;      buffer to the unified diff format with `C-c C-u'.

;; `C-c C-d'
;;      Convert the entire buffer to the "context diff format"
;;      (`diff-unified->context').  With a prefix argument, convert only
;;      the text within the region.
;; `C-c C-u'
;;      Convert the entire buffer to unified diff format
;;      (`diff-context->unified').  With a prefix argument, convert
;;      unified format to context format.  When the mark is active, convert
;;      only the text within the region.

;; `C-c C-w' 重新生成diff文件,此次忽略空格
;;      Refine the current hunk so that it disregards changes in whitespace
;;      (`diff-refine-hunk').

;; `C-x 4 A'
;;      Generate a ChangeLog entry, like `C-x 4 a' does (*note Change
;;      Log::), for each one of the hunks
;;      (`diff-add-change-log-entries-other-window').  This creates a
;;      skeleton of the log of changes that you can later fill with the
;;      actual descriptions of the changes.  `C-x 4 a' itself in Diff mode
;;      operates on behalf of the current hunk's file, but gets the
;;      function name from the patch itself.  This is useful for making
;;      log entries for functions that are deleted by the patch.
;; `M-x diff-show-trailing-whitespaces RET'
;;      Highlight trailing whitespace characters, except for those used by
;;      the patch syntax (*note Useless Whitespace::).
(eval-after-load 'diff-mode
  ;;为*Diff* mode 设置高度face
  '(require 'diff-mode)
  )
;; Ediff
(setq-default ediff-window-setup-function (quote ediff-setup-windows-plain))
;;Ediff常用的命令
;; `ediff-files' `ediff-current-file' `ediff-directories'
;; `edir-revisions' `edir-merge-revisions' `ediff-show-registry'
;; `edir-merge-revisions-with-ancestor'
;; `ediff-revision' `ediff-patch-file'
;; `ediff-merge-files' `ediff-merge-files-with-ancestor'
;; `ediff-merge-directories' `ediff-merge-revisions'

;; `v'            scroll A and B
;; `V'            scroll the buffers down
;; `wd'           write diff to a file
;; `wb' `wc'`wa'  Saves buffer A, if it was modified.
;; `a'  `b' `c'   把A中相前的的difference region copy到B中相应相应位置,`rb' 可以恢复B到原状
;; `ab'           同`a'不过是在3个文件对比的时候
;; `p' `n'         选中上一个(下一个)difference region
;; `j' `-j' `Nj';; `n' 与'p'是相对跳转,此为绝对跳转,N是数字,表示跳到第N个difference region,
;; -j表示跳到跳后
;; `ga'              将A中跳(point)最近的difference region选中
;; `!'               Recomputes the difference regions ,防止因为修改导致高亮出错
;;  `m'             调整窗口在大小尽量大(toggle
;;   `|'            toggle 是水平还是垂直摆放两个window
;;   'r'            重置merge中的内容到未修改前(只在merge会话中有用)
;;   `ra' `rb' `rc' ::: `a' `b' `c' undo操作 (只在compare会话中有用)
;;   `##'          跳过只因 空格TAB不同引起的difference
;;   `#c'          跳过只因 大小写不同引起的difference
;;`#h' `#f'       处理因为大量相同的变量替换引起的difference
;;`A' `B' `C'    toggle Read-Only in A(B,C)
;; ~             交换A B窗口
;; i            显示当前进行的Ediff Session的信息,如在对哪两个文件进行对比等
;; D             显示diff命令的输出结果,生成diff文件
;; R             显示所有可用的Ediff Session,基本就是历史浏览`ediff-show-registry'
;; z            暂时挂起,(关闭相关窗口,) 可以`R' 进行恢复会话
;; q               quit.
;;`/' Displays the ancestor file during merges.
;; s            收缩merge窗口(toggle) ,`4s' 则增大4行
;; +            合并A B 的当前 difference region
;; =           启用一个新的子会话对当前difference region进行对比

;;git mergetool 使用ediff ,前提可以正常使用emacsclient ,并且Emacs已经启动。
;; ~/.gitconfig
;; [mergetool "ediff"]
;; cmd = emacsclient --eval \"(git-mergetool-emacsclient-ediff \\\"$LOCAL\\\" \\\"$REMOTE\\\" \\\"$BASE\\\" \\\"$MERGED\\\")\"
;; trustExitCode = false
;; [mergetool]
;; prompt = false
;; [merge]
;; tool = ediff
;;
;; Setup for ediff.
;;
;;(require 'ediff)

(defvar ediff-after-quit-hooks nil
  "* Hooks to run after ediff or emerge is quit.")

(defadvice ediff-quit (after edit-after-quit-hooks activate)
  (run-hooks 'ediff-after-quit-hooks))

(setq git-mergetool-emacsclient-ediff-active nil)

(defun local-ediff-frame-maximize ()
  (when (boundp 'display-usable-bounds)
    (let* ((bounds (display-usable-bounds))
           (x (nth 0 bounds))
           (y (nth 1 bounds))
           (width (/ (nth 2 bounds) (frame-char-width)))
           (height (/ (nth 3 bounds) (frame-char-height))))
      (set-frame-width (selected-frame) width)
      (set-frame-height (selected-frame) height)
      (set-frame-position (selected-frame) x y))  )
  )
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(defun local-ediff-before-setup-hook ()
  (setq local-ediff-saved-frame-configuration (current-frame-configuration))
  (setq local-ediff-saved-window-configuration (current-window-configuration))
  (local-ediff-frame-maximize)
  (if git-mergetool-emacsclient-ediff-active
      (raise-frame)))

(defun local-ediff-quit-hook ()
  (set-frame-configuration local-ediff-saved-frame-configuration)
  (set-window-configuration local-ediff-saved-window-configuration))

(defun local-ediff-suspend-hook ()
  (set-frame-configuration local-ediff-saved-frame-configuration)
  (set-window-configuration local-ediff-saved-window-configuration))

(add-hook 'ediff-before-setup-hook 'local-ediff-before-setup-hook)
(add-hook 'ediff-quit-hook 'local-ediff-quit-hook 'append)
(add-hook 'ediff-suspend-hook 'local-ediff-suspend-hook 'append)

;; Useful for ediff merge from emacsclient.
(defun git-mergetool-emacsclient-ediff (local remote base merged)
  (setq git-mergetool-emacsclient-ediff-active t)
  (if (file-readable-p base)
      (ediff-merge-files-with-ancestor local remote base nil merged)
    (ediff-merge-files local remote nil merged))
  (recursive-edit))

(defun git-mergetool-emacsclient-ediff-after-quit-hook ()
  (exit-recursive-edit))

(add-hook 'ediff-after-quit-hooks 'git-mergetool-emacsclient-ediff-after-quit-hook 'append)



(defun svn-settings ()
  "Settings for `svn'."
  (setq svn-status-hide-unmodified t)
  (setq svn-status-hide-unknown t)

  (svn-status-update-state-mark-tooltip "svn")

  (defun svn-status-toggle-hide-unmodified-unknown ()
    "隐藏/不隐藏没有修改的文件和没加入版本控制的文件"
    (interactive)
    (let ((unmodified svn-status-hide-unmodified) (unknown svn-status-hide-unknown))
      (unless (or (and unmodified unknown) (not (or unmodified unknown)))
        (setq svn-status-hide-unmodified nil)
        (setq svn-status-hide-unknown nil))
      (svn-status-toggle-hide-unmodified)
      (svn-status-toggle-hide-unknown)))

  (defun svn-status-commit-all ()
    "Commit all files."
    (interactive)
    (call-interactively 'svn-status-mark-changed)
    (call-interactively 'svn-status-commit))

  (defsubst svn-status-interprete-state-mode-color (stat)
    "Interpret vc-svn-state symbol to mode line color"
    (if window-system
        (case stat
          ('up-to-date "GreenYellow")
          ('edited     "tomato")
          ('unknown    "gray")
          ('added      "blue")
          ('deleted    "magenta")
          ('unmerged   "purple")
          ('conflict   "red")
          (t           "black"))
      (case stat
        ('up-to-date "-")
        ('edited     "*")
        ('unknown    "u")
        ('added      "A")
        ('deleted    "D")
        ('unmerged   "M")
        ('conflict   "C")
        (t           "U"))))

  (defun svn-status-state-mark-modeline-dot (color)
    (if window-system
        (propertize "    "
                    'help-echo 'svn-status-state-mark-tooltip
                    'display
                    `(image :type xpm
                            :data ,(format "/* XPM */
static char * data[] = {
\"18 13 3 1\",
\"  c None\",
\"+ c #000000\",
\". c %s\",
\"                  \",
\"       +++++      \",
\"      +.....+     \",
\"     +.......+    \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"     +.......+    \",
\"      +.....+     \",
\"       +++++      \",
\"                  \"};"
                                           color)
                            :ascent center))
      color)))

(eval-after-load "psvn"
  `(svn-settings))



(provide 'goblin-vc)

;;; goblin-vc.el ends here
