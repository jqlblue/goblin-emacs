;;; prelude-flymake.el

;;; Copyright © 2014 jqlblue <gaoyuan.blue@gmail.com>

;;; URL: https://github.com/jqlblue/goblin-emacs

;;; Commentary:

;; The package is fork from http://batsov.com/prelude, enhance in web development.

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

;; Set up flymake-mode
;; Static analysis can be slow, so only run flymake if I've not been typing for 5 seconds.
;; It will still run on save or hitting return.
(setq flymake-no-changes-timeout 5)
;; Disable in-place checking, and tell it to use ~/.emacs.d/tmp/ for the temp files.

(setq temporary-file-directory "~/.emacs.d/tmp/")
(setq flymake-run-in-place nil)

;; Only need these two if you plan to debug Flymake.
(setq flymake-log-file-name (concat temporary-file-directory "flymake.log"))
(setq flymake-log-level -1)
;; Tune how many checks can run in parallel, default of 4 should be fine.
;(setq flymake-max-parallel-syntax-checks 1)
(setq flymake-number-of-errors-to-display 4)
(setq flymake-max-parallel-syntax-checks 1)

;; pyflakes
;; adapted from http://plope.com/Members/chrism/flymake-mode
;; and http://www.emacswiki.org/emacs/FlymakeRuby

(defun flymake-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking FILE-NAME.
   This is a replacement for `flymake-create-temp-inplace'. The difference is that it gives a file name in
   `temporary-file-directory' instead of the same directory as FILE-NAME.
   For the use of PREFIX see that function.
   Note that not making the temporary file in another directory \(like here) will not if the file you are checking depends on
   relative paths to other files \(for the type of checks flymake makes)."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "_" prefix))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext))
         )
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s"
                 file-name temp-name)
    temp-name))

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-intemp))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (flymake-log 3 "flymake-pyflakes-init: dir=%s %s %s %s %s"
                   buffer-file-name (file-name-directory temp-file) temp-file local-file (list local-file))
      (list "~/.emacs.d/utils/pycheckers" (list temp-file)
            (file-name-directory temp-file))))

  (defun flymake-php-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-intemp))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (flymake-log 3 "flymake-pyflakes-init: dir=%s %s %s %s %s"
                   buffer-file-name (file-name-directory temp-file) temp-file local-file (list local-file))
      (list "~/.emacs.d/utils/flymake_phpcs" (list temp-file)
            (file-name-directory temp-file))))


  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init))

  (delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks)

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.php\\'"
                 flymake-php-init))

  (add-to-list 'flymake-err-line-patterns
               '("\\(Parse\\|Fatal\\) error: \\(.*?\\) in \\(.*?\\) on line \\([0-9]+\\)" 3 4 nil 2)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

(add-hook 'python-mode-hook 'flymake-mode)
(add-hook 'php-mode-hook 'flymake-mode)

(defun my-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
   (let ((help (get-char-property (point) 'help-echo)))
    (if help (message "%s" help)))))

(add-hook 'post-command-hook 'my-flymake-show-help)

(provide 'goblin-flymake)

;;; goblin-flymake.el ends here
