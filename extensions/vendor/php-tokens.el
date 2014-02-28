(defvar php-tokens-mode-target-buffer nil)

(defun php-tokens-mode-find-occurrence ()
  (let ((pos (get-text-property (point) 'occur-target)))
    (when pos
      (unless (buffer-live-p (get-buffer php-tokens-mode-target-buffer))
        (error "Buffer for this occurrence was killed"))
      pos)))

(defun php-tokens-mode-goto-occurrence (&optional event)
  "Go to the occurrence the current line describes."
  (interactive (list last-nonmenu-event))
  (let ((pos
         (if (null event)
             ;; Actually `event-end' works correctly with a nil argument as
             ;; well, so we could dispense with this test, but let's not
             ;; rely on this undocumented behavior.
             (php-tokens-mode-find-occurrence)
           (with-current-buffer (window-buffer (posn-window (event-end event)))
             (save-excursion
               (goto-char (posn-point (event-end event)))
               (php-tokens-mode-find-occurrence)))))
        same-window-buffer-names
        same-window-regexps)
    (pop-to-buffer php-tokens-mode-target-buffer)
    (goto-line pos)))

(defun php-tokens-mode (buffer-name regexp)
  (pop-to-buffer buffer-name)
  (setq buffer-read-only nil)
  (let ((matchbeg 0)
        (origpt nil)
        (begpt nil)
        (contloop t)
        (endpt nil))
    (save-excursion
      (goto-char (point-min)) ;; begin searching in the buffer
      (while (and contloop (not (eobp)))
        (setq origpt (point))
        (if (setq endpt (re-search-forward regexp nil t))
            (add-text-properties (line-beginning-position) (line-end-position)
                                 (append
                                  `(mouse-face (highlight))
                                  `(occur-target ,(string-to-number (match-string 0)))))
          (setq contloop nil)))))
  (local-set-key '[return] 'php-tokens-mode-goto-occurrence)
  (local-set-key '[mouse-1] 'php-tokens-mode-goto-occurrence))

(defgroup php-tokens nil
  "Generates an overview of PHP tokens"
  :group 'convenience)

(defun php-tokens ()
  "Lists tokens for a PHP-buffer"
  (interactive)
  (let* ((source-buffername (buffer-name))
         (buffername "*php-tokens*")
         (contents (buffer-substring-no-properties (point-min) (point-max))))
    (when (get-buffer buffername)
      (kill-buffer buffername))
    (save-excursion
      (pop-to-buffer buffername)
      (with-temp-buffer
        (insert contents)
        (shell-command-on-region (point-min) (point-max)
                                 (format "php %s" (shell-quote-argument (expand-file-name "~/.emacs.d/utils/php_tokens.php")))
                                 buffername nil))
      (setq php-tokens-mode-target-buffer source-buffername)
      (php-tokens-mode buffername "^\\([0-9]+\\)[ ]+"))))



;; 设置字体 add by sanpu begin
;; Set up font locking
(defconst php-font-lock-keywords-1
  (list
   ;; Fontify constants
   (cons
    (concat "[^_$]?\\<\\(" php-constants "\\)\\>[^_]?")
    '(1 font-lock-constant-face))

   ;; Fontify keywords
   (cons
    (concat "[^_$]?\\<\\(" php-keywords "\\)\\>[^_]?")
    '(1 font-lock-keyword-face))

   ;; Fontify keywords and targets, and case default tags.
   (list "\\<\\(break\\|case\\|continue\\)\\>\\s-+\\(-?\\sw+\\)?"
         '(1 font-lock-keyword-face) '(2 font-lock-constant-face t t))
   ;; This must come after the one for keywords and targets.
   '(":" ("^\\s-+\\(\\sw+\\)\\s-+\\s-+$"
          (beginning-of-line) (end-of-line)
          (1 font-lock-constant-face)))

   ;; treat 'print' as keyword only when not used like a function name
   '("\\<print\\s-*(" . php-default-face)
   '("\\<print\\>" . font-lock-keyword-face)

   ;; Fontify PHP tag
   (cons php-tags-key font-lock-preprocessor-face)

   ;; Fontify ASP-style tag
   '("<\\%\\(=\\)?" . font-lock-preprocessor-face)
   '("\\%>" . font-lock-preprocessor-face)

   )
  "Subdued level highlighting for PHP mode.")

(defconst php-font-lock-keywords-2
  (append
   php-font-lock-keywords-1
   (list

    ;; class declaration
    '("\\<\\(class\\|interface\\)\\s-+\\(\\sw+\\)?"
      (1 font-lock-keyword-face) (2 font-lock-type-face nil t))
    ;; handle several words specially, to include following word,
    ;; thereby excluding it from unknown-symbol checks later
    ;; FIX to handle implementing multiple
    ;; currently breaks on "class Foo implements Bar, Baz"
    '("\\<\\(new\\|extends\\|implements\\)\\s-+\\$?\\(\\sw+\\)"
      (1 font-lock-keyword-face) (2 font-lock-type-face))

    ;; function declaration
    '("\\<\\(function\\)\\s-+&?\\(\\sw+\\)\\s-*("
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t))

    ;; class hierarchy
    '("\\<\\(self\\|parent\\)\\>" (1 font-lock-constant-face nil nil))

    ;; method and variable features
    '("\\<\\(private\\|protected\\|public\\)\\s-+\\$?\\sw+"
      (1 font-lock-keyword-face))

    ;; method features
    '("^\\s-*\\(abstract\\|static\\|final\\)\\s-+\\$?\\sw+"
      (1 font-lock-keyword-face))

    ;; variable features
    '("^\\s-*\\(static\\|const\\)\\s-+\\$?\\sw+"
      (1 font-lock-keyword-face))
    ))
  "Medium level highlighting for PHP mode.")

(defconst php-font-lock-keywords-3
  (append
   php-font-lock-keywords-2
   (list

    ;; <word> or </word> for HTML
    ;;'("</?\\sw+[^> ]*>" . font-lock-constant-face)
    ;;'("</?\\sw+[^>]*" . font-lock-constant-face)
    ;;'("<!DOCTYPE" . font-lock-constant-face)
    '("</?[a-z!:]+" . font-lock-constant-face)

    ;; HTML >
    '("<[^>]*\\(>\\)" (1 font-lock-constant-face))

    ;; HTML tags
    '("\\(<[a-z]+\\)[[:space:]]+\\([a-z:]+=\\)[^>]*?" (1 font-lock-constant-face) (2 font-lock-constant-face) )
    '("\"[[:space:]]+\\([a-z:]+=\\)" (1 font-lock-constant-face))

    ;; HTML entities
    ;;'("&\\w+;" . font-lock-variable-name-face)

    ;; warn about '$' immediately after ->
    '("\\$\\sw+->\\s-*\\(\\$\\)\\(\\sw+\\)"
      (1 font-lock-warning-face) (2 php-default-face))

    ;; warn about $word.word -- it could be a valid concatenation,
    ;; but without any spaces we'll assume $word->word was meant.
    '("\\$\\sw+\\(\\.\\)\\sw"
      1 font-lock-warning-face)

    ;; Warn about ==> instead of =>
    '("==+>" . font-lock-warning-face)

    ;; exclude casts from bare-word treatment (may contain spaces)
    `(,(concat "(\\s-*\\(" php-types "\\)\\s-*)")
      1 font-lock-type-face)

    ;; PHP5: function declarations may contain classes as parameters type
    `(,(concat "[(,]\\s-*\\(\\sw+\\)\\s-+&?\\$\\sw+\\>")
      1 font-lock-type-face)

    ;; Fontify variables and function calls
    '("\\$\\(this\\|that\\)\\W" (1 font-lock-constant-face nil nil))
    `(,(concat "\\$\\(" php-superglobals "\\)\\W")
      (1 font-lock-constant-face nil nil)) ;; $_GET & co
    '("\\$\\(\\sw+\\)" (1 font-lock-variable-name-face)) ;; $variable
    '("->\\(\\sw+\\)" (1 font-lock-variable-name-face t t)) ;; ->variable
    '("->\\(\\sw+\\)\\s-*(" . (1 php-default-face t t)) ;; ->function_call
    '("\\(\\sw+\\)::\\sw+\\s-*(?" . (1 font-lock-type-face)) ;; class::member
    '("::\\(\\sw+\\>[^(]\\)" . (1 php-default-face)) ;; class::constant
    '("\\<\\sw+\\s-*[[(]" . php-default-face) ;; word( or word[
    '("\\<[0-9]+" . php-default-face) ;; number (also matches word)

    ;; Warn on any words not already fontified
    '("\\<\\sw+\\>" . font-lock-warning-face)

    ))
  "Gauchy level highlighting for PHP mode.")

(provide 'php-tokens)
