;;; editorconfig-ts-mode-test.el --- Tests for editorconfig-ts-mode  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'imenu)
(require 'loaddefs-gen)
(require 'newcomment)
(require 'editorconfig-ts-mode)

(dolist (language '(editorconfig))
  (unless (treesit-ready-p language t)
    (error "The %s grammar is required to run the tests" language)))

;;;; Helpers

(defun editorconfig-ts-mode-test--position (fragment &optional line)
  (save-excursion
    (goto-char (point-min))
    (when line
      (let ((found nil))
        (while (and (not found) (not (eobp)))
          (if (equal line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
              (setq found t)
            (forward-line 1)))
        (unless found (ert-fail (format "Missing fixture line: %S" line)))))
    (unless (search-forward fragment (and line (line-end-position)) t)
      (ert-fail (format "Missing fixture fragment: %S" fragment)))
    (- (point) (length fragment))))

(defun editorconfig-ts-mode-test--face (fragment &optional offset line)
  (get-text-property (+ (editorconfig-ts-mode-test--position fragment line)
                        (or offset 0)) 'face))

(defun editorconfig-ts-mode-test--syntax-class (fragment &optional offset line)
  (syntax-propertize (point-max))
  (syntax-class (syntax-after (+ (editorconfig-ts-mode-test--position fragment line)
                                 (or offset 0)))))

(defun editorconfig-ts-mode-test--buffer-state ()
  (font-lock-ensure)
  (syntax-propertize (point-max))
  (let (state)
    (dotimes (offset (- (point-max) (point-min)))
      (let ((position (+ (point-min) offset)))
        (push (list (get-text-property position 'face) (syntax-after position)) state)))
    (nreverse state)))

(defun editorconfig-ts-mode-test--should-match-fresh-buffer (level)
  (let ((source (buffer-substring-no-properties (point-min) (point-max)))
        (state (editorconfig-ts-mode-test--buffer-state))
        (file buffer-file-name))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert source)
      (let ((treesit-font-lock-level level)) (editorconfig-ts-mode))
      (should (equal state (editorconfig-ts-mode-test--buffer-state))))))

;;;; Grammar

(ert-deftest editorconfig-ts-mode-respects-grammar-sources ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)) received)
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed
                (lambda (language)
                  (setq received (assq language treesit-language-source-alist))
                  t))
          (dolist (source editorconfig-ts-mode--grammar-sources)
            (let* ((language (car source))
                   (custom (list language "/local/grammar" :revision "custom")))
              (dolist (configured (list nil (list custom)))
                (let ((treesit-language-source-alist configured))
                  (should (editorconfig-ts-mode--ensure-grammar language))
                  (should (equal received (if configured custom source)))
                  (should (eq treesit-language-source-alist configured)))))))
      (fset 'treesit-ensure-installed ensure))))

(ert-deftest editorconfig-ts-mode-reports-unavailable-grammar ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)))
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed (lambda (_language) nil))
          (with-temp-buffer
            (let ((buffer-file-name nil))
              (should-error (editorconfig-ts-mode) :type 'user-error)
              (should-not (treesit-parser-list)))))
      (fset 'treesit-ensure-installed ensure))))

(ert-deftest editorconfig-ts-mode-starts-and-reuses-parser ()
  (with-temp-buffer
    (insert "[*]\nx=y\n")
    (editorconfig-ts-mode)
    (should (eq major-mode 'editorconfig-ts-mode))
    (should (eq (treesit-parser-language treesit-primary-parser) 'editorconfig))
    (should (equal (treesit-node-type (treesit-parser-root-node treesit-primary-parser))
                   "document"))
    (editorconfig-ts-mode)
    (should (equal (treesit-parser-list) (list treesit-primary-parser)))))

;;;; Mode Selection

(ert-deftest editorconfig-ts-mode-selects-files ()
  (dolist (entry '(("/tmp/.editorconfig" . t)
                   (".editorconfig" . t)
                   ("/tmp/foo.editorconfig" . nil)
                   ("/tmp/.editorconfig.other" . nil)))
    (with-temp-buffer
      (setq buffer-file-name (car entry))
      (set-auto-mode)
      (should (eq (eq major-mode 'editorconfig-ts-mode) (cdr entry))))))

(ert-deftest editorconfig-ts-mode-generates-autoloads ()
  (let ((output (make-temp-file "editorconfig-ts-mode-loaddefs-"))
        (directory (file-name-directory (locate-library "editorconfig-ts-mode"))))
    (unwind-protect
        (progn
          (loaddefs-generate directory output nil nil nil t)
          (with-temp-buffer
            (insert-file-contents output)
            (dolist (form '("(autoload 'editorconfig-ts-mode" "(add-to-list 'auto-mode-alist"))
              (goto-char (point-min))
              (should (search-forward form nil t)))))
      (delete-file output))))

;;;; Syntax

(ert-deftest editorconfig-ts-mode-classifies-delimiters ()
  (with-temp-buffer
    (insert "root='([{}])'\\\n[{a,b}/[!x]/\\[/literal{word}]\nx=\"[{}]\"\n")
    (editorconfig-ts-mode)
    (dolist (entry '(("[{a" . 4) ("{a" . 4) ("}/[" . 5)
                     ("[!" . 4) ("]/" . 5) ("}]" . 1)
                     ("{word" . 1) ("'(" . 1) ("([" . 1)
                     ("\\[" . 1) ("\"[" . 1)))
      (should (= (editorconfig-ts-mode-test--syntax-class (car entry)) (cdr entry))))
    (should (= (syntax-class (syntax-after
                              (1+ (editorconfig-ts-mode-test--position "\\[")))) 1))
    (let ((opening (editorconfig-ts-mode-test--position "[{a")))
      (should (= (scan-sexps opening 1)
                 (+ (editorconfig-ts-mode-test--position "}]\n") 2))))))

(ert-deftest editorconfig-ts-mode-classifies-comments ()
  (with-temp-buffer
    (insert "# first\n; second\nx=a#b;c\n[#;]\n# cr\rx=y\n")
    (editorconfig-ts-mode)
    (syntax-propertize (point-max))
    (dolist (entry '(("first" . t) ("second" . t) ("b;c" . nil)
                     (";]" . nil) ("cr" . t) ("x=y" . nil)))
      (should (eq (not (null (nth 4 (syntax-ppss
                                     (editorconfig-ts-mode-test--position (car entry))))))
                  (cdr entry))))))

(ert-deftest editorconfig-ts-mode-comments-and-uncomments ()
  (with-temp-buffer
    (insert "x=y\n; note\n")
    (editorconfig-ts-mode)
    (comment-region 1 5)
    (should (equal (buffer-string) "# x=y\n; note\n"))
    (uncomment-region (point-min) (point-max))
    (should (equal (buffer-string) "x=y\nnote\n"))))

;;;; Font Lock

(ert-deftest editorconfig-ts-mode-fontifies-by-level ()
  (dolist (level '(1 2 3 4))
    (let ((treesit-font-lock-level level))
      (with-temp-buffer
        (insert "# note\nroot=true\n[*.c/**/?/[!x]/{a,b}/{-1..3}/\\*]\nunknown=unset\n")
        (editorconfig-ts-mode)
        (font-lock-ensure)
        (dolist (entry '(("#" 1 font-lock-comment-face)
                         ("note" 1 font-lock-comment-face)
                         ("root" 2 font-lock-property-name-face)
                         ("true" 2 font-lock-string-face)
                         ("unknown" 2 font-lock-property-name-face)
                         ("unset" 2 font-lock-string-face)
                         ("x" 2 font-lock-string-face)
                         ("-1" 3 font-lock-number-face)
                         ("\\*" 3 font-lock-escape-face)
                         ("=" 4 font-lock-operator-face)
                         ("*.c" 4 font-lock-constant-face)
                         ("**" 4 font-lock-constant-face)
                         ("?" 4 font-lock-constant-face)
                         ("!" 4 font-lock-negation-char-face)
                         (".." 4 font-lock-operator-face)
                         ("/" 4 font-lock-punctuation-face)
                         ("," 4 font-lock-punctuation-face)
                         ("[" 4 font-lock-bracket-face)))
          (should (equal (editorconfig-ts-mode-test--face (car entry))
                         (and (>= level (nth 1 entry)) (nth 2 entry)))))))))

;;;; Navigation

(ert-deftest editorconfig-ts-mode-navigates-structures ()
  (with-temp-buffer
    (insert "root=true\n[a]\nx=y\n# tail\n\n[]\nz=w\n[unfinished")
    (editorconfig-ts-mode)
    (goto-char (editorconfig-ts-mode-test--position "x=y"))
    (beginning-of-defun)
    (should (= (point) 11))
    (end-of-defun)
    (should (= (point) (editorconfig-ts-mode-test--position "[]")))
    (goto-char (editorconfig-ts-mode-test--position "z=w"))
    (beginning-of-defun)
    (should (= (point) (editorconfig-ts-mode-test--position "[]")))
    (end-of-defun)
    (should (= (point) (editorconfig-ts-mode-test--position "[unfinished")))
    (goto-char (point-max))
    (beginning-of-defun)
    (should (= (point) (editorconfig-ts-mode-test--position "[unfinished")))))

;;;; Imenu

(ert-deftest editorconfig-ts-mode-indexes-definitions ()
  (with-temp-buffer
    (insert "root=true\n[*.{js,ts}]\nx=y\n[]\n[unfinished")
    (editorconfig-ts-mode)
    (let ((index (funcall imenu-create-index-function)))
      (should (equal (mapcar #'car (cdr (assoc "Section" index)))
                     '("*.{js,ts}" "unfinished"))))
    (goto-char (editorconfig-ts-mode-test--position "unfinished"))
    (delete-region (point) (point-max))
    (insert "renamed]")
    (let ((index (funcall imenu-create-index-function)))
      (should (equal (mapcar #'car (cdr (assoc "Section" index)))
                     '("*.{js,ts}" "renamed"))))))

;;;; Indentation

(ert-deftest editorconfig-ts-mode-indents-structures ()
  (with-temp-buffer
    (insert "  root = true\n  [ a ]\n\tkey  =  value space\n   # note\n  \n  [unfinished\n")
    (editorconfig-ts-mode)
    (indent-region (point-min) (point-max))
    (should (equal (buffer-string)
                   "root = true\n[ a ]\nkey  =  value space\n# note\n  \n[unfinished\n"))
    (let ((once (buffer-string)))
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) once)))
    (goto-char (point-min))
    (forward-line 4)
    (indent-according-to-mode)
    (should (= (line-beginning-position) (line-end-position)))))

;;;; Updates

(ert-deftest editorconfig-ts-mode-updates-like-fresh-buffer ()
  (pcase-dolist (`(,source ,old ,new ,fragment ,face)
                 '(("[a\\]\n" "a\\" "a\\*" "\\*" font-lock-escape-face)
                   ("[*.c]\n" "*.c" "*.{c,h}" "{" font-lock-bracket-face)))
    (ert-info ((format "%S: %S -> %S" source old new))
      (with-temp-buffer

        (insert source)
        (let ((treesit-font-lock-level 4)) (editorconfig-ts-mode))
        (editorconfig-ts-mode-test--buffer-state)
        (goto-char (editorconfig-ts-mode-test--position old))
        (delete-char (length old))
        (insert new)
        (font-lock-ensure)
        (should (eq (editorconfig-ts-mode-test--face fragment) face))
        (editorconfig-ts-mode-test--should-match-fresh-buffer 4)))))

(ert-deftest editorconfig-ts-mode-keeps-incomplete-escape-unfontified-and-repairs-it ()
  (let ((treesit-font-lock-level 4))
    (with-temp-buffer
      (insert "[a\\]")
      (editorconfig-ts-mode)
      (font-lock-ensure)
      (should-not (editorconfig-ts-mode-test--face "\\"))
      (goto-char 4)
      (insert "*")
      (font-lock-ensure)
      (should (eq (editorconfig-ts-mode-test--face "\\*") 'font-lock-escape-face))
      (delete-char -1)
      (font-lock-ensure)
      (should-not (editorconfig-ts-mode-test--face "\\")))))

(ert-deftest editorconfig-ts-mode-reclassifies-delimiters-after-edits ()
  (with-temp-buffer
    (insert "[{a,b}]\n")
    (editorconfig-ts-mode)
    (should (= (editorconfig-ts-mode-test--syntax-class "{a") 4))
    (goto-char (editorconfig-ts-mode-test--position ",b"))
    (delete-char 2)
    (should (= (editorconfig-ts-mode-test--syntax-class "{a") 1))
    (should (= (editorconfig-ts-mode-test--syntax-class "}") 1))))

(ert-deftest editorconfig-ts-mode-preserves-syntax-when-narrowed ()
  (with-temp-buffer
    (insert "# head\nx=y\n; tail\n")
    (editorconfig-ts-mode)
    (narrow-to-region 8 (point-max))
    (syntax-propertize (point-max))
    (should (nth 4 (syntax-ppss 16)))
    (widen)
    (should (nth 4 (syntax-ppss 4)))
    (goto-char 1)
    (insert "x=")
    (narrow-to-region 10 (point-max))
    (syntax-propertize (point-max))
    (widen)
    (should-not (nth 4 (syntax-ppss 6)))))

(provide 'editorconfig-ts-mode-test)

;;; editorconfig-ts-mode-test.el ends here
