;;; editorconfig-ts-mode-test.el --- Tests for editorconfig-ts-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 konomanoasa
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(require 'ert)
(require 'imenu)
(require 'newcomment)
(require 'editorconfig-ts-mode)

(dolist (language '(editorconfig))
  (unless (treesit-ready-p language t)
    (error "The %s grammar is required to run the tests" language)))

;;;; Helpers

(defun editorconfig-ts-mode-test--position (fragment &optional line)
  "Return FRAGMENT's start in LINE, or anywhere when LINE is nil."
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
  "Return the face at OFFSET into FRAGMENT on LINE."
  (get-text-property (+ (editorconfig-ts-mode-test--position fragment line)
                        (or offset 0)) 'face))

(defun editorconfig-ts-mode-test--syntax-class (fragment &optional offset line)
  "Return the syntax class at OFFSET into FRAGMENT on LINE."
  (syntax-propertize (point-max))
  (syntax-class (syntax-after (+ (editorconfig-ts-mode-test--position fragment line)
                                 (or offset 0)))))

(defun editorconfig-ts-mode-test--buffer-state ()
  "Return current face and syntax properties after refreshing the buffer."
  (font-lock-ensure)
  (syntax-propertize (point-max))
  (let (state)
    (dotimes (offset (- (point-max) (point-min)))
      (let ((position (+ (point-min) offset)))
        (push (list (get-text-property position 'face) (syntax-after position)) state)))
    (nreverse state)))

(defun editorconfig-ts-mode-test--should-match-fresh-buffer (&optional level compare-tree)
  "Compare classifications at LEVEL and optionally COMPARE-TREE with fresh parsing."
  (let ((source (buffer-substring-no-properties (point-min) (point-max)))
        (state (editorconfig-ts-mode-test--buffer-state))
        (tree (when compare-tree
                (treesit-node-string (treesit-parser-root-node treesit-primary-parser))))
        (file buffer-file-name)
        (level (or level treesit-font-lock-level)))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert source)
      (let ((treesit-font-lock-level level)) (editorconfig-ts-mode))
      (should (equal state (editorconfig-ts-mode-test--buffer-state)))
      (when compare-tree
        (should (equal tree (treesit-node-string
                             (treesit-parser-root-node treesit-primary-parser))))))))

;;;; Syntax

(ert-deftest editorconfig-ts-mode-classifies-delimiters ()
  (with-temp-buffer
    (insert "root='([{}])'\\\n[{a,b}/[!x]/\\[/literal{word}]\nx=\"[{}]\"`\n")
    (editorconfig-ts-mode)
    (pcase-dolist (`(,fragment . ,expected)
                   '(("[{a" . 4) ("{a" . 4) ("}/[" . 5)
                     ("[!" . 4) ("]/" . 5) ("}]" . 1)
                     ("{word" . 1) ("'(" . 1) ("([" . 1)
                     ("\\[" . 1) ("\"[" . 1) ("`" . 1)))
      (should (= (editorconfig-ts-mode-test--syntax-class fragment) expected)))
    (should (= (syntax-class (syntax-after
                              (1+ (editorconfig-ts-mode-test--position "\\[")))) 1))
    (let ((opening (editorconfig-ts-mode-test--position "[{a")))
      (should (= (scan-sexps opening 1)
                 (+ (editorconfig-ts-mode-test--position "}]\n") 2))))))

;;;; Comment Commands

(ert-deftest editorconfig-ts-mode-inserts-standalone-comments ()
  (pcase-dolist (`(,source ,expected)
                 '(("root = true\n" "# \nroot = true\n")
                   ("[*.c]\n" "# \n[*.c]\n")
                   ("" "# ")
                   ("  " "# ")))
    (with-temp-buffer
      (insert source)
      (editorconfig-ts-mode)
      (goto-char (point-min))
      (end-of-line)
      (call-interactively (key-binding (kbd "M-;")))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     expected))
      (should (nth 4 (syntax-ppss))))))

;;;; Electric Pair

(ert-deftest editorconfig-ts-mode-pairs-delimiters-and-indents-on-return ()
  (pcase-dolist (`(,prefix ,opening ,expected ,column)
                 '(("" ?\[ "[\n]" 0)
                   ("[file" ?{ "[file{\n}" 0)))
    (let ((electric-pair-pairs nil)
          (electric-pair-open-newline-between-pairs t))
      (with-temp-buffer
        (editorconfig-ts-mode)
        (setq-local indent-tabs-mode nil)
        (electric-indent-local-mode 1)
        (electric-pair-local-mode 1)
        (insert prefix)
        (let ((last-command-event opening))
          (self-insert-command 1))
        (call-interactively (key-binding (kbd "RET")))
        (should (equal (buffer-string) expected))
        (should (= (current-column) column))))))

;;;; Navigation

(ert-deftest editorconfig-ts-mode-navigates-editing-units ()
  (with-temp-buffer
    (insert "root = true\nother = false\n[*.{c,h}/[ab]]\n  indent_style = space tabs\n# tail\n\n[next]\nx=y\n")
    (editorconfig-ts-mode)
    (pcase-dolist (`(,fragment ,offset ,count ,target ,target-offset)
                   '(("root" 0 1 "other" 0)
                     ("root" 1 1 "root" 4)
                     ("[*." 0 1 "[next]" 0)
                     ("[next]" 0 -1 "[*." 0)
                     ("*." 0 1 "]]" 1)
                     ("c,h" 0 1 "]]" 1)
                     ("]]" 1 -1 "*." 0)
                     ("  indent_style" 0 1 "# tail" 0)
                     ("# tail" 0 -1 "  indent_style" 0)
                     ("space tabs" 3 1 "space tabs" 10)
                     ("space tabs" 10 -1 "space tabs" 0)))
      (ert-info ((format "%S" (list fragment offset count target target-offset)))
        (goto-char (+ (editorconfig-ts-mode-test--position fragment)
                      offset))
        (forward-sexp count)
        (should (= (point)
                   (+ (editorconfig-ts-mode-test--position target)
                      target-offset)))))
    (goto-char (point-min))
    (forward-sexp 3)
    (should (= (point) (editorconfig-ts-mode-test--position "[next]")))
    (backward-sexp 3)
    (should (= (point) (point-min)))))

(ert-deftest editorconfig-ts-mode-navigates-after-edits-and-narrowing ()
  (with-temp-buffer
    (insert "root=true\n[a]\nx=old\n[b]\ny=new\n")
    (editorconfig-ts-mode)
    (goto-char (editorconfig-ts-mode-test--position "old"))
    (delete-char 3)
    (insert "two words")
    (backward-sexp)
    (should (= (point) (editorconfig-ts-mode-test--position "two words")))
    (forward-sexp)
    (should (= (point) (+ (editorconfig-ts-mode-test--position "two words") 9)))
    (narrow-to-region (editorconfig-ts-mode-test--position "[b]") (point-max))
    (goto-char (point-min))
    (forward-sexp)
    (should (= (point) (point-max)))
    (backward-sexp)
    (should (= (point) (point-min)))
    (goto-char (editorconfig-ts-mode-test--position "y=new"))
    (beginning-of-defun)
    (should (= (point) (point-min)))
    (widen)
    (backward-sexp)
    (should (= (point) (editorconfig-ts-mode-test--position "[a]")))))

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
    (insert "root=true\n[*.{js,ts}]\nx=y\n[]\n[*.{js,ts}]\n[unfinished")
    (editorconfig-ts-mode)
    (let ((index (funcall imenu-create-index-function)))
      (should (equal (mapcar #'car (cdr (assoc "Section" index)))
                     '("*.{js,ts}" "*.{js,ts}" "unfinished")))
      (let ((entries (cdr (assoc "Section" index))))
        (should (< (cdr (nth 0 entries)) (cdr (nth 1 entries))))))
    (goto-char (editorconfig-ts-mode-test--position "unfinished"))
    (delete-region (point) (point-max))
    (insert "renamed]")
    (let ((index (funcall imenu-create-index-function)))
      (should (equal (mapcar #'car (cdr (assoc "Section" index)))
                     '("*.{js,ts}" "*.{js,ts}" "renamed"))))))

;;;; Indentation

(ert-deftest editorconfig-ts-mode-indents-return-to-column-zero ()
  (dolist (source '("[*.{c,h}]" "key = two words" "# note" ""))
    (with-temp-buffer
      (editorconfig-ts-mode)
      (electric-indent-local-mode 1)
      (insert source)
      (call-interactively (key-binding (kbd "RET")))
      (should (equal (buffer-string) (concat source "\n")))
      (should (= (current-column) 0)))))

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

(ert-deftest editorconfig-ts-mode-refreshes-edited-owners-without-parser-notifications ()
  (dolist (narrow '(nil t))
    (ert-info ((format "Narrowed: %s" narrow))
      (with-temp-buffer
        (insert "indent_style = space\n")
        (let ((treesit-font-lock-level 4)) (editorconfig-ts-mode))
        (rename-buffer "editorconfig-ts-mode-test" t)
        (let ((noninteractive nil)) (font-lock-mode 1))
        (editorconfig-ts-mode-test--buffer-state)
        (should (get-text-property 1 'fontified))
        (let ((parser treesit-primary-parser))
          (should-not (treesit-node-check (treesit-parser-root-node parser) 'has-error))
          (dolist (notifier (treesit-parser-notifiers parser))
            (treesit-parser-remove-notifier parser notifier))
          (goto-char (editorconfig-ts-mode-test--position "space"))
          (when narrow
            (narrow-to-region (line-beginning-position) (line-end-position)))
          (insert "x")
          (widen)
          (should (eq parser treesit-primary-parser))
          (should-not (get-text-property 1 'fontified))
          (editorconfig-ts-mode-test--should-match-fresh-buffer 4 t))))))

(ert-deftest editorconfig-ts-mode-reparses-a-bom-moved-to-buffer-start ()
  (dolist (narrow '(nil t))
    (ert-info ((format "Narrowed: %s" narrow))
      (with-temp-buffer
        (insert "root = true\n\ufeff# note\n")
        (let ((treesit-font-lock-level 4)) (editorconfig-ts-mode))
        (editorconfig-ts-mode-test--buffer-state)
        (let ((parser treesit-primary-parser))
          (when narrow (narrow-to-region 1 14))
          (delete-region 1 13)
          (widen)
          (should-not (eq parser treesit-primary-parser))
          (should (equal (treesit-parser-list) (list treesit-primary-parser)))
          (should (equal (buffer-substring-no-properties (point-min) (point-max))
                         "\ufeff# note\n"))
          (editorconfig-ts-mode-test--buffer-state)
          (should-not (get-text-property 1 'face))
          (should (eq (editorconfig-ts-mode-test--face "note") 'font-lock-comment-face))
          (editorconfig-ts-mode-test--should-match-fresh-buffer 4 t))))))

(ert-deftest editorconfig-ts-mode-updates-like-fresh-buffer ()
  (pcase-dolist (`(,source ,old ,new ,fragment ,face)
                 '(("[a\\]\n" "a\\" "a\\*" "\\*" font-lock-escape-face)
                   ("[*.c]\n" "*.c" "*.{c,h}" "{" font-lock-bracket-face)
                   ("[*.c]\r" "\r" "\r\nindent_style=space\n" "indent_style" font-lock-property-name-face)
                   ("[{a*b}]\n" "a*b" "a*b,c" "*" font-lock-constant-face)))
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
