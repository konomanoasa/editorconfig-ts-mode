;;; editorconfig-ts-mode.el --- Tree-sitter mode for EditorConfig  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 konomanoasa
;;
;; Author: konomanoasa <238482287+konomanoasa@users.noreply.github.com>
;; Maintainer: konomanoasa <238482287+konomanoasa@users.noreply.github.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: languages
;; URL: https://github.com/konomanoasa/editorconfig-ts-mode
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

;;; Commentary:
;;
;; Tree-sitter major mode for EditorConfig.

;;; Code:

(require 'treesit)

(defgroup editorconfig-ts nil
  "Tree-sitter mode for EditorConfig."
  :group 'languages)

(defconst editorconfig-ts-mode--grammar-sources
  '((editorconfig "https://github.com/konomanoasa/tree-sitter-editorconfig"
                  :revision "v0.2.0"))
  "Tree-sitter grammar sources for EditorConfig.")

;;;; Syntax

(defvar editorconfig-ts-mode-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (dolist (character '(?# ?\; ?\" ?\' ?\\ ?\( ?\) ?\[ ?\] ?{ ?}))
      (modify-syntax-entry character "." table))
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\r ">" table)
    table)
  "Syntax table for `editorconfig-ts-mode'.")

;;;;; Syntax Queries

(defconst editorconfig-ts-mode-syntax--query
  (treesit-query-compile
   'editorconfig
   '((comment_marker) @comment
     [(section_open) (set_open)] @square-open
     [(section_close) (set_close)] @square-close
     (brace_open) @brace-open
     (brace_close) @brace-close))
  "Compiled syntax query for EditorConfig.")

;;;;; Propertization

(defun editorconfig-ts-mode-syntax--propertize (start end)
  "Apply syntax properties between START and END."
  (let ((accessible-start (point-min)))
    (save-restriction
      (widen)
      (when (and (= start accessible-start)
                 (> accessible-start (point-min)))
        (remove-text-properties (point-min) start '(syntax-table nil))
        (setq start (point-min))
        (syntax-ppss-flush-cache start))
      (dolist (capture (treesit-query-capture
                        (treesit-parser-root-node treesit-primary-parser)
                        editorconfig-ts-mode-syntax--query start end))
        (let ((node (cdr capture))
              (syntax (pcase (car capture)
                        ('comment "<")
                        ('square-open "(]")
                        ('square-close ")[")
                        ('brace-open "(}")
                        ('brace-close "){"))))
          (put-text-property (treesit-node-start node) (treesit-node-end node)
                             'syntax-table (string-to-syntax syntax)))))))

;;;;; Setup

(defun editorconfig-ts-mode-syntax-setup ()
  "Configure syntax handling for the current buffer."
  (setq-local syntax-propertize-function
              #'editorconfig-ts-mode-syntax--propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-wholelines nil t)
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "[#;][ \t\v\f]*")
  (setq-local comment-use-syntax t))

;;;; Font Lock

;;;;; Features

(defconst editorconfig-ts-mode-font-lock--feature-list
  '((comment)
    (property string)
    (number escape)
    (operator constant punctuation bracket))
  "Font-lock features by decoration level.")

;;;;; Settings

(defun editorconfig-ts-mode-font-lock--settings ()
  "Return font-lock settings for the current buffer."
  (treesit-font-lock-rules
   :default-language 'editorconfig

   :feature 'comment
   '([(comment_marker) (comment_text)] @font-lock-comment-face)

   :feature 'property
   '((key_text) @font-lock-property-name-face)

   :feature 'string
   '([(value_text) (glob_literal) (set_text)] @font-lock-string-face)

   :feature 'number
   '((integer) @font-lock-number-face)

   :feature 'escape
   '((escape) @font-lock-escape-face)

   :feature 'operator
   '([(assignment_operator) (range_separator)] @font-lock-operator-face
     (set_negation) @font-lock-negation-char-face)

   :feature 'constant
   '([(wildcard) (recursive_wildcard) (single_character)] @font-lock-constant-face)

   :feature 'punctuation
   '([(alternative_separator) (path_separator)] @font-lock-punctuation-face)

   :feature 'bracket
   '([(section_open) (section_close) (set_open) (set_close)
      (brace_open) (brace_close)] @font-lock-bracket-face)))

;;;;; Setup

(defun editorconfig-ts-mode-font-lock-setup ()
  "Configure font lock for the current buffer."
  (setq-local treesit-font-lock-feature-list
              editorconfig-ts-mode-font-lock--feature-list)
  (setq-local treesit-font-lock-settings
              (editorconfig-ts-mode-font-lock--settings)))

;;;; Navigation

(defconst editorconfig-ts-mode-thing-settings
  '((editorconfig (defun "^section$")))
  "Tree-sitter thing definitions for EditorConfig.")

(defun editorconfig-ts-mode-navigation-setup ()
  "Configure navigation for the current buffer."
  (setq-local treesit-thing-settings
              editorconfig-ts-mode-thing-settings)
  (setq-local treesit-defun-skipper nil))

;;;; Imenu

(defconst editorconfig-ts-mode-imenu-settings
  '(("Section" "^section$" editorconfig-ts-mode--defun-name nil))
  "Tree-sitter Imenu settings for EditorConfig.")

(defun editorconfig-ts-mode--defun-name (node)
  "Return the source name of NODE, or nil if it has no name."
  (when (equal (treesit-node-type node) "section")
    (let* ((header (treesit-node-child-by-field-name node "header"))
           (name (and header (treesit-node-child-by-field-name header "name"))))
      (when name (treesit-node-text name t)))))

(defun editorconfig-ts-mode-imenu-setup ()
  "Configure Imenu for the current buffer."
  (setq-local treesit-defun-name-function
              #'editorconfig-ts-mode--defun-name)
  (setq-local treesit-simple-imenu-settings
              editorconfig-ts-mode-imenu-settings))

;;;; Indentation

(defconst editorconfig-ts-mode-indent-rules
  '((editorconfig (catch-all column-0 0)))
  "Tree-sitter indentation rules for EditorConfig.")

(defun editorconfig-ts-mode-indent-setup ()
  "Configure indentation for the current buffer."
  (setq-local treesit-simple-indent-rules
              editorconfig-ts-mode-indent-rules))

;;;; Mode

(defun editorconfig-ts-mode--ensure-grammar (language)
  "Ensure that the grammar for LANGUAGE is installed."
  (let ((treesit-language-source-alist
         (if (assq language treesit-language-source-alist)
             treesit-language-source-alist
           (cons (assq language editorconfig-ts-mode--grammar-sources)
                 treesit-language-source-alist))))
    (or (treesit-ensure-installed language)
        (user-error "Tree-sitter grammar `%s' is unavailable" language))))

(defun editorconfig-ts-mode--setup ()
  "Configure `editorconfig-ts-mode' in the current buffer."
  (editorconfig-ts-mode--ensure-grammar 'editorconfig)
  (setq-local treesit-primary-parser (treesit-parser-create 'editorconfig))
  (editorconfig-ts-mode-syntax-setup)
  (editorconfig-ts-mode-font-lock-setup)
  (editorconfig-ts-mode-navigation-setup)
  (editorconfig-ts-mode-imenu-setup)
  (editorconfig-ts-mode-indent-setup)
  (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode editorconfig-ts-mode prog-mode "EditorConfig-TS"
  "Major mode for editing EditorConfig."
  :syntax-table editorconfig-ts-mode-syntax-table
  :group 'editorconfig-ts
  (editorconfig-ts-mode--setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\(?:\\`\\|/\\)\\.editorconfig\\'" . editorconfig-ts-mode))

(provide 'editorconfig-ts-mode)

;;; editorconfig-ts-mode.el ends here
