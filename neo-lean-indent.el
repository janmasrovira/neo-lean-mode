;;; neo-lean-indent.el --- Indentation for Lean 4  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jan Mas Rovira

;; Author: Paul Cadman <git@paulcadman.dev>
;; Keywords: languages, lean
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/janmasrovira/neo-lean-mode
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Lightweight Lean indentation, modelled on lean.nvim's heuristic
;; `indentexpr'.  This is intentionally local and synchronous: pressing RET
;; with `electric-indent-mode' enabled, or pressing TAB, can indent immediately
;; without involving Lean or the language server.

;;; Code:

(require 'subr-x)

(defcustom neo-lean-indent-offset 2
  "Number of spaces used for one Lean indentation level."
  :type 'integer
  :safe #'integerp
  :group 'neo-lean)

(defconst neo-lean-indent--indent-after-regexp
  (concat "\\(?:"
          "\\_<\\(?:by\\|do\\|try\\|finally\\|then\\|else\\|where\\|from"
          "\\|extends\\|deriving\\)\\_>"
          "\\|:="
          "\\|=>"
          "\\|[[:blank:]]="
          "\\)[[:blank:]]*$")
  "Regexp matching Lean lines after which the next line is indented.")

(defconst neo-lean-indent--never-indent-regexp
  (concat "[[:blank:]]*\\(?:"
          "attribute\\_>"
          "\\|compile_inductive\\_>"
          "\\|def\\_>"
          "\\|instance\\_>"
          "\\|partial_fixpoint\\_>"
          "\\|structure\\_>"
          "\\|where\\_>"
          "\\|@\\["
          "\\)")
  "Regexp matching Lean lines that should stay at column zero.")

(defun neo-lean-indent--offset ()
  "Return the configured Lean indentation offset."
  (max 0 neo-lean-indent-offset))

(defun neo-lean-indent--current-line ()
  "Return the current line without text properties."
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defun neo-lean-indent--current-line-indent ()
  "Return the indentation of the current line."
  (save-excursion
    (back-to-indentation)
    (current-column)))

(defun neo-lean-indent--line-in-comment-or-string-p ()
  "Return non-nil when current line starts inside a comment or string."
  (let ((state (syntax-ppss (line-beginning-position))))
    (or (nth 3 state) (nth 4 state))))

(defun neo-lean-indent--previous-line-info ()
  "Return information about the previous line.
The result is a plist with `:text' and `:indent', or nil on the first line."
  (unless (bobp)
    (save-excursion
      (forward-line -1)
      (list :text (neo-lean-indent--current-line)
            :indent (neo-lean-indent--current-line-indent)
            :comment-or-string (neo-lean-indent--line-in-comment-or-string-p)))))

(defun neo-lean-indent--focus-line-p (line)
  "Return non-nil when LINE begins with a Lean focus dot."
  (string-match-p "\\`[[:blank:]]*·" line))

(defun neo-lean-indent--sorry-line-p (line)
  "Return non-nil when LINE ends with a standalone `sorry'."
  (string-match-p "\\_<sorry\\_>[[:blank:]]*\\(?:--.*\\)?\\'" line))

(defun neo-lean-indent--inline-sorry-p (line)
  "Return non-nil when LINE's final `sorry' belongs to an inline expression."
  (string-match-p "\\(?::=\\|from\\)[[:blank:]]*sorry[[:blank:]]*\\(?:--.*\\)?\\'"
                  line))

(defun neo-lean-indent--after-previous-line (previous current-indent offset)
  "Return indentation based on PREVIOUS line, CURRENT-INDENT, and OFFSET."
  (if (not previous)
      0
    (let* ((line (plist-get previous :text))
           (trimmed (string-trim line))
           (indent (plist-get previous :indent))
           (focus-adjust (if (neo-lean-indent--focus-line-p line) offset 0))
           (base (+ indent focus-adjust)))
      (cond
       ((string-empty-p trimmed)
        current-indent)
       ((and (neo-lean-indent--sorry-line-p line)
             (not (neo-lean-indent--inline-sorry-p line)))
        (if (neo-lean-indent--focus-line-p line)
            indent
          (max 0 (- indent offset))))
       ((and (string-match-p ":[[:blank:]]*$" line)
             (not (string-match-p ":=[[:blank:]]*$" line)))
        (* 2 offset))
       ((or (string-match-p ":=[[:blank:]]*$" line)
            (string-match-p "{[[:blank:]]*$" line))
        offset)
       ((string-match-p neo-lean-indent--indent-after-regexp line)
        (+ base offset))
       ((neo-lean-indent--focus-line-p line)
        base)
       ((= indent 0)
        current-indent)
       (t
        indent)))))

(defun neo-lean-calculate-indentation ()
  "Return the desired indentation for the current Lean line."
  (let* ((offset (neo-lean-indent--offset))
         (current (neo-lean-indent--current-line))
         (current-trimmed (string-trim-left current))
         (current-indent (neo-lean-indent--current-line-indent))
         (previous (neo-lean-indent--previous-line-info)))
    (cond
     ((or (= (line-number-at-pos) 1)
          (neo-lean-indent--line-in-comment-or-string-p)
          (plist-get previous :comment-or-string))
      current-indent)
     ((string-empty-p current-trimmed)
      (neo-lean-indent--after-previous-line previous current-indent offset))
     ((string-match-p neo-lean-indent--never-indent-regexp current)
      0)
     ((string-match-p "\\`[[:blank:]]*}" current)
      (max 0 (- (neo-lean-indent--after-previous-line
                 previous current-indent offset)
                offset)))
     (t
      (neo-lean-indent--after-previous-line previous current-indent offset)))))

;;;###autoload
(defun neo-lean-indent-line ()
  "Indent the current line as Lean code."
  (interactive)
  (let ((indent (neo-lean-calculate-indentation))
        (point-from-end (- (point-max) (point))))
    (indent-line-to indent)
    (when (> (- (point-max) point-from-end) (point))
      (goto-char (- (point-max) point-from-end)))))

(provide 'neo-lean-indent)
;;; neo-lean-indent.el ends here
