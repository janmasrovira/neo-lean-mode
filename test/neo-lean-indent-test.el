;;; neo-lean-indent-test.el --- Tests for Lean indentation  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Batch ERT tests for Lean indentation.  No Lean server required.

;;; Code:

(require 'ert)
(require 'neo-lean-mode)

(defmacro neo-lean-indent-test--with-buffer (content &rest body)
  "Evaluate BODY in a temporary `neo-lean-mode' buffer containing CONTENT."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let ((neo-lean-input-enable nil))
       (neo-lean-mode))
     (insert ,content)
     (goto-char (point-min))
     ,@body))

(defun neo-lean-indent-test--line (line)
  "Return LINE from the current buffer, without text properties."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position))))

(ert-deftest neo-lean-indent-line-function-installed ()
  (neo-lean-indent-test--with-buffer ""
    (should (eq indent-line-function #'neo-lean-indent-line))
    (should electric-indent-inhibit)
    (should-not indent-tabs-mode)))

(ert-deftest neo-lean-indent-newline-after-by ()
  (neo-lean-indent-test--with-buffer "example : 2 = 2 := by"
    (goto-char (point-max))
    (newline-and-indent)
    (insert "rfl")
    (should (equal (buffer-string)
                   "example : 2 = 2 := by\n  rfl"))))

(ert-deftest neo-lean-indent-after-nested-by ()
  (neo-lean-indent-test--with-buffer
      "example : 37 = 37 := by\n  have : 37 = 37 := by\nrfl"
    (goto-char (point-min))
    (forward-line 2)
    (neo-lean-indent-line)
    (should (equal (neo-lean-indent-test--line 3)
                   "    rfl"))))

(ert-deftest neo-lean-indent-after-arrow ()
  (neo-lean-indent-test--with-buffer
      "example (n : Nat) : n = n := by\n  induction n with\n  | zero =>\nrfl"
    (goto-char (point-min))
    (forward-line 3)
    (neo-lean-indent-line)
    (should (equal (neo-lean-indent-test--line 4)
                   "    rfl"))))

(ert-deftest neo-lean-indent-electric-newline-keeps-branch-line ()
  (neo-lean-indent-test--with-buffer
      (concat "example (n : Nat) : n = n := by\n"
              "  induction n with\n"
              "  | zero =>\n"
              "    rfl\n"
              "  | succ n ih =>")
    (goto-char (point-max))
    (electric-indent-local-mode 1)
    (let ((last-command-event ?\n))
      (insert-char ?\n)
      (electric-indent-post-self-insert-function))
    (should (equal (buffer-string)
                   (concat "example (n : Nat) : n = n := by\n"
                           "  induction n with\n"
                           "  | zero =>\n"
                           "    rfl\n"
                           "  | succ n ih =>\n"
                           "    ")))))

(ert-deftest neo-lean-indent-after-bare-variable ()
  (neo-lean-indent-test--with-buffer "variable\n{α : Type}"
    (goto-char (point-min))
    (forward-line 1)
    (neo-lean-indent-line)
    (should (equal (neo-lean-indent-test--line 2)
                   "  {α : Type}"))))

(ert-deftest neo-lean-indent-after-complete-variable ()
  (neo-lean-indent-test--with-buffer "variable (x : Nat)\ndef foo := x"
    (goto-char (point-min))
    (forward-line 1)
    (neo-lean-indent-line)
    (should (equal (neo-lean-indent-test--line 2)
                   "def foo := x"))))

(ert-deftest neo-lean-indent-after-where ()
  (neo-lean-indent-test--with-buffer "structure Foo where\nfield : Nat"
    (goto-char (point-min))
    (forward-line 1)
    (neo-lean-indent-line)
    (should (equal (neo-lean-indent-test--line 2)
                   "  field : Nat"))))

(ert-deftest neo-lean-indent-after-focused-by ()
  (neo-lean-indent-test--with-buffer
      "theorem foo : 37 = 37 := by\n  · have : 37 = 37 := by\nsorry"
    (goto-char (point-min))
    (forward-line 2)
    (neo-lean-indent-line)
    (should (equal (neo-lean-indent-test--line 3)
                   "      sorry"))))

(ert-deftest neo-lean-indent-after-focus-dot-line ()
  (neo-lean-indent-test--with-buffer
      (concat "example : True ∧ True := by\n"
              "  constructor\n"
              "  · have f := 1")
    (goto-char (point-max))
    (electric-indent-local-mode 1)
    (let ((last-command-event ?\n))
      (insert-char ?\n)
      (electric-indent-post-self-insert-function))
    (should (equal (buffer-string)
                   (concat "example : True ∧ True := by\n"
                           "  constructor\n"
                           "  · have f := 1\n"
                           "    ")))))

(ert-deftest neo-lean-indent-dedents-after-sorry ()
  (neo-lean-indent-test--with-buffer
      "example : 37 = 37 := by\n  sorry\n#check 37"
    (goto-char (point-min))
    (forward-line 2)
    (neo-lean-indent-line)
    (should (equal (neo-lean-indent-test--line 3)
                   "#check 37"))))

(ert-deftest neo-lean-indent-keeps-inline-sorry-level ()
  (neo-lean-indent-test--with-buffer
      "example : 37 = 37 := by\n  have : 37 = 37 := sorry\nrfl"
    (goto-char (point-min))
    (forward-line 2)
    (neo-lean-indent-line)
    (should (equal (neo-lean-indent-test--line 3)
                   "  rfl"))))

(provide 'neo-lean-indent-test)
;;; neo-lean-indent-test.el ends here
