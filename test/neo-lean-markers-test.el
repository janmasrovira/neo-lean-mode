;;; neo-lean-markers-test.el --- Tests for neo-lean-markers  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'seq)
(require 'neo-lean-markers)

(defun neo-lean-markers-test--diagnostic (&optional tag silent)
  "Return a diagnostic with Lean TAG and SILENT flag."
  (list :range (list :start (list :line 0 :character 0)
                     :end (list :line 1 :character 0))
        :leanTags (and tag (vector tag))
        :isSilent (and silent t)))

(ert-deftest neo-lean-goals-accomplished-diagnostic-p ()
  (should (neo-lean-goals-accomplished-diagnostic-p
           (neo-lean-markers-test--diagnostic 2)))
  (should-not (neo-lean-goals-accomplished-diagnostic-p
               (neo-lean-markers-test--diagnostic 1)))
  (should-not (neo-lean-goals-accomplished-diagnostic-p
               (neo-lean-markers-test--diagnostic nil))))

(ert-deftest neo-lean-silent-diagnostic-p ()
  (should (neo-lean--silent-diagnostic-p
           (neo-lean-markers-test--diagnostic 2 t)))
  ;; Eglot's JSON false is truthy in Elisp, so only literal t counts.
  (should-not (neo-lean--silent-diagnostic-p
               (plist-put (neo-lean-markers-test--diagnostic 2)
                          :isSilent :json-false))))

(ert-deftest neo-lean-filter-silent-diagnostics-keeps-vector-shape ()
  (let* ((visible (plist-put (neo-lean-markers-test--diagnostic 1)
                             :isSilent :json-false))
         (silent (neo-lean-markers-test--diagnostic 2 t))
         (filtered (neo-lean--filter-silent-diagnostics
                    (vector visible silent))))
    (should (vectorp filtered))
    (should (= (length filtered) 1))
    (should (eq (aref filtered 0) visible))))

(ert-deftest neo-lean-goals-accomplished-capabilities-respect-enable ()
  (let ((neo-lean-goals-accomplished-enable t))
    (should (equal (neo-lean-goals-accomplished--lean-capabilities)
                   '(:rpcWireFormat "v1" :silentDiagnosticSupport t))))
  (let ((neo-lean-goals-accomplished-enable nil))
    (should (equal (neo-lean-goals-accomplished--lean-capabilities)
                   '(:rpcWireFormat "v1")))))

(ert-deftest neo-lean-goals-accomplished-disabled-preserves-existing-margin ()
  (with-temp-buffer
    (let ((neo-lean-goals-accomplished-enable nil))
      (setq left-margin-width 2)
      (neo-lean-goals-accomplished--redraw
       (list (neo-lean-markers-test--diagnostic 2 t)))
      (should (equal neo-lean-goals-accomplished--overlays nil))
      (should (= left-margin-width 2)))))

(ert-deftest neo-lean-goals-accomplished-redraw-adds-marker ()
  (with-temp-buffer
    (insert "theorem easy : True := by\n  trivial\n")
    (let ((neo-lean-goals-accomplished-character "*"))
      (neo-lean-goals-accomplished--redraw
       (list (neo-lean-markers-test--diagnostic 2 t)))
      (goto-char (point-min))
      (should (neo-lean-goals-accomplished-at-point-p))
      (should (seq-some
               (lambda (overlay)
                 (overlay-get overlay 'neo-lean-goals-accomplished-marker))
               (overlays-at (point))))
      (should (= left-margin-width 1))
      (neo-lean-goals-accomplished-clear)
      (should-not (neo-lean-goals-accomplished-at-point-p (point-min)))
      (should (equal neo-lean-goals-accomplished--overlays nil))
      (should (= left-margin-width 0)))))

(ert-deftest neo-lean-goals-accomplished-line-overlap-includes-line-end ()
  (with-temp-buffer
    (insert "abc\nnext\n")
    (neo-lean-goals-accomplished--redraw
     (list (list :range (list :start (list :line 0 :character 0)
                              :end (list :line 0 :character 3))
                 :leanTags (vector 2)
                 :isSilent t)))
    (goto-char (point-min))
    (end-of-line)
    (should-not (neo-lean-goals-accomplished-at-point-p (point)))
    (should (neo-lean-goals-accomplished-at-line-p (point)))))

(provide 'neo-lean-markers-test)
;;; neo-lean-markers-test.el ends here
