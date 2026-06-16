;;; neo-lean-restart-test.el --- Tests for stale-imports detection  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Batch ERT tests for `neo-lean--imports-out-of-date-p', the pure predicate
;; that recognises Lean's "imports out of date" diagnostic.  No server needed.

;;; Code:

(require 'ert)
(require 'neo-lean-restart)

(defun neo-lean-restart-test--diag (&rest overrides)
  "A stale-imports `Diagnostic' plist, with OVERRIDES taking precedence.
OVERRIDES come first so `plist-get' returns them over the defaults."
  (append overrides
          (list :severity 1
                :message "Imports are out of date and must be rebuilt; \
use the \"Restart File\" command in your editor."
                :range '(:start (:line 0 :character 0)
                                :end (:line 0 :character 0)))))

(ert-deftest neo-lean-imports-out-of-date-p-matches ()
  (should (neo-lean--imports-out-of-date-p (neo-lean-restart-test--diag))))

(ert-deftest neo-lean-imports-out-of-date-p-wrong-severity ()
  ;; A warning (2), not an error (1), is not the stale-imports condition.
  (should-not (neo-lean--imports-out-of-date-p
               (neo-lean-restart-test--diag :severity 2))))

(ert-deftest neo-lean-imports-out-of-date-p-wrong-message ()
  (should-not (neo-lean--imports-out-of-date-p
               (neo-lean-restart-test--diag :message "unrelated error"))))

(ert-deftest neo-lean-imports-out-of-date-p-not-top-of-file ()
  ;; The diagnostic must sit at the very top of the file (line 0, char 0).
  (should-not (neo-lean--imports-out-of-date-p
               (neo-lean-restart-test--diag
                :range '(:start (:line 5 :character 0)
                                :end (:line 5 :character 3))))))

;;;; Import-aware dependency reload

(ert-deftest neo-lean-import-matches-file-p ()
  (should (neo-lean--import-matches-file-p "Foo.Bar" "/p/Foo/Bar.lean"))
  (should (neo-lean--import-matches-file-p "Bar" "/p/Foo/Bar.lean"))
  (should-not (neo-lean--import-matches-file-p "Foo.Bar" "/p/Foo/Baz.lean"))
  ;; Must match on a path-segment boundary, not mid-component.
  (should-not (neo-lean--import-matches-file-p "Foo.Bar" "/p/XFoo/Bar.lean")))

(ert-deftest neo-lean-buffer-import-modules ()
  (with-temp-buffer
    (insert "prelude\n"
            "import Foo.Bar\n"
            "import all Baz.Qux\n"
            "\n"
            "def x := 1\n")
    (should (equal (neo-lean--buffer-import-modules)
                   '("Foo.Bar" "Baz.Qux")))))

(provide 'neo-lean-restart-test)
;;; neo-lean-restart-test.el ends here
