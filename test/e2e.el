;;; e2e.el --- Batch end-to-end check of the RPC keystone  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;; Drives a real Lean server: open the fixture, start Eglot, wait for the
;; server, then fetch the interactive goal at a point inside a proof and
;; inside a `sorry'.  Prints the rendered goals.  Exits non-zero on failure.
;;
;; Run with:
;;   emacs -Q --batch -L . -l test/e2e.el

;;; Code:

(require 'neo-lean-mode)

(defvar e2e-dir (file-name-directory (or load-file-name buffer-file-name)))

(defun e2e-pump (predicate seconds what)
  "Pump events until PREDICATE returns non-nil or SECONDS elapse.
Signal an error mentioning WHAT on timeout."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.1))
    (unless (funcall predicate)
      (error "Timed out after %ss waiting for %s" seconds what))))

(defun e2e-goal-at (line col label)
  "Move to LINE/COL, run `neo-lean-goal', and return the rendered string.
LABEL is used in progress messages."
  (goto-char (point-min))
  (forward-line (1- line))
  (move-to-column col)
  (message "[e2e] requesting goal at %s (line %d col %d)..." label line col)
  (let ((before (with-current-buffer (neo-lean-infoview--buffer) (buffer-string))))
    (neo-lean-goal)
    (e2e-pump
     (lambda ()
       (not (equal before
                   (with-current-buffer (neo-lean-infoview--buffer) (buffer-string)))))
     20 (format "goal response (%s)" label))
    (with-current-buffer (neo-lean-infoview--buffer) (buffer-string))))

(let ((file (expand-file-name "fixture/Fixture.lean" e2e-dir)))
  (find-file file)
  (cl-assert (derived-mode-p 'neo-lean-mode) nil "fixture did not open in neo-lean-mode")
  (message "[e2e] starting Eglot (lake serve); first run may build core...")
  ;; `eglot-ensure' defers via `post-command-hook', which never fires under
  ;; --batch, so connect directly.  Wait synchronously for initialization.
  (let ((eglot-sync-connect 120)
        (eglot-connect-timeout 120))
    (apply #'eglot (eglot--guess-contact)))
  (e2e-pump (lambda () (and (eglot-current-server)
                            (eglot--capabilities (eglot-current-server))))
            120 "Eglot to connect")
  (message "[e2e] connected: %s" (eglot-current-server))
  nil)

;; Give the server a moment to receive didOpen and elaborate the file.
(let ((deadline (+ (float-time) 5)))
  (while (< (float-time) deadline) (accept-process-output nil 0.1)))

;; Goal inside completed `by / rfl' (line 2), should show the completed-proof
;; status from Lean's goals-accomplished marker.
(let ((g1 (e2e-goal-at 2 2 "by rfl")))
  (message "[e2e] --- goal #1 (by rfl) ---\n%s" g1)
  (cl-assert (string-match-p "Goals accomplished" g1)
             nil "expected completed-proof status, got: %s" g1))

;; Goal inside the `sorry' example (line 8), should show `⊢ True'.
(let ((g2 (e2e-goal-at 8 2 "sorry")))
  (message "[e2e] --- goal #2 (sorry) ---\n%s" g2)
  (cl-assert (string-match-p "True" g2) nil "expected `True' goal, got: %s" g2))

;; Goal inside `pending' (line 11), exercises hypothesis rendering: should list
;; the hypotheses a, b, h and the target `⊢ b = a'.
(let ((g3 (e2e-goal-at 11 2 "pending body")))
  (message "[e2e] --- goal #3 (hypotheses) ---\n%s" g3)
  (cl-assert (and (string-match-p "a b : Nat" g3)
                  (string-match-p "h : a = b" g3)
                  (string-match-p "⊢ b = a" g3))
             nil "expected hypotheses + `b = a' goal, got: %s" g3))

;; --- cursor-follow auto-update ---
;; With the infoview visible, moving the cursor should refresh the goal with
;; no explicit `neo-lean-goal'.  Batch has no windows, so stub the visibility
;; gate and fire the post-command hook directly after moving point.
(cl-letf (((symbol-function 'neo-lean-infoview-visible-p) (lambda () t)))
  (e2e-goal-at 8 2 "seed (sorry)")      ; seed infoview with the `True' goal
  (goto-char (point-min))
  (forward-line 1)                      ; move to the `by rfl' line, col 2
  (move-to-column 2)
  (message "[e2e] cursor-follow: moved to `by rfl', firing post-command (no neo-lean-goal)...")
  (neo-lean--goal-post-command)
  (e2e-pump
   (lambda ()
     (string-match-p "Goals accomplished"
                     (with-current-buffer (neo-lean-infoview--buffer) (buffer-string))))
   20 "cursor-follow auto-update")
  (message "[e2e] --- cursor-follow result ---\n%s"
           (with-current-buffer (neo-lean-infoview--buffer) (buffer-string)))
  (message "[e2e] cursor-follow auto-update works."))

(message "[e2e] PASS: interactive RPC keystone works end-to-end.")
;;; e2e.el ends here
