;;; neo-lean-rpc.el --- Interactive RPC with the Lean server  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jan Mas Rovira

;; Author: Jan Mas Rovira <janmasrovira@gmail.com>
;; Keywords: languages, lean
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Low-level interactive RPC with the Lean language server, the layer that
;; powers a real infoview (clickable goals, widgets, hovers).  It is built on
;; Emacs' `jsonrpc.el' (the library Eglot is built on).
;;
;; The model: for each open document we establish one RPC *session* via
;; `$/lean/rpc/connect', keep it alive with periodic `$/lean/rpc/keepAlive'
;; notifications, and issue requests with `$/lean/rpc/call'.  A session is
;; bound to a document URI; individual calls additionally carry a text
;; position.  `neo-lean-rpc-open' returns a position-bound handle and
;; `neo-lean-rpc-subsession-call' dispatches a method on it.
;;
;; This milestone deliberately omits two things from the Lua original, noted
;; inline below: releasing `RpcRef's via finalizers (not doing so only leaks
;; a little server memory) and the multi-attempt reconnect wrapper.  A single
;; transparent reconnect on a dead session is implemented.

;;; Code:

(require 'cl-lib)
(require 'jsonrpc)
(require 'eglot)

;; `eglot-uri-to-path' is the public name on Emacs 30+ (eglot 1.16); on 29.x
;; only the internal `eglot--uri-to-path' exists.  Pick whichever is present.
(defalias 'neo-lean-uri-to-path
  (if (fboundp 'eglot-uri-to-path) 'eglot-uri-to-path 'eglot--uri-to-path)
  "Convert an LSP document URI to a local file path.")

;;;; Diagnostic tags

(defconst neo-lean--diagnostic-tag-unsolved-goals 1
  "Lean diagnostic tag for unsolved-goals diagnostics.")

(defconst neo-lean--diagnostic-tag-goals-accomplished 2
  "Lean diagnostic tag for goals-accomplished diagnostics.")

;;;; Error codes

;; Lean LSP/JSON-RPC error codes that mean the RPC session is dead and a new
;; `$/lean/rpc/connect' is required.  See Lean/Data/JsonRpc.lean.
(defconst neo-lean-rpc--needs-reconnect -32900)
(defconst neo-lean-rpc--content-modified -32801)
(defconst neo-lean-rpc--worker-exited -32901)
(defconst neo-lean-rpc--worker-crashed -32902)

(defconst neo-lean-rpc--dead-codes
  (list neo-lean-rpc--needs-reconnect
        neo-lean-rpc--content-modified
        neo-lean-rpc--worker-exited
        neo-lean-rpc--worker-crashed)
  "RPC error codes that mean the session is dead and must be reconnected.")

(defun neo-lean-rpc--dead-code-p (code)
  "Return non-nil if CODE indicates the RPC session is dead."
  (and (integerp code)
       (memq code neo-lean-rpc--dead-codes)))

;;;; Keep-alive period

;; `Lean.Server.FileWorker.Utils' expects a keep-alive at least every 30s, so
;; we send one a comfortable margin under that.
(defconst neo-lean-rpc--keepalive-seconds 20)

;;;; Session

(cl-defstruct (neo-lean-rpc-session (:constructor neo-lean-rpc--session-create))
  "An interactive RPC session bound to a document URI."
  connection                  ; the jsonrpc/eglot connection
  uri                         ; the document URI string
  ref-key                     ; "p" (wire v0) or "__rpcref" (wire v1)
  session-id                  ; integer session id from the server
  (connected nil)             ; t once the connect handshake has resolved
  connect-error               ; non-nil error if the handshake failed
  keepalive-timer
  (pending nil)               ; thunks queued until `connected'
  (closed nil))               ; t once the session must not be used

;; URI string -> `neo-lean-rpc-session'.
(defvar neo-lean-rpc--sessions (make-hash-table :test #'equal)
  "Registry of live RPC sessions, keyed by document URI.")

(defun neo-lean-rpc--ref-key (connection)
  "Determine the JSON key used for RPC references on CONNECTION.
Wire format v1 uses \"__rpcref\"; v0 (the default) uses \"p\".  This is
read from the server's `experimental.rpcProvider.rpcWireFormat'
capability."
  (let* ((caps (eglot--capabilities connection))
         (provider (plist-get (plist-get caps :experimental) :rpcProvider))
         (wire (plist-get provider :rpcWireFormat)))
    (if (equal wire "v1") "__rpcref" "p")))

(defun neo-lean-rpc--flush-pending (session)
  "Run and clear the queued thunks on SESSION."
  (let ((thunks (nreverse (neo-lean-rpc-session-pending session))))
    (setf (neo-lean-rpc-session-pending session) nil)
    (dolist (thunk thunks)
      (funcall thunk))))

(defun neo-lean-rpc--start-keepalive (session)
  "Start the periodic keep-alive notifications for SESSION."
  (setf (neo-lean-rpc-session-keepalive-timer session)
        (run-with-timer
         neo-lean-rpc--keepalive-seconds neo-lean-rpc--keepalive-seconds
         (lambda ()
           (unless (or (neo-lean-rpc-session-closed session)
                       (null (neo-lean-rpc-session-session-id session)))
             (ignore-errors
               (jsonrpc-notify
                (neo-lean-rpc-session-connection session)
                :$/lean/rpc/keepAlive
                (list :uri (neo-lean-rpc-session-uri session)
                      :sessionId (neo-lean-rpc-session-session-id session)))))))))

(defun neo-lean-rpc--close (session)
  "Mark SESSION closed and stop its keep-alive timer.
Does not release outstanding `RpcRef's (a later milestone)."
  (setf (neo-lean-rpc-session-closed session) t)
  (when (timerp (neo-lean-rpc-session-keepalive-timer session))
    (cancel-timer (neo-lean-rpc-session-keepalive-timer session))
    (setf (neo-lean-rpc-session-keepalive-timer session) nil)))

(defun neo-lean-rpc--connect (connection uri)
  "Open a new RPC session on CONNECTION for document URI.
Register it immediately (so callers can queue work) and resolve the
handshake asynchronously."
  (let ((session (neo-lean-rpc--session-create
                  :connection connection
                  :uri uri
                  :ref-key (neo-lean-rpc--ref-key connection))))
    (puthash uri session neo-lean-rpc--sessions)
    (jsonrpc-async-request
     connection :$/lean/rpc/connect (list :uri uri)
     :success-fn
     (lambda (result)
       (setf (neo-lean-rpc-session-session-id session) (plist-get result :sessionId)
             (neo-lean-rpc-session-connected session) t)
       (neo-lean-rpc--start-keepalive session)
       (neo-lean-rpc--flush-pending session))
     :error-fn
     (lambda (err)
       (setf (neo-lean-rpc-session-connect-error session) err
             (neo-lean-rpc-session-connected session) t)
       (neo-lean-rpc--close session)
       (neo-lean-rpc--flush-pending session)))
    session))

(defun neo-lean-rpc--session-for (connection uri)
  "Return a live session for URI on CONNECTION, connecting if needed."
  (let ((existing (gethash uri neo-lean-rpc--sessions)))
    (if (and existing
             (not (neo-lean-rpc-session-closed existing))
             (not (neo-lean-rpc-session-connect-error existing)))
        existing
      (neo-lean-rpc--connect connection uri))))

(defun neo-lean-rpc--do-call (session pos method params success error)
  "Issue `$/lean/rpc/call' on SESSION; the connect handshake has resolved.
POS is an LSP text-document-position plist.  METHOD/PARAMS are the Lean
RPC method name and its parameters.  SUCCESS and ERROR are callbacks."
  (cond
   ((neo-lean-rpc-session-connect-error session)
    (funcall error (neo-lean-rpc-session-connect-error session)))
   ((neo-lean-rpc-session-closed session)
    (funcall error (list :code neo-lean-rpc--needs-reconnect
                         :message "RPC session is closed")))
   (t
    (jsonrpc-async-request
     (neo-lean-rpc-session-connection session)
     :$/lean/rpc/call
     (append pos (list :sessionId (neo-lean-rpc-session-session-id session)
                       :method method
                       :params params))
     :success-fn success
     :error-fn
     (lambda (err)
       (when (neo-lean-rpc--dead-code-p (plist-get err :code))
         (neo-lean-rpc--close session))
       (funcall error err))))))

;;;; Public API

;;;###autoload
(defun neo-lean-rpc-position-params ()
  "Return the LSP text-document-position params at point.
A plist of the form (:textDocument (:uri URI) :position (:line L
:character C)), where C is the correct UTF-16 offset."
  (eglot--TextDocumentPositionParams))

(cl-defstruct (neo-lean-rpc-subsession (:constructor neo-lean-rpc--subsession-create))
  "A position-bound handle for issuing RPC calls."
  session                     ; the underlying `neo-lean-rpc-session'
  pos)                        ; the bound text-document-position plist

;;;###autoload
(defun neo-lean-rpc-open (pos)
  "Open (or reuse) an RPC session for POS and return a subsession handle.
POS is an LSP text-document-position plist, e.g. from
`neo-lean-rpc-position-params'.  Requires an active Eglot server in the
current buffer."
  (let* ((connection (eglot--current-server-or-lose))
         (uri (plist-get (plist-get pos :textDocument) :uri))
         (session (neo-lean-rpc--session-for connection uri)))
    (neo-lean-rpc--subsession-create :session session :pos pos)))

(defun neo-lean-rpc-subsession-call (subsession method params success error)
  "Call Lean RPC METHOD with PARAMS on SUBSESSION.
SUCCESS and ERROR are callbacks receiving the result plist or an error
plist respectively.  If the session is mid-handshake the call is queued
until it resolves.  On a dead session, reconnect once transparently."
  (let ((session (neo-lean-rpc-subsession-session subsession))
        (pos (neo-lean-rpc-subsession-pos subsession))
        (retried nil))
    (letrec ((on-error
              (lambda (err)
                ;; One transparent reconnect on a dead session.
                (if (and (not retried)
                         (neo-lean-rpc--dead-code-p (plist-get err :code)))
                    (let* ((connection (neo-lean-rpc-session-connection session))
                           (uri (neo-lean-rpc-session-uri session)))
                      (setq retried t)
                      (remhash uri neo-lean-rpc--sessions)
                      (setq session (neo-lean-rpc--session-for connection uri))
                      (setf (neo-lean-rpc-subsession-session subsession) session)
                      (funcall run))
                  (funcall error err))))
             (run
              (lambda ()
                (if (neo-lean-rpc-session-connected session)
                    (neo-lean-rpc--do-call session pos method params success on-error)
                  (push (lambda ()
                          (neo-lean-rpc--do-call session pos method params success on-error))
                        (neo-lean-rpc-session-pending session))))))
      (funcall run))))

;;;; Convenience wrappers for specific Lean RPC methods

(defun neo-lean-rpc-get-interactive-goals (subsession success error)
  "Request the interactive goals on SUBSESSION.
Calls `Lean.Widget.getInteractiveGoals'."
  (neo-lean-rpc-subsession-call
   subsession "Lean.Widget.getInteractiveGoals"
   (neo-lean-rpc-subsession-pos subsession) success error))

(defun neo-lean-rpc-get-interactive-term-goal (subsession success error)
  "Request the interactive term goal on SUBSESSION.
Calls `Lean.Widget.getInteractiveTermGoal'."
  (neo-lean-rpc-subsession-call
   subsession "Lean.Widget.getInteractiveTermGoal"
   (neo-lean-rpc-subsession-pos subsession) success error))

(defun neo-lean-rpc-get-interactive-diagnostics (subsession line-range success error)
  "Request interactive diagnostics on SUBSESSION.
LINE-RANGE is an LSP `LineRange' plist of the form (:start L :end L2), or nil
to request all diagnostics.  Calls `Lean.Widget.getInteractiveDiagnostics'."
  (neo-lean-rpc-subsession-call
   subsession "Lean.Widget.getInteractiveDiagnostics"
   (if line-range (list :lineRange line-range) '())
   success error))

(defun neo-lean-rpc-lazy-trace-children-to-interactive (subsession children success error)
  "Request lazy trace CHILDREN on SUBSESSION.
CHILDREN is the RPC reference stored in an interactive diagnostic trace node.
Calls `Lean.Widget.lazyTraceChildrenToInteractive'."
  (neo-lean-rpc-subsession-call
   subsession "Lean.Widget.lazyTraceChildrenToInteractive"
   children success error))

(defun neo-lean-rpc-get-go-to-location (subsession kind info success error)
  "Request go-to locations for INFO on SUBSESSION.
KIND is one of the strings \"definition\", \"declaration\" or \"type\".
INFO is the `InfoWithCtx' handle taken from a subexpression's tag (the
`neo-lean-info' text property).  Calls `Lean.Widget.getGoToLocation' and
yields a vector of LSP `LocationLink's."
  (neo-lean-rpc-subsession-call
   subsession "Lean.Widget.getGoToLocation"
   (list :kind kind :info info) success error))

(defun neo-lean-rpc-info-to-interactive (subsession info success error)
  "Request the info popup for INFO on SUBSESSION.
INFO is the `InfoWithCtx' handle taken from a subexpression's tag (the
`neo-lean-info' text property).  Calls
`Lean.Widget.InteractiveDiagnostics.infoToInteractive' and yields an
`InfoPopup' plist with optional :exprExplicit, :type and :doc."
  (neo-lean-rpc-subsession-call
   subsession "Lean.Widget.InteractiveDiagnostics.infoToInteractive"
   info success error))

(provide 'neo-lean-rpc)
;;; neo-lean-rpc.el ends here
