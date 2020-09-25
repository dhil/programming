;; file: tailcalls.scm

;; An example of how an operational (i.e. semantic) characterisation
;; of tail calls can be subtle.

;; Suppose we define tail position such that a subexpression e' of e
;; appears in tail position within e, if when the flow of control
;; reaches e' the remaining observable effects of e are those of e'.

;; A function call that appears in tail position is a tail call.

;; In the presence of first-class control such a definition fails to
;; classify some (syntactic tail) calls as tail calls.

;; The following example illustrative this point.
;; [ it is adapted from Danvy
;;     (https://groups.csail.mit.edu/mac/ftpdir/scheme-mail/HTML/rrrs-1997/msg00085.html)
;; ]

(define foo
  (lambda (f)
    (let ((k (call/cc (lambda (k) k))))
      (begin
        (display "Observable effect of foo\n")
        (f k)))))
;;      ^^^^
;;      we'd usually think of this application as being in tail
;;      position (syntactically). But according to the above
;;      definition it is not in tail position, because an application
;;      of `k` inside `f` would cause the observable effect of `foo`
;;      to be repeated.

(define id
  (lambda (x) x))

(begin
  (display
   ((foo (lambda (k) (k id))) 42))
  (display "\n"))

;; With Chez Scheme version 9.5
;; $ scheme --script tailcalls.scm
;; Observable effect of foo
;; Observable effect of foo
;; 42
