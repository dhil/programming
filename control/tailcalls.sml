(* file: tailcalls.sml
 *
 * An example of how an operational (i.e. semantic) characterisation
 * of tail calls can be subtle.
 *
 * Suppose we define tail position such that a subexpression e' of e
 * appears in tail position within e, if when the flow of control
 * reaches e' the remaining observable effects of e are those of e'.
 *
 * A function call that appears in tail position is a tail call.
 *
 * In the presence of first-class control such a definition fails to
 * classify some (syntactic tail) calls as tail calls.
 *
 * The following example function foo illustrative this point. *)

open SMLofNJ.Cont

(* We have to store continuations in a reference cell to avoid type
   circularity during continuation capture. For simplicity we fix
   continuations to operate over integers. *)
type continuation = (int -> int) ref

(* The identity continuation. *)
fun identity x = x

(* Invokes a continuation with a provided argument. *)
fun resume (r : continuation) (x : int) : int =
    let val f = !r
    in
       (* We simulate "safe" one-shot continuation by overwriting the
        continuation cell with the identity continuation.  *)
        r := identity;
       (* Invoke the continuation with x. *)
        f x
    end

(* Captures the current continuation. *)
fun capturecc () : (int -> int) =
    let val r = ref identity
    in
        callcc
            (fn k => (r := (fn x => throw k x);
                      0) (* a dummy answer to fix the return type. *)
            );
        resume r
    end

(* This function performs a syntactic tail call, that fails to meet
   the operational specification of a tail call. *)
fun foo (f : (int -> int) -> int) : int =
    let val k = capturecc ()
    in
        print "Observable effect of foo\n";
        f k
     (* ^^^
      * we'd usually think of this application as being in tail
      * position (syntactically). But according to the above
      * definition it is not in tail position, because an application
      * of `k` inside `f` would cause the observable effect of `foo`
      * to be repeated. *)
    end

(* With SML NJ v110.79:
 * $ sml tailcalls.sml
 * Observable effect of foo
 * Observable effect of foo
 * 42
 *)
val _ =
    let val result = foo (fn k => k 42)
    in
        print ((Int.toString result) ^ "\n")
    end


