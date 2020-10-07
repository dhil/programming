(* This program illustrates the problem with abortive continuations
   such as those captured by `callcc`. This example is adapted from
   Gunter et al. (1995). *)

open SMLofNJ.Cont

val f = callcc (fn k => fn x =>
                   throw k (fn y => x + 4))

fun g () = 5 > (f 2)

(* $ sml callcc_toplevel_problem.sml
   - g ();
   Error: throw from one top-level expression into another
   /usr/lib/smlnj/bin/sml: Fatal error -- Uncaught exception TopLevelCallcc with 0
   raised at ../compiler/Execution/main/isolate.sml:24.28-24.42 *)
