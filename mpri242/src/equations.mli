open Types

(* [entailment hyps goals] tells whether the hypotheses [hyps] imply
   the equations [goals]. *)

val entailment: equations -> equations -> bool

(* [inconsistent hyps] tells whether the hypotheses [hyps] are
   inconsistent, that is, whether they imply the goal [false]. *)

val inconsistent: equations -> bool

