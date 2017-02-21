(* Dead cases in pattern matching. *)

type eq a b

data constructor Eq : forall a b. {} -> eq a b
  where a = b

type int

type bool

type unit

data constructor Unit : {} -> unit

program

fun absurd [ c ] (p : eq int bool) : c =
  match p return c with
  | Eq [ _ ] [ _ ] {} ->
      let rec loop (u : unit) : c = loop u in loop (Unit {})
  end

