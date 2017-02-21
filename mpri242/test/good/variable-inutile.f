type false

program

let loop =
  fix loop : forall a . false -> false =
    (* the type variable [a] is intentionally unused *)
    fun [ a ] (x : false) = loop [ a ] x
in

loop

