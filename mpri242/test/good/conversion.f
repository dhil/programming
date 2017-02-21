(* This example file illustrates a somewhat subtle point where we consider it
   acceptable for your type-checker to reject a program, even though this program
   is well-typed according to Pottier and Gauthier's paper. *)

type eq a b

data constructor Eq : forall a b. {} -> eq a b
  where a = b

program

fun convert2arrow [ a ] [ b ] (p : eq a (b -> b)) (f : a) (x : b) : b =
  match p return b with
  | Eq [ _ ] [ _ ] {} ->
      (f : b -> b) x
      (* here, the explicit type annotation helps the type-checker
         convert the type a into the type b -> b *)
      (* without the explicit annotation, the program would still
         be well-typed according to Pottier and Gauthier's paper,
         yet we consider it acceptable for your type-checker to
         reject the modified program. *)
  end

