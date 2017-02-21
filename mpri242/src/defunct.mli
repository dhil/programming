open Terms

(* Defunctionalization accepts a program that has been type-checked (so it carries
   extra type information at [TeAbs] and [TeApp] nodes, as described in [Terms])
   and produces a defunctionalized program. (The new program need not carry the
   extra type information.) *)

val translate: program -> program

