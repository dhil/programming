# Types

Course materials from https://www.cl.cam.ac.uk/teaching/1617/Types/.

Principal lecturer: Prof Andrew Pitts
Taken by: Part II
Past exam questions

No. of lectures: 12
suggested hours of supervisions: 3
Prerequisite courses: Computation Theory, Semantics of Programming Languages

## Aims

The aim of this course is to show by example how type systems for programming languages can be defined and their properties developed, using techniques that were introduced in the Part IB course on Semantics of Programming Languages. The emphasis is on type systems for functional languages and their connection to constructive logic.

## Lectures

* Introduction. The role of type systems in programming languages. Review of rule-based formalisation of type systems. [1 lecture]
* ML polymorphism. ML-style polymorphism. Principal type schemes and type inference. [2 lectures]
* Polymorphic reference types. The pitfalls of combining ML polymorphism with reference types. [1 lecture]
* Polymorphic lambda calculus (PLC). Explicit versus implicitly typed languages. PLC syntax and reduction semantics. Examples of datatypes definable in the polymorphic lambda calculus. [3 lectures]
* Dependent types. Dependent function types. Pure type systems. System F-omega. [2 lectures]
* Propositions as types. Example of a non-constructive proof. The Curry-Howard correspondence between intuitionistic second-order propositional calculus and PLC. The calculus of Constructions. Inductive types. [3 lectures]

## Objectives

At the end of the course students should

* be able to use a rule-based specification of a type system to carry out type checking and type inference;
* understand by example the Curry-Howard correspondence between type systems and logics;
* appreciate the expressive power of parametric polymorphism and dependent types.

## Recommended reading

* Pierce, B.C. (2002). Types and programming languages. MIT Press.
* Pierce, B. C. (Ed.) (2005). Advanced Topics in Types and Programming Languages. MIT Press.
* Girard, J-Y. (tr. Taylor, P. & Lafont, Y.) (1989). Proofs and types. Cambridge University Press. 


# Course materials

Lecture notes (including slides).
List of corrections to the notes.
* Annotated slides:
 + lecture 1 (6 Oct)
 + lecture 2 (11 Oct)
 + lecture 3 (13 Oct)
 + lecture 4 (18 Oct)
 + lecture 5 (20 Oct)
 + lecture 6 (25 Oct)
 + lecture 7 (27 Oct)
 + lecture 8 (1 Nov)
 + lecture 9 (3 Nov)
 + lecture 10 (8 Nov)
 + lecture 11 (10 Nov)
 + lecture 12 (15 Nov)
* Exercise sheet
* Additional material:
 + Naive implementations of the ML type inference algorithm (Section 2.4) and the PLC type inference algorithm (Section 4.3). Both are written in Fresh OCaml, which is a patch of OCaml with nice facilities for handling binding and freshness of names.
 + Need more help understanding the material in Section 3 (Polymorphic Reference Types)? Try Section 1.1.2.1 Value Polymorphism of the "SML'97 Conversion Guide" provided by SML/NJ.
 + The types forum carries discussion and announcements concerning research into type systems.
 + Some interesting examples of dependently typed programming languages:
   - Agda is a both a dependently typed functional programming language with Haskell-like syntax and a proof assistant.
   - Coq is both a proof assistant and a dependently typed functional programming language with an OCaml-like syntax.
   - F* is a verification-oriented dialect of ML.
   - Idris is a general purpose pure functional programming language with dependent types, with features that are influenced by both Haskell and ML.
   - Here is a very recent example of the kind phenomenon discussed in section 3 of the notes - in this case an unsoundness in Java and Scala arising from the combination of ML-style polymorphim with subtype polymorphism: Nada Amin and Ross Tate's Unsound Playground and accompanying paper at OOPSLA 2016.
