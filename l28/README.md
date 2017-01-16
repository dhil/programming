# Advanced Functional Programming

Course materials from https://www.cl.cam.ac.uk/teaching/1617/L28/.

Principal lecturer: Dr Jeremy Yallop
Additional lecturers: Dr Neel Krishnaswami, Dr Leo White
Taken by: MPhil ACS, Part III
Code: L28
Hours: 16
Prerequisites: Students wishing to take the module should have some experience of a typed functional programming language and an understanding of type inference.

## Aims

This module shows how to use the features of modern typed functional programming languages such as OCaml and Haskell to design and implement libraries and DSLs. It introduces a variety of programming techniques for improving both correctness and efficiency.

Students wishing to take the module should have some experience of a typed functional programming language and an understanding of type inference.

## Syllabus

Part 1: Types in functional languages.

* Typed lambda calculi. Hindley-Milner type inference and beyond. The Curry-Howard isomorphism.
* Higher-rank polymorphism. Duality of existentials and universals. Parametricity and abstraction.
* Modules, signatures, and functors.
* Records and variants
* Phantom types and generalised algebraic data types.

Part 2: Programming in functional languages.

* Typeful interfaces and domain-specific languages.
* Monads and related abstractions.
* Datatype-generic programming.
* Staging and metaprogramming.

## Objectives

On completion of this module students should be able to:

* Understand extensions to Hindley-Milner type systems common in modern typed functional programming languages (e.g. OCaml, Haskell).
* Apply modern approaches to designing libraries and DSLs, with appropriate use of monads, applicative functors, and related abstractions.
* Use advanced type system features to specify precise typed interfaces.
* Understand how to use metaprogramming to improve performance.

## Coursework

Coursework will consist of three take-home problem sets.

## Practical work

In addition to the take-home problem sets there will be (unassessed) interactive exercises to accompany many of the lectures.

## Assessment

Three take-home problem sets, each accounting for approximately a third of the marks.

## Recommended reading

* Minsky, Y. & Madhavapeddy, A. and Hickey, J. (2013). Real World OCaml. O'Reilly Media (1st edition). (Also available online.) We'll assume familiarity with the concepts introduced in chapters 1--6.
* Pierce, B. C. (2002). Types and Programming Languages. The MIT Press (1st edition). Pierce's book will be a useful reference for some of the concepts introduced in the first half of the course, although we'll be taking a less formal approach.

# Preparation

(See also Exercise 0 on the assessment page.)

If you haven't used a typed functional language before then it would be wise to familiarize yourself with the basics before the course starts. We'll be using OCaml in the course, and recommend starting with either of the following books:

* OCaml from the Very Beginning
* John Whitington
* Available in print, or as a PDF ($14.99)
* Real World OCaml
* Yaron Minsky, Anil Madhavapeddy, Jason Hickey
* Available in print, or online. Chapters 1 to 6 give enough background for this course.

The OCaml Beginners mailing list is also a useful resource.

The first few lectures are quite theory oriented: we'll be looking at various typed lambda calculi, along with type inference in ML-family languages. If you haven't studied the lambda calculus before then some background reading is likely to make these lectures much easier to follow. There are many introductions available; one which fits well with our approach is

* Types and Programming Languages
* Benjamin C. Pierce
* There are copies in the Computer Laboratory library and many of the college libraries.

The material in chapters 5 (The Untyped Lambda-Calculus) and 9 (Simply Typed Lambda-Calculus) is essential. We'll also be covering material from later chapters (11, 23, 24, 29, 30), although in much less detail than Pierce.

Additionally, the Part II (third year undergraduate) Types course is a less-advanced course covering ML, System F and the Curry-Howard correspondence.

## Tools

Installing the tools beforehand will also make things easier when the course starts.

The OPAM package manager is the recommended way to install OCaml.

We will be using System Fω in the course to illustrate theoretical aspects of functional programming. We have provided an Fω interpreter (based on the interpreters by Pierce for the "Types and Programming Languages" book) so you can try out the examples.

The interpreter can be installed using OPAM with the following instructions:

    $ opam remote add advanced-fp git://github.com/ocamllabs/advanced-fp-repo
    $ opam install fomega

The interpreter can also be used directly in the browser.

## Preliminary lecture schedule

Thursday 19 January 2017 (12pm)
 * Introduction. The lambda calculus
   Jeremy Yallop
Monday 23 January 2017 (12pm)
 * The lambda calculus (continued)
   Jeremy Yallop
Thursday 26 January 2017 (12pm)
 * Type inference
   Jeremy Yallop
Monday 30 January 2017 (12pm)
 * The Curry-Howard correspondence
   Neel Krishnaswami
Thursday 2 February 2017 (12pm)
 * Abstraction and parametricity
   Leo White
Monday 6 February 2017 (12pm)
 * Abstraction and parametricity
   Leo White
Thursday 9 February 2017 (12pm)
 * Abstraction and parametricity
   Leo White
Monday 13 February 2017 (12pm)
 * Generalized algebraic data types (GADTs)
   Jeremy Yallop
Thursday 16 February 2017 (12pm)
 * Application: a typed foreign function interface
   Jeremy Yallop
Monday 20 February 2017 (12pm)
 * Modular implicits
   Jeremy Yallop
Thursday 23 February 2017 (12pm)
 * Monads etc.
   Jeremy Yallop
Monday 27 February 2017 (12pm)
 * Monads etc. (continued)
   Jeremy Yallop
Thursday 2 March 2017 (12pm)
 * Generic programming
   Jeremy Yallop
Monday 6 March 2017 (12pm)
 * Staging
   Jeremy Yallop
Thursday 9 March 2017 (12pm)
 * Staging (continued)
   Jeremy Yallop
Monday 13 March 2017 (12pm)
