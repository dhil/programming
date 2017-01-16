# Denotational Semantics

Course materials from https://www.cl.cam.ac.uk/teaching/1617/DenotSem/

Principal lecturer: Prof Marcelo Fiore
Taken by: Part II
Past exam questions

No. of lectures: 10
Suggested hours of supervisions: 3

##Aims

The aims of this course are to introduce domain theory and denotational semantics, and to show how they provide a mathematical basis for reasoning about the behaviour of programming languages.

## Lectures

* Introduction. The denotational approach to the semantics of programming languages. Recursively defined objects as limits of successive approximations.
* Least fixed points. Complete partial orders (cpos) and least elements. Continuous functions and least fixed points.
* Constructions on domains. Flat domains. Product domains. Function domains.
* Scott induction. Chain-closed and admissible subsets of cpos and domains. Scott’s fixed-point induction principle.
* PCF. The Scott-Plotkin language PCF. Evaluation. Contextual equivalence.
* Denotational semantics of PCF. Denotation of types and terms. Compositionality. Soundness with respect to evaluation. [2 lectures].
* Relating denotational and operational semantics. Formal approximation relation and its fundamental property. Computational adequacy of the PCF denotational semantics with respect to evaluation. Extensionality properties of contextual equivalence. [2 lectures].
* Full abstraction. Failure of full abstraction for the domain model. PCF with parallel or.

## Objectives

At the end of the course students should

* be familiar with basic domain theory: cpos, continuous functions, admissible subsets, least fixed points, basic constructions on domains;
* be able to give denotational semantics to simple programming languages with simple types;
* be able to apply denotational semantics; in particular, to understand the use of least fixed points to model recursive programs and be able to reason about least fixed points and simple recursive programs using fixed point induction;
* understand the issues concerning the relation between denotational and operational semantics, adequacy and full abstraction, especially with respect to the language PCF.

## Recommended reading

* Winskel, G. (1993). The formal semantics of programming languages: an introduction. MIT Press.
* Gunter, C. (1992). Semantics of programming languages: structures and techniques. MIT Press.
* Tennent, R. (1991). Semantics of programming languages. Prentice Hall. 
