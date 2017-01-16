# Category Theory and Logic

Course materials from https://www.cl.cam.ac.uk/teaching/1617/L108/.

* Principal lecturer: Prof Andrew Pitts
* Taken by: MPhil ACS, Part III
* Code: L108
* Hours: 16
* Prerequisites: Basic familiarity with logic and set theory (e.g. Discrete Mathematics I and II from Part 1A Computer Science course), with the lambda calculus (e.g. Part 1A Foundations of Computer Science) and with inductively-defined type systems (e.g. Part 1B course on Semantics of Programming Languages)

## Aims

Category theory provides a unified treatment of mathematical properties and constructions that can be expressed in terms of "morphisms" between structures. It gives a precise framework for comparing one branch of mathematics (organized as a category) with another and for the transfer of problems in one area to another. Since its origins in the 1940s motivated by connections between algebra and geometry, category theory has been applied to diverse fields, including computer science, logic and linguistics. This course introduces the basic notions of category theory: category, functor, natural tansformation and adjunction. We will use category theory to organize and develop the kinds of structure that arise in semantics for logics and programming languages.

## Syllabus

    Introduction; some history. Definition of category. The category of sets and functions. Alternative definitions of category.
    Commutative diagrams. Examples of categories: preorders and monotone functions; monoids and monoid homomorphisms; a preorder as a category; a monoid as a category. Definition of isomorphism. Informal notion of a "category-theoretic" property.
    Terminal objects. The opposite of a category and the duality principle. Initial objects. Free monoids as initial objects.
    Binary products and coproducts. Cartesian categories.
    Algebraic signatures. Terms over an algebraic signature and their interpretation in a cartesian category.
    Exponential objects: in the category of sets and in general. Cartesian closed categories: definition and examples.
    Intuitionistic Propositional Logic (IPL) in Natural Deduction style. Semantics of IPL in a cartesian closed preorder.
    Simply Typed Lambda Calculus (STLC). Alpha equivalence of terms. The typing relation. Semantics of STLC types and terms in a cartesian closed category (ccc).
    Capture-avoiding substitution for lambda terms. Semantics of substitution in a ccc. Soundness of the ccc semantics for beta-eta equality of lambda terms.
    The internal language of a ccc. STLC theories as cccs. The Curry-Howard-Lawvere correspondence. Functors. Contravariance.
    Identity and composition for functors. Size: small categories and locally small categories. The category of small categories. Finite products of categories.
    Natural transformations. Functor categories. The category of small categories is cartesian closed.
    Hom functors. Natural isomorphisms. Adjunctions. Examples of adjoint functors.
    Theorem characterizing the existence of right (respectively left) adjoints in terms of a universal property.
    Dependent types. Dependent product sets and dependent function sets as adjoint functors. Equivalence of categories. Example: the category of I-indexed sets and functions is equivalent to the slice category Set/I.
    Presheaves. The Yoneda Lemma. Categories of presheaves are cartesian closed.

## Objectives

On completion of this module, students should:

    be familiar with the basic notions of category theory
    be able to interpret logic, proof and programs in categories with appropriate structure.

## Coursework

The coursework in this module will consist of a graded exercise sheet.
Assessment

Assessment will consists of:

    * a graded exercise sheet (25% of the final mark), and
    * a take-home test (75%).

## Recommended reading

* Awodey, S. (2010). Category theory. Oxford University Press (2nd ed.).
* Crole, R. L. (1994). Categories for types. Cambridge University Press.
* Lambek, J. and Scott, P. J. (1986). Introduction to higher order categorical logic. Cambridge University Press.
* Pitts, A. M. (2000). Categorical Logic. Chapter 2 of S. Abramsky, D. M. Gabbay and T. S. E. Maibaum (Eds) Handbook of Logic in Computer Science, Volume 5. Oxford University Press. (Draft copy available here.)

