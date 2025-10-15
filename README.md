# add1

## What is this?

This project is one exercise in building up an intuition about how to write
software using denotational and algebraic design.

It allows us to store a set of numbers (positive integers including 0) and
provide "statistics" on them, which are mathematical operations.
This all takes place via a line-based command line interface.
It can show what's been put into the store in insertion order, and also
work out the sum, product, and statistical mean of the items in the store,
based on standard arithmetic.

Note that there are a number of Haskell source files that start with "Step" in the `src` directory.
These are historical versions, illustrative of the steps taken to arrive at the current version.
They hopefully provide some useful insight as process documentation for how we arrived at
the current version.

## Why Denotational Design?

In denotational and algebraic design, the semantics reign supreme and the equations enforce the semantics.
They come first, and shape the implementation space. In a very similar way to how test driven development is
supposed to cover all *features* of implementation, the equations of algebraic design are supposed to cover
all the *meanings* of the implumentation of the software being implemented.

The general idea of this is that this defines what it means for the implementation to be correct (ie to do
the right thing as it has a living code spec usually in terms of property tests), irrespective of implementation
details which then frees us up in the implementation so that we can firstly implement a simple obvious inefficient
solution that satisfies our meanings, and only later when we have a need to be efficient and fast, we can spend more time
implementing that version. Focusing on the important equations and keeping the meanings and equations as loose as possible
while also maintaining correctness allows us the freedom to discover efficient fast implementations later.

As a trivial example, let's say we need to represent some textual info, and all we care about is that a data type can
represent textual information, say, then we can start with the inefficient String type, ensuring we create an interface
and specs that enforce the equations of this textual interface. Once we have a prototype working with String, we can
easily swap it out to use Text later, gaining speed and efficiency without changing our program's meaning or its useful interface
at all. Separating the semantic from the operational layer of the program is potentially a massive boon, especially for encouraging
and maintaining more modular and bug-free software.

For some background, see [Conal Elliott](http://conal.net)'s work, in particular [Denotational design with type class morphisms](https://www.cs.tufts.edu/~nr/cs257/archive/conal-elliott/type-class-morphisms-long.pdf) and the [Lambda the Ultimate article on it](http://lambda-the-ultimate.org/node/3215). Also the book [Algebra Driven Design](https://leanpub.com/algebra-driven-design) by Sandy Macguire. 

