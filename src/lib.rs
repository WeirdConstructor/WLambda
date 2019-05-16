// Copyright (c) 2019 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!
# WLambda Scripting Language

This crate provides you with a small and simple embeddable
scripting language. It's primary feature are functions and calling
functions. It could be viewed as Lisp without parenthesis.

Here are some of it's features:

- Performance in the ball park of Python.
- Garbage collection relies only on reference counting.
- Simple API.
- Main data structures are Lists and Maps.
- Closures can capture up values either by value, by reference
  or by weak reference. Giving you the ability to keep cyclic
  references in check.

For a syntax overview you can consult the [parser](parser/index.html).

The API relies on a data structure made of [VVal](vval/index.html) nodes.

# Example WLambda Code

# Basic API Usage

*/

pub mod vval;
pub mod parser;
pub mod compiler;
pub mod prelude;
