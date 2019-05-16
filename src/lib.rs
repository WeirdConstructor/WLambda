// Copyright (c) 2019 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!
# WLambda Scripting Language

This crate provides you with a small and simple embeddable
scripting language. It's primary feature are functions and calling
functions. It could be viewed as Lisp without parenthesis.

Here are some of it's properties:

- Performance in the ball park of Python.
- Garbage collection relies only on reference counting.
- Simple API.
- Main data structures are Lists and Maps.
- Closures can capture up values either by value, by reference
  or by weak reference. Giving you the ability to keep cyclic
  references in check.
- Small and simple syntax, for a reference look at the [parser](parser/index.html).

The API relies on a data structure made of [VVal](vval/index.html) nodes.

# Example WLambda Code

Just a quick glance at the WLambda syntax and semantics.

```wlambda
# This is a comment

# Definition:
!a = 10;

# Assignment:
.a = 20;

# List variable definition:
!a_list = $[1, 2, 3, 4];

# Map assignment:
!a_map = ${a: 10, b: 20};

# Function definition/assignment:
!a_func = {
    _ + _2  # Arguments are not named, they are put into _, _2, _3
};

a_func(2, 3);   # Function call
a_func 2 3;     # Equivalent function call

# There is no `if` statement. Booleans can be called
# with two arguments. The first one is called when the boolean
# is true, the second one is called when the boolean is false.
[a == 10] {
    # called if a == 10
} {
    # called if a != 10
}

# Counting loop:
!:ref sum = 0; # Defining a reference that can be assignment
               # from inside a function.

# `range` calls the given function for each iteration
# and passes the counter as first argument in `_`
range 0 10 1 { # This is a regular function.
    sum = sum + _;
}

# `range` loop with `break`
!break_value = range 0 10 1 {
    [_ == 5] { break 22 };
};


# Basic OOP:
!some_obj = ${};
some_obj.do_something = {
    # do something here
};
some_obj.do_something(); # Method call
```

Currently there are many more examples in the test cases in `compiler.rs`.

# Basic API Usage

The API is far from feature complete, but this is roughly
how it looks currently:

```
use wlambda::prelude::create_wlamba_prelude;

let s = "$[1,2,3]";
let global = create_wlamba_prelude();
let r = wlambda::compiler::eval(&s, global);
println!("Res: {}", r.s());
```

# Possible Roadmap

There are several things that can be added more or less easily to
WLambda. But I am currently working on making the language more
complete for real world use. So my current goals are:

- Add namespacing and importing for managing the global environment.
- Make namespaces for ultility functions in the areas:
    - List handling
    - Map handling
    - Iteration
    - Basic I/O for testing purposes
      (WLambda is for embedding, there are currently no goals
       to provide a binary beyond basic needs.)
- Improve and further document the VVal API for interacting with WLambda.
- Add `panic` and `assert` and also make the compiler aware of
  the debugging positions that the parser augmented the AST with for
  error reporting.

Future plans could be:

- Prototyped inheritance, sketched out like this:

    ```wlambda
        !proto = ${ print: { println _ }, };
        !o = to_obj { _proto_: proto };
        o.print(123);

        # MetaMap(Rc<RefCell<std::collections::HashMap<String, VVal>>>),
        # => invokes _proto_ lookup on field access (not write)
    ```

- Augment functions with tagged values:

    ```wlambda
        !tag = 123;
        !v = tag 10 tag;
        !fun = { println("not tagged!") };
        .fun = add_tag fun tag { println("tagged with 123"); }
        fun(v); # prints "tagged with 123"
        fun(10); # prints "not tagged!"

        # TagFun(Rc<RefCell<std::collections::HashMap<String, Rc<VValFun>>>>),
    ```

- There are currently no plans to change the internal evaluator
from a closure tree to a VM and/or JIT speedup.
However, if someone is able to significantly speed up the
evaluation this can be changed.

*/

pub mod vval;
pub mod parser;
pub mod compiler;
pub mod prelude;
