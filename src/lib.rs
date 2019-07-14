// Copyright (c) 2019 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!
WLambda - Embeddable Scripting Language for Rust
================================================

This crate provides you with a small and simple embeddable scripting language. It's syntax
gravitates around functions and argument composition for functions. A core concept is, that
everything is callable. It could be viewed as Lisp without parenthesis.

Here are some of it's properties:

- Simple syntax. For a reference look at the [WLambda Language Reference](prelude/index.html#wlambda-reference) and the [parser](parser/index.html).
- Easily embeddable into Rust programs due to a simple API.
- It's about getting things done quickly, so performance is not a main priority.
  Current performance is roughly in the ball park of (C)Python. Which means,
  it's too slow if you need speed. But fast enough if you are not primarily concerned
  about speed.
- No garbage collector. Garbage collection relies only on reference counting.
- Main data structures are Lists and Maps.
- Closures can capture up values either by value, by reference
  or by weak reference. Giving you the ability to keep cyclic
  references in check.
- Easy maintenance of the implementation.

The embedding API relies on a data structure made of [VVal](vval/index.html) nodes.

Here you can find the [WLambda Language Reference](prelude/index.html#wlambda-reference).

# Example WLambda Code

Just a quick glance at the WLambda syntax and semantics.

More details for the syntax and the provided global functions
can be found in the [WLambda Language Reference](prelude/index.html#wlambda-reference).

```wlambda
# This is a comment

# Definition:
!a = 10;

# Assignment:
.a = 20;

# List variable definition:
!a_list = $[1, 2, 3, 4];

# Map assignment:
!a_map = ${a = 10, b = 20};

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

# Returning early from functions:
!some_fun = \:some_fun_lbl { # \:xxx defines a function label for returning
    !x = 10;
    .x = do_something_to x;
    [x > 20] {
        return :some_fun_lbl 20; # explicit argument for return returns from
                                 # the specified block.
    }
    .x = 20;
    x
};

# `return` implicitly jumps to the topmost $nul label
# you may specify the _ label to jump out some unnamed func:
!some_fun = {
    [x == 20] \:_{ return 30 } # returns from some_fun, not from the if-branch
};

# Error reporting:
    # There are special error values, that will make the program panic
    # if they are not handled correctly at statement block level:
    !some_erroring_func = {
        return $error "An error happened!"
    };
    !value = some_erroring_func();
    # on_error calls the first argument if the second argument
    #
    on_error {
        # handle error here, eg. report, or make a new error value
    } value;

    # with the ~ operator, you can chain it nicely:
    on_error { handle_err(_) } ~ some_erroring_func();
    # or without ~:
    on_error { handle_err(_) } [some_erroring_func()];
    # or with |
    some_erroring_func() | on_error { handle_err(_) };

    # _? transforms an error value, and returns it from the current
    #    function. optionally jumping outwards.

    _? $e "ok" # is with an error value the same as: `return $e "ok"`
    _? 10 # passes the value through

!some_erroring_func = {
    on_error {
        report_my_error _;
    } block :outer {
        # do something...
        [_ != 10] {
            return :from_outer $error "Something really failed"
            # same as, with the difference, that _? only returns
            # from :outer if it is an error value.
            _? :outer $error "Something really failed"
        }
        # do more ...
    }
    # cleanup ...
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
let r = wlambda::compiler::eval(&s).unwrap();
println!("Res: {}", r.s());
```

# Possible Roadmap

There are several things that can be added more or less easily to
WLambda. But I am currently working on making the language more
complete for real world use. So my current goals are:

- Add namespacing and importing for managing the global environment.
- Make namespaces for utility functions in the areas:
    - List handling
    - Map handling
    - Iteration
    - Basic I/O for testing purposes
      (WLambda is for embedding, there are currently no goals
       to provide a binary beyond basic needs.)
- Improve and further document the VVal API for interacting with WLambda.
- Make VVal::Sym hold an interned string instead of a `String` instance.

Future plans could be:

- Prototyped inheritance, sketched out like this:

    ```wlambda
        !proto = ${ print = { println _ }, };
        !o = to_obj { _proto_ = proto };
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

# License

This project is licensed under the GNU General Public License Version 3 or
later.

## Why GPL?

Picking a license for my code bothered me for a long time. I read many
discussions about this topic. Read the license explanations. And discussed
this matter with other developers.

First about _why I write code for free_ at all:

- It's my passion to write computer programs. In my free time I can
write the code I want, when I want and the way I want. I can freely
allocate my time and freely choose the projects I want to work on.
- To help a friend or member of my family.
- To solve a problem I have.

Those are the reasons why I write code for free. Now the reasons
_why I publish the code_, when I could as well keep it to myself:

- So that it may bring value to users and the free software community.
- Show my work as an artist.
- To get into contact with other developers.
- And it's a nice change to put some more polish on my private projects.

Most of those reasons don't yet justify GPL. The main point of the GPL, as far
as I understand: The GPL makes sure the software stays free software until
eternity. That the user of the software always stays in control. That the users
have _at least the means_ to adapt the software to new platforms or use cases.
Even if the original authors don't maintain the software anymore.
It ultimately prevents _"vendor lock in"_. I really dislike vendor lock in,
especially as developer. Especially as developer I want and need to stay
in control of the computers I use.

Another point is, that my work has a value. If I give away my work without
_any_ strings attached, I effectively work for free. Work for free for
companies. I would compromise the price I can demand for my skill, workforce
and time.

This makes two reasons for me to choose the GPL:

1. I do not want to support vendor lock in scenarios. At least not for free.
   I want to prevent those when I have a choice.
   And before you ask, yes I work for a company that sells closed source
   software. I am not happy about the closed source fact.
   But it pays my bills and gives me the freedom to write free software
   in my free time.
2. I don't want to low ball my own wage and prices by giving away free software
   with no strings attached (for companies).

## If you need a permissive or private license (MIT)

Please contact me if you need a different license and really want to use
my code. As long as I am the only author, I can change the license.
We might find an agreement.

# Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in WLambda by you, shall be licensed as GPLv3 or later,
without any additional terms or conditions.

# Authors

* Weird Constructor <weirdconstructor@gmail.com>
  (You may find me as `WeirdConstructor` on the Rust Discord.)

*/

pub mod vval;
pub mod parser;
pub mod compiler;
pub mod prelude;
