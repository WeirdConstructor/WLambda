// Copyright (c) 2020 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!
WLambda - Embeddable Scripting Language for Rust
================================================

WLambda is an embeddable dynamic scripting language for Rust, where every value
can be called and the syntax is a blend of Perl, Lua, JavaScript and LISP/Scheme/Clojure.

Here are some of its properties:

- Simple but unique syntax. For a reference look at the [WLambda Language Reference](https://docs.rs/wlambda/newest/wlambda/prelude/index.html#wlambda-reference).
- Easily embeddable into Rust programs due to a simple API.
- The language is about getting things done quickly, so performance is not a main priority.
  Current performance is roughly in the ball park of (C)Python or Perl, which means
  the language is quite possibly too slow where speed is the focus, but fast enough if
  you do any heavy lifting in Rust.
- Main data structures are Vectors and Maps.
- Builtin data structure pattern matchers and selectors which lead to a
very powerful `match` operation.
- No garbage collector. Memory and resource management relies only on reference counting and RAII.
  You can create your own drop functions.
- Preserving Rust safety by not using `unsafe`.
- WLambda makes no guarantees that it will not panic and crash your application
  if bad code is executed. More hardening is required for running untrusted
  code on the application side (resource limits (ram/cpu), catching panic
  unwinding, limit file system access, ...).
- No exceptions, except WLambda level panics. Error handling is accomplished by
  a specialized data type. It can be thought of as dynamic counterpart of
  Rust's Result type.
- Prototyped object orientation.
- Easy maintenance and hackability of the implementation.
- Custom user data implementation using [VValUserData](https://docs.rs/wlambda/newest/wlambda/vval/trait.VValUserData.html).
- Threading support with shared atoms and message queues.
- Register based VM evaluator and code generator.
- Builtin pattern matching and structure selector [Pattern and Selector Syntax](https://docs.rs/wlambda/newest/wlambda/selector/index.html).
- Has a testable wasm32 version: [WASM WLambda Evaluator](http://wlambda.m8geil.de/#!/main).

The embedding API and all internal operations rely on a data structure
made of [VVal](https://docs.rs/wlambda/newest/wlambda/vval/index.html) nodes.

Here you can find the [WLambda Language Reference](https://docs.rs/wlambda/newest/wlambda/prelude/index.html#wlambda-reference).

# API Hello World

```
use wlambda::*;

match wlambda::eval("40 + 2") {
    Ok(v)  => { println!("Output: {}", v.s()); },
    Err(e) => { eprintln!("Error: {}", e); },
}
```

See further down below for more API usage examples!

# WLambda Language Guide

**Try out WLambda right away in the [WASM WLambda Evaluator](http://wlambda.m8geil.de/#!/main).**

## Variables

```wlambda
!x = 10;        # Variable definition

.x = 20;        # Variable assignment
```

## Operators

```wlambda
!x = (1 + 2) * (8 - 4) / 2;

std:assert_eq x 6;
```

## If

```wlambda
? $true {
    std:displayln "It's true!";
} {
    std:displayln "It's false!";
};
```

```wlambda
!x = 10 / 2;

? x == 5 {
    std:displayln "x == 5";
};
```

## While

```wlambda
!x = 10;

while x > 0 {
    std:displayln x;

    (x == 5) {
        break[];
    };
    .x = x - 1;
};
```

```wlambda
!x = 10;

while x > 0 {
    std:displayln x;

    ? x == 5 {
        # break is a function, first arg
        # is the return value for `while`:
        break[];
    };
    .x = x - 1;
};

std:assert_eq x 5;
```

## Counting Loop

```wlambda
!sum = 0;

iter i 0 => 10 {
    .sum = sum + i;
};

std:assert_eq sum 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9;
```

## Endless loop

```wlambda
!x = 10;

while $true {
    std:displayln x;
    .x = x - 1;
    ? x == 0 break[];
};
```

## Functions

```wlambda
!add = { _ + _1 };  # argument names _, _1, _2, ...

!result = add 2 3;

std:assert_eq result 5;
```

### Different function call syntaxes:

```wlambda
!add = {!(x, y) = @;    # named variables, @ evals to list of all args
    x + y
};

std:displayln[add[2, 3]];   # [] parenthesis calling syntax

std:displayln add[2, 3];    # less parenthesis

std:displayln (add 2 3);    # explicit expression delimiting with `( ... )`

std:displayln ~ add 2 3;    # `~` means: evaluate rest as one expression

!add5 = { _ + 5 };

std:displayln 3 &> add5;    # '&>' is an argument pipe operator

std:displayln add5 <& 3;    # '<&' is the reverse argument pipe operator
```

### Returning from nested functions:

```wlambda

!test = \:ret_label_a {!(x) = @;

    # an `if` is actually a call to another function, so we need to
    # dynamically jump upwards the call stack to the given label:
    ? x > 10 {
        return :ret_label_a x * 2;
    };
};

std:assert_eq (test 11) 22;
```

## Vectors

```wlambda
!v = $[1, 2, 3];
v.1 = 5;

std:assert_eq v.1 5;

std:assert_eq (std:pop v) 3;
std:assert_eq (std:pop v) 5;
std:assert_eq (std:pop v) 1;
```

## Iterating over an Vector

```wlambda
!sum = 0;

iter i $[1, 2, 3, 4] { .sum = sum + i; };

std:assert_eq sum 10;
```

## Accumulate values in a vector

```wlambda

!new_vec =
    $@vec iter i $i(0, 4) {
        $+ i;
    };

std:assert_eq (str new_vec) (str $[0,1,2,3]);
```

## Accumulate a sum

```wlambda
!sum =
    $@int iter i $i(0, 4) {
        $+ i;
    };

std:assert_eq sum 1 + 2 + 3;
```

## Hash tables/maps

```wlambda
!m = ${ a = 10, c = 2 };

m.b = m.a + m.c;

std:assert_eq m.b 12;
```

## Strings

```wlambda
!name = "Mr. X";

std:assert_eq name.4 "X";           # index a character
std:assert_eq (name 0 3) "Mr.";     # substring

!stuff = "日本人";
std:assert_eq stuff.0 "日";         # Unicode support
```

## Unicode identifiers:

```wlambda
!人 = "jin";

std:assert_eq 人 "jin";
```

## Handling Errors

```wlambda
!some_fun = {
    ? _ == :fail {
        $error :FAIL_HAVING_FUN
    } {
        :ok
    }
};

!res1 =
    match some_fun[:ok]
        ($error :FAIL_HAVING_FUN) => :failed
        ?                         => :ok;
std:assert_eq res1 :ok;

!res1 =
    match some_fun[:fail]
        ($error :FAIL_HAVING_FUN) => :failed
        ?                         => :ok;
std:assert_eq res1 :failed;
```

## Builtin Structure Selectors

Selectors work similar to XPath:
`$S( *:{a=10} /b/1 )` first selects all maps from a vector,
checks if they got a key-value pair that matches key=`a` and value=`10`.
The selector path is walked for the matching maps and the `b` key
is selected. Next the element at index `1` is selected and
captured.

```wlambda
!struct = $[
    ${ a = 10, b = $[ 1, 2, 3 ] },
    ${ a = 10, b = $[ 4, 5, 6 ] },
    ${ a = 20, b = $[ 8, 9,  20 ] },
    ${ a = 20, b = $[ 8, 10, 30 ] },
    ${ x = 99 },
    ${ y = 99 },
];

? struct &> $S( *:{a=10} /b/1 ) {
    std:assert_str_eq $\    $[2,5];
} {
    panic "Should've matched!";
};
```

## Builtin Structure Matchers

A bit different but similar to the structure selectors `$S ...` are the `$M
...` or `match` structure matchers:

```wlambda
!struct = $[
    ${ a = 10, b = $[ 1, 2, 3 ] },
    ${ a = 10, b = $[ 4, 5, 6 ] },
    ${ a = 20, b = $[ 8, 9,  20 ] },
    ${ a = 20, b = $[ 8, 10, 30 ] },
    ${ x = 99 },
    ${ y = 99 },
];

!res = $@vec iter elem struct {
    $+ ~
        match elem
            ${ a = 10, b = childs }     => $[:childs_10, $\.childs]
            ${ a = 20, b = childs }     => $[:childs_20, $\.childs]
            :other;
};

std:assert_str_eq res $[
    $[:childs_10,$[1, 2,   3]],
    $[:childs_10,$[4, 5,   6]],
    $[:childs_20,$[8, 9,  20]],
    $[:childs_20,$[8, 10, 30]],
    :other,
    :other,
];
```

## Builtin (Regex) Pattern Matching

```wlambda
!some_url = "http://crates.io/crates/wlambda";

!crate  = $none;
!domain = $none;

? some_url &> $r{$^ (^$+[^:]) \:\/\/ (^$*[^/]) \/crates\/ (^$+[a-z]) } {
    .domain = $\.2;
    .crate = $\.3;
};

std:assert_eq domain "crates.io";
std:assert_eq crate  "wlambda";
```

## Object Oriented Programming with prototypes

```wlambda
!MyClass = ${
    new = {
        ${
            _proto = $self,
            _data = ${ balance = 0, }
        }
    },
    deposit = {
        $data.balance = $data.balance + _;
    },
};

!account1 = MyClass.new[];

account1.deposit 100;
account1.deposit 50;

std:assert_eq account1._data.balance 150;
```

## Object Oriented Programming with closures

```wlambda

!MyClass = {
    !self = ${ balance = 0, };

    self.deposit = { self.balance = self.balance + _; };

    $:self
};

!account1 = MyClass[];

account1.deposit 100;
account1.deposit 50;

std:assert_eq account1.balance 150;
```

## WLambda Modules

```txt
# util.wl:
!@import std std;
!@wlambda;

!@export print_ten = { std:displayln ~ str 10; };
```

For import you do:

```txt
!@import u util;

u:print_ten[]
```

# Example WLambda Code

That was just a quick glance at the WLambda syntax and semantics.

More details for the syntax and the provided global functions
can be found in the [WLambda Language Reference](prelude/index.html#wlambda-reference).

Currently there are many more examples in the test cases in `tests/language.rs`.

# API Usage Examples

## Basic API Usage

Here is how you can quickly evaluate a piece of WLambda code:

```
let s = "$[1,2,3]";
let r = wlambda::eval(&s).unwrap();
println!("Res: {}", r.s());
```

## More Advanced API Usage

If you want to quickly add some of your own functions,
you can use the GlobalEnv `add_func` method:

```
use wlambda::vval::{VVal, VValFun, Env};

let global_env = wlambda::GlobalEnv::new_default();
global_env.borrow_mut().add_func(
    "my_crazy_add",
    |env: &mut Env, _argc: usize| {
        Ok(VVal::Int(
              env.arg(0).i() * 11
            + env.arg(1).i() * 13
        ))
    }, Some(2), Some(2));

let mut ctx = wlambda::compiler::EvalContext::new(global_env);

// Please note, you can also add functions later on,
// but this time directly to the EvalContext:

ctx.set_global_var(
    "my_crazy_mul",
    &VValFun::new_fun(|env: &mut Env, _argc: usize| {
       Ok(VVal::Int(
          (env.arg(0).i() + 11)
        * (env.arg(1).i() + 13)))
    }, Some(2), Some(2), false));


let res_add : VVal = ctx.eval("my_crazy_add 2 4").unwrap();
assert_eq!(res_add.i(), 74);

let res_mul : VVal = ctx.eval("my_crazy_mul 2 4").unwrap();
assert_eq!(res_mul.i(), 221);
```

## Maintaining state

```
use wlambda::*;

let mut ctx = EvalContext::new_default();

ctx.eval("!x = 10").unwrap();

ctx.set_global_var("y", &VVal::Int(32));

let r = ctx.eval("x + y").unwrap();

assert_eq!(r.s(), "42");
```

# Possible Roadmap

Current remaining goals for WLambda are:

- Fix remaining bugs.
- Add missing standard library functions without dragging in more
dependencies.
- Improve and further document the VVal API for interacting with WLambda.
- Improve [WLambda Language Reference](https://docs.rs/wlambda/newest/wlambda/prelude/index.html#wlambda-reference) documentation.
- DONE: Complete function reference documentation in [WLambda Language Reference](https://docs.rs/wlambda/newest/wlambda/prelude/index.html#wlambda-reference).
- DONE: Add proper module support (via `!@import` and `!@export`).
- DONE: Add prototyped inheritance for OOP paradigm.
- DONE: Add data structure matching/destructuring/selection primitives
to the language.
- DONE: Replace compiler and closure based evaluator with a VM
and more or less clever code generator.

# License

This project is licensed under the GNU General Public License Version 3 or
later.

## Why GPL?

Picking a license for my code bothered me for a long time. I read many
discussions about this topic. Read the license explanations. And discussed
this matter with other developers.

First about _why I write code for free_ at all, the reasons are:

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
eternity. That the _end user_ of the software always stays in control. That the users
have the means to adapt the software to new platforms or use cases.
Even if the original authors don't maintain the software anymore.
It ultimately prevents _"vendor lock in"_. I really dislike vendor lock in,
especially as developer. Especially as developer I want and need to stay
in control of the computers and software I use.

Another point is, that my work (and the work of any other developer) has a
value. If I give away my work without _any_ strings attached, I effectively
work for free. This compromises the price I (and potentially other developers)
can demand for the skill, workforce and time.

This makes two reasons for me to choose the GPL:

1. I do not want to support vendor lock in scenarios for free.
   I want to prevent those when I have a choice, when I invest my private
   time to bring value to the end users.
2. I don't want to low ball my own wage and prices by giving away the work
   I spent my scarce private time on with no strings attached. So that companies
   are able to use it in closed source projects.

## Conversion to MIT / Apache-2.0

I (WeirdConstructor) herby promise to release WLambda under MIT / Apache-2.0
license if you use it in an open source / free software game (licensed under
MIT and/or Apache-2.0) written in Rust (and WLambda) with a playable beta
release, non trivial amount of content and enough gameplay to keep me occupied
for at least 2 hours. You may use WLambda for your release as if it was
released under MIT and/or Apache-2.0. Proper attribution as required by MIT
and/or Apache-2.0.

## If you need a permissive or private license (MIT) right now

Please contact me if you need a different license and want to use my code. As
long as I am the only author, I can change the license the for code that was
written by me. We might find an agreement that involves money or something
else.  For your price estimations: At this point in time (May 2020) I invested
about 6 months of my private time into this project.

# Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in WLambda by you, shall be licensed as GPLv3 or later,
without any additional terms or conditions.

# Author

* Weird Constructor <weirdconstructor@gmail.com> (WeirdConstructor on GitHub)
  (You may find me as `WeirdConstructor` on the Rust Discord.)

# Contributors

* Cedric Hutchings <cedhut02@gmail.com> (cedric-h on GitHub)

*/

pub mod vval;
pub mod parser;
pub mod compiler;
pub mod ops;
pub mod vm;
pub mod prelude;
pub mod threads;
pub mod rpc_helper;
pub mod util;
pub mod nvec;
pub mod vval_user_obj;
pub mod csv;
pub mod selector;
pub mod struct_pattern;
pub mod formatter;
mod prog_writer;
mod io;
mod str_int;

pub use vval::VVal;
pub use vval::Env;
pub use vval::StackAction;
pub use vval::VValUserData;
pub use threads::AVal;
pub use compiler::GlobalEnv;
pub use compiler::EvalContext;
pub use compiler::SymbolTable;

/// Evaluates a piece of WLambda code in a default global environment.
///
/// ```
/// println!("> {}", wlambda::eval("${a = 10, b = 20}").unwrap().s());
/// ```
#[allow(dead_code)]
pub fn eval(s: &str) -> Result<VVal, crate::compiler::EvalError>  {
    let mut ctx = EvalContext::new_default();
    ctx.eval(s)
}
