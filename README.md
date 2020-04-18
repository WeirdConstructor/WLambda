<img align="left" width="60" height="60" src="http://m8geil.de/data/git/wlambda/res/wlambda_logo_60.png">

WLambda - Embeddable Scripting Language for Rust
================================================

This crate provides a small and simple embeddable scripting language.
Its syntax gravitates around functions and argument composition for functions.
A core concept is that everything is callable. It could be regarded as LISP
without parenthesis, or as a mixture of Perl, JavaScript and LISP/Scheme.

Here are some of its properties:

- Simple but unique syntax. For a reference look at the [WLambda Language Reference](https://docs.rs/wlambda/newest/wlambda/prelude/index.html#wlambda-reference) and the [parser](https://docs.rs/wlambda/newest/wlambda/parser/index.html).
- Easily embeddable into Rust programs due to a simple API.
- The language is about getting things done quickly, so performance is not a main priority.
  Current performance is roughly in the ball park of (C)Python or Perl, which means
  the language is quite possibly too slow where speed is the focus, but fast enough if
  you do any heavy lifting in Rust.
- No garbage collector. Memory and resource management relies only on reference counting and RAII.
You can create your own drop functions.
- Main data structures are Lists and Maps.
- No exceptions, except panics. Error handling is accomplished
by a specialized data type. It can be thought of as dynamic counterpart
of Rust's Result type.
- Prototyped object orientation.
- Easy maintenance and hackability of the implementation.
- Custom user data implementation using [VValUserData](https://docs.rs/wlambda/newest/wlambda/vval/trait.VValUserData.html).
- Threading support with shared atoms and message queues.
- Register based VM evaluator and code generator.
- Has a testable wasm32 version: [WASM WLambda Evaluator](http://wlambda.m8geil.de/#!/main).

The embedding API and all internal operations rely on a data structure
made of [VVal](https://docs.rs/wlambda/newest/wlambda/vval/index.html) nodes.

Here you can find the [WLambda Language Reference](prelude/index.html#wlambda-reference).

## API Hello World

```rust
use wlambda::*;

match wlambda::compiler::eval("40 + 2") {
    Ok(v)  => { println!("Output: {}", v.s()); },
    Err(e) => { eprintln!("Error: {}", e); },
}
```

See further down below for more API usage examples!

## WLambda Language Guide

**Try out WLambda right away in the [WASM WLambda Evaluator](http://wlambda.m8geil.de/#!/main).**

### Variables

```wlambda
!x = 10;        # Variable definition

.x = 20;        # Variable assignment
```

### Operators

```wlambda
!x = (1 + 2) * (8 - 4) / 2;

std:assert_eq x 6;
```

### If

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

### While

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

### Counting Loop

```wlambda
!sum = 0;

iter i $i(0, 10, 1) {
    .sum = sum + i;
};

std:assert_eq sum 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9;
```

### Endless loop

```wlambda
!x = 10;

while $true {
    std:displayln x;
    .x = x - 1;
    ? x == 0 break[];
};
```

### Functions

```wlambda
!add = { _ + _1 };  # argument names _, _1, _2, ...

!result = add 2 3;

std:assert_eq result 5;
```

Different function call syntaxes:

```wlambda
!add = {!(x, y) = @;    # named variables, @ evals to list of all args
    x + y
};

std:displayln[add[2, 3]];   # [] parenthesis calling syntax

std:displayln add[2, 3];    # less parenthesis

std:displayln (add 2 3);    # explicit expression delimiting with `( ... )`

std:displayln ~ add 2 3;    # `~` means: evaluate rest as one expression
```

#### Returning from nested functions:

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

### Vectors

```wlambda
!v = $[1, 2, 3];
v.1 = 5;

std:assert_eq v.1 5;

std:assert_eq (std:pop v) 3;
std:assert_eq (std:pop v) 5;
std:assert_eq (std:pop v) 1;
```

### Iterating over an Vector

```wlambda
!sum = 0;

iter i $[1, 2, 3, 4] { .sum = sum + i; };

std:assert_eq sum 10;
```

### Accumulate values in a vector

```wlambda

!new_vec =
    $@vec iter i $i(0, 4) {
        $+ i;
    };

std:assert_eq (str new_vec) (str $[0,1,2,3]);
```

### Accumulate a sum

```wlambda
!sum =
    $@int iter i $i(0, 4) {
        $+ i;
    };

std:assert_eq sum 1 + 2 + 3;
```

### Hash tables/maps

```wlambda
!m = ${ a = 10, c = 2 };

m.b = m.a + m.c;

std:assert_eq m.b 12;
```

### Strings

```wlambda
!name = "Mr. X";

std:assert_eq name.4 "X";           # index a character
std:assert_eq (name 0 3) "Mr.";     # substring

!stuff = "日本人";
std:assert_eq stuff.0 "日";         # Unicode support
```

### Unicode identifiers:

```wlambda
!人 = "jin";

std:assert_eq 人 "jin";
```

### Object Oriented Programming with prototypes

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

### Object Oriented Programming with closures

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

### Modules

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

## Example WLambda Code

That was just a quick glance at the WLambda syntax and semantics.

More details for the syntax and the provided global functions
can be found in the [WLambda Language Reference](prelude/index.html#wlambda-reference).

Currently there are many more examples in the test cases in `tests/language.rs`.

## API Usage Examples

### Basic API Usage

Here is how you can quickly evaluate a piece of WLambda code:

```rust
let s = "$[1,2,3]";
let r = wlambda::compiler::eval(&s).unwrap();
println!("Res: {}", r.s());
```

### More Advanced API Usage

If you want to quickly add some of your own functions,
you can use the GlobalEnv `add_func` method:

```rust
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

### Maintaining state

```rust
use wlambda::*;

let mut ctx = EvalContext::new_default();

ctx.eval("!x = 10").unwrap();

ctx.set_global_var("y", &VVal::Int(32));

let r = ctx.eval("x + y").unwrap();

assert_eq!(r.s(), "42");
```

## Possible Roadmap

There are several things that can be added more or less easily to
WLambda. But I am currently working on making the language more
complete for real world use. So my current goals are:

- Improve and further document the VVal API for interacting with WLambda.
- Improve reference documentation.
- DONE: Add proper module support (via !@import and !@export).
- DONE: Add prototyped inheritance for OOP paradigm.
- DONE: Replace compiler and closure based evaluator with a VM
and more or less clever code generator.

## License

This project is licensed under the GNU General Public License Version 3 or
later.

### Conversion to MIT / Apache-2.0

I (WeirdConstructor) herby promise to release WLambda under MIT / Apache-2.0
license if you use it in an open source / free software game (licensed under
MIT and/or Apache-2.0) written in Rust (and WLambda) with a playable beta
release, non trivial amount of content and enough gameplay to keep me occupied
for at least 2 hours. You may use WLambda for your release as if it was
released under MIT and/or Apache-2.0. Proper attribution as required by MIT
and/or Apache-2.0.

### Why GPL?

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

### If you need a permissive or private license (MIT)

Please contact me if you need a different license and really want to use
my code. As long as I am the only author, I can change the license.
We might find an agreement.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in WLambda by you, shall be licensed as GPLv3 or later,
without any additional terms or conditions.

## Author

* Weird Constructor <weirdconstructor@gmail.com> (WeirdConstructor on GitHub)
  (You may find me as `WeirdConstructor` on the Rust Discord.)

## Contributors

* Cedric Hutchings <cedhut02@gmail.com> (cedric-h on GitHub)

