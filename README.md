<img align="left" width="60" height="60" src="http://m8geil.de/data/git/wlambda/res/wlambda_logo_60.png">

WLambda - Embeddable Scripting Language for Rust
================================================

This crate provides a small and simple embeddable scripting language.
Its syntax gravitates around functions and argument composition for functions.
A core concept is that everything is callable. It could be viewed as LISP
without parenthesis, or as a mixture of Perl, JavaScript and LISP/Scheme.

Here are some of its properties:

- Simple but unique syntax. For a reference look at the [WLambda Language Reference](https://docs.rs/wlambda/newest/wlambda/prelude/index.html#wlambda-reference) and the [parser](https://docs.rs/wlambda/newest/wlambda/parser/index.html).
- Easily embeddable into Rust programs due to a simple API.
- It's about getting things done quickly, so performance is not a main priority.
  Current performance is roughly in the ball park of (C)Python. Which means,
  it's too slow if you need speed. But fast enough if you do the heavy
  lifting (if required) in Rust.
- No garbage collector. Memory and resource management relies only on reference counting.
You can create your own drop functions.
- Main data structures are Lists and Maps.
- No exceptions, except panics. Error handling is accomplished
by a specialized data type. It can be thought of as dynamic counterpart
of Rust's Result type.
- Prototyped object orientation.
- Easy maintenance of the implementation.
- Custom user data implementation using [VValUserData](https://docs.rs/wlambda/newest/wlambda/vval/trait.VValUserData.html).

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
$true {
    std:displayln "It's true!";
} {
    std:displayln "It's false!";
};
```

```wlambda
!x = 10 / 2;

(x == 5) {
    std:displayln "x == 5";
};
```

### While

```wlambda
!x = 10;

while { x > 0 } {
    std:displayln x;

    (x == 5) {
        break[];
    };
    .x = x - 1;
};
```

```wlambda
!x = 10;

!r = while { x > 0 } {
    std:displayln x;

    (x == 5) {
        # break is a function, first arg
        # is the return value for `while`:
        break 5;
    };
    .x = x - 1;
};

std:assert_eq r 5;
```

### Counting Loop

```wlambda
range 1 10 1 {
    std:displayln "> " _;
};
```

With named counting variable:

```wlambda
range 1 10 1 {!(i) = @;     # or just `!i = _`
    std:displayln "> " i;
};
```

### Endless loop

```wlambda
!x = 10;

while $true {
    std:displayln x;
    .x = x - 1;
    (x == 0) break;
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
    (x > 10) {
        return :ret_label_a x * 2;
    };
};

std:assert_eq (test 11) 22;
```

### Arrays

```wlambda
!v = $[1, 2, 3];
v.1 = 5;

std:assert_eq v.1 5;

std:assert_eq (std:pop v) 3;
std:assert_eq (std:pop v) 5;
std:assert_eq (std:pop v) 1;
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

!stuff = "æ—¥æœ¬äºº";
std:assert_eq stuff.0 "æ—¥";         # Unicode support
```

### Unicode identifiers:

```wlambda
!äºº = "jin";

std:assert_eq äºº "jin";
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
    _ + _1  # Arguments are not named, they are put into _, _1, _2
};

a_func[2, 3];   # Function call
a_func 2 3;     # Equivalent function call

# Shortened one statement function definition:
!do_something_to = \_ * 2;

# There is no `if` statement. Booleans can be called
# with two arguments. The first one is called when the boolean
# is true, the second one is called when the boolean is false.
(a == 10) {
    # called if a == 10
} {
    # called if a != 10
};

# Counting loop:
!sum = $&0; # Defining a reference that can be assignment
            # from inside a function.

# `range` calls the given function for each iteration
# and passes the counter as first argument in `_`
range 0 10 1 { # This is a regular function.
    .*sum = $*sum + _; # $* is a dereferencing operator
                       # and .* starts a reference assignment
};

# `range` loop with `break`
!break_value = range 0 10 1 {
    (_ == 5) { break 22 };
};

# Returning early from functions:
!some_fun = \:some_fun_lbl { # \:xxx defines a function label for returning
    !x = 10;
    .x = do_something_to x;
    (x > 20) {
        return :some_fun_lbl 20; # explicit argument for return returns from
                                 # the specified block.
    }
    .x = 20;
    x
};

# `return` implicitly jumps to the topmost $nul label
# you may specify a small unused label like `_` to jump out some unnamed func:
!some_fun = {
    !(x) = @;
    (x == 20) \:_{ return 30 } # returns from some_fun, not from the if-branch
};

# Error reporting:
    # There are special error values, that will make the program panic
    # if they are not handled correctly at statement block level:
    !some_erroring_func = {
        return $error "An error happened!"
    };
    !value = some_erroring_func[];
    # on_error calls the first argument if the second argument
    # is an error value.
    on_error {
        # handle error here, eg. report, or make a new error value
        !(err_value, line, col, file) = @;
        std:displayln err_value;
    } value;

    !handle_err = { std:displayln _ };

    # with the ~ operator, you can chain it nicely:
    on_error {|| handle_err[_] } ~ some_erroring_func[];
    # or without ~:
    on_error {|| handle_err[_] } (some_erroring_func[]);
    # or with |
    some_erroring_func[] | on_error {|| handle_err[_] };

    # _? transforms an error value, and returns it from the current
    #    function. optionally jumping outwards.

    std:assert_eq (str ~ std:to_ref ~ {
        _? ~ $e "ok"; # is with an error value the same as: `return $e "ok"`
    }[]) "$&&$e[98,17:<wlambda::eval>(Err)] \"ok\"";

    _? 10; # passes the value through

!report_my_error = { std:displayln _ };

!some_erroring_func = {
    on_error {
        report_my_error _;
    } block :outer {
        # do something...
        (_ != 10) {
            return :outer $error "Something really failed"
            # same as, with the difference, that _? only returns
            # from :outer if it is an error value.
            _? :outer $error "Something really failed"
        }
        # do more ...
    }
    # cleanup ...
};

# Basic closure OOP:
# $& to make any closure capture of some_obj a weak reference, so
# we don't get any cyclic references:
!some_obj = $&${};
some_obj.do_something = {
    # do something here with some_obj captured (weakly)
    # from the upper lexical scope.
};
some_obj.do_something[]; # Method call

# Basic prototyped OOP:
!some_class = ${
    new = {
        ${
            _proto = $self,
            a = 10,
        }
    },
    bang = {
        std:str:cat "bang!" _ ":" $self.a
    },
};

!o = some_class.new[];
!r = o.bang 22;
std:assert_eq r "bang!22:10";
```

Currently there are many more examples in the test cases in `compiler.rs`.

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
WLambda, but I am currently working on making the language more
complete for real world use. So my current goals are:

- Improve and further document the VVal API for interacting with WLambda.
- Improve reference documentation.
- DONE: Add proper module support (via !@import and !@export).
- DONE: Add prototyped inheritance for OOP paradigm.
- There are currently no plans to change the internal evaluator
from a closure tree to a VM and/or JIT speedup.
However, help is appreciated if someone is able to significantly speed up the
evaluation without too much breakage.

## License

This project is licensed under the GNU General Public License Version 3 or
later.

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

## Authors

* Weird Constructor <weirdconstructor@gmail.com>
  (You may find me as `WeirdConstructor` on the Rust Discord.)

< i m g   a l i g n = " l e f t "   w i d t h = " 6 0 "   h e i g h t = " 6 0 "   s r c = " h t t p : / / m 8 g e i l . d e / d a t a / g i t / w l a m b d a / r e s / w l a m b d a _ l o g o _ 6 0 . p n g " >  
  
 W L a m b d a   -   E m b e d d a b l e   S c r i p t i n g   L a n g u a g e   f o r   R u s t  
 = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  
  
 T h i s   c r a t e   p r o v i d e s   a   s m a l l   a n d   s i m p l e   e m b e d d a b l e   s c r i p t i n g   l a n g u a g e .  
 I t s   s y n t a x   g r a v i t a t e s   a r o u n d   f u n c t i o n s   a n d   a r g u m e n t   c o m p o s i t i o n   f o r   f u n c t i o n s .  
 A   c o r e   c o n c e p t   i s   t h a t   e v e r y t h i n g   i s   c a l l a b l e .   I t   c o u l d   b e   v i e w e d   a s   L I S P  
 w i t h o u t   p a r e n t h e s i s ,   o r   a s   a   m i x t u r e   o f   P e r l ,   J a v a S c r i p t   a n d   L I S P / S c h e m e .  
  
 H e r e   a r e   s o m e   o f   i t s   p r o p e r t i e s :  
  
 -   S i m p l e   b u t   u n i q u e   s y n t a x .   F o r   a   r e f e r e n c e   l o o k   a t   t h e   [ W L a m b d a   L a n g u a g e   R e f e r e n c e ] ( h t t p s : / / d o c s . r s / w l a m b d a / n e w e s t / w l a m b d a / p r e l u d e / i n d e x . h t m l # w l a m b d a - r e f e r e n c e )   a n d   t h e   [ p a r s e r ] ( h t t p s : / / d o c s . r s / w l a m b d a / n e w e s t / w l a m b d a / p a r s e r / i n d e x . h t m l ) .  
 -   E a s i l y   e m b e d d a b l e   i n t o   R u s t   p r o g r a m s   d u e   t o   a   s i m p l e   A P I .  
 -   T h e   l a n g u a g e   i s   a b o u t   g e t t i n g   t h i n g s   d o n e   q u i c k l y ,   s o   p e r f o r m a n c e   i s   n o t   a   m a i n   p r i o r i t y .  
     C u r r e n t   p e r f o r m a n c e   i s   r o u g h l y   i n   t h e   b a l l   p a r k   o f   ( C ) P y t h o n ,   w h i c h   m e a n s  
     t h e   l a n g u a g e   i s   q u i t e   p o s s i b l y   t o o   s l o w   w h e r e   s p e e d   i s   t h e   f o c u s ,   b u t   f a s t   e n o u g h   i f  
     y o u   d o   a n y   h e a v y   l i f t i n g   i n   R u s t .  
 -   N o   g a r b a g e   c o l l e c t o r .   M e m o r y   a n d   r e s o u r c e   m a n a g e m e n t   r e l i e s   o n l y   o n   r e f e r e n c e   c o u n t i n g .  
 Y o u   c a n   c r e a t e   y o u r   o w n   d r o p   f u n c t i o n s .  
 -   M a i n   d a t a   s t r u c t u r e s   a r e   L i s t s   a n d   M a p s .  
 -   N o   e x c e p t i o n s ,   e x c e p t   p a n i c s .   E r r o r   h a n d l i n g   i s   a c c o m p l i s h e d  
 b y   a   s p e c i a l i z e d   d a t a   t y p e .   I t   c a n   b e   t h o u g h t   o f   a s   d y n a m i c   c o u n t e r p a r t  
 o f   R u s t ' s   R e s u l t   t y p e .  
 -   P r o t o t y p e d   o b j e c t   o r i e n t a t i o n .  
 -   E a s y   m a i n t e n a n c e   o f   t h e   i m p l e m e n t a t i o n .  
 -   C u s t o m   u s e r   d a t a   i m p l e m e n t a t i o n   u s i n g   [ V V a l U s e r D a t a ] ( h t t p s : / / d o c s . r s / w l a m b d a / n e w e s t / w l a m b d a / v v a l / t r a i t . V V a l U s e r D a t a . h t m l ) .  
  
 T h e   e m b e d d i n g   A P I   a n d   a l l   i n t e r n a l   o p e r a t i o n s   r e l y   o n   a   d a t a   s t r u c t u r e  
 m a d e   o f   [ V V a l ] ( h t t p s : / / d o c s . r s / w l a m b d a / n e w e s t / w l a m b d a / v v a l / i n d e x . h t m l )   n o d e s .  
  
 H e r e   y o u   c a n   f i n d   t h e   [ W L a m b d a   L a n g u a g e   R e f e r e n c e ] ( p r e l u d e / i n d e x . h t m l # w l a m b d a - r e f e r e n c e ) .  
  
 # #   A P I   H e l l o   W o r l d  
  
 ` ` ` r u s t  
 u s e   w l a m b d a : : * ;  
  
 m a t c h   w l a m b d a : : c o m p i l e r : : e v a l ( " 4 0   +   2 " )   {  
         O k ( v )     = >   {   p r i n t l n ! ( " O u t p u t :   { } " ,   v . s ( ) ) ;   } ,  
         E r r ( e )   = >   {   e p r i n t l n ! ( " E r r o r :   { } " ,   e ) ;   } ,  
 }  
 ` ` `  
  
 S e e   f u r t h e r   d o w n   b e l o w   f o r   m o r e   A P I   u s a g e   e x a m p l e s !  
  
 # #   W L a m b d a   L a n g u a g e   G u i d e  
  
 # # #   V a r i a b l e s  
  
 ` ` ` w l a m b d a  
 ! x   =   1 0 ;                 #   V a r i a b l e   d e f i n i t i o n  
  
 . x   =   2 0 ;                 #   V a r i a b l e   a s s i g n m e n t  
 ` ` `  
  
 # # #   O p e r a t o r s  
  
 ` ` ` w l a m b d a  
 ! x   =   ( 1   +   2 )   *   ( 8   -   4 )   /   2 ;  
  
 s t d : a s s e r t _ e q   x   6 ;  
 ` ` `  
  
 # # #   I f  
  
 ` ` ` w l a m b d a  
 $ t r u e   {  
         s t d : d i s p l a y l n   " I t ' s   t r u e ! " ;  
 }   {  
         s t d : d i s p l a y l n   " I t ' s   f a l s e ! " ;  
 } ;  
 ` ` `  
  
 ` ` ` w l a m b d a  
 ! x   =   1 0   /   2 ;  
  
 ( x   = =   5 )   {  
         s t d : d i s p l a y l n   " x   = =   5 " ;  
 } ;  
 ` ` `  
  
 # # #   W h i l e  
  
 ` ` ` w l a m b d a  
 ! x   =   1 0 ;  
  
 w h i l e   {   x   >   0   }   {  
         s t d : d i s p l a y l n   x ;  
  
         ( x   = =   5 )   {  
                 b r e a k [ ] ;  
         } ;  
         . x   =   x   -   1 ;  
 } ;  
 ` ` `  
  
 ` ` ` w l a m b d a  
 ! x   =   1 0 ;  
  
 ! r   =   w h i l e   {   x   >   0   }   {  
         s t d : d i s p l a y l n   x ;  
  
         ( x   = =   5 )   {  
                 #   b r e a k   i s   a   f u n c t i o n ,   f i r s t   a r g  
                 #   i s   t h e   r e t u r n   v a l u e   f o r   ` w h i l e ` :  
                 b r e a k   5 ;  
         } ;  
         . x   =   x   -   1 ;  
 } ;  
  
 s t d : a s s e r t _ e q   r   5 ;  
 ` ` `  
  
 # # #   C o u n t i n g   L o o p  
  
 ` ` ` w l a m b d a  
 r a n g e   1   1 0   1   {  
         s t d : d i s p l a y l n   " >   "   _ ;  
 } ;  
 ` ` `  
  
 W i t h   n a m e d   c o u n t i n g   v a r i a b l e :  
  
 ` ` ` w l a m b d a  
 r a n g e   1   1 0   1   { ! ( i )   =   @ ;           #   o r   j u s t   ` ! i   =   _ `  
         s t d : d i s p l a y l n   " >   "   i ;  
 } ;  
 ` ` `  
  
 # # #   E n d l e s s   l o o p  
  
 ` ` ` w l a m b d a  
 ! x   =   1 0 ;  
  
 w h i l e   $ t r u e   {  
         s t d : d i s p l a y l n   x ;  
         . x   =   x   -   1 ;  
         ( x   = =   0 )   b r e a k ;  
 } ;  
 ` ` `  
  
 # # #   F u n c t i o n s  
  
 ` ` ` w l a m b d a  
 ! a d d   =   {   _   +   _ 1   } ;     #   a r g u m e n t   n a m e s   _ ,   _ 1 ,   _ 2 ,   . . .  
  
 ! r e s u l t   =   a d d   2   3 ;  
  
 s t d : a s s e r t _ e q   r e s u l t   5 ;  
 ` ` `  
  
 D i f f e r e n t   f u n c t i o n   c a l l   s y n t a x e s :  
  
 ` ` ` w l a m b d a  
 ! a d d   =   { ! ( x ,   y )   =   @ ;         #   n a m e d   v a r i a b l e s ,   @   e v a l s   t o   l i s t   o f   a l l   a r g s  
         x   +   y  
 } ;  
  
 s t d : d i s p l a y l n [ a d d [ 2 ,   3 ] ] ;       #   [ ]   p a r e n t h e s i s   c a l l i n g   s y n t a x  
  
 s t d : d i s p l a y l n   a d d [ 2 ,   3 ] ;         #   l e s s   p a r e n t h e s i s  
  
 s t d : d i s p l a y l n   ( a d d   2   3 ) ;         #   e x p l i c i t   e x p r e s s i o n   d e l i m i t i n g   w i t h   ` (   . . .   ) `  
  
 s t d : d i s p l a y l n   ~   a d d   2   3 ;         #   ` ~ `   m e a n s :   e v a l u a t e   r e s t   a s   o n e   e x p r e s s i o n  
 ` ` `  
  
 # # # #   R e t u r n i n g   f r o m   n e s t e d   f u n c t i o n s :  
  
 ` ` ` w l a m b d a  
  
 ! t e s t   =   \ : r e t _ l a b e l _ a   { ! ( x )   =   @ ;  
  
         #   a n   ` i f `   i s   a c t u a l l y   a   c a l l   t o   a n o t h e r   f u n c t i o n ,   s o   w e   n e e d   t o  
         #   d y n a m i c a l l y   j u m p   u p w a r d s   t h e   c a l l   s t a c k   t o   t h e   g i v e n   l a b e l :  
         ( x   >   1 0 )   {  
                 r e t u r n   : r e t _ l a b e l _ a   x   *   2 ;  
         } ;  
 } ;  
  
 s t d : a s s e r t _ e q   ( t e s t   1 1 )   2 2 ;  
 ` ` `  
  
 # # #   A r r a y s  
  
 ` ` ` w l a m b d a  
 ! v   =   $ [ 1 ,   2 ,   3 ] ;  
 v . 1   =   5 ;  
  
 s t d : a s s e r t _ e q   v . 1   5 ;  
  
 s t d : a s s e r t _ e q   ( s t d : p o p   v )   3 ;  
 s t d : a s s e r t _ e q   ( s t d : p o p   v )   5 ;  
 s t d : a s s e r t _ e q   ( s t d : p o p   v )   1 ;  
 ` ` `  
  
 # # #   H a s h   t a b l e s / m a p s  
  
 ` ` ` w l a m b d a  
 ! m   =   $ {   a   =   1 0 ,   c   =   2   } ;  
  
 m . b   =   m . a   +   m . c ;  
  
 s t d : a s s e r t _ e q   m . b   1 2 ;  
 ` ` `  
  
 # # #   S t r i n g s  
  
 ` ` ` w l a m b d a  
 ! n a m e   =   " M r .   X " ;  
  
 s t d : a s s e r t _ e q   n a m e . 4   " X " ;                       #   i n d e x   a   c h a r a c t e r  
 s t d : a s s e r t _ e q   ( n a m e   0   3 )   " M r . " ;           #   s u b s t r i n g  
  
 ! s t u f f   =   " µ ù Ñ µ £ ¼ £Q%Q%" ;  
 s t d : a s s e r t _ e q   s t u f f . 0   " µ ù Ñ " ;                   #   U n i c o d e   s u p p o r t  
 ` ` `  
  
 # # #   U n i c o d e   i d e n t i f i e r s :  
  
 ` ` ` w l a m b d a  
 ! £Q%Q%  =   " j i n " ;  
  
 s t d : a s s e r t _ e q   £Q%Q%  " j i n " ;  
 ` ` `  
  
 # # #   O b j e c t   O r i e n t e d   P r o g r a m m i n g   w i t h   p r o t o t y p e s  
  
 ` ` ` w l a m b d a  
 ! M y C l a s s   =   $ {  
         n e w   =   {  
                 $ {  
                         _ p r o t o   =   $ s e l f ,  
                         _ d a t a   =   $ {   b a l a n c e   =   0 ,   }  
                 }  
         } ,  
         d e p o s i t   =   {  
                 $ d a t a . b a l a n c e   =   $ d a t a . b a l a n c e   +   _ ;  
         } ,  
 } ;  
  
 ! a c c o u n t 1   =   M y C l a s s . n e w [ ] ;  
  
 a c c o u n t 1 . d e p o s i t   1 0 0 ;  
 a c c o u n t 1 . d e p o s i t   5 0 ;  
  
 s t d : a s s e r t _ e q   a c c o u n t 1 . _ d a t a . b a l a n c e   1 5 0 ;  
 ` ` `  
  
 # # #   O b j e c t   O r i e n t e d   P r o g r a m m i n g   w i t h   c l o s u r e s  
  
 ` ` ` w l a m b d a  
  
 ! M y C l a s s   =   {  
         ! s e l f   =   $ {   b a l a n c e   =   0 ,   } ;  
  
         s e l f . d e p o s i t   =   {   s e l f . b a l a n c e   =   s e l f . b a l a n c e   +   _ ;   } ;  
  
         $ : s e l f  
 } ;  
  
 ! a c c o u n t 1   =   M y C l a s s [ ] ;  
  
 a c c o u n t 1 . d e p o s i t   1 0 0 ;  
 a c c o u n t 1 . d e p o s i t   5 0 ;  
  
 s t d : a s s e r t _ e q   a c c o u n t 1 . b a l a n c e   1 5 0 ;  
 ` ` `  
  
 # # #   M o d u l e s  
  
 ` ` ` t x t  
 #   u t i l . w l :  
 ! @ i m p o r t   s t d   s t d ;  
 ! @ w l a m b d a ;  
  
 ! @ e x p o r t   p r i n t _ t e n   =   {   s t d : d i s p l a y l n   ~   s t r   1 0 ;   } ;  
 ` ` `  
  
 F o r   i m p o r t   y o u   d o :  
  
 ` ` ` t x t  
 ! @ i m p o r t   u   u t i l ;  
  
 u : p r i n t _ t e n [ ]  
 ` ` `  
  
 # #   E x a m p l e   W L a m b d a   C o d e  
  
 J u s t   a   q u i c k   g l a n c e   a t   t h e   W L a m b d a   s y n t a x   a n d   s e m a n t i c s .  
  
 M o r e   d e t a i l s   f o r   t h e   s y n t a x   a n d   t h e   p r o v i d e d   g l o b a l   f u n c t i o n s  
 c a n   b e   f o u n d   i n   t h e   [ W L a m b d a   L a n g u a g e   R e f e r e n c e ] ( p r e l u d e / i n d e x . h t m l # w l a m b d a - r e f e r e n c e ) .  
  
 ` ` ` w l a m b d a  
 #   T h i s   i s   a   c o m m e n t  
  
 #   D e f i n i t i o n :  
 ! a   =   1 0 ;  
  
 #   A s s i g n m e n t :  
 . a   =   2 0 ;  
  
 #   L i s t   v a r i a b l e   d e f i n i t i o n :  
 ! a _ l i s t   =   $ [ 1 ,   2 ,   3 ,   4 ] ;  
  
 #   M a p   a s s i g n m e n t :  
 ! a _ m a p   =   $ { a   =   1 0 ,   b   =   2 0 } ;  
  
 #   F u n c t i o n   d e f i n i t i o n / a s s i g n m e n t :  
 ! a _ f u n c   =   {  
         _   +   _ 1     #   A r g u m e n t s   a r e   n o t   n a m e d ,   t h e y   a r e   p u t   i n t o   _ ,   _ 1 ,   _ 2  
 } ;  
  
 a _ f u n c [ 2 ,   3 ] ;       #   F u n c t i o n   c a l l  
 a _ f u n c   2   3 ;           #   E q u i v a l e n t   f u n c t i o n   c a l l  
  
 #   S h o r t e n e d   o n e   s t a t e m e n t   f u n c t i o n   d e f i n i t i o n :  
 ! d o _ s o m e t h i n g _ t o   =   \ _   *   2 ;  
  
 #   T h e r e   i s   n o   ` i f `   s t a t e m e n t .   B o o l e a n s   c a n   b e   c a l l e d  
 #   w i t h   t w o   a r g u m e n t s .   T h e   f i r s t   o n e   i s   c a l l e d   w h e n   t h e   b o o l e a n  
 #   i s   t r u e ,   t h e   s e c o n d   o n e   i s   c a l l e d   w h e n   t h e   b o o l e a n   i s   f a l s e .  
 ( a   = =   1 0 )   {  
         #   c a l l e d   i f   a   = =   1 0  
 }   {  
         #   c a l l e d   i f   a   ! =   1 0  
 } ;  
  
 #   C o u n t i n g   l o o p :  
 ! s u m   =   $ & 0 ;   #   D e f i n i n g   a   r e f e r e n c e   t h a t   c a n   b e   a s s i g n m e n t  
                         #   f r o m   i n s i d e   a   f u n c t i o n .  
  
 #   ` r a n g e `   c a l l s   t h e   g i v e n   f u n c t i o n   f o r   e a c h   i t e r a t i o n  
 #   a n d   p a s s e s   t h e   c o u n t e r   a s   f i r s t   a r g u m e n t   i n   ` _ `  
 r a n g e   0   1 0   1   {   #   T h i s   i s   a   r e g u l a r   f u n c t i o n .  
         . * s u m   =   $ * s u m   +   _ ;   #   $ *   i s   a   d e r e f e r e n c i n g   o p e r a t o r  
                                               #   a n d   . *   s t a r t s   a   r e f e r e n c e   a s s i g n m e n t  
 } ;  
  
 #   ` r a n g e `   l o o p   w i t h   ` b r e a k `  
 ! b r e a k _ v a l u e   =   r a n g e   0   1 0   1   {  
         ( _   = =   5 )   {   b r e a k   2 2   } ;  
 } ;  
  
 #   R e t u r n i n g   e a r l y   f r o m   f u n c t i o n s :  
 ! s o m e _ f u n   =   \ : s o m e _ f u n _ l b l   {   #   \ : x x x   d e f i n e s   a   f u n c t i o n   l a b e l   f o r   r e t u r n i n g  
         ! x   =   1 0 ;  
         . x   =   d o _ s o m e t h i n g _ t o   x ;  
         ( x   >   2 0 )   {  
                 r e t u r n   : s o m e _ f u n _ l b l   2 0 ;   #   e x p l i c i t   a r g u m e n t   f o r   r e t u r n   r e t u r n s   f r o m  
                                                                   #   t h e   s p e c i f i e d   b l o c k .  
         }  
         . x   =   2 0 ;  
         x  
 } ;  
  
 #   ` r e t u r n `   i m p l i c i t l y   j u m p s   t o   t h e   t o p m o s t   $ n u l   l a b e l  
 #   y o u   m a y   s p e c i f y   a   s m a l l   u n u s e d   l a b e l   l i k e   ` _ `   t o   j u m p   o u t   s o m e   u n n a m e d   f u n c :  
 ! s o m e _ f u n   =   {  
         ! ( x )   =   @ ;  
         ( x   = =   2 0 )   \ : _ {   r e t u r n   3 0   }   #   r e t u r n s   f r o m   s o m e _ f u n ,   n o t   f r o m   t h e   i f - b r a n c h  
 } ;  
  
 #   E r r o r   r e p o r t i n g :  
         #   T h e r e   a r e   s p e c i a l   e r r o r   v a l u e s ,   t h a t   w i l l   m a k e   t h e   p r o g r a m   p a n i c  
         #   i f   t h e y   a r e   n o t   h a n d l e d   c o r r e c t l y   a t   s t a t e m e n t   b l o c k   l e v e l :  
         ! s o m e _ e r r o r i n g _ f u n c   =   {  
                 r e t u r n   $ e r r o r   " A n   e r r o r   h a p p e n e d ! "  
         } ;  
         ! v a l u e   =   s o m e _ e r r o r i n g _ f u n c [ ] ;  
         #   o n _ e r r o r   c a l l s   t h e   f i r s t   a r g u m e n t   i f   t h e   s e c o n d   a r g u m e n t  
         #   i s   a n   e r r o r   v a l u e .  
         o n _ e r r o r   {  
                 #   h a n d l e   e r r o r   h e r e ,   e g .   r e p o r t ,   o r   m a k e   a   n e w   e r r o r   v a l u e  
                 ! ( e r r _ v a l u e ,   l i n e ,   c o l ,   f i l e )   =   @ ;  
                 s t d : d i s p l a y l n   e r r _ v a l u e ;  
         }   v a l u e ;  
  
         ! h a n d l e _ e r r   =   {   s t d : d i s p l a y l n   _   } ;  
  
         #   w i t h   t h e   ~   o p e r a t o r ,   y o u   c a n   c h a i n   i t   n i c e l y :  
         o n _ e r r o r   { | |   h a n d l e _ e r r [ _ ]   }   ~   s o m e _ e r r o r i n g _ f u n c [ ] ;  
         #   o r   w i t h o u t   ~ :  
         o n _ e r r o r   { | |   h a n d l e _ e r r [ _ ]   }   ( s o m e _ e r r o r i n g _ f u n c [ ] ) ;  
         #   o r   w i t h   |  
         s o m e _ e r r o r i n g _ f u n c [ ]   |   o n _ e r r o r   { | |   h a n d l e _ e r r [ _ ]   } ;  
  
         #   _ ?   t r a n s f o r m s   a n   e r r o r   v a l u e ,   a n d   r e t u r n s   i t   f r o m   t h e   c u r r e n t  
         #         f u n c t i o n .   o p t i o n a l l y   j u m p i n g   o u t w a r d s .  
  
         s t d : a s s e r t _ e q   ( s t r   ~   s t d : t o _ r e f   ~   {  
                 _ ?   ~   $ e   " o k " ;   #   i s   w i t h   a n   e r r o r   v a l u e   t h e   s a m e   a s :   ` r e t u r n   $ e   " o k " `  
         } [ ] )   " $ & & $ e [ 9 8 , 1 7 : < w l a m b d a : : e v a l > ( E r r ) ]   \ " o k \ " " ;  
  
         _ ?   1 0 ;   #   p a s s e s   t h e   v a l u e   t h r o u g h  
  
 ! r e p o r t _ m y _ e r r o r   =   {   s t d : d i s p l a y l n   _   } ;  
  
 ! s o m e _ e r r o r i n g _ f u n c   =   {  
         o n _ e r r o r   {  
                 r e p o r t _ m y _ e r r o r   _ ;  
         }   b l o c k   : o u t e r   {  
                 #   d o   s o m e t h i n g . . .  
                 ( _   ! =   1 0 )   {  
                         r e t u r n   : o u t e r   $ e r r o r   " S o m e t h i n g   r e a l l y   f a i l e d "  
                         #   s a m e   a s ,   w i t h   t h e   d i f f e r e n c e ,   t h a t   _ ?   o n l y   r e t u r n s  
                         #   f r o m   : o u t e r   i f   i t   i s   a n   e r r o r   v a l u e .  
                         _ ?   : o u t e r   $ e r r o r   " S o m e t h i n g   r e a l l y   f a i l e d "  
                 }  
                 #   d o   m o r e   . . .  
         }  
         #   c l e a n u p   . . .  
 } ;  
  
 #   B a s i c   c l o s u r e   O O P :  
 #   $ &   t o   m a k e   a n y   c l o s u r e   c a p t u r e   o f   s o m e _ o b j   a   w e a k   r e f e r e n c e ,   s o  
 #   w e   d o n ' t   g e t   a n y   c y c l i c   r e f e r e n c e s :  
 ! s o m e _ o b j   =   $ & $ { } ;  
 s o m e _ o b j . d o _ s o m e t h i n g   =   {  
         #   d o   s o m e t h i n g   h e r e   w i t h   s o m e _ o b j   c a p t u r e d   ( w e a k l y )  
         #   f r o m   t h e   u p p e r   l e x i c a l   s c o p e .  
 } ;  
 s o m e _ o b j . d o _ s o m e t h i n g [ ] ;   #   M e t h o d   c a l l  
  
 #   B a s i c   p r o t o t y p e d   O O P :  
 ! s o m e _ c l a s s   =   $ {  
         n e w   =   {  
                 $ {  
                         _ p r o t o   =   $ s e l f ,  
                         a   =   1 0 ,  
                 }  
         } ,  
         b a n g   =   {  
                 s t d : s t r : c a t   " b a n g ! "   _   " : "   $ s e l f . a  
         } ,  
 } ;  
  
 ! o   =   s o m e _ c l a s s . n e w [ ] ;  
 ! r   =   o . b a n g   2 2 ;  
 s t d : a s s e r t _ e q   r   " b a n g ! 2 2 : 1 0 " ;  
 ` ` `  
  
 C u r r e n t l y   t h e r e   a r e   m a n y   m o r e   e x a m p l e s   i n   t h e   t e s t   c a s e s   i n   ` c o m p i l e r . r s ` .  
  
 # #   A P I   U s a g e   E x a m p l e s  
  
 # # #   B a s i c   A P I   U s a g e  
  
 H e r e   i s   h o w   y o u   c a n   q u i c k l y   e v a l u a t e   a   p i e c e   o f   W L a m b d a   c o d e :  
  
 ` ` ` r u s t  
 l e t   s   =   " $ [ 1 , 2 , 3 ] " ;  
 l e t   r   =   w l a m b d a : : c o m p i l e r : : e v a l ( & s ) . u n w r a p ( ) ;  
 p r i n t l n ! ( " R e s :   { } " ,   r . s ( ) ) ;  
 ` ` `  
  
 # # #   M o r e   A d v a n c e d   A P I   U s a g e  
  
 I f   y o u   w a n t   t o   q u i c k l y   a d d   s o m e   o f   y o u r   o w n   f u n c t i o n s ,  
 y o u   c a n   u s e   t h e   G l o b a l E n v   ` a d d _ f u n c `   m e t h o d :  
  
 ` ` ` r u s t  
 u s e   w l a m b d a : : v v a l : : { V V a l ,   V V a l F u n ,   E n v } ;  
  
 l e t   g l o b a l _ e n v   =   w l a m b d a : : G l o b a l E n v : : n e w _ d e f a u l t ( ) ;  
 g l o b a l _ e n v . b o r r o w _ m u t ( ) . a d d _ f u n c (  
         " m y _ c r a z y _ a d d " ,  
         | e n v :   & m u t   E n v ,   _ a r g c :   u s i z e |   {  
                 O k ( V V a l : : I n t (  
                             e n v . a r g ( 0 ) . i ( )   *   1 1  
                         +   e n v . a r g ( 1 ) . i ( )   *   1 3  
                 ) )  
         } ,   S o m e ( 2 ) ,   S o m e ( 2 ) ) ;  
  
 l e t   m u t   c t x   =   w l a m b d a : : c o m p i l e r : : E v a l C o n t e x t : : n e w ( g l o b a l _ e n v ) ;  
  
 / /   P l e a s e   n o t e ,   y o u   c a n   a l s o   a d d   f u n c t i o n s   l a t e r   o n ,  
 / /   b u t   t h i s   t i m e   d i r e c t l y   t o   t h e   E v a l C o n t e x t :  
  
 c t x . s e t _ g l o b a l _ v a r (  
         " m y _ c r a z y _ m u l " ,  
         & V V a l F u n : : n e w _ f u n ( | e n v :   & m u t   E n v ,   _ a r g c :   u s i z e |   {  
               O k ( V V a l : : I n t (  
                     ( e n v . a r g ( 0 ) . i ( )   +   1 1 )  
                 *   ( e n v . a r g ( 1 ) . i ( )   +   1 3 ) ) )  
         } ,   S o m e ( 2 ) ,   S o m e ( 2 ) ,   f a l s e ) ) ;  
  
  
 l e t   r e s _ a d d   :   V V a l   =   c t x . e v a l ( " m y _ c r a z y _ a d d   2   4 " ) . u n w r a p ( ) ;  
 a s s e r t _ e q ! ( r e s _ a d d . i ( ) ,   7 4 ) ;  
  
 l e t   r e s _ m u l   :   V V a l   =   c t x . e v a l ( " m y _ c r a z y _ m u l   2   4 " ) . u n w r a p ( ) ;  
 a s s e r t _ e q ! ( r e s _ m u l . i ( ) ,   2 2 1 ) ;  
 ` ` `  
  
 # # #   M a i n t a i n i n g   s t a t e  
  
 ` ` ` r u s t  
 u s e   w l a m b d a : : * ;  
  
 l e t   m u t   c t x   =   E v a l C o n t e x t : : n e w _ d e f a u l t ( ) ;  
  
 c t x . e v a l ( " ! x   =   1 0 " ) . u n w r a p ( ) ;  
  
 c t x . s e t _ g l o b a l _ v a r ( " y " ,   & V V a l : : I n t ( 3 2 ) ) ;  
  
 l e t   r   =   c t x . e v a l ( " x   +   y " ) . u n w r a p ( ) ;  
  
 a s s e r t _ e q ! ( r . s ( ) ,   " 4 2 " ) ;  
 ` ` `  
  
 # #   P o s s i b l e   R o a d m a p  
  
 T h e r e   a r e   s e v e r a l   t h i n g s   t h a t   c a n   b e   a d d e d   m o r e   o r   l e s s   e a s i l y   t o  
 W L a m b d a .   B u t   I   a m   c u r r e n t l y   w o r k i n g   o n   m a k i n g   t h e   l a n g u a g e   m o r e  
 c o m p l e t e   f o r   r e a l   w o r l d   u s e .   S o   m y   c u r r e n t   g o a l s   a r e :  
  
 -   I m p r o v e   a n d   f u r t h e r   d o c u m e n t   t h e   V V a l   A P I   f o r   i n t e r a c t i n g   w i t h   W L a m b d a .  
 -   I m p r o v e   r e f e r e n c e   d o c u m e n t a t i o n .  
 -   D O N E :   A d d   p r o p e r   m o d u l e   s u p p o r t   ( v i a   ! @ i m p o r t   a n d   ! @ e x p o r t ) .  
 -   D O N E :   A d d   p r o t o t y p e d   i n h e r i t a n c e   f o r   O O P   p a r a d i g m .  
 -   T h e r e   a r e   c u r r e n t l y   n o   p l a n s   t o   c h a n g e   t h e   i n t e r n a l   e v a l u a t o r  
 f r o m   a   c l o s u r e   t r e e   t o   a   V M   a n d / o r   J I T   s p e e d u p .  
 H o w e v e r ,   h e l p   i s   a p p r e a c h i a t e d   i f   s o m e o n e   i s   a b l e   t o   s i g n i f i c a n t l y   s p e e d   u p   t h e  
 e v a l u a t i o n   w i t h o u t   t o o   m u c h   b r e a k a g e .  
  
 # #   L i c e n s e  
  
 T h i s   p r o j e c t   i s   l i c e n s e d   u n d e r   t h e   G N U   G e n e r a l   P u b l i c   L i c e n s e   V e r s i o n   3   o r  
 l a t e r .  
  
 # # #   W h y   G P L ?  
  
 P i c k i n g   a   l i c e n s e   f o r   m y   c o d e   b o t h e r e d   m e   f o r   a   l o n g   t i m e .   I   r e a d   m a n y  
 d i s c u s s i o n s   a b o u t   t h i s   t o p i c .   R e a d   t h e   l i c e n s e   e x p l a n a t i o n s .   A n d   d i s c u s s e d  
 t h i s   m a t t e r   w i t h   o t h e r   d e v e l o p e r s .  
  
 F i r s t   a b o u t   _ w h y   I   w r i t e   c o d e   f o r   f r e e _   a t   a l l :  
  
 -   I t ' s   m y   p a s s i o n   t o   w r i t e   c o m p u t e r   p r o g r a m s .   I n   m y   f r e e   t i m e   I   c a n  
 w r i t e   t h e   c o d e   I   w a n t ,   w h e n   I   w a n t   a n d   t h e   w a y   I   w a n t .   I   c a n   f r e e l y  
 a l l o c a t e   m y   t i m e   a n d   f r e e l y   c h o o s e   t h e   p r o j e c t s   I   w a n t   t o   w o r k   o n .  
 -   T o   h e l p   a   f r i e n d   o r   m e m b e r   o f   m y   f a m i l y .  
 -   T o   s o l v e   a   p r o b l e m   I   h a v e .  
  
 T h o s e   a r e   t h e   r e a s o n s   w h y   I   w r i t e   c o d e   f o r   f r e e .   N o w   t h e   r e a s o n s  
 _ w h y   I   p u b l i s h   t h e   c o d e _ ,   w h e n   I   c o u l d   a s   w e l l   k e e p   i t   t o   m y s e l f :  
  
 -   S o   t h a t   i t   m a y   b r i n g   v a l u e   t o   u s e r s   a n d   t h e   f r e e   s o f t w a r e   c o m m u n i t y .  
 -   S h o w   m y   w o r k   a s   a n   a r t i s t .  
 -   T o   g e t   i n t o   c o n t a c t   w i t h   o t h e r   d e v e l o p e r s .  
 -   A n d   i t ' s   a   n i c e   c h a n g e   t o   p u t   s o m e   m o r e   p o l i s h   o n   m y   p r i v a t e   p r o j e c t s .  
  
 M o s t   o f   t h o s e   r e a s o n s   d o n ' t   y e t   j u s t i f y   G P L .   T h e   m a i n   p o i n t   o f   t h e   G P L ,   a s   f a r  
 a s   I   u n d e r s t a n d :   T h e   G P L   m a k e s   s u r e   t h e   s o f t w a r e   s t a y s   f r e e   s o f t w a r e   u n t i l  
 e t e r n i t y .   T h a t   t h e   u s e r   o f   t h e   s o f t w a r e   a l w a y s   s t a y s   i n   c o n t r o l .   T h a t   t h e   u s e r s  
 h a v e   _ a t   l e a s t   t h e   m e a n s _   t o   a d a p t   t h e   s o f t w a r e   t o   n e w   p l a t f o r m s   o r   u s e   c a s e s .  
 E v e n   i f   t h e   o r i g i n a l   a u t h o r s   d o n ' t   m a i n t a i n   t h e   s o f t w a r e   a n y m o r e .  
 I t   u l t i m a t e l y   p r e v e n t s   _ " v e n d o r   l o c k   i n " _ .   I   r e a l l y   d i s l i k e   v e n d o r   l o c k   i n ,  
 e s p e c i a l l y   a s   d e v e l o p e r .   E s p e c i a l l y   a s   d e v e l o p e r   I   w a n t   a n d   n e e d   t o   s t a y  
 i n   c o n t r o l   o f   t h e   c o m p u t e r s   I   u s e .  
  
 A n o t h e r   p o i n t   i s ,   t h a t   m y   w o r k   h a s   a   v a l u e .   I f   I   g i v e   a w a y   m y   w o r k   w i t h o u t  
 _ a n y _   s t r i n g s   a t t a c h e d ,   I   e f f e c t i v e l y   w o r k   f o r   f r e e .   W o r k   f o r   f r e e   f o r  
 c o m p a n i e s .   I   w o u l d   c o m p r o m i s e   t h e   p r i c e   I   c a n   d e m a n d   f o r   m y   s k i l l ,   w o r k f o r c e  
 a n d   t i m e .  
  
 T h i s   m a k e s   t w o   r e a s o n s   f o r   m e   t o   c h o o s e   t h e   G P L :  
  
 1 .   I   d o   n o t   w a n t   t o   s u p p o r t   v e n d o r   l o c k   i n   s c e n a r i o s .   A t   l e a s t   n o t   f o r   f r e e .  
       I   w a n t   t o   p r e v e n t   t h o s e   w h e n   I   h a v e   a   c h o i c e .  
       A n d   b e f o r e   y o u   a s k ,   y e s   I   w o r k   f o r   a   c o m p a n y   t h a t   s e l l s   c l o s e d   s o u r c e  
       s o f t w a r e .   I   a m   n o t   h a p p y   a b o u t   t h e   c l o s e d   s o u r c e   f a c t .  
       B u t   i t   p a y s   m y   b i l l s   a n d   g i v e s   m e   t h e   f r e e d o m   t o   w r i t e   f r e e   s o f t w a r e  
       i n   m y   f r e e   t i m e .  
 2 .   I   d o n ' t   w a n t   t o   l o w   b a l l   m y   o w n   w a g e   a n d   p r i c e s   b y   g i v i n g   a w a y   f r e e   s o f t w a r e  
       w i t h   n o   s t r i n g s   a t t a c h e d   ( f o r   c o m p a n i e s ) .  
  
 # # #   I f   y o u   n e e d   a   p e r m i s s i v e   o r   p r i v a t e   l i c e n s e   ( M I T )  
  
 P l e a s e   c o n t a c t   m e   i f   y o u   n e e d   a   d i f f e r e n t   l i c e n s e   a n d   r e a l l y   w a n t   t o   u s e  
 m y   c o d e .   A s   l o n g   a s   I   a m   t h e   o n l y   a u t h o r ,   I   c a n   c h a n g e   t h e   l i c e n s e .  
 W e   m i g h t   f i n d   a n   a g r e e m e n t .  
  
 # #   C o n t r i b u t i o n  
  
 U n l e s s   y o u   e x p l i c i t l y   s t a t e   o t h e r w i s e ,   a n y   c o n t r i b u t i o n   i n t e n t i o n a l l y   s u b m i t t e d  
 f o r   i n c l u s i o n   i n   W L a m b d a   b y   y o u ,   s h a l l   b e   l i c e n s e d   a s   G P L v 3   o r   l a t e r ,  
 w i t h o u t   a n y   a d d i t i o n a l   t e r m s   o r   c o n d i t i o n s .  
  
 # #   A u t h o r s  
  
 *   W e i r d   C o n s t r u c t o r   < w e i r d c o n s t r u c t o r @ g m a i l . c o m >  
     ( Y o u   m a y   f i n d   m e   a s   ` W e i r d C o n s t r u c t o r `   o n   t h e   R u s t   D i s c o r d . )  
  
 