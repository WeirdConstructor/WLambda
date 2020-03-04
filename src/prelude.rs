// Copyright (c) 2020 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!
This module defines some default functions and operations
available in the WLambda language.

You there are two WLambda modules provided by this module:

- [core_symbol_table()](fn.core_symbol_table.html)
- [std_symbol_table()](fn.std_symbol_table.html)

# WLambda Reference

WLambda is a functional programming language. The main goal of this
implementation is the extension of Rust applications with dynamic scripting.
The syntax gravitates around the concept that everything is callable like a
function. There is special syntax for composing arguments of functions, to give
the programmer the ability to express his thoughts as they see fit.

You can use this document as reference or as cover to cover lecture. It starts
out with functions and the base data types of WLambda, where I also explain
some semantics of the language.

Please note: I expect you to know how to program and be familiar with at least
one other dynamic language like _JavaScript_, _Perl_ or at least _Python_. The
syntax and semantics of WLambda are different from what you might know. Think
of it more like a LISP without parenthesis. The syntax is loosely inspired from
Smalltalk, LISP and Perl.

**Table Of Contents:**

- [1](#1-syntax) - Syntax
- [2](#2-functions-part-12) - Functions (part 1/2)
  - [2.1](#21-closures) - Closures
    - [2.1.1](#211-object-oriented-programming-with-closures) - Object Oriented Programming with Closures
  - [2.2](#22-function-calling) - Function calling
  - [2.3](#23-function-arity-checks) - Function arity checks
  - [2.4](#24-calling-fields--method-calling) - Calling fields / Method calling
    - [2.4.1](#241-object-oriented-programming-with-prototypes) - Object Oriented Programming with Prototypes
- [3](#3-data-types) - Data Types
  - [3.1](#31-none-sentinel-value-n-or-none) - None sentinel value: `$n` or `$none`
  - [3.2](#32-error-values-e-expr-or-error-expr) - Error values: `$e expr` or `$error expr`
    - [3.2.1](#321-return-on-error-with-) - Return on error with `_?`
    - [3.2.2](#322-handle-errors-with-onerror) - Handle errors with `on_error`
  - [3.3](#33-booleans) - Booleans
  - [3.4](#34-64-bit-integers) - 64-Bit Integers
  - [3.5](#35-64-bit-floats) - 64-Bit Floats
  - [3.6](#36-strings) - Strings
  - [3.7](#37-bytes-or-byte-vectors) - Bytes (or Byte Vectors)
    - [3.7.1](#371-call-properties-of-bytes) - Call Properties of Bytes
    - [3.7.2](#372-byte-conversion-functions) - Byte Conversion Functions
  - [3.8](#38-symbols) - Symbols
  - [3.9](#39-vectors-or-lists) - Vectors (or Lists)
    - [3.9.1](#391-splicing) - Splicing
  - [3.10](#310-associative-maps-or-string-to-value-mappings) - Associative Maps (or String to Value mappings)
    - [3.10.1](#3101-splicing) - Splicing
  - [3.11](#311-references) - References
    - [3.11.1](#3111-weaken-references) - Weaken References
    - [3.11.2](#3112-strengthening-references) - Strengthening References
  - [3.12](#312-calling-semantics-of-data-types) - Calling Semantics of Data Types
- [4](#4-functions-part-22) - Functions (part 2/2)
  - [4.1](#41-function-call-composition) - Function call composition
    - [4.1.1](#411--tail-argument-function-chaninig) - '|' Tail Argument Function Chaninig
    - [4.1.2](#412--left-hand-function-chaining) - '|>' Left Hand Function Chaining
  - [4.2](#42-control-flow---returning) - Control Flow - Returning
  - [4.3](#43-conditional-execution---if--then--else) - Conditional Execution - if / then / else
  - [4.4](#44-iteration) - Iteration
- [5](#5-lexical-scope-and-variable-assignment) - Lexical Scope and Variable assignment
- [6](#6-arithmetic) - Arithmetic
    - [6.1](#61--operand-1-operand-2-) - + _operand-1_ _operand-2_ ...
    - [6.1.1](#611---operand-1-operand-2-) - - _operand-1_ _operand-2_ ...
    - [6.1.2](#612--op-a-op-b) - * _op-a_ _op-b_
    - [6.1.3](#613--op-a-op-b) - / _op-a_ _op-b_
    - [6.1.4](#614--op-a-op-b) - % _op-a_ _op-b_
    - [6.1.5](#615--op-a-op-b) - ^ _op-a_ _op-b_
- [7](#7-comparison) - Comparison
    - [7.1](#71--op-a-op-b) - == _op-a_ _op-b_
    - [7.1.1](#711--op-a-op-b) - != _op-a_ _op-b_
    - [7.1.2](#712--op-a-op-b) - < _op-a_ _op-b_
    - [7.1.3](#713--op-a-op-b) - <= _op-a_ _op-b_
    - [7.1.4](#714--op-a-op-b) - > _op-a_ _op-b_
    - [7.1.5](#715--op-a-op-b) - >= _op-a_ _op-b_
- [8](#8-bit-operations) - Bit Operations
    - [8.1](#81--op-a-op-b) - & _op-a_ _op-b_
    - [8.1.1](#811--op-a-op-b) - &^ _op-a_ _op-b_
    - [8.1.2](#812--op-a-op-b) - &| _op-a_ _op-b_
    - [8.1.3](#813--op-a-op-b) - << _op-a_ _op-b_
    - [8.1.4](#814--op-a-op-b) - >> _op-a_ _op-b_
- [9](#9-modules) - Modules
  - [9.1](#91-export) - export
  - [9.2](#92-import) - import
- [10](#10-core-library) - Core Library
    - [10.1](#101--label-value) - _? [_label_] _value_
- [11](#11-standard-library) - Standard Library
    - [11.1](#111-stdshuffle-randfunc-vec) - std:shuffle _rand_func_ _vec_
    - [11.1.1](#1111-stdcopy-vecormap) - std:copy _vec_or_map_
    - [11.1.2](#1112-stdsort-comparefun-vec) - std:sort [_compare_fun_] _vec_
    - [11.1.3](#1113-stddisplayln-arg1-) - std:displayln _arg1_ ...
    - [11.1.4](#1114-stdwriteln-arg1-) - std:writeln _arg1_ ...
    - [11.1.5](#1115-stdstrwrite-arg) - std:str:write _arg_
    - [11.1.6](#1116-stdeval-code-string) - std:eval _code-string_
    - [11.1.7](#1117-stdassert-bool-message) - std:assert _bool_ \[_message_]
    - [11.1.8](#1118-stdasserteq-actual-expected-message) - std:assert_eq _actual_ _expected_ \[_message_]
  - [11.2](#112-io) - I/O
    - [11.2.1](#1121-stdiofilereadtext-filename) - std:io:file:read_text _filename_
    - [11.2.2](#1122-stdiofileread-filename) - std:io:file:read _filename_
    - [11.2.3](#1123-stdiofilewritesafe-filename-bytes-or-string) - std:io:file:write_safe _filename_ _bytes-or-string_
    - [11.2.4](#1124-stdiofileappend-filename-bytes-or-string) - std:io:file:append _filename_ _bytes-or-string_
- [12](#12-optional-standard-library) - Optional Standard Library
  - [12.1](#121-serialization) - serialization
    - [12.1.1](#1211-stdserjson-data-nopretty) - std:ser:json _data_ \[_no_pretty_]
    - [12.1.2](#1212-stddeserjson-string) - std:deser:json _string_
    - [12.1.3](#1213-stdsercsv-fielddelim-rowseparator-escapeall-table) - std:ser:csv _field_delim_ _row_separator_ _escape_all_ _table_
    - [12.1.4](#1214-stddesercsv-fielddelim-rowseparator-data) - std:deser:csv _field_delim_ _row_separator_ _data_
    - [12.1.5](#1215-stdsermsgpack-data) - std:ser:msgpack _data_
    - [12.1.6](#1216-stddesermsgpack-bytes) - std:deser:msgpack _bytes_
  - [12.2](#122-regex) - regex
  - [12.3](#123-chrono) - chrono
    - [12.3.1](#1231-stdchronotimestamp-format) - std:chrono:timestamp \[_format_]
  - [12.4](#124-hash) - hash
    - [12.4.1](#1241-stdhashfnv1a-arg1-) - std:hash:fnv1a _arg1_ ...
  - [12.5](#125-rand) - rand
    - [12.5.1](#1251-stdrandsplitmix64new) - std:rand:split_mix64_new
    - [12.5.2](#1252-stdrandsplitmix64newfrom-seed) - std:rand:split_mix64_new_from _seed_
    - [12.5.3](#1253-stdrandsplitmix64next-smstate-count) - std:rand:split_mix64_next _sm_state_ \[_count_]
    - [12.5.4](#1254-stdrandsplitmix64nextopen01-smstate-count) - std:rand:split_mix64_next_open01 _sm_state_ \[_count_]

-----

## <a name="1-syntax"></a>1 - Syntax

A more formal introduction to the syntax can be found in the [parser API documentation](../parser/index.html).

## <a name="2-functions-part-12"></a>2 - Functions (part 1/2)

A function can be defined using the `{ ... }` syntax and the `\ _statement_`
syntax: To give functions a name, you need to assign them to a variable with
the `!_name_ = _expr_` syntax.

### <a name="21-closures"></a>2.1 - Closures

Functions take values from the outer scope by copying their value:

```wlambda
!a = 10;
!b = 20;

!add_a_and_b = { a + b }; # function copies the values 10 and 20

!result = add_a_and_b[];

std:assert_eq result 30;
```

This also means, that functions can not modify the values of
the scope they were created in. To do that, you need a referential
data type, that is described further down this document.

Here is an example how we would write the above example by mutating
the value in the `result` variable:

```wlambda
!a = 10;
!b = 20;
!result = $& $none; # Create a weakly captured reference

# function copies the values 10 and 20
# but result is captured by reference. As the weakable reference
# type `$&` is used, it's only weakly captured.
!add_a_and_b = { .result = a + b; };

add_a_and_b[];

std:assert_eq $*result 30; # $* dereferences referential types
```

About the weakly capturing of `result`:
It means, that if the outer reference value in `result` goes
out of scope, the reference in the closure does
not keep it alive. This is important to prevent cyclic refences
where closures keep captured values unneccessarily alive.

You will also need this to make referential types such as maps `${ }`
and vectors `$[ ]` weakly referenced by closures for OOP.

#### <a name="211-object-oriented-programming-with-closures"></a>2.1.1 - Object Oriented Programming with Closures

This is how you can use a map data type as object which stores
methods:

```wlambda
!new_Cat = {!(name) = @;
    !self = ${
        name = name,
    };

    # Captures refer to the value in the `self` reference
    # weakly now. `self` has been converted implicit to a _weakable_
    # `$&` reference.
    self.meow     = { std:displayln self.name " meows!"; };
    self.get_name = { self.name };

    # Because access to _weakable_ references is always implicitly
    # dereferenced we need the `$:` capture reference operator to
    # prevent the reference to the map in `self` from being freed
    # once the `new_Cat` function returns:
    $:self
};

!my_cat = new_Cat "Spot";

my_cat.meow[]; # Prints 'Spot meows!'

std:assert_eq my_cat.get_name[] "Spot";
```

Alternatively you can just make the cat name private:

```wlambda
!new_Cat = {!(name) = @;
    # Make a strong reference, so the closures DO keep cat_name alive!
    # This does not make cycles, because name does not store a closure.
    !cat_name = $&& name;

    !meow     = { std:displayln cat_name " meows!"; };
    !get_name = { $*cat_name };
    !set_name = { .*cat_name = _; };

    # Just holds the methods
    ${
        meow     = meow,
        get_name = get_name,
        set_name = set_name,
    };
};

!my_cat = new_Cat "Spot";

my_cat.meow[]; # Prints 'Spot meows!'

std:assert_eq my_cat.get_name[] "Spot";

my_cat.set_name "Spotty";

std:assert_eq my_cat.get_name[] "Spotty";
```

### <a name="22-function-calling"></a>2.2 - Function calling

To call functions, you have at least 3 alternatives. First is the bare
`_expr_ arg1 arg2 arg3 arg4` syntax. And the second is the fully delimited
variant: `_expr_[arg1, arg2, arg3, ...]`. You can always delimit the first
variant using the `( ... )` parenthesis around the whole call,
i.e. `(_expr_ arg1 arg2 arg3 arg4)`.
Third you can call a function with a vector as argument with `_expr_[[_expr_]]`,
where the second expression should return a vector (if it doesn't it will use the
value as first argument).

Here are examples:

```wlambda
# All the second variant:
std:assert_eq[std:str:cat[1, 2, 3], "123"];

# Can also be written as:
std:assert_eq (std:str:cat 1 2 3) "123";

# As the third variant:
!some_args = $[1, 2, 3];
std:assert_eq std:str:cat[[some_args]] "123";
```

The arguments passed to the function are accessible using the `_`, `_1`, `_2`, ..., `_9`
variables. If you need to access more arguments the `@` variable holds a vector of all
arguments.

```wlambda
!twoify = { _ * 2 };

std:assert_eq twoify[2] 4;

!twoify2 = \_ * 2;

std:assert_eq twoify2[2] 4;

# You may also call them directly, notice the parenthesis ( ... ) syntax
# for delimiting the inner function call:
std:assert_eq ({ _ * 2 } 2) 4;
```

If you want to name arguments, you can use the destructuring assignment
syntax:

```wlamdba
!add = {!(a, b) = @;
    a + b
};

std:assert_eq add[1, 2] 3;
```

### <a name="23-function-arity-checks"></a>2.3 - Function arity checks

Functions check the number of arguments passed to them. The compiler tries to
infer the number of arguments the function requires by looking at the parameter
variables `_` to `_9` and `@`. If the compiler gets it wrong, you can:

* Define minimum and maximum number of arguments with: `{|min < max| ... }`
* Define exact number of arguments with: `{|num_of_args| ... }`
* Accept any number of arguments: `{|| ... }`

For the shortened function syntax there is:

* `\|min < max| ...`
* `\|num_of_args| ...`
* `\|| ...`

Here an example:

```wlambda
!dosomething = {|2 < 4| !(a, b, c, d) = @;
    # Please note: We have to assign the
    # parameters to named values here, because
    # the arms of the conditional below have
    # their own set of arguments.

    (is_none c) { a + b } { a * b + c * d }
};

std:assert_eq dosomething[1, 2]         3;
std:assert_eq dosomething[2, 2, 3, 4]  16;
```

### <a name="24-calling-fields--method-calling"></a>2.4 - Calling fields / Method calling

If you use the '.' for accessing fields in a map,
the object the most recent field is accessed of is passed
to the called function. The object the function/method
was called upon can be accessed using the special value '$self'.

```wlambda
!some_map = ${
    some_func = { $self.a_value },
    a_value = 11,
};

std:assert_eq some_map.some_func[] 11;
```

This in combination with the special key `'_proto'` can be used to
implement a basic form of object orientation with prototype inheritance.

It can also be combined with the closure OOP approach or used for
other purposes.

You can also use a vector/list as object, in that case the `_proto`
field that holds the class method map is the first element of the
vector. The second element of the vector can be accessed using `$data`.

#### <a name="241-object-oriented-programming-with-prototypes"></a>2.4.1 - Object Oriented Programming with Prototypes

Instead of using closures for OOP the preferred way is to use
maps of functions as classes and form an inheritance hierarchy
by using the `'_proto'` key of a map:

```wlambda
!class_a = ${
    # $self is set by any key access using the '.' calling form:
    new = { ${ _proto = $self } },
    generate = { "I am A" },  # A method
};

!a_instance = class_a.new[];

std:assert_eq a_instance.generate[] "I am A";
```

The special key `'_data'` can be used (and is encouraged to be used)
as storage for data members of your objects. This is useful to separate
method name space inside objects from the data member namespace.
To quickly access the data members you can use the special value `$data`,
which will evaluate to `$self._data` in case `$self` is a map, and
to `$self.1` in case `$self` is a vector.

Here is an example with a map and data:

```wlambda
!class_b = ${
    new = {
        ${
            _proto = $self, # $self is class_b
            _data = ${
                a = 10
            },
        }
    },
    gen  = { _ * $data.a },     # $data is equivalent to `$self._data` here
    gen2 = { _ * $self._data.a },
};

!inst = class_b.new[];

std:assert_eq inst.gen[2] 20;
std:assert_eq inst.gen2[2] 20;
```

You can also use vectors as objects, which can be beneficial as they are
a bit slimmer and access to `_proto` and `_data` are reduced to a single
vector index lookup instead of an array lookup.

```wlambda
!class_b = ${
    new = {
        $[  # return a vector
            $self, # $self is class_b
            ${ a = 10 },
        ]
    },
    gen  = { _ * $data.a },     # $data is equivalent to `$self.1` here
    gen2 = { _ * $self.1.a },
};

!inst = class_b.new[];

std:assert_eq inst.gen[3] 30;
std:assert_eq inst.gen2[4] 40;
```

## <a name="3-data-types"></a>3 - Data Types

### <a name="31-none-sentinel-value-n-or-none"></a>3.1 - None sentinel value: `$n` or `$none`

This is a special sentinel value that is returned by functions and
when a non existing field of a datastructure is accessed. It's semantic
meaning is that there is no value.

Most functions that expect a string value will turn a `$none` into an
empty string. If you need an unambigous representation use `std:str:write`
for dumping WLambda data structures.

Please note for API design: In case of errornous states you should not
return a `$none` but an `$error` value.

```wlambda
std:assert ~ $n                == $none;
std:assert ~ int[$n]           == 0;
std:assert ~ float[$n]         == 0.0;
std:assert ~ str[$n]           == "";
std:assert ~ std:str:write[$n] == "$n";
std:assert ~ is_none[$n];
```

### <a name="32-error-values-e-expr-or-error-expr"></a>3.2 - Error values: `$e expr` or `$error expr`

There are no exceptions in WLambda, except the panic, that
halts all execution of the currently running WLambda
program. To signal errors, you return an `$error` value.

These error values, if not handled, will cause a panic of your
program. This means, you need to handle returned error values
one way or the other.

The error value wraps any value you pass to the `$error` or `$e`
constructor syntax.

```wlambda
std:assert ~ is_err ~ $e "something went wrong!"
```

There are more routines except `is_err` to handle an error.
`_?` will return from the currently executed function
up until some given label. `on_error` executes a function
if the second argument was an error value. Otherwise it
just passes through the value. `unwrap` will explicitly cause
an panic if an error value was passed to it. All other values
will be passed through. And `unwrap_err` unwraps an error value, it's
the opposite of `unwrap` because it will cause a panic if you don't pass
an error value.

Most functions don't accept errors in their arguments.
If an error is encountered, a panic will occur. There are only
a few functions that accept error values in their arguments:

- panic
- `_?`
- unwrap_err
- error_to_str
- unwrap
- on_error
- return
- break
- bool
- type
- match
- assert
- assert_eq
- is_some
- is_none
- is_err
- is_map
- is_vec
- is_fun
- is_str
- is_wref
- is_ref
- is_bool
- is_bytes
- is_sym
- is_float
- is_int
- ==
- !=
- std:to_ref
- std:ref_id
- std:write_str

All other functions don't accept errors as their argument.

#### <a name="321-return-on-error-with-"></a>3.2.1 - Return on error with `_?`

```wlambda
!func = { $e "this failed!" };

!other = {
    # some code ...

    _? func[]; # If you would not catch the error value here,
               # the program would panic, as an error value
               # must not be ignored!

    # other code ...

    panic "this will never be reached!";

    # something here...
};

std:assert ~ (unwrap_err other[]) == "this failed!";
```

`_?` can take up to 2 arguments. If so, the first argument is interpreted
as jump label. That is handy if you want to jump up multiple call frames:

```wlambda
!failing_func = { $e :FAIL };

!func = \:some_unique_label {
    ( _ == 42 ) {
        std:displayln "We got 42!";

        # The `then` branch we are currently in is a call frame.
        # To jump further up the call stack, we need the label
        # we defined for the function above.
        !val = _? :some_unique_label failing_func[];

        std:displayln "Returned:" val;
    }
};

std:assert_eq (unwrap_err ~ func 42) :FAIL;
```

#### <a name="322-handle-errors-with-onerror"></a>3.2.2 - Handle errors with `on_error`

The first parameter to `on_error` should be a function,
which will be called with four parameters.
The first of these parameters is the error text,
followed by the line number, column number and file name
from which the error originates.

```wlambda
!func = {
    (_ == 13) {
        $e "this failed!"
    } {
        "all ok!"
    }
};

!x = $&$n;

# The first function of on_error will be called with the unwrapped
# error if an error occured.
on_error {|4| .x = _; } ~ func 13;
std:assert_eq $*x "this failed!";

!ret = on_error {|4| .x = _; } ~ func 1;
std:assert_eq ret "all ok!";
```

### <a name="33-booleans"></a>3.3 - Booleans

True and false are represented by `$t` and `$f` or `$true` and `$false`,
whatever suits your coding style better.

You can either use a boolean value with one or two arguments, where `$true`
will call the first argument, and `$false` the second argument. If a second argument
isn't provided and the value is `$false`, `$none` is returned. So to
check for truthness you can just do:

```wlambda
!x = 10;
!some_num =
    (x == 10) { "it is ten" } { "it is not ten" };

std:assert_eq some_num "it is ten";

.x = 20;
.some_num =
    (x == 10) { "it is ten" } { "it is not ten" };
std:assert_eq some_num "it is not ten";
```

You can cast other values into a boolean with the `bool` function:

```wlambda
std:assert_eq (bool 1)          $true;
std:assert_eq (bool 0)          $false;
std:assert_eq (bool $e :x)      $false;
std:assert_eq (bool $n)         $false;
std:assert_eq (bool "")         $false;
std:assert_eq (bool "0")        $false;
std:assert_eq (bool "1")        $true;
std:assert_eq (bool :0)         $false;
std:assert_eq (bool :1)         $true;
std:assert_eq (bool 0.0)        $false;
std:assert_eq (bool 0.1)        $false;
std:assert_eq (bool 1.0)        $true;
std:assert_eq (bool {})         $true;
std:assert_eq (bool $b"")       $false;
std:assert_eq (bool $b"\x00")   $false;
std:assert_eq (bool $b"\x01")   $true;
```

You can also check if something is a boolean with `is_bool`:

```wlambda
std:assert ~ is_bool $true;
std:assert ~ is_bool $false;
std:assert ~ not[is_bool $n];
std:assert ~ not[is_bool ""];
std:assert ~ not[is_bool 0];
```

### <a name="34-64-bit-integers"></a>3.4 - 64-Bit Integers

### <a name="35-64-bit-floats"></a>3.5 - 64-Bit Floats

### <a name="36-strings"></a>3.6 - Strings

### <a name="37-bytes-or-byte-vectors"></a>3.7 - Bytes (or Byte Vectors)

Bytes are a special kind of strings. Their literal form is:

```wlambda
$b"abc";
$b"\xFF\xFD\x00";
$Q/ABCDEF\xFD/;      # \xFD is not an escape sequence here!
```

#### <a name="371-call-properties-of-bytes"></a>3.7.1 - Call Properties of Bytes

You can index inside a byte array by calling it with an integer:

```wlambda
std:assert_eq ($b"ABC" 1) $b"B";
```

You can extract a whole range when calling with 2 integers:

```wlambda
std:assert_eq ($b"ABCDEF" 2 3) $b"CDE";
```

If you call a bytes value with a map as argument, the bytes value is
converted to a string internally using `str` and the value from the map
is returned:

```wlambda
!some_map = ${ a = 20, b = 30 };

std:assert_eq ($b"a" some_map) 20;
std:assert_eq ($b"b" some_map) 30;

std:assert_eq some_map.$b"a" 20;   # with method call syntax
```

#### <a name="372-byte-conversion-functions"></a>3.7.2 - Byte Conversion Functions

You can convert bytes to strings in a multitude of ways:

- str _bytes_
  ```wlambda
  std:assert_eq (str $b"abc")        "abc";
  std:assert_eq (str $b"abc\xFF")    "abcÿ";
  std:assert_eq (str $Q/ABCDEF\xFD/) "ABCDEF\\xFD";
  ```
- std:bytes:to_hex _bytes_ \[_group-len_ \[_group-sep_]]
  ```wlambda
  std:assert_eq (std:bytes:to_hex $b"\xFF\x0A\xBE\xEF")
                "FF0ABEEF";
  std:assert_eq (std:bytes:to_hex $b"\xFF\x0A\xBE\xEF" 2)
                "FF 0A BE EF";
  std:assert_eq (std:bytes:to_hex $b"\xFF\x0A\xBE\xEF" 2 ":")
                "FF:0A:BE:EF";
  ```
- std:str:from_utf8 _bytes_
  ```wlambda
  std:assert_eq (std:str:from_utf8 $b"\xC3\xA4\xC3\x9F\xC3\xBF") "äßÿ";
  std:assert_eq (std:str:from_utf8 [std:str:to_bytes "äßÿ"])         "äßÿ";
  # broken UTF8 will result in an error:
  std:assert ~ is_err (std:str:from_utf8 $b"\xC3\xC3\xA4\xC3\x9F\xC3\xBF");
  ```
- std:str:from_utf8_lossy _bytes_
  ```wlambda
  std:assert_eq (std:str:from_utf8_lossy $b"\xC3\xC3\xA4\xC3\x9F\xC3\xBF") "�äßÿ";
  ```

You can even convert bytes to vectors of integers back and forth:

```wlambda
!v = std:bytes:to_vec $b"ABC";
std:assert_eq (str v) (str $[65, 66, 67]);

std:push v 64;
!b = std:bytes:from_vec v;
std:assert_eq b $b"ABC@";
```

There is also an inverse operation to `bytes:to_hex`:

```wlambda
std:assert_eq (std:bytes:from_hex ~ std:bytes:to_hex $b"ABC") $b"ABC";
```

### <a name="38-symbols"></a>3.8 - Symbols

### <a name="39-vectors-or-lists"></a>3.9 - Vectors (or Lists)

The literal syntax for vectors (or sometimes also called lists in WLambda)
is `$[...]`. You may write any kind of expression in it and you will get
a vector from it.

To access the elements of a vector you have to call a number with a vector
as first argument. The field syntax is a more convenient shorthand syntax.
The following example demonstrates it:

```wlambda
!add20 = { _ + 20 };

!some_vec = $[1, 2 * 10, add20 10]; 

# Index calling:
std:assert_eq (0 some_vec) 1;
std:assert_eq (1 some_vec) 20;
std:assert_eq (2 some_vec) 30;

# Field syntax:
std:assert_eq some_vec.0 1;
std:assert_eq some_vec.1 20;
std:assert_eq some_vec.2 30;
```

#### <a name="391-splicing"></a>3.9.1 - Splicing

You can splice vectors directly into their literal form with the `$[..., * vec_expr, ...]`
syntax. Here is an example:

```wlambda
!make_some = { $[_ + 1, _ + 2] };

!some_vec = $[ 0, *make_some 1 ];

std:assert_eq some_vec.1 2;
std:assert_eq some_vec.2 3;

# There can be any expression after the `.` if you wrap it into `(...)`:
std:assert_eq some_vec.(1 + 1) 3;

# A more direct example:
std:assert_eq (str $[1,2,*$[3,4]]) "$[1,2,3,4]";
```

### <a name="310-associative-maps-or-string-to-value-mappings"></a>3.10 - Associative Maps (or String to Value mappings)

Aside from vectors there are associative maps in WLambda. Their syntax is
`${ key = expr, ... }`. The keys of these maps have to be strings,
the values in the literals can be any expression.

You can call a symbol or a string with an associative map to get the value in
the map with the string value as key. There is also, like vectors, the field
calling syntax. Here are some examples:

```wlambda
!some_map = ${ a = 1, b = 2 };

# Symbol calling:
std:assert_eq (:a some_map) 1;
std:assert_eq (:b some_map) 2;
std:assert_eq ("a" some_map) 1;
std:assert_eq ("b" some_map) 2;

# Field syntax:
std:assert_eq some_map.a 1;
std:assert_eq some_map.b 2;

# There can be any expression after the `.` if you wrap it into `(...)`,
# also strings:
std:assert_eq some_map.("a") 1;
std:assert_eq some_map.("b") 2;
```

Keys can also be computed at runtime in the literal form:

```wlambda
!some_map = ${ (std:str:cat "a" "b") = 10 };

std:assert_eq (str some_map) "${ab=10}";
```

If you call a field that is being accessed directly using
the field accessing syntax `some_map.a`, the function is passed the map `some_map`
via the special value `$self`. There is another special variable `$data`
that allows you to access the `$self._data` field.

#### <a name="3101-splicing"></a>3.10.1 - Splicing

Like vectors you can splice map values directly into map literals:

```wlambda
!map_gen = { ${ (std:str:cat "_" _) = _ } };

!some_map = ${ a = 10, *map_gen "x" };

std:assert_eq some_map.a 10;
std:assert_eq some_map._x "x";

std:assert_eq (str ${*${a=10}}) "${a=10}";

# As a reminder, a full expression can come after the '*':

std:assert_eq (str ${*map_gen "y"}) $q/${_y="y"}/;
```

### <a name="311-references"></a>3.11 - References

Some data structures already have reference characteristics, such as strings,
vectors and maps. There are 3 types of references in WLambda that handle
different usecases. These referential types are neccessary to mutate lexical
variables from a parent scope. To give a rather natural example:

```wlambda
!x = 10;
{ .x = 20; }[];
std:assert_eq x 20;
```

The example works rather intuitively. There is however lots of implicit
referential stuff going on. Once `x` is captured by a closure it ise implicitly
changed in to a _weakable_ `$&` reference and the closure stores only a weak
reference to `x`. This is done to maintain lexical scope and prevent accidental
cyclic references when closures from a scope are leaked.

These types of references exist:

- `$&` - A _weakable_ reference, that is captured weakly by closures.
- `$(&)` - A _weak_ reference, can't be constructed literally, only indirectly
as upvalue of a closure or by `std:weaken`.
- `$&&` - A _strong_ reference, that is captured stongly by closures.
Inside closures they are also implicitly dereferenced by assignment
and access by variable name.

The weakable reference is captured weakly by closures and does not keep the
referenced value alive if the value reference count drops to zero.
The strong references will stay strong and need explicit care to handle in the
function where they are stored directly in a local variable. But if strong
references are caught, they are also implicitly handled.

```wlambda
!x = $& 10;

{ .x = 20; }[]; # Closures implicitly handle weak references

std:assert_eq x 20;
```

And the same with strong references:

```wlambda
!x = $&& 10;

.*x = 11;

{ .x = 20; }[]; # Closures implicitly handle strong references too

std:assert_eq $*x 20;
```

Strong references can also be created using the `std:to_ref` function:

```wlambda
!x = std:to_ref 10;
std:assert_eq (std:write_str x) "$&&10";
```

#### <a name="3111-weaken-references"></a>3.11.1 - Weaken References

You can weaken any of those two types of references manually using the
`std:weaken` function.

```wlambda
!drop_check = $& $f;

# Make a reference to the value 10 and set `drop_check` to $true
# when all (non weak) references to it are gone.
!x = $&& (std:to_drop 10 {|| .drop_check = $true });

# Create a weakened reference to the value referred to by x:
!y = std:weaken x;

# Deref y gives you 10:
std:assert_eq $*y 10;

# The reference to 10 is removed and this means that the weak reference
# in y is invalidated and returns $n in future.
.x = $n;

# Deref y now gives you $n:
std:assert_eq $*y $n;

std:assert drop_check;
```

#### <a name="3112-strengthening-references"></a>3.11.2 - Strengthening References

You can convert a weak reference (weakened by `std:weaken`) or a captured weak
reference `$&` to strong with `std:strengthen`.

TODO: Example

### <a name="312-calling-semantics-of-data-types"></a>3.12 - Calling Semantics of Data Types

You can call almost all basic data types of WLambda.
Here is an overview of the data type calling semantics:

| Type      | Args              | Semantics |
|-----------|-------------------|-----------|
| `$none`   | -                 | Any call to `$none` will result in a panic. |
| `$error`  | -                 | Any call to `$error` will result in a panic. |
| function  | *                 | Will call the function with the specified arguments. |
| `$true`   | `f1, f2`          | Will call `f1`.          |
| `$false`  | `f1, f2`          | Will call `f2` or return `$n` if `f2` is not provided.          |
| symbol    | map, userval      | Will retrieve the value in the map at the key equal to the symbol. |
| map       | anything          | Will call `anything` for each value and key in the map and return a list with the return values. |
|           |                   | |
|           |                   | |
|           |                   | |
|           |                   | |
|           |                   | |
|           |                   | |

## <a name="4-functions-part-22"></a>4 - Functions (part 2/2)

### <a name="41-function-call-composition"></a>4.1 - Function call composition

- chaining
- traditional () call syntax
- ~ syntax
- || syntax

>> $[] || push 10
> $[10]
>> $[] || push 10 || push 20
> $[10,20]
>> !x = { push _1 _ };
> $n
>> $[] | x 10 | x 20
> $[10,20]
>>

- [...] syntax

#### <a name="411--tail-argument-function-chaninig"></a>4.1.1 - '|' Tail Argument Function Chaninig

This syntax is useful if you have following function call composition:

    (fn arg1 arg2 (fn2 arg_b1 arg_b2 (fn3 arg_c1 arg_c2 ...)))

These can be written more comfortably like this:

    fn3 arg1 arg2 | fn2 arg_b1 arg_b2 | fn arg1 arg2

An example with actual values:

```wlambda
!x = 10 | { _ * 4 } | { _ + 2 };

std:assert_eq x 42;
```

Think of it as if the value `10` was _piped_ through the
functions on the right.

The call reordering of the `|` operator looks like this:

```text
    fn1 a1 a2 | fn2 b1 b2 (   )   =>   fn2 b1 b2 (fn1 a1 a2)
    """""""""               ^
        v                   |
        --------------------|
```

#### <a name="412--left-hand-function-chaining"></a>4.1.2 - '|>' Left Hand Function Chaining

This syntax is useful if you want to make deep call chains like these:

    (((fn arg1 arg2 ...) arg_b1 arg_b2 ...) arg_c1 arg_c2 ...)

These can be written more comfortably like this:

    fn arg1 arg2 |> arg_b1 arg_b2 |> arg_c1 arg_c2

or nicer formatted:

    fn arg1 arg2
        |> arg_b1 arg_b2
        |> arg_c1 arg_c2

Here an actual example:

```wlambda
!res =
    $[1,2,3]
    |> { _ * 2 }
    |> { _ + 3 };

std:assert_eq (str res) "$[5,7,9]";
```

The call reordering of the `|>` operator looks like this:

```text
    fn1 a1 a2 |> b1 b2    =>   ((   )   )
    """""""""    """""            ^   ^
        v          v              |   |
        -----------|--------------|   |
                   -------------------|
```
### <a name="42-control-flow---returning"></a>4.2 - Control Flow - Returning

- \:lbl { ... } syntax and returning

WLambda uses labelled blocks for control flow, as returning from the current function would not be
very helpful for the control flow in wlambda in case of conditional execution.

```wlambda
!some_func = \:outer {
    !x = 10;
# does stuff

    (x == 10) {
return :outer 20
    }

# more stuff that is not executed if x == 10.
}
```

### <a name="43-conditional-execution---if--then--else"></a>4.3 - Conditional Execution - if / then / else

WLambda has no `if`. Conditional execution is provided by the bool
data type. As in WLambda everything can be called like a function, you
can just pass other functions as arguments to `$true` and `$false`.
If you pass a function as first argument to `$true`, it will
be executed. If you pass a function as second argument to `$false` then that
will be executed.

```wlambda
(10 == 10) { std:displayln "10 is 10" };         #=> prints "10 is 10"
(10 != 10) { std:displayln "10 is not 10" };     #=> doesn't print anything

!x = 20;

(x == 20) {
    std:displayln "x is 20";
} {
    std:displayln "x is 20";
}; # Do not forget the ";"!
```

Actually, as the values `$true` and `$false` can be called like any other
function you may write it also like this, which is not the recommended
syntax, but still works:

```wlambda
(10 == 10)[{ std:displayln "10 is 10" }];

!x = 21;
(x == 20)[{ std:displayln "x is 20" }, { std:displayln "x isn't 20" }]; #=> print "x isn't 20"
```

<<<<<<< HEAD
Often, you may want to choose one variable or another based on some predicate.
For these situations, the `pick` function is available.
For example, perhaps you want to make a function which can take any number of parameters,
or a single list parameter.
```wlambda
!sum = \|| std:fold 0 { _ + _1 } ~ pick (is_vec _) _ @;
```
Booleans can also be used to index into lists.
When this is done, `$t` represents `1` and `$f` represents `0`.
This means that we can also express our `sum` function as:
```wlambda
!sum = \|| std:fold 0 { _ + _1 } $[@, _].(is_vec _);
```
When comparing the `pick` and indexing approaches it is important to note
that the two possible return values are inverted:
```
pick (x == 20) "x is 20" "x isn't 20";
$["x isn't 20", "x is 20"].(x == 20);
```
With `pick`, the value to return in the `$t` case comes first, followed by the `$f` case's value,
whereas with indexing approach, the opposite is true.

### <a name="44-iteration"></a>4.4 - Iteration

WLambda has many ways to iterate:

- Counting loop with `range`
- While some condition is `$true` with `while`
- Over the items in a vector with either `for` or by calling the vector
with a function as first argument.
- Over the items in a map with either `for` or by calling the map
with a function as first argument.
- Over the characters in a string with either `for` or by calling it
with a function.
- Over the bytes in a byte vector with either `for` or by calling it
with a function.

`for` just iterates through the value and provides the individual items as first argument to the
iteration function. But if you call the value with a function as first argument a mapping iteration
is done. That means, the return value of the operation is a list with the return values of the
iteration function. If you don't need that list you should use `for`.

## <a name="5-lexical-scope-and-variable-assignment"></a>5 - Lexical Scope and Variable assignment

- !x = y                  variable definition
- .x = y                  assignments
- !:ref x = y             upvalue references
- !:wref x = y            weak upvalue references
- !(x, y) = list / map    destructuring assignments

## <a name="6-arithmetic"></a>6 - Arithmetic

The output type (float vs. integer) of the numerical arithmetic operators is defined
by the _first_ operand of the operation. Use the casting functions `float` or
`int` if you are unsure.

Please note that not all operators are available as plain identifiers and need
to be quoted when used in their prefix form or as functions, some of them are
`*`, `/`, `%` and some others.

#### <a name="61--operand-1-operand-2-"></a>6.1 - + _operand-1_ _operand-2_ ...

This function implements arithmetic addition.  If the first operand is a
float number, the substraction will return a float result. If it is an integer
or anything else (like a string), an integer result is returned.

```wlambda
std:assert_eq (+ 5.5 0.5) 6.0;
std:assert_eq (5.5 + 0.5) 6.0;
std:assert_eq (+ 5 2) 7;
std:assert_eq (+ "5" 2) 7;
std:assert_eq (+ :5 2) 7;
```

#### <a name="611---operand-1-operand-2-"></a>6.1.1 - - _operand-1_ _operand-2_ ...

This function implements arithmetic substraction.  If the first operand is a
float number, the substraction will return a float result. If it is an integer
or anything else (like a string), an integer result is returned.

```wlambda
std:assert_eq (- 5.5 0.5) 5.0;
std:assert_eq (5.5 - 0.5) 5.0;
std:assert_eq (- 5 2) 3;
std:assert_eq (- "5" 2) 3;
std:assert_eq (- :5 2) 3;
```

#### <a name="612--op-a-op-b"></a>6.1.2 - * _op-a_ _op-b_

Returns the multiplication of the two operands.

```wlambda
std:assert 10   * 4 == 40;
std:assert 10.1 * 4 == 40.4;
std:assert "10" * 4 == 40;

std:assert (`*` 10 4) == 40;

std:assert (float "10.1") * 4 == 40.4;
```

#### <a name="613--op-a-op-b"></a>6.1.3 - / _op-a_ _op-b_

Returns the division of the two operands.

```wlambda
std:assert 10   / 4 == 2;
std:assert 10.0 / 4 == 2.5;
std:assert "10" / 2 == 5;

std:assert (`/` 10 4) == 2;

std:assert (float "10.1") * 4 == 40.4;
```

#### <a name="614--op-a-op-b"></a>6.1.4 - % _op-a_ _op-b_

Returns the remainder of the division of _op-a_ by _op-b_.

```wlambda
std:assert     5 % 4 == 1;
std:assert (`%` 5 4) == 1;
```

#### <a name="615--op-a-op-b"></a>6.1.5 - ^ _op-a_ _op-b_

Returns _op-a_ raised by the power of _op-b_.
Supports float and integers.

```wlambda
std:assert_eq 2 ^ 4     16;
std:assert_eq std:num:round[(2.0 ^ 2.1) * 1000] 4287.0;
std:assert_eq 2 ^ 2.1   4; # first arg type matters!
```

## <a name="7-comparison"></a>7 - Comparison

#### <a name="71--op-a-op-b"></a>7.1 - == _op-a_ _op-b_

Checks whether the two operands are equal to each other. Data types like
booleans, integers, floats, symbols and strings are compared by their contents.
Other types like vectors, maps, functions, errors or references are compared
by referential equality.

```wlambda
std:assert        $none == $none;
std:assert            1 == 2 - 1;
std:assert         "aa" == ("a" "a");
std:assert         :xxy == :xxy;
std:assert not ~ $[1,2] == $[1,2];

std:assert ~ `==` 1 (2 - 1); # prefix form
```

#### <a name="711--op-a-op-b"></a>7.1.1 - != _op-a_ _op-b_

Checks whether the two operands are distinct from each other.  Data types like
booleans, integers, floats, symbols and strings are compared by their contents.
Other types like vectors, maps, functions, errors or references are compared
by referential equality.

It's generally the opposite of `==`.

```wlambda
std:assert         1 != 2;
std:assert     not[2 != 2];
std:assert     "foo" != "bar";
std:assert not["foo" != "foo"];

std:assert ~ `!=` 1 2;

!r1 = $[1,2];
!r2 = $[1,2];
std:assert r1 != r2;
```

#### <a name="712--op-a-op-b"></a>7.1.2 - < _op-a_ _op-b_

Numerical comparison operator that checks whether _op-a_ is less than _op-b_

```wlambda
std:assert   10   < 11;
std:assert   10.1 < 10.2;
std:assert not[10 < 10.1];  # the type of the first argument decides return type!
```

#### <a name="713--op-a-op-b"></a>7.1.3 - <= _op-a_ _op-b_

Numerical comparison operator that checks whether _op-a_ is less or equal to _op-b_

```wlambda
std:assert 10   <= 11;
std:assert 10.1 <= 10.2;
std:assert 10   <= 10.1;  # integer <=, the type of the first argument decides return type!
```

#### <a name="714--op-a-op-b"></a>7.1.4 - > _op-a_ _op-b_

Numerical comparison operator that checks whether _op-a_ is greater than _op-b_

```wlambda
std:assert   11.1 > 11;
std:assert   11.1 > 11.0;
std:assert not[10 > 10.1];  # the type of the first argument decides return type!
```

#### <a name="715--op-a-op-b"></a>7.1.5 - >= _op-a_ _op-b_

Numerical comparison operator that checks whether _op-a_ is greater or equal to _op-b_

```wlambda
std:assert 11   >= 11;
std:assert 10.2 >= 10.1;
std:assert 10 >= 10.1;  # integer >=, the type of the first argument decides return type!
```

## <a name="8-bit-operations"></a>8 - Bit Operations

#### <a name="81--op-a-op-b"></a>8.1 - & _op-a_ _op-b_

Binary `and` operation between two integers.

```wlambda
std:assert (0b0011 & 0b1011) == 0b011;
std:assert (3      &     11) == 3;
```

#### <a name="811--op-a-op-b"></a>8.1.1 - &^ _op-a_ _op-b_

Binary `xor` operation between two integers.

```wlambda
std:assert (0b0011 &^ 0b1011) == 0b1000;
std:assert (3      &^     11) == 8;
```

#### <a name="812--op-a-op-b"></a>8.1.2 - &| _op-a_ _op-b_

Binary `or` operation between two integers.

```wlambda
std:assert (0b0011 &| 0b1000) == 0b1011;
std:assert (3      &|      8) == 11;
```

#### <a name="813--op-a-op-b"></a>8.1.3 - << _op-a_ _op-b_

Binary `left shift` operation of _op-a_ by _op-b_ bits.

```wlambda
std:assert (0b0011 << 3)   == 0b11000;
std:assert (`<<` 0b1011 2) == 0b101100
```

#### <a name="814--op-a-op-b"></a>8.1.4 - >> _op-a_ _op-b_

Binary `right shift` operation of _op-a_ by _op-b_ bits.

```wlambda
std:assert (0b0011 >> 2)      == 0b0;
std:assert (0b1100 >> 2)      == 0b11;
std:assert (`>>` 0b1011000 3) == 0b1011
```

## <a name="9-modules"></a>9 - Modules

### <a name="91-export"></a>9.1 - export

```wlambda

!expr = { _ + 30 };

!@export symbol = expr; # exports symbol with value of expr (a function)

```

### <a name="92-import"></a>9.2 - import

```wlambda

!@import x = tests:test_mod; # prefixes everything from modixes with x:

std:assert ~ (x:symbol 10) == 40;

```

You can also skip the prefix:

```wlambda
!@import std;
!v = $[];
push v 10; push v 20;
std:assert_eq (str v) "$[10,20]";
```

## <a name="10-core-library"></a>10 - Core Library

This library contains all the core functions which belong to the
core of the WLambda Programming Language. These functions can be seen
as keywords of WLambda. Some functions are also available as operators.

#### <a name="101--label-value"></a>10.1 - _? [_label_] _value_

Return from the current function up to a given _label_ if _value_ is an
error value.  If no _label_ is given only the current function is returned from
with the error value.  If there is no error, the given value is returned.

The best usecase is, if you just want to hand any errors that might be returned
further upwards the call stack for the parent functions to handle.

```wlambda
!do_fail = $false;

!maybe_fails1 = { 10 };
!maybe_fails2 = {
    do_fail { $error "something is wrong" }
            { .do_fail = $true; 2 };
};

!a = {
    !x = _? maybe_fails1[];
    .x = x + (_? maybe_fails2[]);
    x
};

!first  = a[];
!second = a[];

std:assert_eq first 12;
std:assert (is_err second);
```


## <a name="11-standard-library"></a>11 - Standard Library

#### <a name="111-stdshuffle-randfunc-vec"></a>11.1 - std:shuffle _rand_func_ _vec_

Shuffles the _vec_ in place. The function _rand_func_ needs to return
a random 64 bit integer on each call. Here is an example:

```wlambda
!sm  = std:rand:split_mix64_new_from 1234;
!vec = $[1,2,3,4,5,6,7,8];
std:shuffle { std:rand:split_mix64_next sm } vec;

std:assert_eq (str vec) "$[2,1,7,4,8,5,3,6]";
```

#### <a name="1111-stdcopy-vecormap"></a>11.1.1 - std:copy _vec_or_map_

Makes a shallow copy of the given vector or map.

```wlambda
!a = $[1,2,3];
!b = std:copy a;
b.0 = 10;

std:assert_eq a.0 1;
std:assert_eq b.0 10;
```

#### <a name="1112-stdsort-comparefun-vec"></a>11.1.2 - std:sort [_compare_fun_] _vec_

Sorts the given _vec_ in place. The comparison function _compare_fun_ gets the
two values a and b and needs to return -1 if a < b, 0 if a = b and 1 if a > b.

There are four functions that implement numeric and lexicographic ordering:

- `std:cmp:num:asc`
- `std:cmp:num:desc`
- `std:cmp:str:asc`
- `std:cmp:str:desc`

If no _compare_fun_ is given, the ordering will be ascending and lexicographic
vs. numeric will be chosen by the type of the `a` value (if it is an integer or
float it will be numeric, otherwise lexicographic).

```wlambda
!v = $[$[1], $[-1], $[3]];
std:sort { std:cmp:num:desc _.0 _1.0 } v;

std:assert_eq v.0.0 3;
std:assert_eq v.1.0 1;
std:assert_eq v.2.0 -1;
```

#### <a name="1113-stddisplayln-arg1-"></a>11.1.3 - std:displayln _arg1_ ...

This function writes a humand readable version of all the arguments
(with a space inbetween) to the standard output. This means that:

```text
std:displayln "foo"
```

Will just print `foo` and a newline.

If you need a less ambigous form, use `std:writeln`, which
handles it's argument like written via `std:str:write` instead of `str`.

#### <a name="1114-stdwriteln-arg1-"></a>11.1.4 - std:writeln _arg1_ ...

This function writes the WLambda representation of it's arguments
(with a space inbetween) to standard output. This means that:

```text
std:displayln "foo"
```

Will print `"foo"` and a newline.

See also the description of `std:str:write`.

If you need a more human readable form use `std:displayln`.

#### <a name="1115-stdstrwrite-arg"></a>11.1.5 - std:str:write _arg_

Returns the WLambda representation of the value _arg_ as string.

Most values have the same represenation like a WLambda literal,
but there are other values that don't have a literal representation.

Warning: Consider all values that don't have a fixed literal representation
in the WLambda syntax as debug output that might change in future versions.

```wlambda
std:assert_eq (std:str:write "foo") $q|"foo"|;
std:assert_eq (std:str:write $none) $q|$n|;
std:assert_eq (std:str:write $[1,:a]) $q|$[1,:"a"]|;
```

#### <a name="1116-stdeval-code-string"></a>11.1.6 - std:eval _code-string_

Evaluates _code-string_ in the current global environment and returns
the generated value. If the code leads to any kind of evaluation error,
an error object is returned.

```wlambda
std:assert_eq (std:eval "1 + 2") 3;
!:global X = 20;
std:assert_eq (std:eval "1 + X") 21;
```

#### <a name="1117-stdassert-bool-message"></a>11.1.7 - std:assert _bool_ \[_message_]

Just a simple assertion function that panics if the first argument is not true.
Returns the passed value if it is a true value.
You can pass an optional message as second parameter.

```norun_wlambda
std:assert $false; #=> Panic
std:assert 120;    #=> 120
```

#### <a name="1118-stdasserteq-actual-expected-message"></a>11.1.8 - std:assert_eq _actual_ _expected_ \[_message_]

This function check if the _actual_ value is equal to the
_expected_ value and panics if not. The optional _message_ is
passed in the panic for reference.

```wlambda
!x = 30 * 2;
std:assert_eq x 60 "30 * 2 == 60";
```

### <a name="112-io"></a>11.2 - I/O

#### <a name="1121-stdiofilereadtext-filename"></a>11.2.1 - std:io:file:read_text _filename_

Opens the file _filename_ and returns it's contents interpreted as UTF8
text as string.

```wlambda
std:io:file:write_safe "prelude_test.txt" "abcäöü";

!t = std:io:file:read_text "prelude_test.txt";
std:assert_eq t "abcäöü" "reading text from file works";
```

#### <a name="1122-stdiofileread-filename"></a>11.2.2 - std:io:file:read _filename_

Opens the file _filename_ and returns it's contents as byte buffer.

```wlambda
std:io:file:write_safe "prelude_test.txt" "abcäöü";

!t = std:io:file:read "prelude_test.txt";
.t = std:str:from_utf8 t;
std:assert_eq t "abcäöü" "reading binary from file works";
```

#### <a name="1123-stdiofilewritesafe-filename-bytes-or-string"></a>11.2.3 - std:io:file:write_safe _filename_ _bytes-or-string_

Creates a new file with the given filename but with a "~" appended
and writes the contents into it. After successful write, it renames
the file to the given filename.

#### <a name="1124-stdiofileappend-filename-bytes-or-string"></a>11.2.4 - std:io:file:append _filename_ _bytes-or-string_

Opens the given filename in append mode and appends _bytes-or-string_ to the
end of the file.

## <a name="12-optional-standard-library"></a>12 - Optional Standard Library

### <a name="121-serialization"></a>12.1 - serialization

#### <a name="1211-stdserjson-data-nopretty"></a>12.1.1 - std:ser:json _data_ \[_no_pretty_]

Serializes the _data_ and returns a JSON formatted (and pretty printed) string.
Optionally not pretty printed if _no_pretty_ is a true value.

```wlambda
!str = std:ser:json $[1,2.3,${a=4}] $t;
std:assert_eq str "[1,2.3,{\"a\":4}]";
```

#### <a name="1212-stddeserjson-string"></a>12.1.2 - std:deser:json _string_

Deserializes the JSON formatted _string_ into a data structure.

```wlambda
!data = std:deser:json ~ std:ser:json $[1,2.3,${a=4}];
std:assert_eq data.0 1;
std:assert_eq data.1 2.3;
std:assert_eq data.(2).a 4;
```

#### <a name="1213-stdsercsv-fielddelim-rowseparator-escapeall-table"></a>12.1.3 - std:ser:csv _field_delim_ _row_separator_ _escape_all_ _table_

This serializes the _table_ as CSV with the given _field_delim_
and _row_separator_. If _escape_all_ is `$true` all fields will be
put into '"'.

```wlambda
!csv_str =
    std:ser:csv
        ";" "|" $f
        $[ $[1,2,3,4,$q/foo"bar/],
           $[44,55],
           $[]]
    | std:displayln;

std:assert_eq csv_str $q/1;2;3;4;"foo""bar"|44;55||/;

std:assert_eq
    (std:ser:csv ";" "|" $f $[$[:a,$q/;/, $q/|/, $q/ /]])
    "a;\";\";\"|\";\" \"|";
```

#### <a name="1214-stddesercsv-fielddelim-rowseparator-data"></a>12.1.4 - std:deser:csv _field_delim_ _row_separator_ _data_

Parses the string _data_ as CSV. With the field delimiter _field_delim_
and the _row_separator_ for the data rows.

```wlambda
!table = std:deser:csv ";" "\r\n" "foo;bar\r\nx;y\r\n";
std:assert_eq table.0.0 "foo";
std:assert_eq table.0.1 "bar";
std:assert_eq table.1.1 "y";
```

#### <a name="1215-stdsermsgpack-data"></a>12.1.5 - std:ser:msgpack _data_

Serializes the _data_ and returns a msgpack bytes value.

```wlambda
std:assert_eq (std:ser:msgpack $b"abc") $b"\xC4\x03abc";
```

#### <a name="1216-stddesermsgpack-bytes"></a>12.1.6 - std:deser:msgpack _bytes_

Deserializes the msgpack bytes value into a data structure.

```wlambda
std:assert_eq (std:deser:msgpack $b"\xC4\x03abc") $b"abc";
```

### <a name="122-regex"></a>12.2 - regex


### <a name="123-chrono"></a>12.3 - chrono

#### <a name="1231-stdchronotimestamp-format"></a>12.3.1 - std:chrono:timestamp \[_format_]

For the documentation of _format_ please consule the
chrono Rust crate documentation: [chrono crate strftime format](https://docs.rs/chrono/latest/chrono/format/strftime/index.html#specifiers).

```wlambda
!year_str = std:chrono:timestamp "%Y";
std:displayln :XXXX ~ (year_str | int) == 2020;
std:assert ~ (year_str | int) == 2020;

!now_str = std:chrono:timestamp[];
```

### <a name="124-hash"></a>12.4 - hash

#### <a name="1241-stdhashfnv1a-arg1-"></a>12.4.1 - std:hash:fnv1a _arg1_ ...

Hashes all the arguments as FNV1a and returns an integer.

### <a name="125-rand"></a>12.5 - rand

#### <a name="1251-stdrandsplitmix64new"></a>12.5.1 - std:rand:split_mix64_new

Initializes the _sm_state_ from the current time (seconds) and returns it.
The time is retrieved in seconds, so don't expect different seed states
if you call this multiple times in the same wall clock second.
The returned value is supposed to be passed to `rand:split_mix64_next`
or `rand:split_mix64_next_open01`.

#### <a name="1252-stdrandsplitmix64newfrom-seed"></a>12.5.2 - std:rand:split_mix64_new_from _seed_

Initializes the _sm_state_ from the given _seed_ and returns it.
The returned value is supposed to be passed to `rand:split_mix64_next`
or `rand:split_mix64_next_open01`.

#### <a name="1253-stdrandsplitmix64next-smstate-count"></a>12.5.3 - std:rand:split_mix64_next _sm_state_ \[_count_]

Returns the _count_ next integer values generated from the given
_sm_state_.

#### <a name="1254-stdrandsplitmix64nextopen01-smstate-count"></a>12.5.4 - std:rand:split_mix64_next_open01 _sm_state_ \[_count_]

Returns the _count_ next float values (in an open [0, 1) interval)
generated from the given _sm_state_.

*/

const VERSION: &str = env!("CARGO_PKG_VERSION");

use crate::compiler::*;
use crate::vval::*;
use crate::util;
use std::rc::Rc;

macro_rules! func {
    ($g: ident, $name: expr, $cb: expr, $min: expr, $max: expr, $err_arg_ok: expr) => {
        $g.fun($name, $cb, $min, $max, $err_arg_ok);
    }
}

macro_rules! add_func {
    ($g: ident, $op: tt, $env: ident, $argc: ident, $b: block, $min: expr, $max: expr, $err_arg_ok: expr) => {
        $g.fun(stringify!($op), |$env: &mut Env, $argc: usize| $b, $min, $max, $err_arg_ok);
    }
}

macro_rules! add_multi_op {
    ($g: ident, $op: tt) => {
        add_func!($g, $op, env, argc, {
            if argc <= 0 { return Ok(VVal::Nul); }
            if let VVal::Flt(f) = env.arg(0) {
                let mut accum = f;
                for i in 1..argc { accum = accum $op env.arg(i).f() }
                Ok(VVal::Flt(accum))
            } else {
                let mut accum = env.arg(0).i();
                for i in 1..argc { accum = accum $op env.arg(i).i() }
                Ok(VVal::Int(accum))
            }
        }, Some(2), None, false)
    }
}

macro_rules! add_bool_bin_op {
    ($g: ident, $op: tt) => {
        add_func!($g, $op, env, argc, {
            if argc < 2 { return Ok(VVal::Nul); }
            let a = env.arg(0);
            if let VVal::Flt(af) = a { Ok(VVal::Bol(af $op env.arg(1).f())) }
            else { Ok(VVal::Bol(a.i() $op env.arg(1).i())) }
        }, Some(2), Some(2), false)
    }
}

macro_rules! add_fi_bin_op {
    ($g: ident, $op: tt, $a: ident, $b: ident, $ef: expr, $ei: expr) => {
        add_func!($g, $op, env, argc, {
            if argc < 2 { return Ok(VVal::Nul); }
            let $a = env.arg(0);
            let $b = env.arg(1);
            if let VVal::Flt(_) = $a { $ef }
            else { $ei }
        }, Some(2), Some(2), false)
    }
}

macro_rules! add_bin_op_err_ok {
    ($g: ident, $op: tt, $a: ident, $b: ident, $e: expr) => {
        add_func!($g, $op, env, argc, {
            if argc < 2 { return Ok(VVal::Nul); }
            let $a = env.arg(0);
            let $b = env.arg(1);
            $e
        }, Some(2), Some(2), true)
    }
}

macro_rules! add_sbin_op {
    ($g: ident, $op: literal, $a: ident, $b: ident, $e: expr) => {
        func!($g,
                $op, |env: &mut Env, argc: usize| {
                if argc < 2 { return Ok(VVal::Nul); }
                let $a = env.arg(0);
                let $b = env.arg(1);
                $e
                }, Some(2), Some(2), false);
    }
}

macro_rules! add_num_fun_flt {
    ($g: ident, $op: literal, $e: tt) => {
        func!($g, $op,
            |env: &mut Env, _argc: usize| {
                Ok(VVal::Flt(env.arg(0).f().$e()))
            }, Some(1), Some(1), false);
    }
}

macro_rules! add_num_fun_flt2 {
    ($g: ident, $op: literal, $e: tt) => {
        func!($g, $op,
            |env: &mut Env, _argc: usize| {
                Ok(VVal::Flt(env.arg(0).f().$e(env.arg(1).f())))
            }, Some(2), Some(2), false);
    }
}

fn match_next(env: &mut Env, val: &VVal, mut arg_idx: usize, argc: usize) -> Result<VVal, StackAction> {
    while arg_idx < argc {
        if env.arg(arg_idx).is_fun() {
            return
                env.with_restore_sp(|e: &mut Env| {
                    e.push(val.clone());
                    e.arg(arg_idx).call_internal(e, 1)
                });
        }

        let mut match_vals = vec![arg_idx];
        arg_idx += 1;
        while arg_idx < argc && !env.arg(arg_idx).is_fun() {
            match_vals.push(arg_idx);
            arg_idx += 1;
        }

        if arg_idx >= argc { return Ok(val.clone()); }

        let fun_idx = arg_idx;

        if    env.arg(match_vals[0]).is_sym()
            && env.arg(match_vals[0]).s_raw().chars().nth(0).unwrap_or('_') == '?' {

            match &env.arg(match_vals[0]).s_raw()[..] {
                "?t" => {
                    let val_type_name = val.type_name();
                    for i in match_vals.iter().skip(1) {
                        if env.arg(*i).s_raw() == val_type_name {
                            return env.arg(fun_idx).call(env, &[val.clone()]);
                        }
                    }
                },
                "?s" => {
                    let val_s = val.s_raw();
                    for i in match_vals.iter().skip(1) {
                        if env.arg(*i).s_raw() == val_s {
                            return env.arg(fun_idx).call(env, &[val.clone()]);
                        }
                    }
                },
                "?e" => {
                    if let VVal::Err(e) = val {
                        let err_val = e.borrow().0.at(0).unwrap_or_else(|| e.borrow().0.clone());

                        for i in match_vals.iter().skip(1) {
                            if env.arg(*i).eqv(&err_val) {
                                let args = vec![
                                    e.borrow().0.clone(),
                                    VVal::Int(i64::from(e.borrow().1.line)),
                                    VVal::Int(i64::from(e.borrow().1.col)),
                                    VVal::new_str(e.borrow().1.file.s()),
                                ];
                                return env.arg(fun_idx).call(env, &args);
                            }
                        }
                    }
                },
                "?p" => {
                    if fun_idx + 1 >= argc { return Ok(VVal::Nul); }
                    let fun_idx = fun_idx + 1;

                    let pred_res = env.arg(arg_idx).call(env, &[val.clone()]);
                    match pred_res {
                        Ok(v) => {
                            arg_idx += 1;
                            if v.b() {
                                return env.arg(fun_idx).call(env, &[val.clone()]);
                            }
                        },
                        Err(sa) => { return Err(sa); }
                    }
                },
                _ => {
                    // TODO: Usually we should bail out with an error here.
                }
            }
        } else {
            for i in match_vals.iter() {
                if env.arg(*i).eqv(val) {
                    return env.arg(fun_idx).call(env, &[val.clone()]);
                }
            }
        }

        arg_idx += 1;
    }

    Ok(VVal::Nul)
}

/// Returns a SymbolTable with all WLambda core language symbols.
#[allow(clippy::cast_lossless,clippy::assign_op_pattern)]
pub fn core_symbol_table() -> SymbolTable {
    let mut st = SymbolTable::new();

    add_multi_op!(st, +);
    add_multi_op!(st, -);
    add_multi_op!(st, *);
    add_multi_op!(st, /);
    add_multi_op!(st, %);

    add_bool_bin_op!(st, <);
    add_bool_bin_op!(st, >);
    add_bool_bin_op!(st, <=);
    add_bool_bin_op!(st, >=);

    add_bin_op_err_ok!(st, ==, a, b, Ok(VVal::Bol(a.eqv(&b))));
    add_bin_op_err_ok!(st, !=, a, b, Ok(VVal::Bol(!a.eqv(&b))));

    add_sbin_op!(st, "&|", a, b,
        Ok(VVal::Int(((a.i() as u32) | (b.i() as u32)) as i64)));
    add_sbin_op!(st, "&", a, b,
        Ok(VVal::Int(((a.i() as u32) & (b.i() as u32)) as i64)));
    add_sbin_op!(st, "&^", a, b,
        Ok(VVal::Int(((a.i() as u32) ^ (b.i() as u32)) as i64)));
    add_sbin_op!(st, "<<", a, b,
        Ok(VVal::Int(((a.i() as u32) << (b.i() as u32)) as i64)));
    add_sbin_op!(st, ">>", a, b,
        Ok(VVal::Int(((a.i() as u32) >> (b.i() as u32)) as i64)));

    add_fi_bin_op!(st, ^, a, b,
        Ok(VVal::Flt(a.f().powf(b.f()))),
        Ok(VVal::Int(a.i().pow(b.i() as u32))));

    func!(st, "not",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Bol(!env.arg(0).b()))
        }, Some(1), Some(1), false);

    func!(st, "panic",
        |env: &mut Env, _argc: usize| {
            Err(StackAction::panic(env.arg(0), None))
        }, Some(1), Some(1), true);

    func!(st, "block",
        |env: &mut Env, argc: usize| {
            let mut label = VVal::Nul;
            let fn_arg_idx = if argc <= 1 { 0 } else { label = env.arg(0); 1 };
            match env.arg(fn_arg_idx).call_no_args(env) {
                Ok(v)   => Ok(v),
                Err(StackAction::Return((v_lbl, v))) => {
                    if v_lbl.eqv(&label) { Ok(v) }
                    else { Err(StackAction::Return((v_lbl, v))) }
                },
                Err(e)  => Err(e),
            }
        }, Some(1), Some(2), false);

    func!(st, "_?",
        |env: &mut Env, argc: usize| {
            let mut lbl = VVal::Nul;
            let err_val = if argc > 1 {
                lbl = env.arg(0);
                env.arg(1)
            } else { env.arg(0) };

            match err_val {
                VVal::Err(e) => Err(StackAction::Return((lbl, VVal::Err(e)))),
                v            => Ok(v),
            }
        }, Some(1), Some(2), true);

    func!(st, "error_to_str",
        |env: &mut Env, _argc: usize| {
            match env.arg(0) {
                VVal::Err(_) => {
                    Ok(VVal::new_str_mv(env.arg(0).s()))
                },
                v => {
                    Err(StackAction::panic_msg(
                        format!("error_to_str on non error value: {}", v.s())))
                },
            }
        }, Some(1), Some(1), true);

    func!(st, "unwrap_err",
        |env: &mut Env, _argc: usize| {
            match env.arg(0) {
                VVal::Err(err_v) => {
                    Ok(err_v.borrow().0.clone())
                },
                v => {
                    Err(StackAction::panic_msg(
                        format!("unwrap_err on non error value: {}", v.s())))
                },
            }
        }, Some(1), Some(1), true);

    func!(st, "unwrap",
        |env: &mut Env, _argc: usize| {
            match env.arg(0) {
                VVal::Err(err_v) => {
                    Err(StackAction::panic_str(
                        format!("unwrap error: {}", err_v.borrow().0.s()),
                        Some(err_v.borrow().1.clone())))
                },
                v => Ok(v)
            }
        }, Some(1), Some(1), true);

    func!(st, "on_error",
        |env: &mut Env, _argc: usize| {
            let err_fn = env.arg(0);
            match env.arg(1) {
                VVal::Err(err_v) => {
                    env.with_restore_sp(|e: &mut Env| {
                        e.push(VVal::new_str(err_v.borrow().1.file.s()));
                        e.push(VVal::Int(err_v.borrow().1.col as i64));
                        e.push(VVal::Int(err_v.borrow().1.line as i64));
                        e.push(err_v.borrow().0.clone());
                        err_fn.call_internal(e, 4)
                    })
                },
                e => Ok(e)
            }
        }, Some(2), Some(2), true);

    func!(st, "return",
        |env: &mut Env, argc: usize| {
            if argc < 1 { return Err(StackAction::Return((VVal::Nul, VVal::Nul))); }
            if argc < 2 { return Err(StackAction::Return((VVal::Nul, env.arg(0)))); }
            Err(StackAction::Return((env.arg(0), env.arg(1))))
        }, Some(1), Some(2), true);

    func!(st, "break",
        |env: &mut Env, argc: usize| {
            if argc < 1 { return Err(StackAction::Break(VVal::Nul)); }
            Err(StackAction::Break(env.arg(0)))
        }, Some(0), Some(1), true);

    func!(st, "next",
        |_env: &mut Env, _argc: usize| {
            Err(StackAction::Next)
        }, Some(0), Some(0), false);

    func!(st, "pick",
        |env: &mut Env, _argc: usize| Ok(if env.arg(0).b() { env.arg(1) } else { env.arg(2) }),
        Some(3), Some(3), false);

    func!(st, "bool",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).b())) },
        Some(1), Some(1), true);
    func!(st, "float",
        |env: &mut Env, _argc: usize| { Ok(VVal::Flt(env.arg(0).f())) },
        Some(1), Some(1), false);
    func!(st, "int",
        |env: &mut Env, _argc: usize| { Ok(VVal::Int(env.arg(0).i())) },
        Some(1), Some(1), false);
    func!(st, "str",
        |env: &mut Env, _argc: usize| { Ok(VVal::new_str_mv(env.arg(0).s_raw())) },
        Some(1), Some(1), false);
    func!(st, "sym",
        |env: &mut Env, _argc: usize| { Ok(VVal::new_sym(&env.arg(0).s_raw())) },
        Some(1), Some(1), false);
    func!(st, "is_some",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(!env.arg(0).is_none())) },
        Some(1), Some(1), true);
    func!(st, "is_none",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_none())) },
        Some(1), Some(1), true);
    func!(st, "is_err",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_err())) },
        Some(1), Some(1), true);
    func!(st, "is_map",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_map())) },
        Some(1), Some(1), true);
    func!(st, "is_vec",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_vec())) },
        Some(1), Some(1), true);
    func!(st, "is_fun",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_fun())) },
        Some(1), Some(1), true);
    func!(st, "is_str",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_str())) },
        Some(1), Some(1), true);
    func!(st, "is_wref",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_wref())) },
        Some(1), Some(1), true);
    func!(st, "is_ref",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_ref())) },
        Some(1), Some(1), true);
    func!(st, "is_bool",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_bool())) },
        Some(1), Some(1), true);
    func!(st, "is_bytes",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_bytes())) },
        Some(1), Some(1), true);
    func!(st, "is_sym",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_sym())) },
        Some(1), Some(1), true);
    func!(st, "is_float",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_float())) },
        Some(1), Some(1), true);
    func!(st, "is_int",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_int())) },
        Some(1), Some(1), true);

    func!(st, "len",
        |env: &mut Env, _argc: usize| { Ok(VVal::Int(env.arg(0).len() as i64)) },
        Some(1), Some(1), false);

    func!(st, "type",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::new_str_mv(env.arg(0).type_name()))
        }, Some(1), Some(1), true);

    func!(st, "match",
        |env: &mut Env, argc: usize| {
            if argc < 1 { return Ok(VVal::Nul); }
            if argc == 1 { return Ok(VVal::Nul) }
            match_next(env, &env.arg(0), 1, argc)
        }, Some(1), None, true);

    func!(st, "while",
        |env: &mut Env, _argc: usize| {
            let test = env.arg(0);
            let f    = env.arg(1);

            let mut ret = VVal::Nul;
            loop {
                match test.call_no_args(env) {
                    Ok(v)                      => { if !v.b() { return Ok(ret); } },
                    Err(StackAction::Break(v)) => { return Ok(v); },
                    Err(StackAction::Next)     => { continue; },
                    Err(e)                     => { return Err(e); }
                }

                match f.call_no_args(env) {
                    Ok(v)                      => { ret = v; },
                    Err(StackAction::Break(v)) => { return Ok(v); },
                    Err(StackAction::Next)     => { },
                    Err(e)                     => { return Err(e); }
                }
            }
        }, Some(2), Some(2), false);

    func!(st, "for",
        |env: &mut Env, _argc: usize| {
            let val = env.arg(0);
            let f   = env.arg(1);

            let mut ret = VVal::Nul;
            for v in val.iter() {
                env.push(v);
                match f.call_internal(env, 1) {
                    Ok(v)                      => { ret = v; },
                    Err(StackAction::Break(v)) => { env.popn(1); return Ok(v); },
                    Err(StackAction::Next)     => { },
                    Err(e)                     => { env.popn(1); return Err(e); }
                }
                env.popn(1);
            }

            Ok(ret)
        }, Some(2), Some(2), false);

    func!(st, "range",
        |env: &mut Env, _argc: usize| {
            let from     = env.arg(0);
            let to       = env.arg(1);
            let step     = env.arg(2);
            let f        = env.arg(3);
            //println!("RAGEN from={} to={} f={}", from.s(), to.s(), f.s());

            if let VVal::Flt(_) = from {
                let mut from = from.f();
                let to       = to.f();
                let step     = step.f();

                let mut ret = VVal::Nul;
                #[allow(unused_must_use)]
                while from <= to {
                    ret = VVal::Nul;
                    env.push(VVal::Flt(from));
                    match f.call_internal(env, 1) {
                        Ok(v)                      => { ret = v; },
                        Err(StackAction::Break(v)) => { env.popn(1); return Ok(v); },
                        Err(StackAction::Next)     => { },
                        Err(e)                     => { env.popn(1); return Err(e); }
                    }
                    from += step;
                    env.popn(1);
                }
                Ok(ret)
            } else {
                let mut from = from.i();
                let to       = to.i();
                let step     = step.i();

                let mut ret = VVal::Nul;
                #[allow(unused_must_use)]
                while from <= to {
                    ret = VVal::Nul;
                    env.push(VVal::Int(from));
                    match f.call_internal(env, 1) {
                        Ok(v)                      => { ret = v; },
                        Err(StackAction::Break(v)) => { env.popn(1); return Ok(v); },
                        Err(StackAction::Next)     => { },
                        Err(e)                     => { env.popn(1); return Err(e); }
                    }
                    from += step;
                    env.popn(1);
                }
                Ok(ret)
            }
        }, Some(4), Some(4), false);

    st
}

/// Returns a SymbolTable with all WLambda standard library language symbols.
pub fn std_symbol_table() -> SymbolTable {
    let mut st = SymbolTable::new();

    func!(st, "neg",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Int(!env.arg(0).i()))
        }, Some(1), Some(1), false);

    func!(st, "uneg",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Int(i64::from(!(env.arg(0).i() as u32))))
        }, Some(1), Some(1), false);

    func!(st, "unshift",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            v.unshift(env.arg(1));
            Ok(v)
        }, Some(2), Some(2), false);

    func!(st, "push",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            v.push(env.arg(1));
            Ok(v)
        }, Some(2), Some(2), false);

    func!(st, "prepend",
        |env: &mut Env, argc: usize| {
            let v = env.arg(0);
            let v =
                if v.is_vec() { v }
                else {
                    let r = VVal::vec();
                    r.push(v);
                    r
                };

            for i in 1..argc {
                match env.arg(i) {
                    VVal::Lst(b) => {
                        for item in b.borrow().iter() {
                            v.list_operation(|r: &mut std::cell::RefMut<Vec<VVal>>| {
                                r.insert(0, item.clone());
                            })?;
                        }
                    },
                    item => {
                        v.list_operation(|r: &mut std::cell::RefMut<Vec<VVal>>| {
                            r.insert(0, item.clone());
                        })?;
                    },
                }
            }

            Ok(v)
        }, Some(1), None, false);

    func!(st, "append",
        |env: &mut Env, argc: usize| {
            let v = env.arg(0);
            let v =
                if v.is_vec() { v }
                else {
                    let r = VVal::vec();
                    r.push(v);
                    r
                };

            for i in 1..argc {
                match env.arg(i) {
                    VVal::Lst(b) => {
                        for item in b.borrow().iter() {
                            v.list_operation(|r: &mut std::cell::RefMut<Vec<VVal>>| {
                                r.push(item.clone());
                            })?;
                        }
                    },
                    item => {
                        v.list_operation(|r: &mut std::cell::RefMut<Vec<VVal>>| {
                            r.push(item.clone());
                        })?;
                    }
                }
            }

            Ok(v)
        }, Some(1), None, false);

    func!(st, "pop",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            Ok(v.pop())
        }, Some(1), Some(1), false);

    func!(st, "take",
        |env: &mut Env, _argc: usize| {
            let cnt = env.arg(0).i() as usize;
            let lst = env.arg(1);

            lst.list_operation(|r: &mut std::cell::RefMut<Vec<VVal>>| {
                let svec : Vec<VVal> =
                    r.iter().take(cnt).cloned().collect();
                VVal::vec_mv(svec)
            })
        }, Some(2), Some(2), false);

    func!(st, "drop",
        |env: &mut Env, _argc: usize| {
            let cnt = env.arg(0).i() as usize;
            let lst = env.arg(1);

            lst.list_operation(|r: &mut std::cell::RefMut<Vec<VVal>>| {
                let svec : Vec<VVal> =
                    r.iter().skip(cnt).cloned().collect();
                VVal::vec_mv(svec)
            })
        }, Some(2), Some(2), false);

    func!(st, "str:write",
        |env: &mut Env, _argc: usize| { Ok(VVal::new_str_mv(env.arg(0).s())) },
        Some(1), Some(1), false);
    func!(st, "str:len",
        |env: &mut Env, _argc: usize| { Ok(VVal::Int(env.arg(0).s_len() as i64)) },
        Some(1), Some(1), false);
    func!(st, "str:to_lowercase",
        |env: &mut Env, _argc: usize| { Ok(VVal::new_str_mv(env.arg(0).s_raw().to_lowercase())) },
        Some(1), Some(1), false);
    func!(st, "str:to_uppercase",
        |env: &mut Env, _argc: usize| { Ok(VVal::new_str_mv(env.arg(0).s_raw().to_uppercase())) },
        Some(1), Some(1), false);
    func!(st, "str:padl",
        |env: &mut Env, _argc: usize| {
            let len = env.arg(0).i() as usize;
            let pads = env.arg(1).s_raw();
            let mut s = env.arg(2).s_raw();

            while s.len() < len {
                s = pads.to_string() + &s;
            }

            Ok(VVal::new_str_mv(s))
        }, Some(3), Some(3), false);

    func!(st, "str:padr",
        |env: &mut Env, _argc: usize| {
            let len = env.arg(0).i() as usize;
            let pads = env.arg(1).s_raw();
            let mut s = env.arg(2).s_raw();

            while s.len() < len {
                s += &pads;
            }

            Ok(VVal::new_str_mv(s))
        }, Some(3), Some(3), false);
    func!(st, "str:cat",
        |env: &mut Env, argc: usize| {
            let lst = env.arg(0);
            if let VVal::Lst(l) = lst {
                let svec : Vec<String> = l.borrow().iter().map(|v| v.s_raw()).collect();
                Ok(VVal::new_str_mv((&svec).concat()))

            } else {
                let mut s = String::from("");
                for i in 0..argc {
                    s += &env.arg(i).s_raw();
                }
                Ok(VVal::new_str_mv(s))
            }
        }, None, None, false);
    func!(st, "str:replace_n",
        |env: &mut Env, _argc: usize| {
            let pat  = env.arg(0).s_raw();
            let to   = env.arg(1).s_raw();
            let cnt  = env.arg(2).i() as usize;
            let data = env.arg(3).s_raw();
            Ok(VVal::new_str_mv(data.replacen(&pat, &to, cnt)))
        }, Some(3), Some(3), false);
    func!(st, "str:replace",
        |env: &mut Env, _argc: usize| {
            let pat  = env.arg(0).s_raw();
            let to   = env.arg(1).s_raw();
            let data = env.arg(2).s_raw();
            Ok(VVal::new_str_mv(data.replace(&pat, &to)))
        }, Some(3), Some(3), false);
    func!(st, "str:join",
        |env: &mut Env, _argc: usize| {
            let sep = env.arg(0).s_raw();
            let lst = env.arg(1);
            if let VVal::Lst(l) = lst {
                let svec : Vec<String> = l.borrow().iter().map(|v| v.s_raw()).collect();
                Ok(VVal::new_str_mv((&svec).join(&sep)))

            } else {
                Ok(VVal::err_msg(
                    &format!(
                        "str:join only works with lists as second argument, got '{}'",
                        lst.s())))
            }
        }, Some(2), Some(2), false);
    func!(st, "str:from_utf8_lossy",
        |env: &mut Env, _argc: usize| {
            let b = env.arg(0);
            Ok(
                if let VVal::Byt(u) = b {
                    VVal::new_str_mv(String::from_utf8_lossy(&u.borrow()).to_string())
                } else {
                    VVal::Nul
                })
        }, Some(1), Some(1), false);
    func!(st, "str:from_utf8",
        |env: &mut Env, _argc: usize| {
            let b = env.arg(0);
            if let VVal::Byt(u) = b {
                match String::from_utf8(u.borrow().to_vec()) {
                    Ok(s) => Ok(VVal::new_str_mv(s)),
                    Err(e) => {
                        Ok(VVal::err_msg(
                            &format!("str:from_utf8 decoding error: {}", e)))
                    }
                }
            } else {
                Ok(VVal::Nul)
            }
        }, Some(1), Some(1), false);

    func!(st, "str:to_char_vec",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::vec_mv(
                env.arg(0).s_raw()
                    .chars()
                    .map(|c| VVal::Int(i64::from(c as u32)))
                    .collect()))
        }, Some(1), Some(1), false);

    func!(st, "str:from_char_vec",
        |env: &mut Env, _argc: usize| {
            let mut s = String::new();
            for vc in env.arg(0).iter() {
                s.push(std::char::from_u32(vc.i() as u32).unwrap_or('?'));
            }
            Ok(VVal::new_str_mv(s))
        }, Some(1), Some(1), false);

    func!(st, "str:to_bytes",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::new_byt(env.arg(0).as_bytes()))
        }, Some(1), Some(1), false);

    func!(st, "bytes:from_vec",
        |env: &mut Env, _argc: usize| {
            if let VVal::Lst(u) = env.arg(0) {
                Ok(VVal::new_byt(u.borrow().iter().map(|v| v.i() as u8).collect()))

            } else {
                Ok(VVal::Nul)
            }
        }, Some(1), Some(1), false);

    func!(st, "bytes:to_vec",
        |env: &mut Env, _argc: usize| {
            if let VVal::Byt(u) = env.arg(0) {
                Ok(VVal::vec_mv(
                    u.borrow().iter()
                        .map(|u| VVal::Int(i64::from(*u)))
                        .collect()))
            } else {
                Ok(VVal::vec_mv(
                    env.arg(0).as_bytes().iter()
                        .map(|u| VVal::Int(i64::from(*u)))
                        .collect()))
            }
        }, Some(1), Some(1), false);

    func!(st, "bytes:from_hex",
        |env: &mut Env, _argc: usize| {
            let s = env.arg(0).s_raw();
            let out : Vec<u8> = Vec::with_capacity((s.len() + 1) / 2);
            Ok(VVal::new_byt(
                s.chars()
                 .map(|c|
                     match c { '0'..='9' => i16::from( 9 - (b'9' - (c as u8))),
                               'a'..='f' => i16::from(15 - (b'f' - (c as u8))),
                               'A'..='F' => i16::from(15 - (b'F' - (c as u8))),
                               _ => -1 })
                 .fold((256, out), |(last, mut out), c: i16|
                     if c == -1 { (last, out) }
                     else if last == 256 { (c, out) }
                     else {
                         out.push((((last << 4) | (c & 0x0F)) & 0xFF) as u8);
                         (256, out)
                     }).1))
        }, Some(1), Some(1), false);

    func!(st, "bytes:to_hex",
        |env: &mut Env, argc: usize| {
            static HEXCHARS : &[char] =
                &['0', '1', '2', '3', '4', '5', '6', '7',
                  '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'];

            if let VVal::Byt(u) = env.arg(0) {
                let mut out : String = String::with_capacity(u.borrow().len() * 2);

                if argc == 1 {
                    for (a, b) in u.borrow().iter().map(|u|
                                        (HEXCHARS[(u >> 4) as usize],
                                         HEXCHARS[(u & 0x0F) as usize])) {
                        out.push(a);
                        out.push(b);
                    }
                } else {
                    let group_len = env.arg(1).i();
                    let group_sep =
                        if env.arg(2).is_none() { String::from(" ") }
                        else { env.arg(2).s_raw() };

                    let mut len_counter = 0;
                    for (a, b) in u.borrow().iter().map(|u|
                                        (HEXCHARS[(u >> 4) as usize],
                                         HEXCHARS[(u & 0x0F) as usize])) {
                        if len_counter >= group_len { out.push_str(&group_sep); len_counter = 0; }
                        out.push(a);
                        len_counter += 1;
                        if len_counter >= group_len { out.push_str(&group_sep); len_counter = 0; }
                        out.push(b);
                        len_counter += 1;
                    }
                }

                Ok(VVal::new_str_mv(out))

            } else {
                Ok(VVal::Nul)
            }
        }, Some(1), Some(3), false);

    func!(st, "write_str",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::new_str_mv(env.arg(0).s()))
        }, Some(1), Some(1), true);

    func!(st, "yay",
        |env: &mut Env, _argc: usize| {
            println!("YAAAAY {}", env.arg(0).s());
            env.dump_stack();
            Ok(VVal::Nul)
        }, None, None, false);

    func!(st, "to_no_arity",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            Ok(v.disable_function_arity())
        }, Some(1), Some(1), false);

    func!(st, "to_drop",
        |env: &mut Env, _argc: usize| {
            let fun = env.arg(1).disable_function_arity();
            let v   = env.arg(0);

            Ok(VVal::DropFun(Rc::new(DropVVal { v, fun })))
        }, Some(2), Some(2), false);

    func!(st, "fold",
        |env: &mut Env, _argc: usize| {
            let mut acc = env.arg(0);
            let f       = env.arg(1);
            let lst     = env.arg(2);

            if let VVal::Lst(l) = lst {
                for i in l.borrow().iter() {
                    env.push(acc.clone());
                    env.push(i.clone());
                    let rv = f.call_internal(env, 2);
                    env.popn(2);

                    match rv {
                        Ok(v)                      => { acc = v;  },
                        Err(StackAction::Break(v)) => { acc = v; break; },
                        Err(StackAction::Next)     => { },
                        Err(e)                     => { return Err(e); },
                    }
                }
            } else {
                return Ok(VVal::err_msg(
                    &format!(
                        "fold only works with lists as argument, got '{}'",
                        lst.s())));
            }

            Ok(acc)
        }, Some(3), Some(3), false);

    add_num_fun_flt!(st, "num:ceil",       ceil);
    add_num_fun_flt!(st, "num:sqrt",       sqrt);
    add_num_fun_flt!(st, "num:cbrt",       cbrt);
    add_num_fun_flt!(st, "num:floor",      floor);
    add_num_fun_flt!(st, "num:round",      round);
    add_num_fun_flt!(st, "num:trunc",      trunc);
    add_num_fun_flt!(st, "num:to_degrees", to_degrees);
    add_num_fun_flt!(st, "num:to_radians", to_radians);
    add_num_fun_flt!(st, "num:tan",        tan);
    add_num_fun_flt!(st, "num:tanh",       tanh);
    add_num_fun_flt!(st, "num:sin",        sin);
    add_num_fun_flt!(st, "num:sinh",       sinh);
    add_num_fun_flt!(st, "num:cos",        cos);
    add_num_fun_flt!(st, "num:cosh",       cosh);
    add_num_fun_flt!(st, "num:asin",       asin);
    add_num_fun_flt!(st, "num:asinh",      asinh);
    add_num_fun_flt!(st, "num:acos",       acos);
    add_num_fun_flt!(st, "num:acosh",      acosh);
    add_num_fun_flt!(st, "num:recip",      recip);
    add_num_fun_flt!(st, "num:log2",       log2);
    add_num_fun_flt!(st, "num:log10",      log10);
    add_num_fun_flt!(st, "num:ln",         ln);
    add_num_fun_flt!(st, "num:exp_m1",     exp_m1);
    add_num_fun_flt!(st, "num:exp",        exp);
    add_num_fun_flt!(st, "num:exp2",       exp2);
    add_num_fun_flt!(st, "num:atan",       atan);
    add_num_fun_flt!(st, "num:atanh",      atanh);

    add_num_fun_flt2!(st, "num:log",        log);
    add_num_fun_flt2!(st, "num:atan2",      atan2);
    add_num_fun_flt2!(st, "num:hypot",      hypot);
    add_num_fun_flt2!(st, "num:pow",        powf);

    func!(st, "num:abs",
        |env: &mut Env, _argc: usize| {
            Ok(match env.arg(0) {
                VVal::Int(i) => VVal::Int(i.abs()),
                VVal::Flt(i) => VVal::Flt(i.abs()),
                _ => VVal::Int(env.arg(0).i().abs())
            })
        }, Some(1), Some(1), false);

    func!(st, "io:stdout:newline",
        |_env: &mut Env, _argc: usize| {
            println!("");
            Ok(VVal::Bol(true))
        }, Some(0), Some(0), false);

    func!(st, "io:stdout:write",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            print!("{}", v.s());
            Ok(v)
        }, Some(1), Some(1), false);

    func!(st, "io:stdout:print",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            print!("{}", v.s_raw());
            Ok(v)
        }, Some(1), Some(1), false);

    func!(st, "io:file:read_text",
        |env: &mut Env, _argc: usize| {
            let filename = env.arg(0).s_raw();

            use std::io::prelude::*;
            use std::fs::OpenOptions;

            let file =
                OpenOptions::new()
                .write(false)
                .create(false)
                .read(true)
                .open(&filename);

            match file {
                Err(e) => {
                    Ok(VVal::err_msg(
                        &format!(
                            "Couldn't open file '{}': {}",
                            filename, e)))
                },
                Ok(mut f) => {
                    let mut contents = String::new();
                    if let Err(e) = f.read_to_string(&mut contents) {
                        Ok(VVal::err_msg(
                            &format!(
                                "Couldn't read text from file '{}': {}",
                                filename, e)))
                    } else {
                        Ok(VVal::new_str_mv(contents))
                    }
                },
            }
        }, Some(1), Some(1), false);

    func!(st, "io:file:read",
        |env: &mut Env, _argc: usize| {
            let filename = env.arg(0).s_raw();

            use std::io::prelude::*;
            use std::fs::OpenOptions;

            let file =
                OpenOptions::new()
                .write(false)
                .create(false)
                .read(true)
                .open(&filename);

            match file {
                Err(e) => {
                    Ok(VVal::err_msg(
                        &format!(
                            "Couldn't open file '{}': {}",
                            filename, e)))
                },
                Ok(mut f) => {
                    let mut contents : Vec<u8> = Vec::new();
                    if let Err(e) = f.read_to_end(&mut contents) {
                        Ok(VVal::err_msg(
                            &format!(
                                "Couldn't read text from file '{}': {}",
                                filename, e)))
                    } else {
                        Ok(VVal::new_byt(contents))
                    }
                },
            }
        }, Some(1), Some(1), false);

    func!(st, "io:file:write_safe",
        |env: &mut Env, _argc: usize| {
            let filename     = env.arg(0).s_raw();
            let tmp_filename = format!("{}~", filename);
            let contents     = env.arg(1);
            let buf = match contents {
                VVal::Byt(b) => b.borrow().clone(),
                v => v.s_raw().as_bytes().to_vec(),
            };

            use std::io::prelude::*;
            use std::fs::OpenOptions;

            let file =
                OpenOptions::new()
                .create(true)
                .write(true)
                .truncate(true)
                .open(&tmp_filename);

            match file {
                Err(e) => {
                    Ok(VVal::err_msg(
                        &format!(
                            "Couldn't open file '{}': {}",
                            filename, e)))
                },
                Ok(mut f) => {
                    if let Err(e) = f.write_all(&buf) {
                        return Ok(VVal::err_msg(
                            &format!(
                                "Couldn't write to file '{}': {}",
                                tmp_filename, e)));
                    }

                    if let Err(e) = std::fs::rename(&tmp_filename, &filename) {
                        return Ok(VVal::err_msg(
                            &format!(
                                "Couldn't rename file '{}' to file '{}': {}",
                                tmp_filename, filename, e)));
                    }

                    Ok(VVal::Bol(true))
                },
            }
        }, Some(2), Some(2), false);

    func!(st, "io:file:append",
        |env: &mut Env, _argc: usize| {
            let filename     = env.arg(0).s_raw();
            let contents     = env.arg(1);
            let buf = match contents {
                VVal::Byt(b) => b.borrow().clone(),
                v => v.s_raw().as_bytes().to_vec(),
            };

            use std::io::prelude::*;
            use std::fs::OpenOptions;

            let file =
                OpenOptions::new()
                .create(true)
                .write(true)
                .append(true)
                .open(&filename);

            match file {
                Err(e) => {
                    Ok(VVal::err_msg(
                        &format!(
                            "Couldn't open file '{}': {}",
                            filename, e)))
                },
                Ok(mut f) => {
                    if let Err(e) = f.write_all(&buf) {
                        Ok(VVal::err_msg(
                            &format!(
                                "Couldn't write to file '{}': {}",
                                filename, e)))
                    } else {
                        Ok(VVal::Bol(true))
                    }
                },
            }
        }, Some(2), Some(2), false);

    func!(st, "writeln",
        |env: &mut Env, argc: usize| {
            for i in 0..argc {
                if i == (argc - 1) {
                    if i > 0 {
                        println!(" {}", env.arg(i).s());
                    } else {
                        println!("{}", env.arg(i).s());
                    }
                } else if i > 0 {
                    print!(" {}", env.arg(i).s());
                } else {
                    print!("{}", env.arg(i).s());
                }
            }
            if argc == 0 { println!(""); }
            if argc > 0 {
                Ok(env.arg(argc - 1))
            } else {
                Ok(VVal::Nul)
            }
        }, None, None, false);

    func!(st, "displayln",
        |env: &mut Env, argc: usize| {
            for i in 0..argc {
                if i == (argc - 1) {
                    if i > 0 {
                        println!(" {}", env.arg(i).s_raw());
                    } else {
                        println!("{}", env.arg(i).s_raw());
                    }
                } else if i > 0 {
                    print!(" {}", env.arg(i).s_raw());
                } else {
                    print!("{}", env.arg(i).s_raw());
                }
            }
            if argc == 0 { println!(""); }
            if argc > 0 {
                Ok(env.arg(argc - 1))
            } else {
                Ok(VVal::Nul)
            }
        }, None, None, false);

    func!(st, "dump_func",
        |env: &mut Env, _argc: usize| {
            if let VVal::Fun(f) = env.arg(0) {
                return Ok(f.dump_upvals());
            }
            Ok(VVal::Nul)
        }, Some(1), Some(1), false);

    func!(st, "wlambda:version",
        |_env: &mut Env, _argc: usize| {
            Ok(VVal::new_str(VERSION))
        }, Some(0), Some(0), false);

    func!(st, "measure_time",
        |env: &mut Env, _argc: usize| {
            use std::convert::TryFrom;
            let t = std::time::Instant::now();
            let unit = env.arg(0).s_raw();
            match env.arg(1).call_no_args(env) {
                Ok(v) => {
                    let ret = VVal::vec();
                    match &unit[..] {
                        "s"  => { ret.push(VVal::Int(i64::try_from(t.elapsed().as_secs())  .unwrap_or(0))); },
                        "ms" => { ret.push(VVal::Int(i64::try_from(t.elapsed().as_millis()).unwrap_or(0))); },
                        "us" => { ret.push(VVal::Int(i64::try_from(t.elapsed().as_micros()).unwrap_or(0))); },
                        "ns" => { ret.push(VVal::Int(i64::try_from(t.elapsed().as_nanos()) .unwrap_or(0))); },
                        _    => { ret.push(VVal::Int(i64::try_from(t.elapsed().as_millis()).unwrap_or(0))); },
                    }
                    ret.push(v);
                    Ok(ret)
                },
                Err(e) => Err(e),
            }
        }, Some(2), Some(2), false);

    func!(st, "assert_eq",
        |env: &mut Env, _argc: usize| {
            let a = env.arg(0);
            let b = env.arg(1);
            if !a.eqv(&b) {
                if env.arg(2).is_none() {
                    Err(StackAction::panic_msg(
                        format!(
                            "assertion failed: expected: '{}', got: '{}'",
                            b.s(), a.s())))
                } else {
                    Err(StackAction::panic_msg(
                        format!(
                            "assertion '{}' failed: expected: '{}', got: '{}'",
                            env.arg(2).s_raw(), b.s(), a.s())))
                }
            } else {
                Ok(VVal::Bol(true))
            }
        }, Some(2), Some(3), true);

    func!(st, "set_ref",
        |env: &mut Env, _argc: usize| {
            Ok(env.arg(0).set_ref(env.arg(1)))
        }, Some(2), Some(2), false);

    func!(st, "to_ref",
        |env: &mut Env, _argc: usize| {
            Ok(env.arg(0).to_ref())
        }, Some(1), Some(1), true);

    func!(st, "ref_id",
        |env: &mut Env, _argc: usize| {
            if let Some(id) = env.arg_ref(0).unwrap_or(&VVal::Nul).ref_id() {
                Ok(VVal::Int(id))
            } else {
                Ok(VVal::Nul)
            }
        }, Some(1), Some(1), true);

    func!(st, "strengthen",
        |env: &mut Env, _argc: usize| {
            Ok(env.arg(0).upgrade())
        }, Some(1), Some(1), false);

    func!(st, "weaken",
        |env: &mut Env, _argc: usize| {
            Ok(env.arg(0).downgrade())
        }, Some(1), Some(1), false);

    func!(st, "assert",
        |env: &mut Env, _argc: usize| {
            if !env.arg(0).b() {
                if env.arg(1).is_none() {
                    Err(StackAction::panic_msg("assertion failed".to_string()))
                } else {
                    Err(StackAction::panic_msg(format!("assertion failed '{}'", env.arg(1).s_raw())))
                }
            } else {
                Ok(env.arg(0))
            }
        }, Some(1), Some(2), true);

    func!(st, "ser:csv",
        |env: &mut Env, _argc: usize| {
            use crate::csv;
            let delim =
                if env.arg(0).is_none() {
                    ",".to_string()
                } else {
                    env.arg(0).s_raw()
                };
            let row_sep =
                if env.arg(1).is_none() {
                    "\r\n".to_string()
                } else {
                    env.arg(1).s_raw()
                };
            let escape_all = env.arg(2).b();
            let val = env.arg(3);

            Ok(VVal::new_str_mv(csv::to_csv(
                delim.chars().nth(0).unwrap_or(','),
                &row_sep,
                escape_all,
                val)))
        }, Some(4), Some(4), false);

    func!(st, "deser:csv",
        |env: &mut Env, _argc: usize| {
            use crate::csv;
            let delim =
                if env.arg(0).is_none() {
                    ",".to_string()
                } else {
                    env.arg(0).s_raw()
                };
            let row_sep =
                if env.arg(1).is_none() {
                    "\r\n".to_string()
                } else {
                    env.arg(1).s_raw()
                };
            let data = env.arg(2).s_raw();

            match csv::parse_csv(
                    delim.chars().nth(0).unwrap_or(','),
                    &row_sep,
                    &data)
            {
                Ok(v) => Ok(v),
                Err(e) => Ok(VVal::err_msg(&e)),
            }
        }, Some(3), Some(3), false);

    #[cfg(feature="regex")]
    func!(st, "re:replace_all",
        |env: &mut Env, _argc: usize| {
            use regex::Regex;
            let re   = env.arg(0).s_raw();
            let f    = env.arg(1);
            let text = env.arg(2).s_raw();

            let rx = Regex::new(&re);
            if let Err(e) = rx {
                return Ok(VVal::err_msg(
                    &format!("Regex '{}' did not compile: {}", re, e)));
            }
            let rx = rx.unwrap();

            let mut finished = false;
            let mut ret = Ok(VVal::Nul);
            let ret_str = VVal::new_str_mv(String::from(rx.replace_all(&text, |capts: &regex::Captures| {
                let captures = VVal::vec();
                for cap in capts.iter() {
                    match cap {
                        None    => { captures.push(VVal::Nul); },
                        Some(c) => {
                            captures.push(VVal::new_str(c.as_str()));
                        }
                    }
                }

                let repl = captures.at(0).unwrap_or(VVal::Nul).s_raw();
                if finished { return repl; }

                if f.is_fun() {
                    env.push(captures);
                    let rv = f.call_internal(env, 1);
                    env.popn(1);

                    match rv {
                        Ok(v)                      => v.s_raw(),
                        Err(StackAction::Break(v)) => { finished = true; v.s_raw() },
                        Err(StackAction::Next)     => { repl },
                        Err(e)                     => { finished = true; ret = Err(e); repl },
                    }
                } else {
                    f.s_raw()
                }
            })));
            if ret.is_err() { return ret; }
            Ok(ret_str)
        }, Some(3), Some(3), false);

    #[cfg(feature="regex")]
    func!(st, "re:match",
        |env: &mut Env, _argc: usize| {
            use regex::Regex;
            let re   = env.arg(0).s_raw();
            let text = env.arg(1).s_raw();
            let f    = env.arg(2);

            let rx = Regex::new(&re);
            if let Err(e) = rx {
                return Ok(VVal::err_msg(
                    &format!("Regex '{}' did not compile: {}", re, e)));
            }
            let rx = rx.unwrap();

            match rx.captures(&text) {
                Some(c) => {
                    let captures = VVal::vec();
                    for cap in c.iter() {
                        match cap {
                            None    => { captures.push(VVal::Nul); },
                            Some(c) => {
                                captures.push(VVal::new_str(c.as_str()));
                            }
                        }
                    }
                    env.push(captures);
                    let rv = f.call_internal(env, 1);
                    env.popn(1);
                    rv
                },
                None => {
                    Ok(VVal::Nul)
                }
            }
        }, Some(3), Some(3), false);

    #[cfg(feature="regex")]
    func!(st, "re:map",
        |env: &mut Env, _argc: usize| {
            use regex::Regex;
            let re   = env.arg(0).s_raw();
            let f    = env.arg(1);
            let text = env.arg(2).s_raw();

            let rx = Regex::new(&re);
            if let Err(e) = rx {
                return Ok(VVal::err_msg(
                    &format!("Regex '{}' did not compile: {}", re, e)));
            }
            let rx = rx.unwrap();

            let ret = VVal::vec();
            for capts in rx.captures_iter(&text) {
                let captures = VVal::vec();
                for cap in capts.iter() {
                    match cap {
                        None    => { captures.push(VVal::Nul); },
                        Some(c) => {
                            captures.push(VVal::new_str(c.as_str()));
                        }
                    }
                }
                env.push(captures);
                let rv = f.call_internal(env, 1);
                env.popn(1);

                match rv {
                    Ok(v)                      => { ret.push(v); },
                    Err(StackAction::Break(v)) => { ret.push(v); break; },
                    Err(StackAction::Next)     => { },
                    Err(e)                     => { return Err(e); },
                }
            }
            Ok(ret)
        }, Some(3), Some(3), false);

    #[cfg(feature="chrono")]
    func!(st, "chrono:timestamp",
        |env: &mut Env, _argc: usize| {
            use chrono::prelude::*;
            let dt = Local::now();

            let fmt = env.arg(0);
            let fmt = if fmt.is_str() {
                fmt.s_raw()
            } else {
                String::from("%Y-%m-%d %H:%M:%S.%f")

            };

            Ok(VVal::new_str_mv(dt.format(&fmt).to_string()))
        }, Some(0), Some(1), false);

    #[cfg(feature="serde_json")]
    func!(st, "ser:json",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            let pp = env.arg(1).b();

            match v.to_json(pp) {
                Ok(s) => Ok(VVal::new_str_mv(s)),
                Err(e) => Ok(VVal::err_msg(&e)),
            }
        }, Some(1), Some(2), false);

    #[cfg(feature="serde_json")]
    func!(st, "deser:json",
        |env: &mut Env, _argc: usize| {
            let s = env.arg(0).s_raw();

            match VVal::from_json(&s) {
                Ok(v) => Ok(v),
                Err(e) => Ok(VVal::err_msg(&e)),
            }
        }, Some(1), Some(1), false);

    #[cfg(feature="rmp-serde")]
    func!(st, "ser:msgpack",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            match v.to_msgpack() {
                Ok(s) => Ok(VVal::new_byt(s)),
                Err(e) => Ok(VVal::err_msg(&e)),
            }
        }, Some(1), Some(1), false);

    #[cfg(feature="rmp-serde")]
    func!(st, "deser:msgpack",
        |env: &mut Env, _argc: usize| {
            if let VVal::Byt(u) = env.arg(0) {
                match VVal::from_msgpack(&u.borrow()[..]) {
                    Ok(v) => Ok(v),
                    Err(e) => Ok(VVal::err_msg(&e)),
                }
            } else {
                Ok(VVal::err_msg("deser:msgpack expects bytes"))
            }
        }, Some(1), Some(1), false);

    func!(st, "copy",
        |env: &mut Env, _argc: usize| {
            Ok(env.arg(0).shallow_clone())
        }, Some(1), Some(1), false);

    func!(st, "cmp:num:asc",
        |env: &mut Env, _argc: usize| {
            match env.arg(0).compare_num(&env.arg(1)) {
                std::cmp::Ordering::Greater => Ok(VVal::Int(-1)),
                std::cmp::Ordering::Less    => Ok(VVal::Int(1)),
                std::cmp::Ordering::Equal   => Ok(VVal::Int(0)),
            }
        }, Some(2), Some(2), false);

    func!(st, "cmp:num:desc",
        |env: &mut Env, _argc: usize| {
            match env.arg(0).compare_num(&env.arg(1)) {
                std::cmp::Ordering::Greater => Ok(VVal::Int(1)),
                std::cmp::Ordering::Less    => Ok(VVal::Int(-1)),
                std::cmp::Ordering::Equal   => Ok(VVal::Int(0)),
            }
        }, Some(2), Some(2), false);

    func!(st, "cmp:str:asc",
        |env: &mut Env, _argc: usize| {
            match env.arg(0).compare_str(&env.arg(1)) {
                std::cmp::Ordering::Greater => Ok(VVal::Int(-1)),
                std::cmp::Ordering::Less    => Ok(VVal::Int(1)),
                std::cmp::Ordering::Equal   => Ok(VVal::Int(0)),
            }
        }, Some(2), Some(2), false);

    func!(st, "cmp:str:desc",
        |env: &mut Env, _argc: usize| {
            match env.arg(0).compare_str(&env.arg(1)) {
                std::cmp::Ordering::Greater => Ok(VVal::Int(1)),
                std::cmp::Ordering::Less    => Ok(VVal::Int(-1)),
                std::cmp::Ordering::Equal   => Ok(VVal::Int(0)),
            }
        }, Some(2), Some(2), false);

    func!(st, "sort",
        |env: &mut Env, argc: usize| {
            if argc == 1 {
                let mut list = env.arg(0);
                list.sort(|a: &VVal, b: &VVal| {
                    if a.is_int() || a.is_float() {
                        a.compare_num(b)
                    } else {
                        a.compare_str(b)
                    }
                });
                Ok(list)
            } else {
                let fun = env.arg(0);
                let mut list = env.arg(1);
                let mut ret = Ok(VVal::Nul);
                list.sort(|a: &VVal, b: &VVal| {
                    env.push(a.clone());
                    env.push(b.clone());
                    let i =
                        match fun.call_internal(env, 2) {
                            Ok(v)  => { v.i() },
                            Err(e) => { ret = Err(e); 1 },
                        };
                    env.popn(2);
                    match i {
                        _ if i == 0 => std::cmp::Ordering::Equal,
                        _ if i >  0 => std::cmp::Ordering::Greater,
                        _           => std::cmp::Ordering::Less,
                    }
                });
                if ret.is_ok() { ret = Ok(list); }
                ret
            }
        }, Some(1), Some(2), false);

    func!(st, "shuffle",
        |env: &mut Env, _argc: usize| {
            let fun = env.arg(0);
            let mut list = env.arg(1);
            list.fisher_yates_shuffle(|| {
                fun.call_no_args(env).unwrap_or(VVal::Nul).i()
            });
            Ok(list)
        }, Some(2), Some(2), false);

    func!(st, "hash:fnv1a",
        |env: &mut Env, argc: usize| {
            let mut hash = util::FnvHasher::default();
            for i in 0..argc {
                match env.arg(i) {
                    VVal::Int(i) => hash.write_i64(i),
                    VVal::Flt(f) => hash.write_f64(f),
                    _ => {
                        let s = env.arg(i).s_raw();
                        hash.write(&s.into_bytes()[..]);
                    }
                }
            }
            Ok(VVal::Int(hash.finish_i64()))
        }, Some(1), None, false);

    func!(st, "rand:split_mix64_new",
        |_env: &mut Env, _argc: usize| {
            let v = VVal::vec();
            v.push(VVal::Int(util::now_timestamp() as i64));
            Ok(v)
        }, Some(0), Some(0), false);

    func!(st, "rand:split_mix64_new_from",
        |env: &mut Env, _argc: usize| {
            let v = VVal::vec();
            v.push(VVal::Int(env.arg(0).i()));
            Ok(v)
        }, Some(1), Some(1), false);

    func!(st, "rand:split_mix64_next",
        |env: &mut Env, argc: usize| {
            let mut sm =
                util::SplitMix64::new_from_i64(
                    env.arg(0).at(0).unwrap_or(VVal::Int(0)).i());

            let ret =
                if argc == 2 {
                    let v = VVal::vec();
                    for _i in 0..env.arg(1).i() {
                        v.push(VVal::Int(sm.next_i64()));
                    }
                    v
                } else {
                    VVal::Int(sm.next_i64())
                };
            env.arg(0).set_at(0,
                VVal::Int(i64::from_be_bytes(sm.0.to_be_bytes())));
            Ok(ret)
        }, Some(1), Some(2), false);

    func!(st, "num:int_to_open01", |env: &mut Env, _argc: usize| {
        Ok(VVal::Flt(util::u64_to_open01(env.arg(0).i() as u64)))
    }, Some(1), Some(1), false);

    func!(st, "num:int_to_open_closed01", |env: &mut Env, _argc: usize| {
        Ok(VVal::Flt(util::u64_to_open_closed01(env.arg(0).i() as u64)))
    }, Some(1), Some(1), false);

    func!(st, "num:int_to_closed_open01", |env: &mut Env, _argc: usize| {
        Ok(VVal::Flt(util::u64_to_closed_open01(env.arg(0).i() as u64)))
    }, Some(1), Some(1), false);

    func!(st, "rand:split_mix64_next_closed_open01",
        |env: &mut Env, argc: usize| {
            let mut sm =
                util::SplitMix64::new_from_i64(
                    env.arg(0).at(0).unwrap_or(VVal::Int(0)).i());
            let ret =
                if argc == 2 {
                    let v = VVal::vec();
                    for _i in 0..env.arg(1).i() {
                        v.push(VVal::Flt(util::u64_to_closed_open01(sm.next_u64())));
                    }
                    v
                } else {
                    VVal::Flt(util::u64_to_closed_open01(sm.next_u64()))
                };
            env.arg(0).set_at(0,
                VVal::Int(i64::from_be_bytes(sm.0.to_be_bytes())));
            Ok(ret)
        }, Some(1), Some(2), false);

    func!(st, "rand:split_mix64_next_open_closed01",
        |env: &mut Env, argc: usize| {
            let mut sm =
                util::SplitMix64::new_from_i64(
                    env.arg(0).at(0).unwrap_or(VVal::Int(0)).i());
            let ret =
                if argc == 2 {
                    let v = VVal::vec();
                    for _i in 0..env.arg(1).i() {
                        v.push(VVal::Flt(util::u64_to_open_closed01(sm.next_u64())));
                    }
                    v
                } else {
                    VVal::Flt(util::u64_to_open_closed01(sm.next_u64()))
                };
            env.arg(0).set_at(0,
                VVal::Int(i64::from_be_bytes(sm.0.to_be_bytes())));
            Ok(ret)
        }, Some(1), Some(2), false);

    func!(st, "rand:split_mix64_next_open01",
        |env: &mut Env, argc: usize| {
            let mut sm =
                util::SplitMix64::new_from_i64(
                    env.arg(0).at(0).unwrap_or(VVal::Int(0)).i());
            let ret =
                if argc == 2 {
                    let v = VVal::vec();
                    for _i in 0..env.arg(1).i() {
                        v.push(VVal::Flt(util::u64_to_open01(sm.next_u64())));
                    }
                    v
                } else {
                    VVal::Flt(util::u64_to_open01(sm.next_u64()))
                };
            env.arg(0).set_at(0,
                VVal::Int(i64::from_be_bytes(sm.0.to_be_bytes())));
            Ok(ret)
        }, Some(1), Some(2), false);


    st
}
