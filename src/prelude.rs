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

-----
**Table Of Contents:**

- [1](#1-syntax) - Syntax
- [2](#2-variable-definition-and-assignment) - Variable Definition and Assignment
  - [2.1](#21-global-variables) - Global Variables
- [3](#3-functions-part-12) - Functions (part 1/2)
  - [3.1](#31-closures) - Closures
    - [3.1.1](#311-object-oriented-programming-with-closures) - Object Oriented Programming with Closures
  - [3.2](#32-function-calling) - Function calling
  - [3.3](#33-function-arity-checks) - Function arity checks
    - [3.3.1](#331-stdtonoarity-function) - std:to_no_arity _function_
  - [3.4](#34-calling-fields--method-calling) - Calling fields / Method calling
    - [3.4.1](#341-object-oriented-programming-with-prototypes) - Object Oriented Programming with Prototypes
- [4](#4-data-types) - Data Types
  - [4.1](#41-none-sentinel-value-n-or-none) - None sentinel value: `$n` or `$none`
    - [4.1.1](#411-isnone-value) - is_none _value_
    - [4.1.2](#412-issome-value) - is_some _value_
  - [4.2](#42-error-values-e-expr-or-error-expr) - Error values: `$e expr` or `$error expr`
    - [4.2.1](#421--label-value) - _? [_label_] _value_
    - [4.2.2](#422-onerror-handler-maybe-error-value) - on_error _handler_ _maybe-error-value_
    - [4.2.3](#423-errortostr-value) - error_to_str _value_
  - [4.3](#43-booleans) - Booleans
    - [4.3.1](#431-isbool-any-value) - is_bool _any-value_
    - [4.3.2](#432-bool-any-value) - bool _any-value_
    - [4.3.3](#433-not-value) - not _value_
    - [4.3.4](#434-boolean-list-indexing) - Boolean List Indexing
  - [4.4](#44-64-bit-integers) - 64-Bit Integers
    - [4.4.1](#441-stdnegi64-integer) - std:neg_i64 _integer_
    - [4.4.2](#442-stdnoti64-integer) - std:not_i64 _integer_
    - [4.4.3](#443-stdnegu32-integer) - std:neg_u32 _integer_
    - [4.4.4](#444-stdnotu32-integer) - std:not_u32 _integer_
  - [4.5](#45-64-bit-floats) - 64-Bit Floats
    - [4.5.1](#451-float-value) - float _value_
    - [4.5.2](#452-isfloat-value) - is_float _value_
    - [4.5.3](#453-stdnumacos-float) - std:num:acos _float_
    - [4.5.4](#454-stdnumacosh-float) - std:num:acosh _float_
    - [4.5.5](#455-stdnumasin-float) - std:num:asin _float_
    - [4.5.6](#456-stdnumasinh-float) - std:num:asinh _float_
    - [4.5.7](#457-stdnumatan-float) - std:num:atan _float_
    - [4.5.8](#458-stdnumatan2-y-x) - std:num:atan2 _y_ _x_
    - [4.5.9](#459-stdnumatanh-float) - std:num:atanh _float_
    - [4.5.10](#4510-stdnumcbrt-float) - std:num:cbrt _float_
    - [4.5.11](#4511-stdnumceil-float) - std:num:ceil _float_
    - [4.5.12](#4512-stdnumcos-float) - std:num:cos _float_
    - [4.5.13](#4513-stdnumcosh-float) - std:num:cosh _float_
    - [4.5.14](#4514-stdnumexp-float) - std:num:exp _float_
    - [4.5.15](#4515-stdnumexp2-float) - std:num:exp2 _float_
    - [4.5.16](#4516-stdnumexpm1-float) - std:num:exp_m1 _float_
    - [4.5.17](#4517-stdnumfloor-float) - std:num:floor _float_
    - [4.5.18](#4518-stdnumhypot-y-x) - std:num:hypot _y_ _x_
    - [4.5.19](#4519-stdnumln-float) - std:num:ln _float_
    - [4.5.20](#4520-stdnumlog-float) - std:num:log _float_
    - [4.5.21](#4521-stdnumlog10-float) - std:num:log10 _float_
    - [4.5.22](#4522-stdnumlog2-float) - std:num:log2 _float_
    - [4.5.23](#4523-stdnumpow-float) - std:num:pow _float_
    - [4.5.24](#4524-stdnumrecip-float) - std:num:recip _float_
    - [4.5.25](#4525-stdnumround-float) - std:num:round _float_
    - [4.5.26](#4526-stdnumsin-float) - std:num:sin _float_
    - [4.5.27](#4527-stdnumsinh-float) - std:num:sinh _float_
    - [4.5.28](#4528-stdnumsqrt-float) - std:num:sqrt _float_
    - [4.5.29](#4529-stdnumtan-float) - std:num:tan _float_
    - [4.5.30](#4530-stdnumtanh-float) - std:num:tanh _float_
    - [4.5.31](#4531-stdnumtodegrees-float) - std:num:to_degrees _float_
    - [4.5.32](#4532-stdnumtoradians-float) - std:num:to_radians _float_
    - [4.5.33](#4533-stdnumtrunc-float) - std:num:trunc _float_
  - [4.6](#46-numeric-functions) - Numeric Functions
    - [4.6.1](#461-stdnumabs-number) - std:num:abs _number_
  - [4.7](#47-strings) - Strings
    - [4.7.1](#471-stdstrcat-a-b-) - std:str:cat _a_ _b_ ...
    - [4.7.2](#472-stdstrjoin-sep-vector) - std:str:join _sep_ _vector_
    - [4.7.3](#473-stdstrlen-value) - std:str:len _value_
    - [4.7.4](#474-stdstrtrim-value) - std:str:trim _value_
    - [4.7.5](#475-stdstrtrimstart-value) - std:str:trim_start _value_
    - [4.7.6](#476-stdstrtrimend-value) - std:str:trim_end _value_
    - [4.7.7](#477-stdstrpadstart-len-pad-str-value) - std:str:pad_start _len_ _pad-str_ _value_
    - [4.7.8](#478-stdstrpadend-len-pad-str-value) - std:str:pad_end _len_ _pad-str_ _value_
  - [4.8](#48-bytes-or-byte-vectors) - Bytes (or Byte Vectors)
    - [4.8.1](#481-call-properties-of-bytes) - Call Properties of Bytes
    - [4.8.2](#482-byte-conversion-functions) - Byte Conversion Functions
  - [4.9](#49-symbols) - Symbols
  - [4.10](#410-vectors-or-lists) - Vectors (or Lists)
    - [4.10.1](#4101-splicing) - Splicing
    - [4.10.2](#4102-stdappend-vec-a-value-or-vec-) - std:append _vec-a_ _value-or-vec_ ...
    - [4.10.3](#4103-stdtake-count-vector) - std:take _count_ _vector_
    - [4.10.4](#4104-stddrop-count-vector) - std:drop _count_ _vector_
  - [4.11](#411-associative-maps-or-string-to-value-mappings) - Associative Maps (or String to Value mappings)
    - [4.11.1](#4111-splicing) - Splicing
  - [4.12](#412-references) - References
    - [4.12.1](#4121-stdtoref-value) - std:to_ref _value_
    - [4.12.2](#4122-weaken-references) - Weaken References
    - [4.12.3](#4123-strengthening-references) - Strengthening References
    - [4.12.4](#4124-stdsetref-ref-value) - std:set_ref _ref_ _value_
  - [4.13](#413-calling-semantics-of-data-types) - Calling Semantics of Data Types
- [5](#5-functions-part-22) - Functions (part 2/2)
  - [5.1](#51-function-call-composition) - Function call composition
    - [5.1.1](#511--tail-argument-function-chaninig) - '|' Tail Argument Function Chaninig
    - [5.1.2](#512--left-hand-function-chaining) - '|>' Left Hand Function Chaining
  - [5.2](#52-control-flow---returning) - Control Flow - Returning
    - [5.2.1](#521-return-label-value) - return [_label_] _value_
    - [5.2.2](#522-block-label-function) - block [label] _function_
- [6](#6-conditional-execution---if--then--else) - Conditional Execution - if / then / else
- [7](#7-loops-and-iteration) - Loops And Iteration
  - [7.1](#71-control-flow) - Control Flow
    - [7.1.1](#711-while-predicate-fun) - while _predicate_ _fun_
    - [7.1.2](#712-range-start-end-step-fun) - range _start_ _end_ _step_ _fun_
    - [7.1.3](#713-break-value) - break _value_
  - [7.2](#72-collection-iteration) - Collection Iteration
    - [7.2.1](#721-iteration-over-vectors) - Iteration over vectors
    - [7.2.2](#722-iteration-over-maps) - Iteration over maps
    - [7.2.3](#723-for-iteratable-value-function) - for _iteratable-value_ _function_
  - [7.3](#73-accumulation-and-collection) - Accumulation and Collection
    - [7.3.1](#731-transforming-a-vector) - Transforming a vector
    - [7.3.2](#732-example-of-) - Example of `$@@`
    - [7.3.3](#733-transforming-a-vector-to-a-map) - Transforming a vector to a map
    - [7.3.4](#734-iteratively-concatenating-strings) - Iteratively concatenating strings
    - [7.3.5](#735-accumulating-sums) - Accumulating sums
  - [7.4](#74-utilities) - Utilities
    - [7.4.1](#741-stdaccum-collection-a-b-) - std:accum _collection_ _a_ _b_ ...
    - [7.4.2](#742-stdzip-vector-map-fn) - std:zip _vector_ _map-fn_
    - [7.4.3](#743-stdenumerate-map-fn) - std:enumerate _map-fn_
- [8](#8-operators) - Operators
  - [8.1](#81-arithmetic) - Arithmetic
    - [8.1.1](#811--operand-1-operand-2-) - + _operand-1_ _operand-2_ ...
    - [8.1.2](#812---operand-1-operand-2-) - - _operand-1_ _operand-2_ ...
    - [8.1.3](#813--op-a-op-b) - * _op-a_ _op-b_
    - [8.1.4](#814--op-a-op-b) - / _op-a_ _op-b_
    - [8.1.5](#815--op-a-op-b) - % _op-a_ _op-b_
    - [8.1.6](#816--op-a-op-b) - ^ _op-a_ _op-b_
  - [8.2](#82-comparison) - Comparison
    - [8.2.1](#821--op-a-op-b) - == _op-a_ _op-b_
    - [8.2.2](#822--op-a-op-b) - != _op-a_ _op-b_
    - [8.2.3](#823--op-a-op-b) - < _op-a_ _op-b_
    - [8.2.4](#824--op-a-op-b) - <= _op-a_ _op-b_
    - [8.2.5](#825--op-a-op-b) - > _op-a_ _op-b_
    - [8.2.6](#826--op-a-op-b) - >= _op-a_ _op-b_
  - [8.3](#83-bit-operations) - Bit Operations
    - [8.3.1](#831--op-a-op-b) - & _op-a_ _op-b_
    - [8.3.2](#832--op-a-op-b) - &^ _op-a_ _op-b_
    - [8.3.3](#833--op-a-op-b) - &| _op-a_ _op-b_
    - [8.3.4](#834--op-a-op-b) - << _op-a_ _op-b_
    - [8.3.5](#835--op-a-op-b) - >> _op-a_ _op-b_
- [9](#9-modules) - Modules
  - [9.1](#91-export) - export
  - [9.2](#92-import) - import
- [10](#10-core-library) - Core Library
- [11](#11-standard-library) - Standard Library
    - [11.0.1](#1101-stdshuffle-randfunc-vec) - std:shuffle _rand_func_ _vec_
    - [11.0.2](#1102-stdcopy-vecormap) - std:copy _vec_or_map_
    - [11.0.3](#1103-stdsort-comparefun-vec) - std:sort [_compare_fun_] _vec_
    - [11.0.4](#1104-stdcmpnumasc-a-b) - std:cmp:num:asc _a_ _b_
    - [11.0.5](#1105-stdcmpnumdesc-a-b) - std:cmp:num:desc _a_ _b_
    - [11.0.6](#1106-stddisplayln-arg1-) - std:displayln _arg1_ ...
    - [11.0.7](#1107-stdwriteln-arg1-) - std:writeln _arg1_ ...
    - [11.0.8](#1108-stdstrwrite-arg) - std:str:write _arg_
    - [11.0.9](#1109-stdeval-code-string) - std:eval _code-string_
    - [11.0.10](#11010-stdassert-bool-message) - std:assert _bool_ \[_message_]
    - [11.0.11](#11011-stdasserteq-actual-expected-message) - std:assert_eq _actual_ _expected_ \[_message_]
    - [11.0.12](#11012-stdwlambdaversion) - std:wlambda:version
  - [11.1](#111-io) - I/O
    - [11.1.1](#1111-stdiofilereadtext-filename) - std:io:file:read_text _filename_
    - [11.1.2](#1112-stdiofileread-filename) - std:io:file:read _filename_
    - [11.1.3](#1113-stdiofilewritesafe-filename-bytes-or-string) - std:io:file:write_safe _filename_ _bytes-or-string_
    - [11.1.4](#1114-stdiofileappend-filename-bytes-or-string) - std:io:file:append _filename_ _bytes-or-string_
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

## <a name="2-variable-definition-and-assignment"></a>2 - Variable Definition and Assignment

As this manual assumes you have some programming knowledge,
we will just take a short look at the variable definition and assignment
syntax:

```wlambda
!a = 10;            # variable definition & initialization

.a = 20;            # assignment of a new value to a variable
```

WLambda also supports destructuring assignment of vectors:

```wlambda
!v = $[1,2,3];
!(a, b, c) = v;     # destructuring definition of variables
.(a, b, c) = v;     # destructuring assignment

std:assert_eq a 1;
std:assert_eq b 2;
std:assert_eq c 3;
```

This also works with maps, where the key names are matched to
the variable names:

```wlambda
!m = ${ a = 10, b = 20, c = 30 };
!(a, b, c) = m;     # destructuring definition by map
.(a, b, c) = m;     # destructuring assignment by map

std:assert_eq a 10;
std:assert_eq b 20;
std:assert_eq c 30;
```

### <a name="21-global-variables"></a>2.1 - Global Variables

You can define global variables that are not bound to
a lexical scope as follows:

```wlambda
{
    !:global a = 13;
}[];

std:assert_eq a 13;
```

Global variables however do not live beyond file or module boundaries.

## <a name="3-functions-part-12"></a>3 - Functions (part 1/2)

A function can be defined using the `{ ... }` syntax and the `\ _statement_`
syntax: To give functions a name, you need to assign them to a variable with
the `!_name_ = _expr_` syntax.

### <a name="31-closures"></a>3.1 - Closures

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

#### <a name="311-object-oriented-programming-with-closures"></a>3.1.1 - Object Oriented Programming with Closures

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

### <a name="32-function-calling"></a>3.2 - Function calling

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

### <a name="33-function-arity-checks"></a>3.3 - Function arity checks

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

#### <a name="331-stdtonoarity-function"></a>3.3.1 - std:to_no_arity _function_

This function disables all arity checks of a function. Use this with care
and diligence.

```wlambda
!f = { _ }; # accepts exactly 1 param

# f keeps it's arity checks, but f2 will
# call the same function, but without arity checks.
!f2 = std:to_no_arity f;

std:assert_eq (f2 1 2 3) 1;
```

### <a name="34-calling-fields--method-calling"></a>3.4 - Calling fields / Method calling

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

#### <a name="341-object-oriented-programming-with-prototypes"></a>3.4.1 - Object Oriented Programming with Prototypes

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

## <a name="4-data-types"></a>4 - Data Types

### <a name="41-none-sentinel-value-n-or-none"></a>4.1 - None sentinel value: `$n` or `$none`

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

#### <a name="411-isnone-value"></a>4.1.1 - is_none _value_

Returns `$true` if _value_ is `$none`.

```wlambda
std:assert ~ is_none $none;
std:assert ~ not ~ is_none $false;
```

#### <a name="412-issome-value"></a>4.1.2 - is_some _value_

Returns `$true` if _value_ is anything except `$none`.

```wlambda
std:assert ~ not ~ is_some $none;
std:assert ~ is_some $false;
std:assert ~ is_some 30;
```

### <a name="42-error-values-e-expr-or-error-expr"></a>4.2 - Error values: `$e expr` or `$error expr`

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

#### <a name="421--label-value"></a>4.2.1 - _? [_label_] _value_

Unwind the call stack from the current function to a given _label_ if _value_ is an error value.
If no _label_ is given only the current function is returned from with the error value.  If there
is no error, the given value is returned.

The best usecase is, if you just want to hand any errors that might be returned
further upwards the call stack for the parent functions to handle.

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

A more elaborate example:

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

#### <a name="422-onerror-handler-maybe-error-value"></a>4.2.2 - on_error _handler_ _maybe-error-value_

The first parameter to `on_error` should be a _handler_ function,
which will be called with four parameters.
The first of these parameters is the error text,
followed by the line number, column number and file name
from which the error originates.

The given _handler_ is called when an error value is encountered
as second argument, the _maybe-error-value_.

An example to demonstrate the handler arguments:

```wlambda
on_error {!(func, line, col, filename) = @;
    # ...
} ($e "test");
```

A usage example:

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

#### <a name="423-errortostr-value"></a>4.2.3 - error_to_str _value_

This function accepts an error value in contrast to `str`, but does
not panic but transform the error value into it's string representation.

```wlambda
!r = error_to_str $e "TEST";

std:assert_eq r "$e[1,22:<wlambda::eval>(Err)] \"TEST\"";
```

WARNING: The string representation might change between wlambda versions.
Please use `on_error` to access the individual parts
(line, column, filename, error value) of the error.

### <a name="43-booleans"></a>4.3 - Booleans

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

#### <a name="431-isbool-any-value"></a>4.3.1 - is_bool _any-value_

You can check if something is a boolean with `is_bool`:

```wlambda
std:assert ~ is_bool $true;
std:assert ~ is_bool $false;
std:assert ~ not[is_bool $n];
std:assert ~ not[is_bool ""];
std:assert ~ not[is_bool 0];
```

#### <a name="432-bool-any-value"></a>4.3.2 - bool _any-value_

You can cast _any-value_ into a boolean with the `bool` function:

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

#### <a name="433-not-value"></a>4.3.3 - not _value_

This function negates the boolean _value_. If it is not a boolean, it will
be casted into one before negating.

```wlambda
std:assert ~ not $false;
std:assert ~ not 0;
std:assert ~ not $none;
```

#### <a name="434-boolean-list-indexing"></a>4.3.4 - Boolean List Indexing

Booleans can also be used to pick a value from a list
by calling the boolean with a list as first argument:

```wlambda
std:assert_eq ($true  $[:a, :b]) :b;
std:assert_eq ($false $[:a, :b]) :a;
```

### <a name="44-64-bit-integers"></a>4.4 - 64-Bit Integers

WLambda's most basic numeric data type is the 64-Bit integer, aka _i64_ in Rust.
Like with other numbers multiple radix literal forms are supported:

```wlambda
# Decimal:
std:assert_eq 10r99         99;

# Hexadecimal:
std:assert_eq 0xFF01        65281;

# Binary:
std:assert_eq  0b1011       11;
std:assert_eq -0b1011      -11;

# Radix 4:
std:assert_eq 4r31          13;
```

#### <a name="441-stdnegi64-integer"></a>4.4.1 - std:neg_i64 _integer_

Negates the _integer_, which makes a negative from a positive and positive
from a negative number.

```wlambda
std:assert_eq (std:neg_i64 -1)      1;
std:assert_eq (std:neg_i64 1)      -1;

std:assert_eq (std:neg_i64 0xFF)  -255;
```

#### <a name="442-stdnoti64-integer"></a>4.4.2 - std:not_i64 _integer_

Flips the bits of the signed 64-Bit _integer_.

```wlambda
std:assert_eq (std:not_i64 -1)      0;
std:assert_eq (std:not_i64 1)      -2;

std:assert_eq (std:not_i64 0xFF)  -256;
```

#### <a name="443-stdnegu32-integer"></a>4.4.3 - std:neg_u32 _integer_

Negates the _integer_ as if it was an unsigned 32-Bit integer.

```wlambda
std:assert_eq (std:neg_u32 0xFF)   4294967041;
std:assert_eq (std:neg_u32 0x1)    4294967295;
std:assert_eq (std:neg_u32 0x0)    0;
```

#### <a name="444-stdnotu32-integer"></a>4.4.4 - std:not_u32 _integer_

Flips the bits of the _integer_ as if it was an unsigned 32-Bit integer.

```wlambda
std:assert_eq (std:not_u32 0xFF)   4294967040;
std:assert_eq (std:not_u32 0x1)    4294967294;
std:assert_eq (std:not_u32 0x0)    4294967295;
```

### <a name="45-64-bit-floats"></a>4.5 - 64-Bit Floats

WLambda supports 64-Bit floating point numbers, aka _f64_ in Rust.
Like with other numbers multiple radix literal forms are supported:

```wlambda
# Decimal:
std:assert_eq 10r9.92       9.92;

# Hexadecimal:
std:assert_eq 0xFF.1        255.0625;

# Binary:
std:assert_eq 0b1011.101    11.625;

# Radix 4:
std:assert_eq 4r3.3         3.75;
```

#### <a name="451-float-value"></a>4.5.1 - float _value_

This function casts _value_ into a float:

```wlambda

std:assert_eq (float 10)       10.0;
std:assert_eq (float $t)        1.0;
std:assert_eq (float $f)        0.0;
std:assert_eq (float :"32.2")  32.2;
std:assert_eq (float "5.42")   5.42;
std:assert_eq (float "5.42")   5.42;
std:assert_eq (float $b"\xFF") 255.0;

```

#### <a name="452-isfloat-value"></a>4.5.2 - is_float _value_

Returns `$true` if _value_ is a float, otherwise `$false` is returned.

```wlambda
std:assert ~ is_float 4.4;
std:assert ~ is_float 1.0 + 1;
std:assert ~ not ~ is_float 1 + 1.0;
std:assert ~ not ~ is_float 4;
std:assert ~ not ~ is_float $true;
```

#### <a name="453-stdnumacos-float"></a>4.5.3 - std:num:acos _float_

Computes the arccosine of a number. Return value is in radians in the range [0,
pi] or NaN if the number is outside the range [-1, 1].

#### <a name="454-stdnumacosh-float"></a>4.5.4 - std:num:acosh _float_

Inverse hyperbolic cosine function.

#### <a name="455-stdnumasin-float"></a>4.5.5 - std:num:asin _float_

Computes the arcsine of a number. Return value is in radians in the range
[-pi/2, pi/2] or NaN if the number is outside the range [-1, 1].

#### <a name="456-stdnumasinh-float"></a>4.5.6 - std:num:asinh _float_

Inverse hyperbolic sine function.

#### <a name="457-stdnumatan-float"></a>4.5.7 - std:num:atan _float_

Computes the arctangent of a number. Return value is in radians in the range
[-pi/2, pi/2].

#### <a name="458-stdnumatan2-y-x"></a>4.5.8 - std:num:atan2 _y_ _x_

Computes the four quadrant arctangent of _y_ and other _x_ in radians.

- x = 0, y = 0: 0
- x >= 0: arctan(y/x) -> [-pi/2, pi/2]
- y >= 0: arctan(y/x) + pi -> (pi/2, pi]
- y < 0: arctan(y/x) - pi -> (-pi, -pi/2)

#### <a name="459-stdnumatanh-float"></a>4.5.9 - std:num:atanh _float_

Inverse hyperbolic tangent function.

#### <a name="4510-stdnumcbrt-float"></a>4.5.10 - std:num:cbrt _float_

Takes the cubic root of a number.

#### <a name="4511-stdnumceil-float"></a>4.5.11 - std:num:ceil _float_

Returns the smallest integer (still a float) greater than or equal to a number.

#### <a name="4512-stdnumcos-float"></a>4.5.12 - std:num:cos _float_

Computes the cosine of a number (in radians).

#### <a name="4513-stdnumcosh-float"></a>4.5.13 - std:num:cosh _float_

Hyperbolic cosine function.

#### <a name="4514-stdnumexp-float"></a>4.5.14 - std:num:exp _float_

Returns e ^ _float_, (the exponential function).

#### <a name="4515-stdnumexp2-float"></a>4.5.15 - std:num:exp2 _float_

Returns 2 ^ _float_.

#### <a name="4516-stdnumexpm1-float"></a>4.5.16 - std:num:exp_m1 _float_

Returns (e ^ _float_ - 1) in a way that is accurate even if the number is close
to zero.

#### <a name="4517-stdnumfloor-float"></a>4.5.17 - std:num:floor _float_

Returns the largest integer (still as float) less than or equal to a number.

#### <a name="4518-stdnumhypot-y-x"></a>4.5.18 - std:num:hypot _y_ _x_

Calculates the length of the hypotenuse of a right-angle triangle given legs of
length _x_ and _y_.

#### <a name="4519-stdnumln-float"></a>4.5.19 - std:num:ln _float_

Returns the natural logarithm of the number.

#### <a name="4520-stdnumlog-float"></a>4.5.20 - std:num:log _float_

Returns the logarithm of the number with respect to an arbitrary base.

The result may not be correctly rounded owing to implementation details;
`std:log2` can produce more accurate results for base 2, and `std:log10` can
produce more accurate results for base 10.

#### <a name="4521-stdnumlog10-float"></a>4.5.21 - std:num:log10 _float_

Returns the base 10 logarithm of the number.

#### <a name="4522-stdnumlog2-float"></a>4.5.22 - std:num:log2 _float_

Returns the base 2 logarithm of the number.

#### <a name="4523-stdnumpow-float"></a>4.5.23 - std:num:pow _float_

Raises a number to a floating point power.
You may also use the `^` operator, which also works for integers.

#### <a name="4524-stdnumrecip-float"></a>4.5.24 - std:num:recip _float_

Takes the reciprocal (inverse) of a number, 1/x.

#### <a name="4525-stdnumround-float"></a>4.5.25 - std:num:round _float_

Returns the nearest integer (still a float) to a number. Round half-way cases
away from 0.0.

#### <a name="4526-stdnumsin-float"></a>4.5.26 - std:num:sin _float_

Computes the sine of a number (in radians).

#### <a name="4527-stdnumsinh-float"></a>4.5.27 - std:num:sinh _float_

Hyperbolic sine function.

#### <a name="4528-stdnumsqrt-float"></a>4.5.28 - std:num:sqrt _float_

Takes the square root of a number.

#### <a name="4529-stdnumtan-float"></a>4.5.29 - std:num:tan _float_

Computes the tangent of a number (in radians).

#### <a name="4530-stdnumtanh-float"></a>4.5.30 - std:num:tanh _float_

Hyperbolic tangent function.

#### <a name="4531-stdnumtodegrees-float"></a>4.5.31 - std:num:to_degrees _float_

Converts radians to degrees.

#### <a name="4532-stdnumtoradians-float"></a>4.5.32 - std:num:to_radians _float_

Converts degrees to radians.

#### <a name="4533-stdnumtrunc-float"></a>4.5.33 - std:num:trunc _float_

Returns the integer part of a number.

### <a name="46-numeric-functions"></a>4.6 - Numeric Functions

These functions work for all types of numbers.

#### <a name="461-stdnumabs-number"></a>4.6.1 - std:num:abs _number_

Takes the absolute value of _number_. If _number_ is not a number
it will be converted into an integer.

```wlambda
std:assert_eq (std:num:abs -10)     10;
std:assert_eq (std:num:abs -13.3)   13.3;
```

### <a name="47-strings"></a>4.7 - Strings

Strings in WLambda are like Rust UTF-8 encoded Unicode strings.
There is no character data type however. There are two types of literal
forms for strings:

```wlambda
"abc def \"foo\"";
std:assert_eq $q/any delimiter may be used instead of/
    "any delimiter may be used instead of";
# Unicode escapes are also working:
std:assert_eq "\u{2211}" "∑";
```

#### <a name="471-stdstrcat-a-b-"></a>4.7.1 - std:str:cat _a_ _b_ ...

Stringifies (like with `str`) and concatenates all it's arguments.

```wlambda
std:assert_eq
    (std:str:cat :a 10 23.2 "ab" "cd" $[1, 2, 3])
    "a1023.2abcd$[1,2,3]";
```

#### <a name="472-stdstrjoin-sep-vector"></a>4.7.2 - std:str:join _sep_ _vector_

Join's the stringified elements of _vector_ with the _sep_ string.
Will return an error if _vector_ is not a vector.

```wlambda
std:assert_eq
    (std:str:join "::" $[1,2,3])
    "1::2::3";
```

#### <a name="473-stdstrlen-value"></a>4.7.3 - std:str:len _value_

Returns the length of the stringified _value_ in unicode characters.
The core function `len` does return the number of bytes in the string
instead.

```wlambda
std:assert_eq (len         "∑") 3;
std:assert_eq (std:str:len "∑") 1;
std:assert_eq (len         "∑ÄÄ") 7;
std:assert_eq (std:str:len "∑ÄÄ") 3;
std:assert_eq (len         "abcd") 4;
std:assert_eq (std:str:len "abcd") 4;
```

#### <a name="474-stdstrtrim-value"></a>4.7.4 - std:str:trim _value_

Trims off any (unicode) white space from the start and end of the
stringified _value_.

```wlambda
std:assert_eq
    (std:str:trim "\nfooo bar ")
    "fooo bar";
```

#### <a name="475-stdstrtrimstart-value"></a>4.7.5 - std:str:trim_start _value_

Trims off any (unicode) white space from the start of the stringified _value_.

```wlambda
std:assert_eq
    (std:str:trim_start "  \nfooo bar \n")
    "fooo bar \n";
```

#### <a name="476-stdstrtrimend-value"></a>4.7.6 - std:str:trim_end _value_

Trims off any (unicode) white space from the end of the stringified _value_.

```wlambda
std:assert_eq
    (std:str:trim_end "  \nfooo bar \n")
    "  \nfooo bar";
```

#### <a name="477-stdstrpadstart-len-pad-str-value"></a>4.7.7 - std:str:pad_start _len_ _pad-str_ _value_

Pads the stringified _value_ by _pad-str_ up to _len_ characters, inserting
at the start of the string.
The output string is guaranteed to be exactly _len_ unicode characters
long and not longer. If _pad-str_ is empty, nothing is done.

```wlambda
std:assert_eq
    (std:str:pad_start 2 "Ä" "0")
    "Ä0";
std:assert_eq
    (std:str:pad_start 5 "∑∑∑" "∑∑")
    "∑∑∑∑∑";
std:assert_eq
    (std:str:pad_start 8 "Ä∑ßs" "∑∑")
    "ßsÄ∑ßs∑∑";

# Empty _pad-str_ is not an error but a nop:
std:assert_eq
    (std:str:pad_start 8 "" "∑∑")
    "∑∑";
```

#### <a name="478-stdstrpadend-len-pad-str-value"></a>4.7.8 - std:str:pad_end _len_ _pad-str_ _value_

Pads the stringified _value_ by _pad-str_ up to _len_ characters,
appending at the end.
The output string is guaranteed to be exactly _len_ unicode characters
long and not longer. If _pad-str_ is empty, nothing is done.

```wlambda
std:assert_eq
    (std:str:pad_end 2 "Ä" "0")
    "0Ä";
std:assert_eq
    (std:str:pad_end 5 "∑∑∑" "∑∑")
    "∑∑∑∑∑";
std:assert_eq
    (std:str:pad_end 8 "Ä∑ßs" "∑∑")
    "∑∑Ä∑ßsÄ∑";

# Empty _pad-str_ is not an error but a nop:
std:assert_eq
    (std:str:pad_end 8 "" "∑∑")
    "∑∑";
```

### <a name="48-bytes-or-byte-vectors"></a>4.8 - Bytes (or Byte Vectors)

Bytes are a special kind of strings. Their literal form is:

```wlambda
$b"abc";
$b"\xFF\xFD\x00";
$Q/ABCDEF\xFD/;      # \xFD is not an escape sequence here!
```

#### <a name="481-call-properties-of-bytes"></a>4.8.1 - Call Properties of Bytes

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

#### <a name="482-byte-conversion-functions"></a>4.8.2 - Byte Conversion Functions

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

### <a name="49-symbols"></a>4.9 - Symbols

### <a name="410-vectors-or-lists"></a>4.10 - Vectors (or Lists)

The literal syntax for vectors (or sometimes also called lists in WLambda)
is `$[...]`. You may write any kind of expression in it and you will get
a vector from it.

For iteration over a vector please refer to [7.2 Collection Iteration](#72-collection-iteration).

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

#### <a name="4111-splicing"></a>4.11.1 - Splicing

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

#### <a name="4102-stdappend-vec-a-value-or-vec-"></a>4.10.2 - std:append _vec-a_ _value-or-vec_ ...

Appends _value-or-vec_ and all following items to _vec-a_.
If _value-or-vec_ is a vector, all it's items will be appended to _vec-a_.

```wlambda
!v = std:append $[1,2,3] :a :b $[:c, :d];

std:assert_eq (str v) "$[1,2,3,:\"a\",:\"b\",:\"c\",:\"d\"]";
```

If _vec-a_ is not a vector, a vector containing it will be created:

```wlambda
!v = std:append 1 :a :b $[:c, :d];

std:assert_eq (str v) "$[1,:\"a\",:\"b\",:\"c\",:\"d\"]";
```

#### <a name="4103-stdtake-count-vector"></a>4.10.3 - std:take _count_ _vector_

Takes and returns the first _count_ elements of _vector_. Does not
mutate _vector_.

```wlambda
!v = $[1,2,3,4,5,6];

!t = std:take 4 v;

std:assert_eq (str v) "$[1,2,3,4,5,6]";
std:assert_eq (str t) "$[1,2,3,4]";
```

#### <a name="4104-stddrop-count-vector"></a>4.10.4 - std:drop _count_ _vector_

Drops _count_ elements from _vector_ and returns them as new vector.
Does not mutate _vector_.

```wlambda
!v = $[1,2,3,4,5,6];

!t = std:drop 4 v;

std:assert_eq (str v) "$[1,2,3,4,5,6]";
std:assert_eq (str t) "$[5,6]";
```

### <a name="411-associative-maps-or-string-to-value-mappings"></a>4.11 - Associative Maps (or String to Value mappings)

Aside from vectors there are associative maps in WLambda. Their syntax is
`${ key = expr, ... }`. The keys of these maps have to be strings,
the values in the literals can be any expression.

For iteration over a map please refer to [7.2 Collection Iteration](#72-collection-iteration).

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

#### <a name="4111-splicing"></a>4.11.1 - Splicing

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

### <a name="412-references"></a>4.12 - References

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

Strong references can also be created using the `std:to_ref` function.

#### <a name="4121-stdtoref-value"></a>4.12.1 - std:to_ref _value_

Creates a new strong reference that refers to a cell that stores _value_.

```wlambda
!x = std:to_ref 10;

std:assert_eq (std:write_str x) "$&&10";

std:assert_eq $*x 10;
```

#### <a name="4122-weaken-references"></a>4.12.2 - Weaken References

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

#### <a name="4123-strengthening-references"></a>4.12.3 - Strengthening References

You can convert a weak reference (weakened by `std:weaken`) or a captured weak
reference `$&` to strong with `std:strengthen`.

TODO: Example

#### <a name="4124-stdsetref-ref-value"></a>4.12.4 - std:set_ref _ref_ _value_

Sets the value of the reference _ref_ to _value_.
If _ref_ is not a strong, weakable or weak reference nothing happens.

Returns _value_ or `$none`.

```wlambda
!r1 = $&&1;
std:set_ref r1 10;
std:assert_eq $*r1 10;

# Note that $& references in local variables are
# automatically derefernced. Because of that we need to wrap it into
# an extra reference.
!r2 = $& $& 1;
std:set_ref r2 11;
std:assert_eq $*r2 11;

!r3 = $& $& 1;
!w3 = std:weaken r3;
std:set_ref w3 14;      # Set reference via the weak reference in w3 to r3.
std:assert_eq $*r3 14;
```

### <a name="413-calling-semantics-of-data-types"></a>4.13 - Calling Semantics of Data Types

You can call almost all basic data types of WLambda.
Here is an overview of the data type calling semantics:

| Type      | Args              | Semantics |
|-----------|-------------------|-----------|
| `$none`   | -                 | Any call to `$none` will result in a panic. |
| `$error`  | -                 | Any call to `$error` will result in a panic. |
| function  | *                 | Will call the function with the specified arguments. |
| `$true`   | `f1, f2`          | Will call `f1`.          |
| `$false`  | `f1, f2`          | Will call `f2` or return `$n` if `f2` is not provided.          |
| `$true`   | `$[1,2]`          | Will return the second element `2` of the list. |
| `$false`  | `$[1,2]`          | Will return the first element `1` of the list. |
| symbol    | map, userval      | Will retrieve the value in the map at the key equal to the symbol. |
| map       | anything          | Will call `anything` for each value and key in the map and return a list with the return values. |
|           |                   | |
|           |                   | |
|           |                   | |
|           |                   | |
|           |                   | |
|           |                   | |

## <a name="5-functions-part-22"></a>5 - Functions (part 2/2)

### <a name="51-function-call-composition"></a>5.1 - Function call composition

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

#### <a name="511--tail-argument-function-chaninig"></a>5.1.1 - '|' Tail Argument Function Chaninig

This syntax is useful if you have following function call composition:

```text
(fn arg1 arg2 (fn2 arg_b1 arg_b2 (fn3 arg_c1 arg_c2 ...)))
```

These can be written more comfortably like this:

```text
fn3 arg1 arg2 | fn2 arg_b1 arg_b2 | fn arg1 arg2
```

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

#### <a name="512--left-hand-function-chaining"></a>5.1.2 - '|>' Left Hand Function Chaining

This syntax is useful if you want to make deep call chains like these:

```text
(((fn arg1 arg2 ...) arg_b1 arg_b2 ...) arg_c1 arg_c2 ...)
```

These can be written more comfortably like this:

```text
fn arg1 arg2 |> arg_b1 arg_b2 |> arg_c1 arg_c2
```

or nicer formatted:

```text
fn arg1 arg2
    |> arg_b1 arg_b2
    |> arg_c1 arg_c2
```

Here an actual example:

```wlambda
!res = $@v
    1 + 1
    |> $["abc", "def", "ceg"]
    |> { $+ ~ std:str:cat "|" _ "|" };

std:assert_eq res.0 "|c|";
std:assert_eq res.1 "|e|";
std:assert_eq res.2 "|g|";
```

The call reordering of the `|>` operator looks like this:

```text
    fn1 a1 a2 |> b1 b2    =>   ((   )   )
    """""""""    """""            ^   ^
        v          v              |   |
        -----------|--------------|   |
                   -------------------|
```
### <a name="52-control-flow---returning"></a>5.2 - Control Flow - Returning

WLambda uses labelled blocks for control flow, as returning from the current function would not be
very helpful for the control flow in wlambda in case of conditional execution.

```wlambda
!some_func = \:outer {
    !x = 10;

    # does stuff...

    (x == 10) {
        return :outer 20
    };

    # more stuff that is not executed if x == 10.
}
```

#### <a name="521-return-label-value"></a>5.2.1 - return [_label_] _value_

Returns _value_ from the current function if no _label_ is given.
If _label_ is given, the call stack will unwind until either a `block`
or a function with the given _label_ is encountered.

```wlambda
!f = {
    10;
    return 20;
    30
};

std:assert_eq f[] 20;
```

Here an example for unwinding two call frames:

```wlambda
!f = \:x {
    10;
    { return :x 20 }[];
    30;
};

std:assert_eq f[] 20;
```

The labels do not adhere to lexical scoping and are dynamically scoped:

```wlambda
!g = { return :x 30 };

!f = \:x { 20; g[]; 40 };

std:assert_eq f[] 30;
```

#### <a name="522-block-label-function"></a>5.2.2 - block [label] _function_

Calls the _function_ with the given _label_ for `return`to jump to.

If you just want to setup a point inside a function to jump to
with `return` the `block` function is more convenient to use:

```wlambda
!y = 1;

!res = block :x {
    .y = y + 1;
    (y >= 2) \return :x 20;
    .y = y + 1;
    .y = y + 1;
};

std:assert_eq res 20;
```

The alternative is the less clear syntax would be in this case:

```wlambda
!y = 1;

!res = \:x {
    .y = y + 1;
    (y >= 2) \return :x 20;
    .y = y + 1;
    .y = y + 1;
}[];

std:assert_eq res 20;
```


## <a name="6-conditional-execution---if--then--else"></a>6 - Conditional Execution - if / then / else

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

Furthermore, as `a.b` is equivalent to `b[a]`, one can also write this `sum` function
by simply invoking `(is_vec _)` and passing in the list of options as a parameter.

```wlambda
!sum = \|| std:fold 0 { _ + _1 } ~ (is_vec _) $[@, _];
```

When comparing the `pick` and indexing approaches it is important to note
that the two possible return values are inverted:

```wlambda
!x = 20;
!res = pick (x == 20) "x is 20" "x isn't 20";
std:assert_eq res "x is 20";

.res = $["x isn't 20", "x is 20"].(x == 20);
std:assert_eq res "x is 20";
```

With `pick`, the value to return in the `$t` case comes first, followed by the `$f` case's value,
whereas with indexing approach, the opposite is true.

## <a name="7-loops-and-iteration"></a>7 - Loops And Iteration

WLambda has many ways to loop and iterate:

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

### <a name="71-control-flow"></a>7.1 - Control Flow

#### <a name="711-while-predicate-fun"></a>7.1.1 - while _predicate_ _fun_

`while` will call _fun_ until the _predicate_ function returns `$false`.
This is the most basic loop for iteration:

```wlambda
!i   = 0;
!out = $[];

while { i < 10 } {
    std:push out i;
    .i = i + 1;
};

std:assert_eq (str out) "$[0,1,2,3,4,5,6,7,8,9]";
```

If you need an endless loop you can pass `$true` as predicate:

```wlambda

!i = 0;

while $true {
    (i >= 4) break;
    .i = i + 1;
};

std:assert_eq i 4;
```

#### <a name="712-range-start-end-step-fun"></a>7.1.2 - range _start_ _end_ _step_ _fun_

`range` counts from _start_ to _end_ by increments of _step_ and calls _fun_ with the counter. The
iteration is inclusive, this means if _start_ == _end_ the function _fun_ will be called once.

```wlambda
!out = $[];
range 0 9 1 {!(i) = @;
    std:push out i;
};

std:assert_eq (str out) "$[0,1,2,3,4,5,6,7,8,9]";
```

The construct also works for floating point numbers,
but be aware of the inherent floating point errors:

```wlambda
!out = $[];
range 0.3 0.4 0.01 {
    std:push out ~ std:num:round 100.0 * _;
};

# 40 is not in the set because the accumulation of 0.01 results
# in a value slightly above 0.4 and ends the range iteration:
std:assert_eq (str out) "$[30,31,32,33,34,35,36,37,38,39]";
```

#### <a name="713-break-value"></a>7.1.3 - break _value_

`break` stops the inner most iterative construct, which then will return _value_.
This should work for all repeatedly calling operations, such as
`for`, `while` and when calling lists directly. Also most library functions
that iteratively call you react to it, like `std:re:map` and `std:re:replace_all`.

```wlambda
!ret = range 0 9 1 {!(i) = @;
    (i > 4) { break :DONE };
};

std:assert_eq ret :DONE;
```

An example where the list iteration is stopped:

```wlambda
!val = $[1,2,3,4] { (_ > 3) { break :XX }; _ };

std:assert_eq val :XX;
```

### <a name="72-collection-iteration"></a>7.2 - Collection Iteration

#### <a name="721-iteration-over-vectors"></a>7.2.1 - Iteration over vectors

Iterating over a vector is the most basic iteration supported by WLambda.
You just call the vector with a function as first argument:

```wlambda
!sum = 0;
$[1, 2, 3] {
    .sum = sum + _;
};

std:assert_eq sum 6;
```

You can also use `for` if you like.

#### <a name="722-iteration-over-maps"></a>7.2.2 - Iteration over maps

Iterating over a map is as simple as iterating over a vector.
The map can be called with a function as first argument and it starts
iterating over it's key/value pairs. The first argument of the
function is the value, the second argument is the key.

```wlambda
!sum  = 0;
!keys = $[];

${a = 10, b = 20, c = 30} {
    !(v, k) = @;
    .sum = sum + v;
    std:push keys k;
};

std:assert_eq sum 60;
std:assert_eq (std:str:join "," ~ std:sort keys) "a,b,c";
```

You can also use `for` if you like.

#### <a name="723-for-iteratable-value-function"></a>7.2.3 - for _iteratable-value_ _function_

Calls _function_ for every element of _iteratable-value_.
Iteratable values are:

- Vectors
```wlambda
!product = 1;

for $[3,4,5] {
    .product = product * _;
};

std:assert_eq product 60;
```
- Maps
```wlambda
!product = 1;
!keys    = $[];

for ${a = 10, b = 20, c = 30} {
    !(v, k) = @;
    .product = product * v;
    std:push keys k;
};

std:assert_eq (std:str:join "," ~ std:sort keys) "a,b,c";

std:assert_eq product 6000;
```
- Byte Vectors
```wlambda
!byte_sum = 0;

for $b"abc" {
    .byte_sum = byte_sum + (int _);
};

std:assert_eq byte_sum 294;
```
- Strings
```wlambda
!str_chars = $[];

for "abc" {
    std:push str_chars _;
};

std:assert_eq (str str_chars) (str $["a", "b", "c"]);
```
- Symbols
```wlambda
!str_chars = $[];

for :abc {
    std:push str_chars _;
};

std:assert_eq (str str_chars) (str $["a", "b", "c"]);
```

### <a name="73-accumulation-and-collection"></a>7.3 - Accumulation and Collection

WLambda provides special syntax and semantics for accumulating or collecting
values while iterating through lists. There are following special syntax
constructs:

| Syntax            | Semantics |
|-------------------|-----------|
| $@v _expr_        | Setup collection of values in a vector, evaluates _expr_ and returns the vector. |
| $@vec _expr_      | Same as $@v |
| $@m _expr_        | Setup collection of key/value pairs in a map, evaluates _expr_ and returns the vector. |
| $@map _expr_      | Same as $@m |
| $@s _expr_        | Setup appending of values to a string, evaluates _expr_ and returns the string. |
| $@string _expr_   | Same as $@s |
| $@b _expr_        | Setup collection of values in a byte vector, evaluates _expr_ and returns byte vector. |
| $@bytes _expr_    | Same as $@b |
| $@i _expr_        | Setup accumulation in an integer, evaluates _expr_ and returns the integer sum. |
| $@int _expr_      | Same as $@i |
| $@f _expr_        | Setup accumulation in a float, evaluates _expr_ and returns the float sum. |
| $@flt _expr_      | Same as $@f |
| $+                | Evaluated to a function that can be called to add/append a new value to the current collection/accumulation. |
| $@@               | Access the current accumulation value. |

#### <a name="731-transforming-a-vector"></a>7.3.1 - Transforming a vector

If you just want to do something with items in a vector and
construct a new one from the results:

```wlambda
!result = $@vec $[1,2,3,4] \$+ _ * 2;   # multiply each item by 2

std:assert_eq (str result)  "$[2,4,6,8]";
```

#### <a name="732-example-of-"></a>7.3.2 - Example of `$@@`

Here is an interesting example how $@@ might be used:

```wlambda

!list_of_lists = $[];
!result = $@vec $[1,2,3,4] {
    $+ 2 * _;               # put the value into the list
    std:push list_of_lists
        ~ std:copy $@@; # construct a list of intermediate results
};

std:assert_eq (str result) "$[2,4,6,8]";

std:assert_eq (str list_of_lists)
    "$[$[2],$[2,4],$[2,4,6],$[2,4,6,8]]";
```

#### <a name="733-transforming-a-vector-to-a-map"></a>7.3.3 - Transforming a vector to a map

For constructing maps the `$@map` construct is available.
In the following example we transform a vector of pairs into a map:

```wlambda

!result = $@map $[ $[:a, 10], $[:b, 33], $[:c, 99] ] {
    !(key, value) = _;
    $+ key value;
};

std:assert_eq result.a 10;
std:assert_eq result.b 33;
std:assert_eq result.c 99;
```

#### <a name="734-iteratively-concatenating-strings"></a>7.3.4 - Iteratively concatenating strings

In case you need to construct a longer text the `$@string` construct allows
you to efficiently create a long string. For demonstration purposes
we compare the following inefficient code with the usage of `$@string`:

```wlambda
# Inefficient example:

!accum = "";
$["abc", "def", "ghi", "XXX"] {
    .accum = accum _;   # allocates a new string each iteration
};

std:assert_eq accum "abcdefghiXXX";
```

In theory for this constructed example the quickest way would
be to use `std:str:join`:

```wlambda
!accum = std:str:join "" $["abc", "def", "ghi", "XXX"];

std:assert_eq accum "abcdefghiXXX";
```

But maybe you need to transform or construct the strings before joining:

```wlambda
!transform = { ">" _ };

!accum = $@string $["abc", "def", "ghi", "XXX"] {
    $+[transform _] # appends the string to the accumulation string
};

std:assert_eq accum ">abc>def>ghi>XXX";
```

#### <a name="735-accumulating-sums"></a>7.3.5 - Accumulating sums

The following examples show how accumulation of values with `$@int` and `$@float` work.

```wlambda
!sum = $@int $[1,2,3,4] {
    $+ _
};

std:assert_eq sum 10;
```

And with floats:

```wlambda
!sum = $@float $[1.2,1.3,2.2,3.4] {
    $+ _
};

std:assert_eq (std:num:round 10.0 * sum) 81.0;
```


### <a name="74-utilities"></a>7.4 - Utilities

#### <a name="741-stdaccum-collection-a-b-"></a>7.4.1 - std:accum _collection_ _a_ _b_ ...

This function accumulates all it's arguments in the _collection_.
It does the same form of accumulation as `$+` does.

```wlambda
std:assert_eq (str ~ std:accum $[] 1 2 3)   "$[1,2,3]";
std:assert_eq (std:accum "" 1 2 3)          "123";
std:assert_eq (str ~ std:accum $b"" 1 2 3)  "\x01\x02\x03";
std:assert_eq (str ~ std:accum 10 1 2 3)    "16";
```

#### <a name="742-stdzip-vector-map-fn"></a>7.4.2 - std:zip _vector_ _map-fn_

Creates a generator that calls _map_fn_ with the consecutive elements of _vector_
as the first argument of _map-fn_. All arguments passed to std:zip
are appended to the argument list.

This is useful for combining the iteration over two vectors or collections.

```wlambda
!l = $@v $[13, 42, 97] ~ std:zip $["Foo", "Bar", "Baz"] { $+ @ };
std:assert_eq (str l) (str $[$["Foo", 13], $["Bar", 42], $["Baz", 97]]);
```

#### <a name="743-stdenumerate-map-fn"></a>7.4.3 - std:enumerate _map-fn_

Creates a generator that calls _map-fn_ with a counter that is incremented
after each call, starting with 0. All received arguments are appended to
the argument list after the counter.

```wlambda
!l = $@v $["lo", "mid", "hi"] ~ std:enumerate { $+ @ };
std:assert_eq (str l) (str $[$[0, "lo"], $[1, "mid"], $[2, "hi"]]);
```

## <a name="8-operators"></a>8 - Operators
### <a name="81-arithmetic"></a>8.1 - Arithmetic

The output type (float vs. integer) of the numerical arithmetic operators is defined
by the _first_ operand of the operation. Use the casting functions `float` or
`int` if you are unsure.

Please note that not all operators are available as plain identifiers and need
to be quoted when used in their prefix form or as functions, some of them are
`*`, `/`, `%` and some others.

#### <a name="811--operand-1-operand-2-"></a>8.1.1 - + _operand-1_ _operand-2_ ...

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

#### <a name="812---operand-1-operand-2-"></a>8.1.2 - - _operand-1_ _operand-2_ ...

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

#### <a name="813--op-a-op-b"></a>8.1.3 - * _op-a_ _op-b_

Returns the multiplication of the two operands.

```wlambda
std:assert 10   * 4 == 40;
std:assert 10.1 * 4 == 40.4;
std:assert "10" * 4 == 40;

std:assert (`*` 10 4) == 40;

std:assert (float "10.1") * 4 == 40.4;
```

#### <a name="814--op-a-op-b"></a>8.1.4 - / _op-a_ _op-b_

Returns the division of the two operands.

```wlambda
std:assert 10   / 4 == 2;
std:assert 10.0 / 4 == 2.5;
std:assert "10" / 2 == 5;

std:assert (`/` 10 4) == 2;

std:assert (float "10.1") * 4 == 40.4;
```

#### <a name="815--op-a-op-b"></a>8.1.5 - % _op-a_ _op-b_

Returns the remainder of the division of _op-a_ by _op-b_.

```wlambda
std:assert     5 % 4 == 1;
std:assert (`%` 5 4) == 1;
```

#### <a name="816--op-a-op-b"></a>8.1.6 - ^ _op-a_ _op-b_

Returns _op-a_ raised by the power of _op-b_.
Supports float and integers.

```wlambda
std:assert_eq 2 ^ 4     16;
std:assert_eq std:num:round[(2.0 ^ 2.1) * 1000] 4287.0;
std:assert_eq 2 ^ 2.1   4; # first arg type matters!
```

### <a name="82-comparison"></a>8.2 - Comparison

#### <a name="821--op-a-op-b"></a>8.2.1 - == _op-a_ _op-b_

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

#### <a name="822--op-a-op-b"></a>8.2.2 - != _op-a_ _op-b_

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

#### <a name="823--op-a-op-b"></a>8.2.3 - < _op-a_ _op-b_

Numerical comparison operator that checks whether _op-a_ is less than _op-b_

```wlambda
std:assert   10   < 11;
std:assert   10.1 < 10.2;
std:assert not[10 < 10.1];  # the type of the first argument decides return type!
```

#### <a name="824--op-a-op-b"></a>8.2.4 - <= _op-a_ _op-b_

Numerical comparison operator that checks whether _op-a_ is less or equal to _op-b_

```wlambda
std:assert 10   <= 11;
std:assert 10.1 <= 10.2;
std:assert 10   <= 10.1;  # integer <=, the type of the first argument decides return type!
```

#### <a name="825--op-a-op-b"></a>8.2.5 - > _op-a_ _op-b_

Numerical comparison operator that checks whether _op-a_ is greater than _op-b_

```wlambda
std:assert   11.1 > 11;
std:assert   11.1 > 11.0;
std:assert not[10 > 10.1];  # the type of the first argument decides return type!
```

#### <a name="826--op-a-op-b"></a>8.2.6 - >= _op-a_ _op-b_

Numerical comparison operator that checks whether _op-a_ is greater or equal to _op-b_

```wlambda
std:assert 11   >= 11;
std:assert 10.2 >= 10.1;
std:assert 10 >= 10.1;  # integer >=, the type of the first argument decides return type!
```

### <a name="83-bit-operations"></a>8.3 - Bit Operations

#### <a name="831--op-a-op-b"></a>8.3.1 - & _op-a_ _op-b_

Binary `and` operation between two integers.

```wlambda
std:assert (0b0011 & 0b1011) == 0b011;
std:assert (3      &     11) == 3;
```

#### <a name="832--op-a-op-b"></a>8.3.2 - &^ _op-a_ _op-b_

Binary `xor` operation between two integers.

```wlambda
std:assert (0b0011 &^ 0b1011) == 0b1000;
std:assert (3      &^     11) == 8;
```

#### <a name="833--op-a-op-b"></a>8.3.3 - &| _op-a_ _op-b_

Binary `or` operation between two integers.

```wlambda
std:assert (0b0011 &| 0b1000) == 0b1011;
std:assert (3      &|      8) == 11;
```

#### <a name="834--op-a-op-b"></a>8.3.4 - << _op-a_ _op-b_

Binary `left shift` operation of _op-a_ by _op-b_ bits.

```wlambda
std:assert (0b0011 << 3)   == 0b11000;
std:assert (`<<` 0b1011 2) == 0b101100
```

#### <a name="835--op-a-op-b"></a>8.3.5 - >> _op-a_ _op-b_

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

**Warning:** Do not expect the declared variables in the module to exist beyond
execution time. Weak caught values will vanish like usual once the module scope
is exited. This means, if you declare helper functions in local variables, do
this with the `:global` modifier:

```wlambda
!:global helper = { _ * 2 };

!@export doit = { helper 10 };
```

Alternatively make the helper a strong reference:

```wlambda
!helper = $&& { _ * 2 };

!@export doit = { helper 10 };
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

## <a name="11-standard-library"></a>11 - Standard Library

#### <a name="1101-stdshuffle-randfunc-vec"></a>11.0.1 - std:shuffle _rand_func_ _vec_

Shuffles the _vec_ in place. The function _rand_func_ needs to return
a random 64 bit integer on each call. Here is an example:

```wlambda
!sm  = std:rand:split_mix64_new_from 1234;
!vec = $[1,2,3,4,5,6,7,8];
std:shuffle { std:rand:split_mix64_next sm } vec;

std:assert_eq (str vec) "$[2,1,7,4,8,5,3,6]";
```

#### <a name="1102-stdcopy-vecormap"></a>11.0.2 - std:copy _vec_or_map_

Makes a shallow copy of the given vector or map.

```wlambda
!a = $[1,2,3];
!b = std:copy a;
b.0 = 10;

std:assert_eq a.0 1;
std:assert_eq b.0 10;
```

#### <a name="1103-stdsort-comparefun-vec"></a>11.0.3 - std:sort [_compare_fun_] _vec_

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

#### <a name="1104-stdcmpnumasc-a-b"></a>11.0.4 - std:cmp:num:asc _a_ _b_

Compares _a_ and _b_ numerically and returns:

| Cases         | Return Value |
|---------------|--------------|
| _a_ > _b_     | -1           |
| _a_ == _b_    | 0            |
| _a_ < _b_     | 1            |

```wlambda
std:assert_eq (std:cmp:num:asc 20 2)        -1;
std:assert_eq (std:cmp:num:asc "20" "20")    0;
std:assert_eq (std:cmp:num:asc 20 21)        1;
```

#### <a name="1105-stdcmpnumdesc-a-b"></a>11.0.5 - std:cmp:num:desc _a_ _b_

Compares _a_ and _b_ numerically descending and returns:

| Cases         | Return Value |
|---------------|--------------|
| _a_ > _b_     | 1            |
| _a_ == _b_    | 0            |
| _a_ < _b_     | -1           |

```wlambda
std:assert_eq (std:cmp:num:desc "20" "2")     1;
std:assert_eq (std:cmp:num:desc "20" "20")    0;
std:assert_eq (std:cmp:num:desc 20 21)       -1;
```

#### <a name="1106-stddisplayln-arg1-"></a>11.0.6 - std:displayln _arg1_ ...

This function writes a humand readable version of all the arguments
(with a space inbetween) to the standard output. This means that:

```text
std:displayln "foo"
```

Will just print `foo` and a newline.

If you need a less ambigous form, use `std:writeln`, which
handles it's argument like written via `std:str:write` instead of `str`.

#### <a name="1107-stdwriteln-arg1-"></a>11.0.7 - std:writeln _arg1_ ...

This function writes the WLambda representation of it's arguments
(with a space inbetween) to standard output. This means that:

```text
std:displayln "foo"
```

Will print `"foo"` and a newline.

See also the description of `std:str:write`.

If you need a more human readable form use `std:displayln`.

#### <a name="1108-stdstrwrite-arg"></a>11.0.8 - std:str:write _arg_

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

#### <a name="1109-stdeval-code-string"></a>11.0.9 - std:eval _code-string_

Evaluates _code-string_ in the current global environment and returns
the generated value. If the code leads to any kind of evaluation error,
an error object is returned.

```wlambda
std:assert_eq (std:eval "1 + 2") 3;
!:global X = 20;
std:assert_eq (std:eval "1 + X") 21;
```

#### <a name="11010-stdassert-bool-message"></a>11.0.10 - std:assert _bool_ \[_message_]

Just a simple assertion function that panics if the first argument is not true.
Returns the passed value if it is a true value.
You can pass an optional message as second parameter.

```norun_wlambda
std:assert $false; #=> Panic
std:assert 120;    #=> 120
```

#### <a name="11011-stdasserteq-actual-expected-message"></a>11.0.11 - std:assert_eq _actual_ _expected_ \[_message_]

This function check if the _actual_ value is equal to the
_expected_ value and panics if not. The optional _message_ is
passed in the panic for reference.

```wlambda
!x = 30 * 2;
std:assert_eq x 60 "30 * 2 == 60";
```

#### <a name="11012-stdwlambdaversion"></a>11.0.12 - std:wlambda:version

Returns the version number of the WLambda crate when called.

### <a name="111-io"></a>11.1 - I/O

#### <a name="1111-stdiofilereadtext-filename"></a>11.1.1 - std:io:file:read_text _filename_

Opens the file _filename_ and returns it's contents interpreted as UTF8
text as string.

```wlambda
std:io:file:write_safe "prelude_test.txt" "abcäöü";

!t = std:io:file:read_text "prelude_test.txt";
std:assert_eq t "abcäöü" "reading text from file works";
```

#### <a name="1112-stdiofileread-filename"></a>11.1.2 - std:io:file:read _filename_

Opens the file _filename_ and returns it's contents as byte buffer.

```wlambda
std:io:file:write_safe "prelude_test.txt" "abcäöü";

!t = std:io:file:read "prelude_test.txt";
.t = std:str:from_utf8 t;
std:assert_eq t "abcäöü" "reading binary from file works";
```

#### <a name="1113-stdiofilewritesafe-filename-bytes-or-string"></a>11.1.3 - std:io:file:write_safe _filename_ _bytes-or-string_

Creates a new file with the given filename but with a "~" appended
and writes the contents into it. After successful write, it renames
the file to the given filename.

#### <a name="1114-stdiofileappend-filename-bytes-or-string"></a>11.1.4 - std:io:file:append _filename_ _bytes-or-string_

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
            for (v, k) in val.iter() {
                let n =
                    if let Some(k) = k { env.push(k); 2 }
                    else               { 1 };
                env.push(v);
                match f.call_internal(env, n) {
                    Ok(v)                      => { ret = v; },
                    Err(StackAction::Break(v)) => { env.popn(n); return Ok(v); },
                    Err(StackAction::Next)     => { },
                    Err(e)                     => { env.popn(n); return Err(e); }
                }
                env.popn(n);
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

fn print_value(env: &mut Env, argc: usize, raw: bool) -> Result<VVal, StackAction> {
    let mut write = env.stdio.write.borrow_mut();

    for i in 0..argc {
        let s =
            if raw { env.arg(i).s_raw() }
            else { env.arg(i).s() };

        if i == (argc - 1) {
            if i > 0 {
                writeln!(write, " {}", s).ok();
            } else {
                writeln!(write, "{}", s).ok();
            }
        } else if i > 0 {
            write!(write, " {}", s).ok();
        } else {
            write!(write, "{}", s).ok();
        }
    }
    if argc == 0 {
        writeln!(write, "").ok();
    }
    if argc > 0 {
        Ok(env.arg(argc - 1))
    } else {
        Ok(VVal::Nul)
    }
}

/// Returns a SymbolTable with all WLambda standard library language symbols.
pub fn std_symbol_table() -> SymbolTable {
    let mut st = SymbolTable::new();

    func!(st, "not_i64",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Int(!env.arg(0).i()))
        }, Some(1), Some(1), false);
    func!(st, "neg_i64",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Int(env.arg(0).i().wrapping_neg()))
        }, Some(1), Some(1), false);
    func!(st, "not_u32",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Int(i64::from(!(env.arg(0).i() as u32))))
        }, Some(1), Some(1), false);
    func!(st, "neg_u32",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Int(i64::from((env.arg(0).i() as u32).wrapping_neg())))
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

    func!(st, "accum",
        |env: &mut Env, argc: usize| {
            let mut v = env.arg(0);
            for i in 1..argc {
                v.accum(&env.arg(i));
            }
            Ok(v)
        }, Some(2), None, false);

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
    func!(st, "str:trim",
        |env: &mut Env, _argc: usize| { Ok(VVal::new_str_mv(env.arg(0).s_raw().trim().to_string())) },
        Some(1), Some(1), false);
    func!(st, "str:trim_start",
        |env: &mut Env, _argc: usize| { Ok(VVal::new_str_mv(env.arg(0).s_raw().trim_start().to_string())) },
        Some(1), Some(1), false);
    func!(st, "str:trim_end",
        |env: &mut Env, _argc: usize| { Ok(VVal::new_str_mv(env.arg(0).s_raw().trim_end().to_string())) },
        Some(1), Some(1), false);
    func!(st, "str:pad_start",
        |env: &mut Env, _argc: usize| {
            let len   = env.arg(0).i() as usize;
            let pads  = env.arg(1).s_raw();
            let mut s = env.arg(2).s_raw();

            let mut src_len = s.chars().count();
            let pad_len     = pads.chars().count();

            if pad_len == 0 {
                return Ok(VVal::new_str_mv(s));
            }

            while src_len < len {
                if len - src_len < pad_len {
                    for c in pads.chars().rev() {
                        s.insert(0, c);
                        src_len += 1;
                        if src_len >= len {
                            break;
                        }
                    }
                } else {
                    s.insert_str(0, &pads);
                }
                src_len += pad_len;
            }

            Ok(VVal::new_str_mv(s))
        }, Some(3), Some(3), false);

    func!(st, "str:pad_end",
        |env: &mut Env, _argc: usize| {
            let len   = env.arg(0).i() as usize;
            let pads  = env.arg(1).s_raw();
            let mut s = env.arg(2).s_raw();

            let mut src_len = s.chars().count();
            let pad_len     = pads.chars().count();

            if pad_len == 0 {
                return Ok(VVal::new_str_mv(s));
            }

            while src_len < len {
                if len - src_len < pad_len {
                    for c in pads.chars() {
                        s.push(c);
                        src_len += 1;
                        if src_len >= len {
                            break;
                        }
                    }
                } else {
                    s.push_str(&pads);
                }
                src_len += pad_len;
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
            let sep = env.arg(0);
            let lst = env.arg(1);
            if let VVal::Lst(l) = lst {
                let mut s = VVal::new_str("");
                let mut first = true;
                for item in l.borrow().iter() {
                    if !first {
                        s.accum(&sep);
                    } else {
                        first = false;
                    }
                    s.accum(item);
                }
                Ok(s)

            } else {
                Ok(env.new_err(
                    format!(
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
                        Ok(env.new_err(
                            format!("str:from_utf8 decoding error: {}", e)))
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
            for (vc, _) in env.arg(0).iter() {
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
                return Ok(env.new_err(
                    format!(
                        "fold only works with lists as argument, got '{}'",
                        lst.s())));
            }

            Ok(acc)
        }, Some(3), Some(3), false);

    func!(st, "enumerate",
        |env: &mut Env, _argc: usize| {
            let f = env.arg(0);
            let i = Rc::new(std::cell::RefCell::new(0));

            Ok(VValFun::new_fun(
                move |env: &mut Env, argc: usize| {
                    env.push(VVal::Int(*i.borrow() as i64));
                    let r = f.call_internal(env, 1 + argc);
                    *i.borrow_mut() += 1;
                    env.popn(1);
                    r
                }, None, None, false))
        }, Some(1), Some(1), false);

    func!(st, "zip",
        |env: &mut Env, _argc: usize| {
            let o = env.arg(0);
            let f = env.arg(1);
            let i = Rc::new(std::cell::RefCell::new(0));

            Ok(VValFun::new_fun(
                move |env: &mut Env, argc: usize| {
                    env.push(o.at(*i.borrow()).unwrap_or(VVal::Nul));
                    let r = f.call_internal(env, 1 + argc);
                    *i.borrow_mut() += 1;
                    env.popn(1);
                    r
                }, None, None, false))
        }, Some(2), Some(2), false);

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

    func!(st, "io:lines",
        |env: &mut Env, _argc: usize| {
            let f = env.arg(0);
            let mut ret = VVal::Nul;
            loop {
                let mut line = String::new();
                {
                    let mut read = env.stdio.read.borrow_mut();
                    match read.read_line(&mut line) {
                        Ok(n) => { if n == 0 { break; } },
                        Err(e) => {
                            return Ok(env.new_err(
                                format!("IO-Error on std:io:lines: {}", e)))
                        },
                    }
                }

                env.push(VVal::new_str_mv(line));
                match f.call_internal(env, 1) {
                    Ok(v)                      => { ret = v; },
                    Err(StackAction::Break(v)) => { env.popn(1); return Ok(v); },
                    Err(StackAction::Next)     => { },
                    Err(e)                     => { env.popn(1); return Err(e); }
                }
                env.popn(1);
            }

            Ok(ret)
        }, Some(1), Some(1), false);

    func!(st, "io:stdout:flush",
        |env: &mut Env, _argc: usize| {
            if let Err(e) = env.stdio.write.borrow_mut().flush() {
                Ok(env.new_err(
                    format!("IO-Error on std:io:stdout:flush: {}", e)))
            } else {
                Ok(VVal::Bol(true))
            }
        }, Some(0), Some(0), false);

    func!(st, "io:stdout:newline",
        |env: &mut Env, _argc: usize| {
            if let Err(e) = writeln!(*env.stdio.write.borrow_mut(), "") {
                Ok(env.new_err(
                    format!("IO-Error on std:io:stdout:newline: {}", e)))
            } else {
                Ok(VVal::Bol(true))
            }
        }, Some(0), Some(0), false);

    func!(st, "io:stdout:write",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            if let Err(e) = write!(*env.stdio.write.borrow_mut(), "{}", v.s()) {
                Ok(env.new_err(
                    format!("IO-Error on std:io:stdout:write: {}", e)))
            } else {
                Ok(v)
            }
        }, Some(1), Some(1), false);

    func!(st, "io:stdout:print",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            if let Err(e) = write!(*env.stdio.write.borrow_mut(), "{}", v.s_raw()) {
                Ok(env.new_err(
                   format!("IO-Error on std:io:stdout:print: {}", e)))
            } else {
                Ok(v)
            }
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
                    Ok(env.new_err(
                        format!("Couldn't open file '{}': {}", filename, e)))
                },
                Ok(mut f) => {
                    let mut contents = String::new();
                    if let Err(e) = f.read_to_string(&mut contents) {
                        Ok(env.new_err(
                            format!(
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
                    Ok(env.new_err(
                        format!("Couldn't open file '{}': {}", filename, e)))
                },
                Ok(mut f) => {
                    let mut contents : Vec<u8> = Vec::new();
                    if let Err(e) = f.read_to_end(&mut contents) {
                        Ok(env.new_err(
                            format!(
                                "Couldn't read text from file '{}': {}",
                                filename, e)))
                    } else {
                        Ok(VVal::new_byt(contents))
                    }
                },
            }
        }, Some(1), Some(1), false);

    func!(st, "io:file:copy",
        |env: &mut Env, _argc: usize| {
            let from = env.arg(0).s_raw();
            let to   = env.arg(1).s_raw();

            use std::path::Path;

            match std::fs::copy(Path::new(&from), Path::new(&to)) {
                Ok(_) => Ok(VVal::Bol(true)),
                Err(e) => {
                    Ok(env.new_err(
                        format!(
                            "Couldn't copy file '{}' to file '{}': {}",
                            from, to, e)))
                },
            }
        }, Some(2), Some(2), false);

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
                    Ok(env.new_err(
                        format!(
                            "Couldn't open file '{}': {}",
                            filename, e)))
                },
                Ok(mut f) => {
                    if let Err(e) = f.write_all(&buf) {
                        return Ok(env.new_err(
                            format!(
                                "Couldn't write to file '{}': {}",
                                tmp_filename, e)));
                    }

                    if let Err(e) = std::fs::rename(&tmp_filename, &filename) {
                        return Ok(env.new_err(
                            format!(
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
                    Ok(env.new_err(
                        format!(
                            "Couldn't open file '{}': {}",
                            filename, e)))
                },
                Ok(mut f) => {
                    if let Err(e) = f.write_all(&buf) {
                        Ok(env.new_err(
                            format!(
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
            print_value(env, argc, false)
        }, None, None, false);

    func!(st, "displayln",
        |env: &mut Env, argc: usize| {
            print_value(env, argc, true)
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
                Err(e) => Ok(env.new_err(e)),
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
                return Ok(env.new_err(
                    format!("Regex '{}' did not compile: {}", re, e)));
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
                return Ok(env.new_err(
                    format!("Regex '{}' did not compile: {}", re, e)));
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
                return Ok(env.new_err(
                    format!("Regex '{}' did not compile: {}", re, e)));
            }
            let rx = rx.unwrap();

            let mut ret = VVal::Nul;
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
                    Ok(v)                      => { ret = v; },
                    Err(StackAction::Break(v)) => { ret = v; break; },
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
                Err(e) => Ok(env.new_err(e)),
            }
        }, Some(1), Some(2), false);

    #[cfg(feature="serde_json")]
    func!(st, "deser:json",
        |env: &mut Env, _argc: usize| {
            let s = env.arg(0).s_raw();

            match VVal::from_json(&s) {
                Ok(v) => Ok(v),
                Err(e) => Ok(env.new_err(e)),
            }
        }, Some(1), Some(1), false);

    #[cfg(feature="rmp-serde")]
    func!(st, "ser:msgpack",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            match v.to_msgpack() {
                Ok(s) => Ok(VVal::new_byt(s)),
                Err(e) => Ok(env.new_err(e)),
            }
        }, Some(1), Some(1), false);

    #[cfg(feature="rmp-serde")]
    func!(st, "deser:msgpack",
        |env: &mut Env, _argc: usize| {
            if let VVal::Byt(u) = env.arg(0) {
                match VVal::from_msgpack(&u.borrow()[..]) {
                    Ok(v) => Ok(v),
                    Err(e) => Ok(env.new_err(e)),
                }
            } else {
                Ok(env.new_err("deser:msgpack expects bytes".to_string()))
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
