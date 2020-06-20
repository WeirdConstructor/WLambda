// Copyright (c) 2020 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!
This module defines some default functions and operations
available in the WLambda language.

You there are two WLambda modules provided by this module:

- [core_symbol_table()](fn.core_symbol_table.html)
- [std_symbol_table()](fn.std_symbol_table.html)

[]: ---- REFERENCE DOC START ----

# WLambda Language Reference

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

- [1](#1-variable-definition-and-assignment) Variable Definition and Assignment
  - [1.1](#11-destructuring-to-variables) Destructuring to Variables
  - [1.2](#12-global-variables) Global Variables
  - [1.3](#13-constants) Constants
- [2](#2-functions-part-12) Functions (part 1/2)
  - [2.1](#21-closures) Closures
    - [2.1.1](#211-object-oriented-programming-with-closures) Object Oriented Programming with Closures
  - [2.2](#22-function-calling) Function calling
  - [2.3](#23-function-arity-checks) Function arity checks
    - [2.3.1](#231-stdtonoarity-function) std:to\_no\_arity _function_
  - [2.4](#24-calling-fields--method-calling) Calling fields / Method calling
    - [2.4.1](#241-object-oriented-programming-with-prototypes) Object Oriented Programming with Prototypes
  - [2.5](#25-function-call-composition) Function call composition
    - [2.5.1](#251--tail-argument-function-chaninig) '|' Tail Argument Function Chaninig
    - [2.5.2](#252--left-hand-function-chaining) '|>' Left Hand Function Chaining
    - [2.5.3](#253-forward-argument-pipe-arg--fun) Forward Argument Pipe `arg &> fun`
    - [2.5.4](#254-reverse-argument-pipe-fun--arg) Reverse Argument Pipe `fun <& arg`
  - [2.6](#26-control-flow---returning) Control Flow - Returning
    - [2.6.1](#261-return-label-value) return [_label_] _value_
    - [2.6.2](#262-block-label-function) block [label] _function_
    - [2.6.3](#263-stdtodrop-function-or-raii-destructors-or-drop-functions) std:to\_drop _function_ (or RAII, Destructors or Drop Functions)
  - [2.7](#27-function-utilities) Function utilities
    - [2.7.1](#271-isfun-value) is\_fun _value_
- [3](#3-data-types) Data Types
  - [3.1](#31-none-sentinel-value-n-or-none) None sentinel value: `$n` or `$none`
    - [3.1.1](#311-isnone-value) is\_none _value_
    - [3.1.2](#312-issome-value) is\_some _value_
  - [3.2](#32-optional-values-o-and-o) Optional values `$o()` and `$o(...)`
    - [3.2.1](#321-isoptional-value) is\_optional _value_
    - [3.2.2](#322-unwrapping-optionals) Unwrapping optionals
  - [3.3](#33-error-values-e-expr-or-error-expr) Error values: `$e expr` or `$error expr`
    - [3.3.1](#331--label-value) _? [_label_] _value_
    - [3.3.2](#332-onerror-handler-maybe-error-value) on\_error _handler_ _maybe-error-value_
    - [3.3.3](#333-iserr-value) is\_err _value_
    - [3.3.4](#334-errortostr-value) error\_to\_str _value_
  - [3.4](#34-booleans) Booleans
    - [3.4.1](#341-isbool-any-value) is\_bool _any-value_
    - [3.4.2](#342-bool-any-value) bool _any-value_
    - [3.4.3](#343-not-value) not _value_
    - [3.4.4](#344-boolean-list-indexing) Boolean List Indexing
  - [3.5](#35-64-bit-integers) 64-Bit Integers
    - [3.5.1](#351-int-value) int _value_
    - [3.5.2](#352-isint-value) is\_int _value_
    - [3.5.3](#353-stdnegi64-integer) std:neg\_i64 _integer_
    - [3.5.4](#354-stdnoti64-integer) std:not\_i64 _integer_
    - [3.5.5](#355-stdnegu32-integer) std:neg\_u32 _integer_
    - [3.5.6](#356-stdnotu32-integer) std:not\_u32 _integer_
  - [3.6](#36-64-bit-floats) 64-Bit Floats
    - [3.6.1](#361-float-value) float _value_
    - [3.6.2](#362-isfloat-value) is\_float _value_
    - [3.6.3](#363-stdnumacos-float) std:num:acos _float_
    - [3.6.4](#364-stdnumacosh-float) std:num:acosh _float_
    - [3.6.5](#365-stdnumasin-float) std:num:asin _float_
    - [3.6.6](#366-stdnumasinh-float) std:num:asinh _float_
    - [3.6.7](#367-stdnumatan-float) std:num:atan _float_
    - [3.6.8](#368-stdnumatan2-y-x) std:num:atan2 _y_ _x_
    - [3.6.9](#369-stdnumatanh-float) std:num:atanh _float_
    - [3.6.10](#3610-stdnumcbrt-float) std:num:cbrt _float_
    - [3.6.11](#3611-stdnumceil-float) std:num:ceil _float_
    - [3.6.12](#3612-stdnumcos-float) std:num:cos _float_
    - [3.6.13](#3613-stdnumcosh-float) std:num:cosh _float_
    - [3.6.14](#3614-stdnumexp-float) std:num:exp _float_
    - [3.6.15](#3615-stdnumexp2-float) std:num:exp2 _float_
    - [3.6.16](#3616-stdnumexpm1-float) std:num:exp\_m1 _float_
    - [3.6.17](#3617-stdnumfloor-float) std:num:floor _float_
    - [3.6.18](#3618-stdnumhypot-y-x) std:num:hypot _y_ _x_
    - [3.6.19](#3619-stdnumln-float) std:num:ln _float_
    - [3.6.20](#3620-stdnumlog-float) std:num:log _float_
    - [3.6.21](#3621-stdnumlog10-float) std:num:log10 _float_
    - [3.6.22](#3622-stdnumlog2-float) std:num:log2 _float_
    - [3.6.23](#3623-stdnumpow-float) std:num:pow _float_
    - [3.6.24](#3624-stdnumrecip-float) std:num:recip _float_
    - [3.6.25](#3625-stdnumround-float) std:num:round _float_
    - [3.6.26](#3626-stdnumsin-float) std:num:sin _float_
    - [3.6.27](#3627-stdnumsinh-float) std:num:sinh _float_
    - [3.6.28](#3628-stdnumsqrt-float) std:num:sqrt _float_
    - [3.6.29](#3629-stdnumtan-float) std:num:tan _float_
    - [3.6.30](#3630-stdnumtanh-float) std:num:tanh _float_
    - [3.6.31](#3631-stdnumtodegrees-float) std:num:to\_degrees _float_
    - [3.6.32](#3632-stdnumtoradians-float) std:num:to\_radians _float_
    - [3.6.33](#3633-stdnumtrunc-float) std:num:trunc _float_
    - [3.6.34](#3634-stdnumlerp-a-b-x) std:num:lerp _a_ _b_ _x_
    - [3.6.35](#3635-stdnumsmoothstep-a-b-x) std:num:smoothstep _a_ _b_ _x_
  - [3.7](#37-numeric-functions) Numeric Functions
    - [3.7.1](#371-stdnumabs-number) std:num:abs _number_
  - [3.8](#38-numerical-mathematical-vectors) Numerical Mathematical Vectors
    - [3.8.1](#381-vector-conversions) Vector Conversions
    - [3.8.2](#382-vector-component-access) Vector Component Access
    - [3.8.3](#383-named-field-access-and-swizzling) Named Field Access and Swizzling
    - [3.8.4](#384-euler-additionsubtraction) Euler Addition/Subtraction
    - [3.8.5](#385-scalar-multiplicationdivision) Scalar Multiplication/Division
    - [3.8.6](#386-unary-vector-operations) Unary Vector Operations
    - [3.8.7](#387-stdvdims-vec) std:v:dims _vec_
    - [3.8.8](#388-stdvmag2-vec) std:v:mag2 _vec_
    - [3.8.9](#389-stdvmag-vec) std:v:mag _vec_
    - [3.8.10](#3810-stdvnorm-vec) std:v:norm _vec_
    - [3.8.11](#3811-stdvdot-vec1-vec2) std:v:dot _vec1_ _vec2_
    - [3.8.12](#3812-stdvcross-vec1-vec2) std:v:cross _vec1_ _vec2_
    - [3.8.13](#3813-stdvlerp-vec1-vec2-t) std:v:lerp _vec1_ _vec2_ _t_
    - [3.8.14](#3814-stdvslerp-vec1-vec2-t) std:v:slerp _vec1_ _vec2_ _t_
    - [3.8.15](#3815-stdvvec2rad-vec) std:v:vec2rad _vec_
    - [3.8.16](#3816-stdvrad2vec-radians) std:v:rad2vec _radians_
    - [3.8.17](#3817-stdvhex2rgbaf-string) std:v:hex2rgba\_f _string_
    - [3.8.18](#3818-stdvhex2rgbai-string) std:v:hex2rgba\_i _string_
  - [3.9](#39-strings) Strings
    - [3.9.1](#391-string-literal-syntaxes) String Literal Syntaxes
    - [3.9.2](#392-str-value) str _value_
    - [3.9.3](#393-isstr-value) is\_str _value_
    - [3.9.4](#394-stdstrcat-a-b-) std:str:cat _a_ _b_ ...
    - [3.9.5](#395-stdstrjoin-sep-vector) std:str:join _sep_ _vector_
    - [3.9.6](#396-stdstrlen-value) std:str:len _value_
    - [3.9.7](#397-stdstrreplace-pattern-replacement-string) std:str:replace _pattern_ _replacement_ _string_
    - [3.9.8](#398-stdstrreplacen-pattern-replacement-count-string) std:str:replace\_n _pattern_ _replacement_ _count_ _string_
    - [3.9.9](#399-stdstrtrim-value) std:str:trim _value_
    - [3.9.10](#3910-stdstrtrimstart-value) std:str:trim\_start _value_
    - [3.9.11](#3911-stdstrtrimend-value) std:str:trim\_end _value_
    - [3.9.12](#3912-stdstrpadstart-len-pad-str-value) std:str:pad\_start _len_ _pad-str_ _value_
    - [3.9.13](#3913-stdstrpadend-len-pad-str-value) std:str:pad\_end _len_ _pad-str_ _value_
    - [3.9.14](#3914-stdstrtobytes-string) std:str:to\_bytes _string_
    - [3.9.15](#3915-stdstrfromutf8-byte-vector) std:str:from\_utf8 _byte-vector_
    - [3.9.16](#3916-stdstrfromutf8lossy-byte-vector) std:str:from\_utf8\_lossy _byte-vector_
    - [3.9.17](#3917-stdstrtocharvec-string) std:str:to\_char\_vec _string_
    - [3.9.18](#3918-stdstrfromcharvec-vector) std:str:from\_char\_vec _vector_
    - [3.9.19](#3919-stdstrtolowercase-string) std:str:to\_lowercase _string_
    - [3.9.20](#3920-stdstrtouppercase-string) std:str:to\_uppercase _string_
  - [3.10](#310-bytes-or-byte-vectors) Bytes (or Byte Vectors)
    - [3.10.1](#3101-call-properties-of-bytes) Call Properties of Bytes
    - [3.10.2](#3102-byte-conversion-functions) Byte Conversion Functions
    - [3.10.3](#3103-isbytes-value) is\_bytes _value_
  - [3.11](#311-symbols) Symbols
    - [3.11.1](#3111-stdsymbolscollect) std:symbols:collect
  - [3.12](#312-pairs-pa-b) Pairs `$p(a, b)`
    - [3.12.1](#3121-pair-operator-a--b) Pair Operator `a => b`
    - [3.12.2](#3122-pair-constructor-a--b) Pair Constructor `a => b`
    - [3.12.3](#3123-cons-a-b) cons _a_ _b_
    - [3.12.4](#3124-pair-stringbyte-vector-operations) Pair string/byte vector operations
      - [3.12.4.1](#31241-p-from--count--string-or-byte-vec) $p( _from_ , _count_ ) _string-or-byte-vec_
      - [3.12.4.2](#31242-p-pattern--replacement--string-or-byte-vec) $p( _pattern_ , _replacement_ ) _string-or-byte-vec_
      - [3.12.4.3](#31243-p-split-pattern--max--string-or-byte-vec) $p( _split-pattern_ , _max_ ) _string-or-byte-vec_
    - [3.12.5](#3125-pair-to-iterator) Pair to Iterator
      - [3.12.5.1](#31251-iter---range) Iter - Range
      - [3.12.5.2](#31252-iter---enumerate) Iter - Enumerate
      - [3.12.5.3](#31253-iter---values) Iter - Values
      - [3.12.5.4](#31254-iter---keys) Iter - Keys
    - [3.12.6](#3126-ispair-value) is\_pair _value_
  - [3.13](#313-vectors-or-lists) Vectors (or Lists)
    - [3.13.1](#3131-stdpush-vector-item) std:push _vector_ _item_
    - [3.13.2](#3132-stdpop-vector) std:pop _vector_
    - [3.13.3](#3133-stdunshift-vector-item) std:unshift _vector_ _item_
    - [3.13.4](#3134-vector-splicing) Vector Splicing
    - [3.13.5](#3135-stdappend-vec-a-value-or-vec-) std:append _vec-a_ _value-or-vec_ ...
    - [3.13.6](#3136-stdprepend-vec-a-value-or-vec-) std:prepend _vec-a_ _value-or-vec_ ...
    - [3.13.7](#3137-stdtake-count-vector) std:take _count_ _vector_
    - [3.13.8](#3138-stddrop-count-vector) std:drop _count_ _vector_
  - [3.14](#314-associative-maps-or-string-to-value-mappings) Associative Maps (or String to Value mappings)
    - [3.14.1](#3141-map-splicing) Map Splicing
  - [3.15](#315-references) References
    - [3.15.1](#3151-stdtoref-value) std:to\_ref _value_
    - [3.15.2](#3152-stdrefweaken-ref) std:ref:weaken _ref_
    - [3.15.3](#3153-isref-value) is\_ref _value_
    - [3.15.4](#3154-iswref-value) is\_wref _value_
    - [3.15.5](#3155-stdrefstrengthen-ref) std:ref:strengthen _ref_
    - [3.15.6](#3156-stdrefset-ref-value) std:ref:set _ref_ _value_
  - [3.16](#316-iterators-iter-expression) Iterators $iter _expression_
    - [3.16.1](#3161-iterator-kinds) Iterator Kinds
    - [3.16.2](#3162-iterators-on-mutated-data) Iterators on mutated data
    - [3.16.3](#3163-splicing-an-iterator) Splicing an Iterator
    - [3.16.4](#3164-calling-an-iterator-with-a-function) Calling an Iterator with a Function
    - [3.16.5](#3165-zip-iterators) Zip Iterators
  - [3.17](#317-calling-semantics-of-data-types) Calling Semantics of Data Types
- [4](#4-conditional-execution---if--then--else) Conditional Execution - if / then / else
  - [4.1](#41-if-condition-then-expr-else-expr) ?/if _condition_ _then-expr_ [_else-expr_]
  - [4.2](#42-using-booleans-for-conditional-execution) Using Booleans for Conditional Execution
    - [4.2.1](#421-pick-bool-a--b-) pick _bool_ _a_ -b-
    - [4.2.2](#422-indexing-by-booleans) Indexing by Booleans
  - [4.3](#43-value-matching-with---match-value-expr-) Value matching with - match _value-expr_ ...
- [5](#5-loops-and-iteration) Loops And Iteration
  - [5.1](#51-control-flow) Control Flow
    - [5.1.1](#511-while-predicate-body) while _predicate_ _body_
    - [5.1.2](#512-iter-var-iterable-body) iter _var_ _iterable_ _body_
      - [5.1.2.1](#5121-counting-loop-with-iter) Counting loop with _iter_
      - [5.1.2.2](#5122-vector-iteration-with-iter) Vector iteration with _iter_
      - [5.1.2.3](#5123-map-iteration-with-iter) Map iteration with _iter_
      - [5.1.2.4](#5124-closures-and-iter-iter-i-) Closures and _iter_ `iter i ...`
    - [5.1.3](#513-range-start-end-step-fun) range _start_ _end_ _step_ _fun_
    - [5.1.4](#514-break-value) break _value_
    - [5.1.5](#515-next) next
    - [5.1.6](#516-jump-index-val-branch1--last-branch) jump _index-val_ _branch1_ ... _last-branch_
  - [5.2](#52-collection-iteration) Collection Iteration
    - [5.2.1](#521-iteration-over-vectors) Iteration over vectors
    - [5.2.2](#522-iteration-over-maps) Iteration over maps
    - [5.2.3](#523-for-iteratable-value-function) for _iteratable-value_ _function_
  - [5.3](#53-accumulation-and-collection) Accumulation and Collection
    - [5.3.1](#531-transforming-a-vector) Transforming a vector
    - [5.3.2](#532-example-of-) Example of `$@@`
    - [5.3.3](#533-transforming-a-vector-to-a-map) Transforming a vector to a map
    - [5.3.4](#534-iteratively-concatenating-strings) Iteratively concatenating strings
    - [5.3.5](#535-accumulating-sums) Accumulating sums
  - [5.4](#54-utilities) Utilities
    - [5.4.1](#541-stdaccum-collection-a-b-) std:accum _collection_ _a_ _b_ ...
    - [5.4.2](#542-stdzip-vector-map-fn) std:zip _vector_ _map-fn_
    - [5.4.3](#543-stdfold-accumulator-func-iteratable) std:fold _accumulator_ _func_ _iteratable_
    - [5.4.4](#544-stdenumerate-map-fn) std:enumerate _map-fn_
- [6](#6-operators) Operators
  - [6.1](#61-arithmetic) Arithmetic
    - [6.1.1](#611--operand-1-operand-2-) + _operand-1_ _operand-2_ ...
    - [6.1.2](#612---operand-1-operand-2-) - _operand-1_ _operand-2_ ...
    - [6.1.3](#613--op-a-op-b) * _op-a_ _op-b_
    - [6.1.4](#614--op-a-op-b) / _op-a_ _op-b_
    - [6.1.5](#615--op-a-op-b) % _op-a_ _op-b_
    - [6.1.6](#616--op-a-op-b) ^ _op-a_ _op-b_
  - [6.2](#62-comparison) Comparison
    - [6.2.1](#621--op-a-op-b) == _op-a_ _op-b_
    - [6.2.2](#622--op-a-op-b) != _op-a_ _op-b_
    - [6.2.3](#623--op-a-op-b) < _op-a_ _op-b_
    - [6.2.4](#624--op-a-op-b) <= _op-a_ _op-b_
    - [6.2.5](#625--op-a-op-b) > _op-a_ _op-b_
    - [6.2.6](#626--op-a-op-b) >= _op-a_ _op-b_
  - [6.3](#63-bit-operations) Bit Operations
    - [6.3.1](#631--op-a-op-b) & _op-a_ _op-b_
    - [6.3.2](#632--op-a-op-b) &^ _op-a_ _op-b_
    - [6.3.3](#633--op-a-op-b) &| _op-a_ _op-b_
    - [6.3.4](#634--op-a-op-b) << _op-a_ _op-b_
    - [6.3.5](#635--op-a-op-b) >> _op-a_ _op-b_
- [7](#7-data-structure-matchers-selectors-and-string-patternsregex) Data Structure Matchers, Selectors and String Patterns/Regex
  - [7.1](#71-data-structure-matcher) Data Structure Matcher
    - [7.1.1](#711-match-value-expr-match-pair1--default-expr) match _value-expr_ _match-pair1_ ... [_default-expr_]
    - [7.1.2](#712-m-expr) $M _expr_
    - [7.1.3](#713-data-structure-matcher-syntax) Data Structure Matcher Syntax
  - [7.2](#72-data-structure-selectors-s) Data Structure Selectors `$S(...)`
    - [7.2.1](#721-selector-and-wlambda-regex-syntax) Selector and WLambda Regex Syntax:
  - [7.3](#73-string-patterns-regex-r) String Patterns (Regex) `$r/.../`
    - [7.3.1](#731-pattern-syntax-overview) Pattern Syntax Overview
    - [7.3.2](#732-standard-regular-expressions) Standard Regular Expressions
    - [7.3.3](#733-stdpattern-string) std:pattern _string_
- [8](#8-modules) Modules
  - [8.1](#81-export) export
  - [8.2](#82-import) import
- [9](#9-core-library) Core Library
    - [9.0.1](#901-type-value) type _value_
    - [9.0.2](#902-len-value) len _value_
    - [9.0.3](#903-panic-message) panic _message_
- [10](#10-standard-library) Standard Library
    - [10.0.1](#1001-stdshuffle-randfunc-vec) std:shuffle _rand\_func_ _vec_
    - [10.0.2](#1002-stddelete-vector-or-map-index-or-key) std:delete _vector-or-map_ _index-or-key_
    - [10.0.3](#1003-stdcopy-vecormap) std:copy _vec\_or\_map_
    - [10.0.4](#1004-stdsort-comparefun-vec) std:sort [_compare\_fun_] _vec_
    - [10.0.5](#1005-stdcmpnumasc-a-b) std:cmp:num:asc _a_ _b_
    - [10.0.6](#1006-stdcmpnumdesc-a-b) std:cmp:num:desc _a_ _b_
    - [10.0.7](#1007-stddisplayln-arg1-) std:displayln _arg1_ ...
    - [10.0.8](#1008-stdwriteln-arg1-) std:writeln _arg1_ ...
    - [10.0.9](#1009-stdeval-code-string) std:eval _code-string_
    - [10.0.10](#10010-stdassert-bool-message) std:assert _bool_ \[_message_]
    - [10.0.11](#10011-stdasserteq-actual-expected-message) std:assert\_eq _actual_ _expected_ \[_message_]
    - [10.0.12](#10012-stdassertstreq-actual-expected) std:assert\_str\_eq _actual_ _expected_
    - [10.0.13](#10013-stdassertreleq-l-r-epsilon-message) std:assert\_rel\_eq _l_ _r_ _epsilon_ \[_message_]
    - [10.0.14](#10014-stdwlambdaversion) std:wlambda:version
  - [10.1](#101-io) I/O
    - [10.1.1](#1011-stdiofilereadtext-filename) std:io:file:read\_text _filename_
    - [10.1.2](#1012-stdiofileread-filename) std:io:file:read _filename_
    - [10.1.3](#1013-stdiofilewritesafe-filename-bytes-or-string) std:io:file:write\_safe _filename_ _bytes-or-string_
    - [10.1.4](#1014-stdiofileappend-filename-bytes-or-string) std:io:file:append _filename_ _bytes-or-string_
  - [10.2](#102-threading) Threading
- [11](#11-optional-standard-library) Optional Standard Library
  - [11.1](#111-serialization) serialization
    - [11.1.1](#1111-stdserwlambda-arg) std:ser:wlambda _arg_
    - [11.1.2](#1112-stdserjson-data-nopretty) std:ser:json _data_ \[_no\_pretty_]
    - [11.1.3](#1113-stddeserjson-string) std:deser:json _string_
    - [11.1.4](#1114-stdsercsv-fielddelim-rowseparator-escapeall-table) std:ser:csv _field\_delim_ _row\_separator_ _escape\_all_ _table_
    - [11.1.5](#1115-stddesercsv-fielddelim-rowseparator-data) std:deser:csv _field\_delim_ _row\_separator_ _data_
    - [11.1.6](#1116-stdsermsgpack-data) std:ser:msgpack _data_
    - [11.1.7](#1117-stddesermsgpack-bytes) std:deser:msgpack _bytes_
  - [11.2](#112-regex) regex
  - [11.3](#113-chrono) chrono
    - [11.3.1](#1131-stdchronotimestamp-format) std:chrono:timestamp \[_format_]
  - [11.4](#114-hash) hash
    - [11.4.1](#1141-stdhashfnv1a-arg1-) std:hash:fnv1a _arg1_ ...
  - [11.5](#115-rand) rand
    - [11.5.1](#1151-stdrandsplitmix64new) std:rand:split\_mix64\_new
    - [11.5.2](#1152-stdrandsplitmix64newfrom-seed) std:rand:split\_mix64\_new\_from _seed_
    - [11.5.3](#1153-stdrandsplitmix64next-smstate-count) std:rand:split\_mix64\_next _sm\_state_ \[_count_]
    - [11.5.4](#1154-stdrandsplitmix64nextopen01-smstate-count) std:rand:split\_mix64\_next\_open01 _sm\_state_ \[_count_]
  - [11.6](#116-utility-functions) Utility Functions
    - [11.6.1](#1161-stddumpupvals-function) std:dump\_upvals _function_
- [12](#12-wlambda-lexical-syntax-and-grammar) WLambda Lexical Syntax and Grammar
  - [12.1](#121-special-forms) Special Forms

-----

## <a name="1-variable-definition-and-assignment"></a>1 - Variable Definition and Assignment

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

And also with pairs:

```wlambda
!p = $p(10, 20);
!(a, b) = p;
.(a, b) = p;

std:assert_eq a 10;
std:assert_eq b 20;
```

### <a name="11-destructuring-to-variables"></a>1.1 - Destructuring to Variables

Like highlighted in the previous section you can define and assign to
multiple variables at once. Following data types support destructuring:

- Vectors:
```wlambda
!(a, b, c) = $[1, 2, 3];

std:assert_eq a 1;
std:assert_eq b 2;
std:assert_eq c 3;
```
- Maps:
```wlambda
!(x, foo, lol) = ${foo = 33, lol = 42, x = 2};

std:assert_eq x   2;
std:assert_eq foo 33;
std:assert_eq lol 42;
```
- Pairs:
```wlambda
!(x, y) = $p("ex", "uepsilon");

std:assert_eq x "ex";
std:assert_eq y "uepsilon";
```
- Numerical Vectors:
```wlambda
!(x, y, z) = $i(3, 44, 4);

std:assert_eq x 3;
std:assert_eq y 44;
std:assert_eq z 4;

!(r, g, b, a) = $f(0.3, 1.0, 0.4, 1.0);

std:assert_eq r 0.3;
std:assert_eq g 1.0;
std:assert_eq b 0.4;
std:assert_eq a 1.0;
```

### <a name="12-global-variables"></a>1.2 - Global Variables

You can define global variables that are not bound to
a lexical scope as follows:

```wlambda
{
    !:global a = 13;
}[];

std:assert_eq a 13;
```

Global variables however do not live beyond file or module boundaries.

### <a name="13-constants"></a>1.3 - Constants

WLambda supports constant _variables_. These are global variables you can't
assign to. They are resolved at compile time and offer a slight performance
advantage (roughly 3-4%) over (global or local) variables.

```wlambda
!:const X = 11;

std:assert_eq X 11;

# Destructuring works too, but only with compile time literal values
# in the vectors / maps:
!:const (ON, OFF) = $[$true, $false];
!:const (RED, BLUE) = ${
    BLUE = 0x0000FF,
    RED  = 0xFF0000,
};

std:assert_eq ON  $true;
std:assert_eq OFF $false;

std:assert_eq RED 0xFF0000;
std:assert_eq BLUE 0x0000FF;
```

However, be aware that these _constants_ are not really constant.  Due to
performance reasons referential values like Lists or Maps are not copied
(neither shallow, nor deep) if you access them through a constant.

```wlambda
!:const V = $[1,2,3];

std:assert_eq (str V) (str $[1,2,3]);

std:push V 43;  # Mutation of a 'constant'
std:assert_eq V.3 43;
```

Constants also work across module borders:

```wlambda
!:const X = 10;

# When imported the X will remain constant:
!@export X = X;
```

## <a name="2-functions-part-12"></a>2 - Functions (part 1/2)

A function can be defined using the `{ ... }` syntax and the `\ _statement_`
syntax: To give functions a name, you need to assign them to a variable with
the `!_name_ = _expr_` syntax.

### <a name="21-closures"></a>2.1 - Closures

Functions take values from the outer scope by promoting the variable
at runtime to a hidden reference to their previous value:

```wlambda
!a = 10;
!b = 20;

# function transforms a and b to hidden references
!add_a_and_b = { a + b };

std:assert_eq add_a_and_b[] 30;

# The assignment assigns to the hidden reference, so the closure add_a_and_b
# also receives the new value:
.a = 33;

std:assert_eq add_a_and_b[] 53;

# a and b are dereferenced on local variable access.
std:assert_eq a + b         53;
```

#### <a name="211-object-oriented-programming-with-closures"></a>2.1.1 - Object Oriented Programming with Closures

This section explains how to create objects and hide state using closures.
Keep in mind, that there are also `$self` and `$data` available, which
allow a different approach for referring to the object state/data than to
capture the object as reference in a closure.

Keep in mind, that care must be taken (references need to be captures weakly)
with the references as shown below, because otherwise you will get reference
cycles and memory leaks.

```wlambda
!new_Cat = {!(name) = @;
    # Captures by closures upgrade the outer `self` variable to a _hidden_
    # reference, which is then captured. As the closure is stored in
    # `self`, this would create a ref cycle. This is why we needed
    # to make a weak reference to self.

    # Make an explicit hidden reference:
    !self_ = $& ${
        name = name,
    };

    # Create a weak reference form the hidden reference:
    !self = $weak& $:self_;

    self.meow     = { std:displayln self.name " meows!"; };
    self.get_name = { self.name };

    # To keep the object alive, we retrieve a strong reference
    # from the hidden reference:
    $:self
};

!my_cat = new_Cat "Spot";

my_cat.meow[]; # Prints 'Spot meows!'

std:assert_eq my_cat.get_name[] "Spot";
```

Alternatively you can just make the cat name private:

```wlambda
!new_Cat = {!(name) = @;
    # This does not make cycles, because `name` does not contain
    # the closures in the end.
    !cat_name = name;

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

To call functions, you have at least 4 alternatives. First is the bare
`_expr_ arg1 arg2 arg3 arg4` syntax. And the second is the fully delimited
variant: `_expr_[arg1, arg2, arg3, ...]`. You can always delimit the first
variant using the `( ... )` parenthesis around the whole call,
i.e. `(_expr_ arg1 arg2 arg3 arg4)`.

Third you can call a function with a vector as argument with `_expr_[[_expr_]]`,
where the second expression should return a vector (if it doesn't it will use the
value as first argument).

The fourth alternative is the `&>` and `<&` argument pipe operators which
can be conveniently used in conjunction with the first variant to prevent
some parenthesis.

Here are examples:

```wlambda
# All the second variant:
std:assert_eq[std:str:cat[1, 2, 3], "123"];

# Can also be written as:
std:assert_eq (std:str:cat 1 2 3) "123";

# As the third variant:
!some_args = $[1, 2, 3];
std:assert_eq std:str:cat[[some_args]] "123";

# The fourth variant:
std:assert_eq str <& $[1, 2, 3]     "$[1,2,3]";
std:assert_eq $[1, 2, 3] &> str     "$[1,2,3]";
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

#### <a name="231-stdtonoarity-function"></a>2.3.1 - std:to\_no\_arity _function_

This function disables all arity checks of a function. Use this with care
and diligence.

```wlambda
!f = { _ }; # accepts exactly 1 param

# f keeps its arity checks, but f2 will
# call the same function, but without arity checks.
!f2 = std:to_no_arity f;

std:assert_eq (f2 1 2 3) 1;
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

### <a name="25-function-call-composition"></a>2.5 - Function call composition

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

#### <a name="251--tail-argument-function-chaninig"></a>2.5.1 - '|' Tail Argument Function Chaninig

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

#### <a name="252--left-hand-function-chaining"></a>2.5.2 - '|>' Left Hand Function Chaining

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

#### <a name="253-forward-argument-pipe-arg--fun"></a>2.5.3 - Forward Argument Pipe `arg &> fun`

This operator has the highest precedence over all other operators
and is used to be able to write this:

```wlambda
? "foob" &> $r/f(^*)b/ {
    std:assert_eq $\.1 "oo";
} {
    std:assert $false;
}
```

That means `f a &> b` is equivalent to writing `f[b[a]]` or `(f (b a))`.
Chaining multiple is also possible and left associative: `a &> b &> c` is `(c (b a))`.
You can see it as piping operation:

```wlambda
!r = "ABC" &> std:str:to_lowercase &> \std:str:pad_start 10 "0" _;

std:assert_eq r "0000000abc";
```

#### <a name="254-reverse-argument-pipe-fun--arg"></a>2.5.4 - Reverse Argument Pipe `fun <& arg`

Like the `&>` operator this operator, but it has a lower precedence (does not bind
as strongly as `&>`) and is right associative. That means you can write this:

```wlambda
!r = (\std:str:pad_start 10 "0" _) <& std:str:to_lowercase <& "ABC";

std:assert_eq r "0000000abc";
```

That means, writing `f <& a <& x` becomes `f[a[x]]` or `(f (a x))`.

### <a name="26-control-flow---returning"></a>2.6 - Control Flow - Returning

WLambda uses labelled blocks for control flow, as returning from the current function would not be
very helpful for the control flow in wlambda in case of conditional execution using the
boolean calling semantics.

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

#### <a name="261-return-label-value"></a>2.6.1 - return [_label_] _value_

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

#### <a name="262-block-label-function"></a>2.6.2 - block [label] _function_

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

#### <a name="263-stdtodrop-function-or-raii-destructors-or-drop-functions"></a>2.6.3 - std:to\_drop _function_ (or RAII, Destructors or Drop Functions)

You can create a function that is called when it is
dropped/its reference count goes to 0.

```wlambda
!dropped = $false;

!x = std:to_drop { .dropped = $true; };

std:assert not[dropped];

.x = $none;

std:assert dropped;
```

Please note, that the drop function will be executed in a newly constructed
default EvalContext, this means there is some overhead and that the EvalContext
dependent results of `std:eval` might be different.

### <a name="27-function-utilities"></a>2.7 - Function utilities

#### <a name="271-isfun-value"></a>2.7.1 - is\_fun _value_

Returns `$true` if _value_ is a function.

```wlambda
std:assert ~ is_fun {};
std:assert ~ is_fun is_fun;
std:assert ~ not ~ is_fun ${a=10};
```

## <a name="3-data-types"></a>3 - Data Types

### <a name="31-none-sentinel-value-n-or-none"></a>3.1 - None sentinel value: `$n` or `$none`

This is a special sentinel value that is returned by functions and
when a non existing field of a datastructure is accessed. It's semantic
meaning is that there is no value.

Most functions that expect a string value will turn a `$none` into an
empty string. If you need an unambigous representation use `std:ser:wlambda`
for dumping WLambda data structures.

Please note for API design: In case of errornous states you should not
return a `$none` but an `$error` value.

```wlambda
std:assert ~ $n                  == $none;
std:assert ~ int[$n]             == 0;
std:assert ~ float[$n]           == 0.0;
std:assert ~ str[$n]             == "";
std:assert ~ std:ser:wlambda[$n] == "$n";
std:assert ~ is_none[$n];
```

#### <a name="311-isnone-value"></a>3.1.1 - is\_none _value_

Returns `$true` if _value_ is `$none` or `$o()`.

```wlambda
std:assert ~ is_none $none;
std:assert ~ is_none $o();
std:assert ~ not ~ is_none $false;
std:assert ~ not ~ is_none $o(10);
```

#### <a name="312-issome-value"></a>3.1.2 - is\_some _value_

Returns `$true` if _value_ is anything except `$none` or `$o()`.

```wlambda
std:assert ~ not ~ is_some $none;
std:assert ~ not ~ is_some $o();
std:assert ~ is_some $false;
std:assert ~ is_some 30;
std:assert ~ is_some $o(30);
```

### <a name="32-optional-values-o-and-o"></a>3.2 - Optional values `$o()` and `$o(...)`

An optional value can either contain another value, or contain no value at all.
An empty optional value is not much different from `$none`, but it is sometimes
desirabel to make a difference between an optional value and a `$none` value
if the `$none` value is used as sentinel value.

Optional values were introduced for functions that lookup stuff and either
return _something_ that might be `$none` (eg. if some element in a vector is
searched for), or return that nothing was found.

The functions `is_none` and `is_some` like stated above work for these
optional values too:

```wlambda
std:assert ~ is_none $o();
std:assert ~ is_some $o(10);
std:assert ~ is_some $o($none);
std:assert ~ is_some $o($o());
```

Calling an optional value will return it's contents or `$none`:

```wlambda
std:assert_eq $o()[]     $none;
std:assert_eq $o(10)[]   10;

!do_something = {
    ? _ == 0 {
        $o()
    } {
        $o(_ + 10)
    }
};

!result = do_something 11;
std:assert_eq result[] 21;

!result = do_something 0;
std:assert_eq result[] $none;
```

In a boolean context an optional becomes `$true` if it contains
something and `$false` if it has nothing.

```wlambda
std:assert ~ not ~ bool $o();
std:assert ~ bool $o(10);

!x = $o();
!res1 = ? x "something" "nothing";
std:assert_eq res1 "nothing";

.x = $o(30);
!res2 = ? x "something" "nothing";
std:assert_eq res2 "something";
```

Many other operations are just forwarded to the contents of the
optional value:

```wlambda
std:assert_eq $o(33) + 44    77;

!x = $o($[1,2,3]);
std:push x 4;
std:assert_eq (str x) (str $[1,2,3,4]);

std:assert_eq (float $o(4.4))   4.4;
std:assert_eq (int $o(4.4))     4;
```

An optional value can also be dereferenced:

```wlambda
std:assert_eq $*$o()    $none;
std:assert_eq $*$o(10)  10;
```

Calls with more than zero arguments are forwarded to the contents:

```wlambda
std:assert_eq ($o("xx") "yy")  "xxyy";

!x = { _ * 20 };
std:assert_eq ($o(x) 30)    600;
```

#### <a name="321-isoptional-value"></a>3.2.1 - is\_optional _value_

Returns `$true` if _value_ is an optional value. That means either `$o()` or
`$o(...)`.

```wlambda
std:assert ~ is_optional $o();
std:assert ~ is_optional $o($none);
std:assert ~ is_optional $o(10);

std:assert ~ not ~ is_optional $true;
std:assert ~ not ~ is_optional $none;
std:assert ~ not ~ is_optional $false;
std:assert ~ not ~ is_optional 303;
```

#### <a name="322-unwrapping-optionals"></a>3.2.2 - Unwrapping optionals

You can unwrap an optional with `unwrap`. It will panic if there is no value provided.
Otherwise it will return the contents.

```wlambda
std:assert unwrap[$o(10)] 10;
```

### <a name="33-error-values-e-expr-or-error-expr"></a>3.3 - Error values: `$e expr` or `$error expr`

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
- is_optional
- is_int
- ==
- !=
- std:to_ref
- std:ref_id
- std:ser:wlambda

All other functions don't accept errors as their argument.

#### <a name="331--label-value"></a>3.3.1 - _? [_label_] _value_

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

#### <a name="332-onerror-handler-maybe-error-value"></a>3.3.2 - on\_error _handler_ _maybe-error-value_

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

!x = $n;

# The first function of on_error will be called with the unwrapped
# error if an error occured.
on_error {|4| .x = _; } ~ func 13;
std:assert_eq x "this failed!";

!ret = on_error {|4| .x = _; } ~ func 1;
std:assert_eq ret "all ok!";
```

#### <a name="333-iserr-value"></a>3.3.3 - is\_err _value_

Returns `$true` if _value_ is an error value.

```wlambda
std:assert ~ is_err $e "foo";
std:assert ~ not ~ is_err $none;
std:assert ~ not ~ is_err 10;
```

#### <a name="334-errortostr-value"></a>3.3.4 - error\_to\_str _value_

This function accepts an error value in contrast to `str`, but does
not panic but transform the error value into its string representation.

```wlambda
!r = error_to_str $e "TEST";

std:assert_eq r "$e[1,22:<wlambda::eval>(Err)] \"TEST\"";
```

WARNING: The string representation might change between wlambda versions.
Please use `on_error` to access the individual parts
(line, column, filename, error value) of the error.

### <a name="34-booleans"></a>3.4 - Booleans

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

#### <a name="341-isbool-any-value"></a>3.4.1 - is\_bool _any-value_

You can check if something is a boolean with `is_bool`:

```wlambda
std:assert ~ is_bool $true;
std:assert ~ is_bool $false;
std:assert ~ not[is_bool $n];
std:assert ~ not[is_bool ""];
std:assert ~ not[is_bool 0];
```

#### <a name="342-bool-any-value"></a>3.4.2 - bool _any-value_

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

#### <a name="343-not-value"></a>3.4.3 - not _value_

This function negates the boolean _value_. If it is not a boolean, it will
be casted into one before negating.

```wlambda
std:assert ~ not $false;
std:assert ~ not 0;
std:assert ~ not $none;
```

#### <a name="344-boolean-list-indexing"></a>3.4.4 - Boolean List Indexing

Booleans can also be used to pick a value from a list
by calling the boolean with a list as first argument:

```wlambda
std:assert_eq ($true  $[:a, :b]) :b;
std:assert_eq ($false $[:a, :b]) :a;
```

### <a name="35-64-bit-integers"></a>3.5 - 64-Bit Integers

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

#### <a name="351-int-value"></a>3.5.1 - int _value_

Returns the integer casted version of _value_.
Mostly interesting for converting a string to an integer (in radix 10)
or for getting the truncated value of a float.

```wlambda
std:assert_eq (int 4.2)         4;
std:assert_eq (int "402")       402;
std:assert_eq (int "a3")        0;

std:assert_eq (int $b"@")       0x40;   # Returns the byte value of the first char

std:assert_eq (int $[4,4,4])    3; # Same as `len`
std:assert_eq (int ${a=4,b=4})  2; # Same as `len`
```

#### <a name="352-isint-value"></a>3.5.2 - is\_int _value_

Returns `$true` if _value_ is of data type integer. Otherwise it returns `$false`.

#### <a name="353-stdnegi64-integer"></a>3.5.3 - std:neg\_i64 _integer_

Negates the _integer_, which makes a negative from a positive and positive
from a negative number.

```wlambda
std:assert_eq (std:neg_i64 -1)      1;
std:assert_eq (std:neg_i64 1)      -1;

std:assert_eq (std:neg_i64 0xFF)  -255;
```

#### <a name="354-stdnoti64-integer"></a>3.5.4 - std:not\_i64 _integer_

Flips the bits of the signed 64-Bit _integer_.

```wlambda
std:assert_eq (std:not_i64 -1)      0;
std:assert_eq (std:not_i64 1)      -2;

std:assert_eq (std:not_i64 0xFF)  -256;
```

#### <a name="355-stdnegu32-integer"></a>3.5.5 - std:neg\_u32 _integer_

Negates the _integer_ as if it was an unsigned 32-Bit integer.

```wlambda
std:assert_eq (std:neg_u32 0xFF)   4294967041;
std:assert_eq (std:neg_u32 0x1)    4294967295;
std:assert_eq (std:neg_u32 0x0)    0;
```

#### <a name="356-stdnotu32-integer"></a>3.5.6 - std:not\_u32 _integer_

Flips the bits of the _integer_ as if it was an unsigned 32-Bit integer.

```wlambda
std:assert_eq (std:not_u32 0xFF)   4294967040;
std:assert_eq (std:not_u32 0x1)    4294967294;
std:assert_eq (std:not_u32 0x0)    4294967295;
```

### <a name="36-64-bit-floats"></a>3.6 - 64-Bit Floats

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

#### <a name="361-float-value"></a>3.6.1 - float _value_

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

#### <a name="362-isfloat-value"></a>3.6.2 - is\_float _value_

Returns `$true` if _value_ is a float, otherwise `$false` is returned.

```wlambda
std:assert ~ is_float 4.4;
std:assert ~ is_float 1.0 + 1;
std:assert ~ not ~ is_float 1 + 1.0;
std:assert ~ not ~ is_float 4;
std:assert ~ not ~ is_float $true;
```

#### <a name="363-stdnumacos-float"></a>3.6.3 - std:num:acos _float_

Computes the arccosine of a number. Return value is in radians in the range [0,
pi] or NaN if the number is outside the range [-1, 1].

#### <a name="364-stdnumacosh-float"></a>3.6.4 - std:num:acosh _float_

Inverse hyperbolic cosine function.

#### <a name="365-stdnumasin-float"></a>3.6.5 - std:num:asin _float_

Computes the arcsine of a number. Return value is in radians in the range
[-pi/2, pi/2] or NaN if the number is outside the range [-1, 1].

#### <a name="366-stdnumasinh-float"></a>3.6.6 - std:num:asinh _float_

Inverse hyperbolic sine function.

#### <a name="367-stdnumatan-float"></a>3.6.7 - std:num:atan _float_

Computes the arctangent of a number. Return value is in radians in the range
[-pi/2, pi/2].

#### <a name="368-stdnumatan2-y-x"></a>3.6.8 - std:num:atan2 _y_ _x_

Computes the four quadrant arctangent of _y_ and other _x_ in radians.

- x = 0, y = 0: 0
- x >= 0: arctan(y/x) -> [-pi/2, pi/2]
- y >= 0: arctan(y/x) + pi -> (pi/2, pi]
- y < 0: arctan(y/x) - pi -> (-pi, -pi/2)

#### <a name="369-stdnumatanh-float"></a>3.6.9 - std:num:atanh _float_

Inverse hyperbolic tangent function.

#### <a name="3610-stdnumcbrt-float"></a>3.6.10 - std:num:cbrt _float_

Takes the cubic root of a number.

#### <a name="3611-stdnumceil-float"></a>3.6.11 - std:num:ceil _float_

Returns the smallest integer (still a float) greater than or equal to a number.

#### <a name="3612-stdnumcos-float"></a>3.6.12 - std:num:cos _float_

Computes the cosine of a number (in radians).

#### <a name="3613-stdnumcosh-float"></a>3.6.13 - std:num:cosh _float_

Hyperbolic cosine function.

#### <a name="3614-stdnumexp-float"></a>3.6.14 - std:num:exp _float_

Returns e ^ _float_, (the exponential function).

#### <a name="3615-stdnumexp2-float"></a>3.6.15 - std:num:exp2 _float_

Returns 2 ^ _float_.

#### <a name="3616-stdnumexpm1-float"></a>3.6.16 - std:num:exp\_m1 _float_

Returns (e ^ _float_ - 1) in a way that is accurate even if the number is close
to zero.

#### <a name="3617-stdnumfloor-float"></a>3.6.17 - std:num:floor _float_

Returns the largest integer (still as float) less than or equal to a number.

#### <a name="3618-stdnumhypot-y-x"></a>3.6.18 - std:num:hypot _y_ _x_

Calculates the length of the hypotenuse of a right-angle triangle given legs of
length _x_ and _y_.

#### <a name="3619-stdnumln-float"></a>3.6.19 - std:num:ln _float_

Returns the natural logarithm of the number.

#### <a name="3620-stdnumlog-float"></a>3.6.20 - std:num:log _float_

Returns the logarithm of the number with respect to an arbitrary base.

The result may not be correctly rounded owing to implementation details;
`std:log2` can produce more accurate results for base 2, and `std:log10` can
produce more accurate results for base 10.

#### <a name="3621-stdnumlog10-float"></a>3.6.21 - std:num:log10 _float_

Returns the base 10 logarithm of the number.

#### <a name="3622-stdnumlog2-float"></a>3.6.22 - std:num:log2 _float_

Returns the base 2 logarithm of the number.

#### <a name="3623-stdnumpow-float"></a>3.6.23 - std:num:pow _float_

Raises a number to a floating point power.
You may also use the `^` operator, which also works for integers.

#### <a name="3624-stdnumrecip-float"></a>3.6.24 - std:num:recip _float_

Takes the reciprocal (inverse) of a number, 1/x.

#### <a name="3625-stdnumround-float"></a>3.6.25 - std:num:round _float_

Returns the nearest integer (still a float) to a number. Round half-way cases
away from 0.0.

#### <a name="3626-stdnumsin-float"></a>3.6.26 - std:num:sin _float_

Computes the sine of a number (in radians).

#### <a name="3627-stdnumsinh-float"></a>3.6.27 - std:num:sinh _float_

Hyperbolic sine function.

#### <a name="3628-stdnumsqrt-float"></a>3.6.28 - std:num:sqrt _float_

Takes the square root of a number.

#### <a name="3629-stdnumtan-float"></a>3.6.29 - std:num:tan _float_

Computes the tangent of a number (in radians).

#### <a name="3630-stdnumtanh-float"></a>3.6.30 - std:num:tanh _float_

Hyperbolic tangent function.

#### <a name="3631-stdnumtodegrees-float"></a>3.6.31 - std:num:to\_degrees _float_

Converts radians to degrees.

#### <a name="3632-stdnumtoradians-float"></a>3.6.32 - std:num:to\_radians _float_

Converts degrees to radians.

#### <a name="3633-stdnumtrunc-float"></a>3.6.33 - std:num:trunc _float_

Returns the integer part of a number.

#### <a name="3634-stdnumlerp-a-b-x"></a>3.6.34 - std:num:lerp _a_ _b_ _x_

Linear interpolation between _a_ and _b_ by _x_. Where _x_ is
in the range of `[0.0, 1.0]`.

```wlambda
!res = int ~ std:num:lerp 0.0 100.0 0.5;

std:assert_eq res 50;
```

#### <a name="3635-stdnumsmoothstep-a-b-x"></a>3.6.35 - std:num:smoothstep _a_ _b_ _x_

Interpolates smoothly from 0.0 to 1.0 where _x_ is in the range of `[a, b]`.

```wlambda
!res = int ~ 1000.0 * (std:num:smoothstep 0.0 100.0 10.0);

std:assert_eq res 28;
```

### <a name="37-numeric-functions"></a>3.7 - Numeric Functions

These functions work for all types of numbers.

#### <a name="371-stdnumabs-number"></a>3.7.1 - std:num:abs _number_

Takes the absolute value of _number_. If _number_ is not a number
it will be converted into an integer.

```wlambda
std:assert_eq (std:num:abs -10)     10;
std:assert_eq (std:num:abs -13.3)   13.3;
```

### <a name="38-numerical-mathematical-vectors"></a>3.8 - Numerical Mathematical Vectors

In order to aid in the development of GUIs, games, and other physics/geometry adjacent software,
WLambda comes with a built in datatype for mathematical vectors, which can contain floats and integers
and have between two and four dimensions.

```wlambda
# integer vectors
std:assert ~ $i(-1, 2).y                == 2;
std:assert ~ (ivec ${z=3})              == $i(0,0,3);
std:assert ~ (ivec4 $[])                == $i(0,0,0,0);
std:assert ~ $i(1.49, -2.72)            == $i(1,-2);
# float vectors
std:assert ~ $f(1.00, -33).x            == $f(1, 200).first;
std:assert ~ $f(-0, 2.4).y              == $f(1.6, 2.4).second;
std:assert ~ (fvec3 ${w=0.1})           == $f(0,0,0);
# conversion
std:assert ~ (fvec3 $i(1, 2))/10        == $f(0.1, 0.2, 0);
std:assert ~ (ivec2 $f(1.3, 2.7, -5.8)) == $i(1, 2);
std:assert ~ (ivec $f(1.3, 2.7, -5.8))  == $i(1, 2, -5);
```

#### <a name="381-vector-conversions"></a>3.8.1 - Vector Conversions

There are eight functions for converting other values into vectors
and vectors of integers into vectors of floats:

- `ivec` 
- `ivec2`
- `ivec3`
- `ivec4`
- `fvec`
- `fvec2`
- `fvec3`
- `fvec4`

The functions without a dimension suffix fill in as many dimensions
as are present in the object being converted.
The functions with dimension suffixes fill in any missing dimensions
with `0`s and ignore dimensions as necessary.

NOTE: `ivec` will always truncate (i.e. round down) floats into integers when converting,
just like when converting floats into integers implicitly elsewhere in WLambda.

#### <a name="382-vector-component-access"></a>3.8.2 - Vector Component Access

There are 12 functions for accessing the components of vectors,
but only four have unique behavior (the rest are aliases).

- `x`/`r`/`h`/`0`/`first`,
- `y`/`g`/`s`/`1`/`second`,
- `z`/`b`/`v`/`2`/`third`,
- `w`/`3`/`fourth`

```wlambda
!my_vec = $f(39.3, 404.504, 333.8);
std:assert_eq my_vec.x my_vec.0;
std:assert_eq my_vec.x my_vec.first;

std:assert_eq my_vec.y my_vec.1;
std:assert_eq my_vec.y my_vec.second;

std:assert_eq my_vec.z my_vec.2;
std:assert_eq my_vec.z my_vec.third;

std:assert_eq my_vec.w my_vec.3;
std:assert_eq my_vec.w my_vec.fourth;
```

#### <a name="383-named-field-access-and-swizzling"></a>3.8.3 - Named Field Access and Swizzling

You can access the fields of numeric vectors with different keys:

```wlambda
std:assert_eq $i(2,3,4,5).x     2;
std:assert_eq $i(2,3,4,5).y     3;
std:assert_eq $i(2,3,4,5).z     4;
std:assert_eq $i(2,3,4,5).w     5;

std:assert_eq $i(5,6,7,8).r     5;
std:assert_eq $i(5,6,7,8).g     6;
std:assert_eq $i(5,6,7,8).b     7;
std:assert_eq $i(5,6,7,8).a     8;

std:assert_eq $i(5,6,7,8).h     5;
std:assert_eq $i(5,6,7,8).s     6;
std:assert_eq $i(5,6,7,8).v     7;
std:assert_eq $i(5,6,7,8).a     8;

std:assert_eq $i(5,6,7,8).0     5;
std:assert_eq $i(5,6,7,8).1     6;
std:assert_eq $i(5,6,7,8).2     7;
std:assert_eq $i(5,6,7,8).3     8;
```

You can also use **swizzling** to quickly make a new vector:

```wlambda
std:assert_eq $i(2,3,4).xx      $i(2,2);
std:assert_eq $i(2,3,4).xyxz    $i(2,3,2,4);
std:assert_eq $i(2,3,4).bgr     $i(4,3,2);
std:assert_eq $i(2,3).xyrg      $i(2,3,2,3);
std:assert_eq $i(2,3,4,5).zw    $i(4,5);
```


#### <a name="384-euler-additionsubtraction"></a>3.8.4 - Euler Addition/Subtraction

You can add vectors to each other and subtract them from each other.

The type of the resulting vector will be the same as the vector on the left.

The number of dimensions in the resulting vector will be the same as the vector
with the highest number of dimensions that was involved in the operation.

If the value on the right isn't a vector, it will be converted into one,
just as if it were passed through `ivec` or `fvec`, meaning that as many
dimensions are kept as are present.

```wlambda
std:assert_eq[ $i(0.1, 0.9) + $i(1, 0) , $i(1, 0) ];
std:assert_eq[ $f(0.1, 0.9) + $i(1, 0) , $f(1.1, 0.9) ];
std:assert_eq[ $f(0.1, 0.9) + ${ w=7 } , $f(0.1, 0.9, 0, 7) ];
std:assert_eq[ std:v:mag2 $i(-1, 5) + $i(1, -5) , 0.0 ];
```

#### <a name="385-scalar-multiplicationdivision"></a>3.8.5 - Scalar Multiplication/Division

You can multiply and divide integer and float vectors by single numbers.
This copies the vector, multiplies or divides each component of the vector by the single number,
and returns the result.

NOTE: Dividing `ivec`s will always truncate (i.e. round down) floats into integers.

```wlambda
std:assert ~ $i(3, 6)/2       == $i(1, 3);
std:assert ~ $f(3, 6)/2       == $f(1.5, 3);
std:assert ~ $f(0.5, 0) * 1.3 == $f(0.65,0);
std:assert ~ (std:v:mag (std:v:norm $[40.19, 0.399]) * 10) == 10.0;
```

#### <a name="386-unary-vector-operations"></a>3.8.6 - Unary Vector Operations

Calling `-` on a vector returns a new vector with all of its fields negated.
This is equivalent to multiplying the vector by `-1`.

Calling `+` on a vector returns a copy of the exact same vector.
This is equivalent to multiplying the vector by `1`.

```wlambda
!my_vec = $f(1.2, 2.3, 3.4);
std:assert_eq (ivec (-my_vec)) $i(-1, -2, -3);
std:assert_eq (+my_vec) my_vec;
# adding something to its inverse yields all 0s
std:assert_eq[ my_vec + (-my_vec), my_vec * 0 ];
```

#### <a name="387-stdvdims-vec"></a>3.8.7 - std:v:dims _vec_

You can use this function to retrieve the number of dimensions in _vec_.

Like most other std:v functions,
it will coerce whatever value is passed into it into a `ivec`,
if that value is not a `fvec`.

This function always returns an integer, regardless of whether an `ivec` or `fvec` is passed in.

```wlambda
# the least number of dimensions a vector can have is 2.
std:assert_eq (std:v:dims $[]) 2;
# while the most is 4.
std:assert_eq (std:v:dims ${w=0}) 4;
std:assert_eq (std:v:dims $f(1,2)) (std:v:dims $i(1,2));
```

#### <a name="388-stdvmag2-vec"></a>3.8.8 - std:v:mag2 _vec_

Returns the magnitude of _vec_, squared.

Calculating the squared magnitude is a little bit faster,
so you should prefer this method where performance is paramount.

The magnitude is always a float, regardless of whether the parameter is an `ivec` or `fvec`.

```wlambda
std:assert_eq (std:v:mag2 ${w=4}) 16.0;
```

#### <a name="389-stdvmag-vec"></a>3.8.9 - std:v:mag _vec_

Returns the magnitude (also known as the length) of _vec_.

The magnitude is always a float, regardless of whether the parameter is an `ivec` or `fvec`.

```wlambda
std:assert_eq (std:v:mag ${w=4}) 4.0;
```

#### <a name="3810-stdvnorm-vec"></a>3.8.10 - std:v:norm _vec_

Returns a new vector which has a magnitude of `1`, but points in the same direction as _vec_.
Vectors with a length of one are also known as unit vectors.

Note that this still returns an `ivec` when used on `ivec`s,
meaning that when used on an `ivec2` only four values are possible:
- `$i(1, 0)`
- `$i(-1, 0)`
- `$i(0, 1)`
- `$i(0, -1)`

These are the only `ivec2`s that have a length of `1`.

```wlambda
!p1 = fvec ${ x = 20, y = 30.5 };
!p2 = fvec ${ x = -10, y = 0.5 };

# get the delta representing how far you'd have to travel to get from p1 to p2
!delta = p2 - p1;
# the normalized delta represents a single 1 sized step you could take to get to p2 from p1.
!n = std:v:norm delta;

# the length of this step is reflected in the magnitude of the vectors
std:assert_eq[ (std:v:mag delta) - 1, std:v:mag (p1 + n) - p2 ];
```

#### <a name="3811-stdvdot-vec1-vec2"></a>3.8.11 - std:v:dot _vec1_ _vec2_

Returns the sum of all components after multiplying each component
in _vec1_ with the corresponding component of _vec2_.

This can be used to represent the "sameness" of two vectors (especially unit vectors):
the degree to which they are pointing in the same direction.

Returns an integer when used on an `ivec`, and a float when used on an `fvec`.

If _vec1_ is an `fvec`, then _vec2_ will also be coerced into one.
If _vec1_ isn't an `fvec`, then it's coerced into an `ivec`, just like the other `std:v` functions.

```wlambda
!at      = fvec ${ x = 20 , y = 30.5 };           # where you're at
!goal    = fvec ${ x = -10, y = 0.5  };           # where you want to look
!looking = std:v:rad2vec (std:num:to_radians 90); # direction you're looking in

# do you need to turn left or right to look at `goal`,
# if you're standing at `at` looking in `looking`?

# find the unit vector representing the space between where you want to look and where you're at.
!delta = std:v:norm goal - at;

# the direction you need to turn in can be found by checking the sign of
# the dot product of where you're currently looking and where you're at.
!dir = std:v:dot delta looking;

std:assert_eq[ (dir < 0) "left" "right", "left" ];
```

#### <a name="3812-stdvcross-vec1-vec2"></a>3.8.12 - std:v:cross _vec1_ _vec2_

Returns a vector perpendicular to _vec1_ and _vec2_.

Similar to the dot product, but instead of returning a single value it returns another vector,
and is only useful in three (and seven, but WLambda's vectors don't support so many) dimensions.

Regardless of the number of dimensions in the input vectors, this function will return a 3d vector.

```wlambda
!x = fvec ${x=1};
!y = fvec ${y=1};

# the cross product of these two values will represent the third axis, z, and will be
# perpendicular to both other vectors.

!z = std:v:cross x y;

std:assert_eq z (fvec ${z=1});

# because all three vectors are perpindicular, they'll all have the same dot product from each other.
std:assert_eq[(std:v:dot x y), (std:v:dot y z)];
std:assert_eq[(std:v:dot y z), (std:v:dot z x)];
```

#### <a name="3813-stdvlerp-vec1-vec2-t"></a>3.8.13 - std:v:lerp _vec1_ _vec2_ _t_

`lerp` stands for linear interpolation.
This function is useful when animating positions, whereas slerp is useful for animating rotations.

Creates a new vector in a new position relative to _vec1_ and _vec2_.
Aside from the two reference vectors, this function also takes a variable, _t_,
which represents how far relative to the first and second vector the new vector should be.

If _t_ is `0`, _vec1_ is returned. If _t_ is `1`, then _vec2_ is returned.
If _t_ is `0.5`, the resulting vector will be halfway in between the first and second vector.

```wlambda
std:assert_eq[ std:v:lerp $f(1,0) $f(0,1) 0.5 , $f(0.5, 0.5) ];
std:assert_eq[ std:v:lerp $f(5,10) ${y=10} 0.75 , $f(1.25, 10) ];
std:assert_eq[ std:v:lerp $[-2,5] $[2,-5] 0.5 , $i(0, 0) ];
!a = $f(83, -49.5);
std:assert_eq[ (std:v:mag a) / 2 , std:v:mag (std:v:lerp a $[] 0.5) ];
std:assert_eq[ (std:v:mag a) * 2 , std:v:mag (std:v:lerp $f(0,0) a 2.0) ];
!b = $f(-484.58, -19);
std:assert_eq[ std:v:lerp b a 1.5 , std:v:lerp a b -0.5 ];
```

#### <a name="3814-stdvslerp-vec1-vec2-t"></a>3.8.14 - std:v:slerp _vec1_ _vec2_ _t_

`slerp` stands for spherical linear interpolation.
This function is useful when animating rotations, whereas lerp is useful for animating positions.

In most cases, you'll want to pass in unit vectors representing rotations to slerp.
You should get back unit vectors in the vast majority of cases,
but if perfect accuracy is required normalizing the output of this function is suggested.

Creates a new vector in a new position relative to _vec1_ and _vec2_.
Aside from the two reference vectors, this function also takes a variable, _t_,
which represents how far relative to the first and second vector the new vector should be.

If _t_ is `0`, _vec1_ is returned. If _t_ is `1`, then _vec2_ is returned.
If _t_ is `0.5`, the resulting vector will be halfway in between _vec1_ and _vec2_.

```wlambda
# compare this to the one for std:v:lerp! note that the length of this one is almost 1.
# this is definitely not the case for std:v:lerp's output with the same input.
!v = std:v:slerp $f(1,0) $f(0,1) 0.5;
# the values may not be exact because of floating point rounding errors,
# but they should be pretty close.
std:assert_rel_eq v.x 0.7071067811865476 0.000001;
std:assert_rel_eq v.y 0.7071067811865476 0.000001;

# The values are interpolated around a circle, so if you raise t high enough you'll start
# getting the same values as you get with a lower t, although not quite because of float rounding.
!half = (std:v:slerp $f(1,0) $f(0,1) 0.5);
!four = (std:v:slerp $f(1,0) $f(0,1) 4.5);
std:assert_rel_eq half.x four.x 0.000001;
std:assert_rel_eq half.y four.y 0.000001;
```

#### <a name="3815-stdvvec2rad-vec"></a>3.8.15 - std:v:vec2rad _vec_

Creates a rotation in radians from the x and y components of _vec_.

Always returns a float.

Coerces the argument into an `ivec` unless it's a `fvec`.

```wlambda
std:assert_eq[ std:num:to_degrees (std:v:vec2rad ${x=1}) , 0.0 ];
std:assert_eq[ std:num:to_degrees (std:v:vec2rad ${y=1}) , 90.0 ];

# halfway in between 0.0 and 90.0 should be 45.
# note that lerp would work here as well
!h = std:v:slerp $f(1, 0) $f(0, 1) 0.5;
std:assert_eq[ std:num:to_degrees (std:v:vec2rad h) , 45.0 ];
```

#### <a name="3816-stdvrad2vec-radians"></a>3.8.16 - std:v:rad2vec _radians_

Creates a unit vector from _radians_.

Always returns an `fvec`.

```wlambda
std:assert_eq[ std:v:rad2vec (std:num:to_radians 0.0) , $f(1, 0)];
std:assert_eq[ ivec (std:v:rad2vec (std:num:to_radians 90.0)), $i(0, 1)];

# halfway in between 0.0 and 90.0 should be 45.
# note that lerp would NOT work here as well, rad2vec returns a unit vector.
!h = std:v:slerp $f(1, 0) $f(0, 1) 0.5; # slerp because rotations
!r = std:v:rad2vec (std:num:to_radians 45.0);
std:assert_rel_eq r.x h.x 0.0001;
std:assert_rel_eq r.y h.y 0.0001;
```

#### <a name="3817-stdvhex2rgbaf-string"></a>3.8.17 - std:v:hex2rgba\_f _string_

Interprets _string_ as an hex encoded color and
returns a 4 element big float vector. The color components
of the float vector go from 0.0 to 1.0.

The string can be:

- 8 characters: `"RRGGBBAA"`
- 6 characters: `"RRGGBB"`, alpha will be 1.0
- 4 characters: `"RGBA"`
- 3 characters: `"RGB"`, alpha will be 1.0
- 2 characters: `"YY"`, where YY is put into R, G and B. Alpha will be 1.0.

```wlambda
!color = std:v:hex2rgba_f "FF00FFFF";

std:assert_rel_eq color.r 1.0 0.001;
std:assert_rel_eq color.g 0.0 0.001;
std:assert_rel_eq color.b 1.0 0.001;
std:assert_rel_eq color.a 1.0 0.001;

!color2 = std:v:hex2rgba_f "C83F";
std:assert_rel_eq color2.r 0.8   0.001;
std:assert_rel_eq color2.g 0.533 0.001;
std:assert_rel_eq color2.b 0.2   0.001;
std:assert_rel_eq color2.a 1.0   0.001;
```

#### <a name="3818-stdvhex2rgbai-string"></a>3.8.18 - std:v:hex2rgba\_i _string_

Like `std:v:hex2rgba_f` this function converts a hex encoded color
from _string_ but returns an integer vector with 4 elements.
The integers are in the range of 0 to 255.

About the format of _string_ please refer to `std:v:hex2rgba_f`.

```wlambda
!color = std:v:hex2rgba_i "FF00FFFF";

std:assert_eq color.r 255;
std:assert_eq color.g 0;
std:assert_eq color.b 255;
std:assert_eq color.a 255;

!color2 = std:v:hex2rgba_i "C83F";
std:assert_eq color2.r 204;
std:assert_eq color2.g 136;
std:assert_eq color2.b 51;
std:assert_eq color2.a 255;
```

### <a name="39-strings"></a>3.9 - Strings

Strings in WLambda are like Rust UTF-8 encoded immutable Unicode strings.
There is no character data type however. There are two types of literal
forms for strings:

```wlambda
"abc def \"foo\"";
std:assert_eq $q/any delimiter may be used instead of/
    "any delimiter may be used instead of";
# Unicode escapes are also working:
std:assert_eq "\u{2211}" "∑";
```

#### <a name="391-string-literal-syntaxes"></a>3.9.1 - String Literal Syntaxes

There are multiple kinds of syntax constructs you can use to
notate string (and byte vector) literals:

- Regular strings
```wlambda
!s = "a b c";

std:assert_eq s "a b c";
```
- Byte vectors
- Quoted strings
- Quoted byte vectors
- WLambda code strings
```wlambda
# Short form $c works too.
!code = $code {
    !this = is a block;
    It just needs to be in valid WLambda[:Syntax];
    .x = But it does not need to pass the compiler
        phase.x;
};

# Primary use case is `eval` and `std:thread:spawn`:
!v = (std:thread:spawn $code {
    !@import std std;
    !res = "x" "y" "z";
    std:str:cat res 33;
}[]).join[];

std:assert_eq v "xyz33";
```

#### <a name="392-str-value"></a>3.9.2 - str _value_

Casts _value_ to a string and returns it.
Also dereferences a value.

```wlambda
std:assert_eq (str "\xFF")     "ÿ";
std:assert_eq (str "\x0A")     "\n";
std:assert_eq (str 1)          "1";
std:assert_eq (str $n)         "";
std:assert_eq (str $t)         "$true";
std:assert_eq (str $f)         "$false";
std:assert_eq (str $&10)       "10";
std:assert_eq (str $&&10)      "10";
std:assert_eq (str ${a=10})    "${a=10}";
std:assert_eq (str $[1,2,3])   "$[1,2,3]";
std:assert_eq (str $o(42))     "42";
std:assert_eq (str $o())       "";

!x = $&&10;
std:assert_eq (str ~ std:ref:weaken x)   "10";
```

#### <a name="393-isstr-value"></a>3.9.3 - is\_str _value_

Returns `$true` if _value_ is a string.

```wlambda
std:assert ~ is_str "foo";

std:assert ~ not ~ is_str $b"foo";
std:assert ~ not ~ is_str :foo;
std:assert ~ not ~ is_str 324;

std:assert ~ not ~ is_str $&&"foo";
std:assert ~ is_str $*$&&"foo";
```

#### <a name="394-stdstrcat-a-b-"></a>3.9.4 - std:str:cat _a_ _b_ ...

Stringifies (like with `str`) and concatenates all its arguments.
If an argument is a vector, it's elements will be stringified and concatenated.

```wlambda
std:assert_eq
    (std:str:cat :a 10 23.2 "ab" "cd" $[1, 2, 3])
    "a1023.2abcd123";
```

If a vector argument is given, it's elements are stringified, thats
useful if you prepare substrings to be concatenated in one single action:

```wlambda
!out = $[];
std:push out "abc";
std:push out "123";
std:push out "XXX";

!s = std:str:cat out;
std:assert_eq s "abc123XXX";
```

#### <a name="395-stdstrjoin-sep-vector"></a>3.9.5 - std:str:join _sep_ _vector_

Join's the stringified elements of _vector_ with the _sep_ string.
Will return an error if _vector_ is not a vector.

```wlambda
std:assert_eq
    (std:str:join "::" $[1,2,3])
    "1::2::3";
```

#### <a name="396-stdstrlen-value"></a>3.9.6 - std:str:len _value_

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

#### <a name="397-stdstrreplace-pattern-replacement-string"></a>3.9.7 - std:str:replace _pattern_ _replacement_ _string_

Replaces every occurence of _pattern_ in _string_ with _replacement_
and returns a new string. All values will be casted to a string if
they aren't.

```wlambda
!s = std:str:replace "dog" "cat"
    "I really like my dog, because when you dog, you can put dog in the dog!";
std:assert_eq s
    "I really like my cat, because when you cat, you can put cat in the cat!";

!s = std:str:replace "9" "1" "9999";
std:assert_eq s "1111";
```

#### <a name="398-stdstrreplacen-pattern-replacement-count-string"></a>3.9.8 - std:str:replace\_n _pattern_ _replacement_ _count_ _string_

Replaces _count_ occurences of _pattern_ in _string_ with _replacement_
and returns a new string. All values will be casted to a string if
they aren't.

```wlambda
!s = std:str:replace_n "dog" "cat" 2
    "I really like my dog, because when you dog, you can put dog in the dog!";
std:assert_eq s
    "I really like my cat, because when you cat, you can put dog in the dog!";

!s = std:str:replace_n "9" "1" 3 "9999";
std:assert_eq s "1119";
```

#### <a name="399-stdstrtrim-value"></a>3.9.9 - std:str:trim _value_

Trims off any (unicode) white space from the start and end of the
stringified _value_.

```wlambda
std:assert_eq
    (std:str:trim "\nfooo bar ")
    "fooo bar";
```

#### <a name="3910-stdstrtrimstart-value"></a>3.9.10 - std:str:trim\_start _value_

Trims off any (unicode) white space from the start of the stringified _value_.

```wlambda
std:assert_eq
    (std:str:trim_start "  \nfooo bar \n")
    "fooo bar \n";
```

#### <a name="3911-stdstrtrimend-value"></a>3.9.11 - std:str:trim\_end _value_

Trims off any (unicode) white space from the end of the stringified _value_.

```wlambda
std:assert_eq
    (std:str:trim_end "  \nfooo bar \n")
    "  \nfooo bar";
```

#### <a name="3912-stdstrpadstart-len-pad-str-value"></a>3.9.12 - std:str:pad\_start _len_ _pad-str_ _value_

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

#### <a name="3913-stdstrpadend-len-pad-str-value"></a>3.9.13 - std:str:pad\_end _len_ _pad-str_ _value_

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

#### <a name="3914-stdstrtobytes-string"></a>3.9.14 - std:str:to\_bytes _string_

Encodes _string_ in UTF-8 and returns a byte vector containing all it's bytes.

```wlambda
!b = std:str:to_bytes "1234";
std:assert_eq b $b"1234";

!b = std:str:to_bytes "Äß日本人";
std:assert_eq b $b"\xC3\x84\xC3\x9F\xE6\x97\xA5\xE6\x9C\xAC\xE4\xBA\xBA";
```

#### <a name="3915-stdstrfromutf8-byte-vector"></a>3.9.15 - std:str:from\_utf8 _byte-vector_

Converts the _byte-vector_ to a Unicode string and returns it.
If the _byte-vector_ contains invalid UTF-8 sequences an
error value is returned.

```wlambda
!s = _? ~ std:str:from_utf8 $b"\xC3\x84\xC3\x9F\xE6\x97\xA5\xE6\x9C\xAC\xE4\xBA\xBA";
std:assert_eq s "Äß日本人";

!r = on_error {|| _ } ~ std:str:from_utf8 $b"\xFF\xFF";
std:assert_eq r "str:from_utf8 decoding error: invalid utf-8 sequence of 1 bytes from index 0";
```

#### <a name="3916-stdstrfromutf8lossy-byte-vector"></a>3.9.16 - std:str:from\_utf8\_lossy _byte-vector_

Converts the _byte-vector_ to a Unicode string and returns it.
If the _byte-vector_ contains invalid UTF-8 sequences a `"�"` will be
inserted.

```wlambda
!s = _? ~ std:str:from_utf8_lossy
    $b"\xC3\x84\xFF\xC3\x9F\xE6\x97\xA5\xE6\x9C\xAC\xE4\xBA\xBA\xFF\xFF\x00";
std:assert_eq s "Ä�ß日本人��\0";
```

#### <a name="3917-stdstrtocharvec-string"></a>3.9.17 - std:str:to\_char\_vec _string_

Converts the _string_ into a vector of integers which represent the Unicode
character number.

```wlambda
!v = std:str:to_char_vec "1234";
std:assert_eq (str v) ~ str $[49,50,51,52];

!v = std:str:to_char_vec "Äß日本人";
std:assert_eq (str v) ~ str $[196,223,0x65E5,0x672C,0x4EBA];
```

#### <a name="3918-stdstrfromcharvec-vector"></a>3.9.18 - std:str:from\_char\_vec _vector_

The reverse operation of `std:str:to_char_vec`. It converts
a vector of integers to a unicode string. Any integer that has
no associated Unicode character will be converted to `"?"`.

```wlambda
std:assert_eq (std:str:from_char_vec $[9999999999]) "?";
std:assert_eq
    (std:str:from_char_vec
        $[49,50,196,223,0x65E5,0x672C,0x4EBA])
    "12Äß日本人";
```

#### <a name="3919-stdstrtolowercase-string"></a>3.9.19 - std:str:to\_lowercase _string_

Swaps all (Unicode) characters in _string_ to their lowercase version.

```wlambda
std:assert_eq (std:str:to_lowercase "ZABzabÄßÜÖ") "zabzabäßüö";
```

#### <a name="3920-stdstrtouppercase-string"></a>3.9.20 - std:str:to\_uppercase _string_

Swaps all (Unicode) characters in _string_ to their lowercase version.

```wlambda
std:assert_eq (std:str:to_uppercase "ZABzabäßüö") "ZABZABÄSSÜÖ";
```

### <a name="310-bytes-or-byte-vectors"></a>3.10 - Bytes (or Byte Vectors)

Bytes are a vector of bytes. Unlike strings they don't have any encoding.
Literal syntax however supports inserting unicode characters:


```wlambda
$b"abc";
$b"\xFF\xFD\x00";
$Q/ABCDEF\xFD/;      # \xFD is not an escape sequence here!
```

#### <a name="3101-call-properties-of-bytes"></a>3.10.1 - Call Properties of Bytes

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

#### <a name="3102-byte-conversion-functions"></a>3.10.2 - Byte Conversion Functions

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

#### <a name="3103-isbytes-value"></a>3.10.3 - is\_bytes _value_

Returns `$true` if _value_ is a byte vector.

```wlambda
std:assert ~ is_bytes $b"ABC";
std:assert ~ not ~ is_bytes "ABC";
```

### <a name="311-symbols"></a>3.11 - Symbols

Symbols are a special kind of strings that are interned by the runtime.  That
means, comparing two symbols is an O(1) operation and not an O(n) operation on
the length of the string. Symbols are also used as keys for maps.  Use them
however you see fit. They will do a key lookup (on maps, vectors (as indices)
and user values) if they are called with an argument.

```wlambda
std:assert_eq (:1 $[1,2,3]) 2;
std:assert_eq (:a ${a=30}) 30;
```

They are basically the same as string, but strings have
slightly different calling semantics and a different literal syntax.
Often you can use them as shortform literal in places where a string
is expected:

```wlambda
std:assert_eq (std:str:replace :A :a "All AbabA") "all ababa";
```

They can be very useful as sentinel values or custom enums:

```wlambda
!x = :ON;
!y = :OFF;

std:assert_eq ((x == :ON) { 10 }) 10;

# They don't match with strings:
std:assert_eq ((x == "ON") { 10 } { 20 }) 20;

# Work together nicely with `match`:

!state = "";
match x
    :ON  => { .state = "is on" }
    :OFF => { .state = "is off" };
std:assert_eq state "is on";

match y
    :ON  => { .state = "is on" }
    :OFF => { .state = "is off" };
std:assert_eq state "is off";
```

Keep in mind, that all symbols are interned strings. And if you create many
symbols that are not used anymore, you might need to trigger a cleanup
with `std:symbols::collect`.

#### <a name="3111-stdsymbolscollect"></a>3.11.1 - std:symbols:collect

Collect and remove all interned symbols in the current thread that are no
longer used. Returns the number of freed symbols. Please keep in mind, that
the `std:ref_id` of any collected symbol will be different from a symbol that
is created later with the same characters.

If you rely on the reference ID of a symbol, you should make sure to keep it
around. Literal symbols are always kept around as long as the code is running
or referenced somewhere (eg.  by a function).

```wlambda
std:symbols:collect[];

!probably_unique_sym = sym "onceonly_used";

std:assert_eq
    (std:ref_id ~ sym "onceonly_used")
    (std:ref_id probably_unique_sym);

std:assert_eq std:symbols:collect[] 0;

.probably_unique_sym = $none;

std:assert_eq std:symbols:collect[] 1;
```

### <a name="312-pairs-pa-b"></a>3.12 - Pairs `$p(a, b)`

A pair is an immutable tuple of 2 values. You can use it for returning two
values from a function as it is a slight bit slimmer than a vector with two
values. Unlike a vector, pairs are compared by `==` according to their contents
and not by referencial equality.

There are two ways to form a pair:

- Pair value syntax: `$p(a, b)`
- Pair right associative operator: `a => b`

You can access the pair values by the following keys:

```wlambda
!v = $p(11, 12);

std:assert_eq v.0     11;
std:assert_eq v.1     12;
std:assert_eq (0 v)   11;
std:assert_eq (1 v)   12;

std:assert_eq v.car   11;
std:assert_eq v.cdr   12;

std:assert_eq v.head  11;
std:assert_eq v.tail  12;

std:assert_eq v.first    11;
std:assert_eq v.second   12;

# Pairs are often used to represent map entries,
# so you can use `key` and `value`
# and the short forms `k` and `v` too:
std:assert_eq v.value 11;
std:assert_eq v.key   12;

std:assert_eq v.v     11;
std:assert_eq v.k     12;
```

Comparison does happen by their contents:

```wlambda
std:assert $p(1, 2) == $p(1, 2);
std:assert $p(2, 2) != $p(1, 2);

# In contrast to vectors:
std:assert not ~ $[1, 2] == $[1, 2];
```

The index is wrapping around, that means `$p(a, b).2` is the first element again:

```wlambda
!v = $p(33, 44);
!l = $[];

iter i $i(0, 4)
    ~ std:push l v.(i);

std:assert_eq (str l) (str $[33, 44, 33, 44]);
```

A pair is a referencial data type, that means you can use `std:ref_id` on it:

```wlambda
!a = $p(1, 2);
!b = $p(2, 3);
!id_a = std:ref_id a;
!id_b = std:ref_id b;

std:assert (id_a != id_b);
std:assert std:ref_id[a] == id_a;

!v = $[a];
std:assert std:ref_id[v.0] == id_a;
```

#### <a name="3121-pair-operator-a--b"></a>3.12.1 - Pair Operator `a => b`

Writing `a => b` operator is the same as writing `$p(a, b)`.  However, the
precedence of the `=>` operator is the lowest and right associative, so writing
this is possible:

```wlambda
!p = 1 + 2 => 3 + 4;

std:assert_eq p $p(3, 7);
```

The following example shows off the associativity of the operator:

```wlambda
!a = 1 => 2;
!b = 2 => 3 => 4;

std:assert_eq a $p(1, 2);
std:assert_eq b $p(2, $p(3, 4));
std:assert_eq b 2 => 3 => 4;
```

#### <a name="3122-pair-constructor-a--b"></a>3.12.2 - Pair Constructor `a => b`


#### <a name="3123-cons-a-b"></a>3.12.3 - cons _a_ _b_

Creates a new pair from the values _a_ and _b_.

```wlambda
!p = cons 3 4;

std:assert_eq p $p(3, 4);
```

#### <a name="3124-pair-stringbyte-vector-operations"></a>3.12.4 - Pair string/byte vector operations

If you call a pair with a string or byte vector as argument, there are some
operations that can be done:

##### <a name="31241-p-from--count--string-or-byte-vec"></a>3.12.4.1 - $p( _from_ , _count_ ) _string-or-byte-vec_

Returns a substring starting at _from_ with the length _count_.

```wlambda
std:assert_eq ($p(2, 4) "abcdefgh") "cdef";
```

The same works for byte vectors:

```wlambda
std:assert_eq ($p(2, 4) $b"abcdefgh") $b"cdef";
```

##### <a name="31242-p-pattern--replacement--string-or-byte-vec"></a>3.12.4.2 - $p( _pattern_ , _replacement_ ) _string-or-byte-vec_

Replaces all _pattern_ occurences in _string_ by _replacement_.

```wlambda
std:assert_eq ($p(";", "_") "A;B;D;EFG;HI") "A_B_D_EFG_HI";
```

The same works for byte vectors:

```wlambda
std:assert_eq ($p($b";", $b"_") $b"A;B;D;EFG;HI") $b"A_B_D_EFG_HI";
```

##### <a name="31243-p-split-pattern--max--string-or-byte-vec"></a>3.12.4.3 - $p( _split-pattern_ , _max_ ) _string-or-byte-vec_

Splits _string_ at _split-pattern_ a _max_ number of times.
If _max_ is 0, it is split completely.

```wlambda
std:assert_eq str[$p(";", 3) "A;B;D;EFG;HI"] ~ str $["A", "B", "D;EFG;HI"];

std:assert_eq str[$p(";", 0) "A;B;D;EFG;HI"] ~ str $["A", "B", "D", "EFG", "HI"];
```

The same works for byte vectors:

```wlambda
std:assert_eq str[$p($b";", 0) $b"A;B;D;EFG;HI"] ~ str $[$b"A", $b"B", $b"D", $b"EFG", $b"HI"];
```

#### <a name="3125-pair-to-iterator"></a>3.12.5 - Pair to Iterator

Pairs play a special role if you make an iterator from it.
It can be used to create a specialized iterator that only
iterates over keys or values of a map. Or that enumerates
a vector or map.

##### <a name="31251-iter---range"></a>3.12.5.1 - Iter - Range

`$iter $p(0, 10)` is the same as `$iter $i(0, 10)` and will construct an
iterator that iterates from `0` to `9` (inclusive).
Because of the pair operator `a => b` we can nicely write a counting loop like this:

```wlambda
!sum = $@i
    iter i 0 => 10 {
        $+ i;
    };
std:assert_eq sum 45;
```

##### <a name="31252-iter---enumerate"></a>3.12.5.2 - Iter - Enumerate

If the first value of the pair is `:enumerate`
it will enumerate entries in a map or values in a vector.

```wlambda
!v = $[];

# $iter is only explicit here for demonstration
# purposes! `iter` will make an iter from the pair
# if you don't pass one!
iter i ($iter $p(:enumerate, $[:a, :b, :c]))
    ~ std:push v i;

std:assert_eq (str v) (str $[0, 1, 2]);
```

For maps:

```wlambda
!v = $[];
iter i $p(:enumerate, ${a = 10, b = 20})
    ~ std:push v i;

std:assert_eq (str v) (str $[0, 1]);
```

##### <a name="31253-iter---values"></a>3.12.5.3 - Iter - Values

This is useful for iterating over the values in a map in an undefined order:

```wlambda
!m = ${ a = 10, b = 20, c = 33 };

!sum = $@i iter v $p(:values, m) ~ $+ v;

std:assert_eq sum 63;
```

##### <a name="31254-iter---keys"></a>3.12.5.4 - Iter - Keys

You can also iterate over map keys in an undefined order:

```wlambda
!m = ${ :10 = :c, :20 = :b, :30 = :a };

!sum = $@i iter v $p(:keys, m) ~ $+ v;

std:assert_eq sum 60;
```

#### <a name="3126-ispair-value"></a>3.12.6 - is\_pair _value_

Checks if _value_ is a pair.

```wlambda
std:assert ~ is_pair $p(1, 2);
std:assert not ~ is_pair $[1, 2];
std:assert not ~ is_pair $i(1, 2);
```

### <a name="313-vectors-or-lists"></a>3.13 - Vectors (or Lists)

The literal syntax for vectors (or sometimes also called lists in WLambda)
is `$[...]`. You may write any kind of expression in it and you will get
a vector from it.

For iteration over a vector please refer to [5.2 Collection Iteration](#52-collection-iteration).

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

#### <a name="3131-stdpush-vector-item"></a>3.13.1 - std:push _vector_ _item_

Pushes _item_ to the end of _vector_. Returns _item_.

```wlambda
!v = $[1,2];

std:push v 3;

std:assert_eq (str v) (str $[1,2,3]);
```

#### <a name="3132-stdpop-vector"></a>3.13.2 - std:pop _vector_

Pops off the last element of _vector_. Returns `$none` if the vector is empty
or if _vector_ is not a vector.

```wlambda
!v = $[1,2,3];

std:assert_eq (std:pop v) 3;
std:assert_eq (str v) (str $[1,2]);
```

#### <a name="3133-stdunshift-vector-item"></a>3.13.3 - std:unshift _vector_ _item_

Inserts _item_ at the front of _vector_. Returns _item_ and mutates _vector_
inplace. Be aware that this operation is of O(n) complexity.

```wlambda
!v = $[1,2];

std:unshift v 3;

std:assert_eq (str v) (str $[3,1,2]);
```

#### <a name="3134-vector-splicing"></a>3.13.4 - Vector Splicing

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

#### <a name="3135-stdappend-vec-a-value-or-vec-"></a>3.13.5 - std:append _vec-a_ _value-or-vec_ ...

Appends _value-or-vec_ and all following items to _vec-a_.
If _value-or-vec_ is a vector, all its items will be appended to _vec-a_.

```wlambda
!v = std:append $[1,2,3] :a :b $[:c, :d];

std:assert_eq (str v) "$[1,2,3,:a,:b,:c,:d]";
```

If _vec-a_ is not a vector, a vector containing it will be created:

```wlambda
!v = std:append 1 :a :b $[:c, :d];

std:assert_eq (str v) "$[1,:a,:b,:c,:d]";
```

#### <a name="3136-stdprepend-vec-a-value-or-vec-"></a>3.13.6 - std:prepend _vec-a_ _value-or-vec_ ...

Prepends _value-or-vec_ and all following items to the front of _vec-a_.
If _value-or-vec_ is a vector, all its items will be prepended to _vec-a_.

```wlambda
!v = std:prepend $[1,2,3] :a :b $[:c, :d];

std:assert_eq (str v) (str $[:d, :c, :b, :a, 1, 2, 3]);
```

If _vec-a_ is not a vector, a vector containing it will be created:

```wlambda
!v = std:prepend 1 :a :b $[:c, :d];

std:assert_eq (str v) (str $[:d, :c, :b, :a, 1]);
```

#### <a name="3137-stdtake-count-vector"></a>3.13.7 - std:take _count_ _vector_

Takes and returns the first _count_ elements of _vector_. Does not
mutate _vector_.

```wlambda
!v = $[1,2,3,4,5,6];

!t = std:take 4 v;

std:assert_eq (str v) "$[1,2,3,4,5,6]";
std:assert_eq (str t) "$[1,2,3,4]";
```

#### <a name="3138-stddrop-count-vector"></a>3.13.8 - std:drop _count_ _vector_

Drops _count_ elements from _vector_ and returns them as new vector.
Does not mutate _vector_.

```wlambda
!v = $[1,2,3,4,5,6];

!t = std:drop 4 v;

std:assert_eq (str v) "$[1,2,3,4,5,6]";
std:assert_eq (str t) "$[5,6]";
```

### <a name="314-associative-maps-or-string-to-value-mappings"></a>3.14 - Associative Maps (or String to Value mappings)

Aside from vectors there are associative maps in WLambda. Their syntax is
`${ key = expr, ... }`. The keys of these maps have to be symbols (or strings),
the values in the literals can be any expression. Keys for maps are interned strings,
so keep that in mind if you fill a map with garbage keys.

For iteration over a map please refer to [5.2 Collection Iteration](#52-collection-iteration).

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

#### <a name="3141-map-splicing"></a>3.14.1 - Map Splicing

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

### <a name="315-references"></a>3.15 - References

TODO
    - 3 types: strong, hidden, weak
        - strong:
            - `$&&`
            - mention DWIM'ery
        - hidden:
            - `$&`
            - mention usage for closures
            - how to "Unhide" a reference using `$:`
        - weak:
            - std:ref:weaken and `$w&`
            - how to break reference cycles
            - how weak references are also caught weakly
              by closures and not strongly.
            - how to get a strong reference using `$:`

Some data structures already have reference characteristics, such as strings,
vectors and maps. But you can also wrap other values like integers, floats, ...
into a reference. There are 3 types of references in WLambda that have
different usecases and semantics. These referential types are neccessary to
mutate lexical variables from a parent scope. To give a rather natural example:

```wlambda
!x = 10;
{ .x = 20; }[];
std:assert_eq x 20;
```

The example works rather intuitively. There is however lots of implicit
referential stuff going on. Once `x` is captured by a closure its contents is implicitly
changed in to a _hidden_ `$&` reference. The closure then stores this hidden
reference too. You have to be aware of this, because in some use cases this can lead
to cyclic reference structures, which are not automatically freed. Please use
weak references `$w&` for mitigating this.

Hidden references and weak references captured by a closure are dereferenced
implicitly if you access the variables. Weak references in the local scope are
not implicitly dereferenced. However, sometimes it's desirable to have a more
explicit reference data types. For this the strong references `$&&` are
available. Use `$*` for accessing the value of a strong reference or a weak reference
in the local scope.

TODO

These types of references exist:

- `$&` - A _hidden_ reference, that is captured by closures or constructed using `$&`.
- `$w&` - A _weak_ reference, can't be constructed literally, only indirectly
as upvalue of a closure or by `std:ref:weaken`.
- `$&&` - A _strong_ reference, that is captured strongly by closures.
Inside closures they are also implicitly dereferenced by assignment
and access by variable name.

```wlambda
!x = $& 10;

{ .x = 20; }[]; # Closures implicitly handle weak references

std:assert_eq x 20;
```

And the same with strong references:

```wlambda
!x = $&& 10;

.*x = 11;

{ .*x = 20; }[]; # Closures need explicit handling of strong references

std:assert_eq $*x 20;
```

Strong references can also be created using the `std:to_ref` function and
the `$:` operation.

#### <a name="3151-stdtoref-value"></a>3.15.1 - std:to\_ref _value_

Creates a new strong reference that refers to a cell that stores _value_.

```wlambda
!x = std:to_ref 10;

std:assert_eq (std:ser:wlambda x) "$&&10";

std:assert_eq $*x 10;
```

#### <a name="3152-stdrefweaken-ref"></a>3.15.2 - std:ref:weaken _ref_

You can weaken any of those two types of references manually using the
`std:ref:weaken` function.

```wlambda
!drop_check = $& $f;

# Set `drop_check` to $true when all (non weak) references to it are gone.
!x = $&& (std:to_drop {|| .drop_check = $true });

# Create a weakened reference to the value referred to by x:
!y = std:ref:weaken x;

# The reference to the drop function is removed and this means
# that the weak reference in y is invalidated and returns $n in future.
.x = $n;

# Deref y now gives you $n:
std:assert_eq $*y $n;

std:assert drop_check;
```

Please note that you can use `$w&`/`$weak&` as a shortcut to calling the library function:

```wlambda
!x      = $&& 10;
!x_weak = $w& x;

std:assert_eq x      &> type "ref_strong";
std:assert_eq x_weak &> type "ref_weak";
```

#### <a name="3153-isref-value"></a>3.15.3 - is\_ref _value_

Returns `$true` if _value_ is a reference (strong, hidden or weak).

```wlambda
!x = $&&10;
std:assert ~ is_ref ~ std:ref:weaken x;
std:assert ~ is_ref $&10;
std:assert ~ is_ref $&&10;

std:assert ~ not ~ is_ref $[1,2,3];
std:assert ~ not ~ is_ref ${a=10};
std:assert ~ not ~ is_ref $true;
std:assert ~ not ~ is_ref $none;
```

#### <a name="3154-iswref-value"></a>3.15.4 - is\_wref _value_

Returns `$true` if _value_ is a weak reference.

```wlambda
!x = $&& 10;
!y = std:ref:weaken x;
std:assert ~ is_wref y;
std:assert ~ not ~ is_wref x;
```

#### <a name="3155-stdrefstrengthen-ref"></a>3.15.5 - std:ref:strengthen _ref_

You can convert a weak reference (weakened by `std:ref:weaken`) or a captured weak
reference `$&` to strong with `std:ref:strengthen

```wlambda
!x = $&&10;
!y = std:ref:weaken x;

.x = $none;
std:assert ~ is_none $*y;

.x = $&&10;
.y = std:ref:weaken x;
!y2 = std:ref:strengthen y; # Here we take a second strong reference from a weak one

.x = $none;
std:assert ~ is_some $*y;
std:assert ~ is_some $*y2;

.y2 = $none;
std:assert ~ is_none $*y;
```

#### <a name="3156-stdrefset-ref-value"></a>3.15.6 - std:ref:set _ref_ _value_

Sets the value of the reference _ref_ to _value_.
If _ref_ is not a strong, hidden or weak reference nothing happens.

Returns _value_ or `$none`.

```wlambda
!r1 = $&&1;
std:ref:set r1 10;
std:assert_eq $*r1 10;

# Note that $& references in local variables are
# automatically dereferenced. Because of that we need to wrap it into
# an extra reference.
!r2 = $& $& 1;
std:ref:set r2 11;
std:assert_eq $*r2 11;

!r3 = $& $& 1;
!w3 = std:ref:weaken r3;
std:ref:set w3 14;      # Set reference via the weak reference in w3 to r3.
std:assert_eq $*r3 14;
```

### <a name="316-iterators-iter-expression"></a>3.16 - Iterators $iter _expression_

As a companion to the `iter` operation there are the iterator values.
These are a special kind of values that generate a value when they are called.
It supports to make an iterator from the same values as the `iter` operation.

You can create an iterator from vectors and maps, but also specialized
iterators that return a range of numbers or only keys of a map.
About this see the section _Iterator Kinds_ below.

The `$iter` syntax takes a complete expression as argument, that means
you can directly write `$iter function arg1 arg2 ...` without
delimiting the function call.

Here is an example how to make an iterator over a vector:

```wlambda
!it = $iter $[1,2,3,4];

!first  = it[];     # returns an optional value $o(1)
!second = it[];     # returns an optional value $o(2)

std:assert_eq first    $o(1);
std:assert_eq second   $o(2);
```

You can also directly cast an iterator, which will also
make it return a value:

```wlambda
!it = $iter $[1,2,3,4];

std:assert_eq (int it)  1;
std:assert_eq (int it)  2;
```

You can pass an iterator also to the `iter` operation:

```wlambda
!it = $iter $[1,2,3];

!sum = 0;

iter i it {
    .sum = sum + i;
};

std:assert_eq sum 6;
```

#### <a name="3161-iterator-kinds"></a>3.16.1 - Iterator Kinds

Here is a table of the behaviour of iterators created from WLambda data.

| Data      | Iterator return values |
|-----------|-----------|
| vector  | Each element of the vector. |
| map  | Each key/value pair of the map in undefined order. |
| `$none`   | Returns nothing  |
| optional | Returns the optional value on first invocation. |
| `$o()` | Returns nothing. |
| int  | Returns the integer value on first invocation. |
| float  | Returns the integer value on first invocation. |
| string | Returns the individual characters as string. |
| symbol | Returns the individual characters as string. |
| byte vector | Returns the individual bytes as byte vector. |
| `$error`  | Returns the error value on first invocation. |
| `$i(a, b)`  | The integers in the range of _a_ to _b_, not including _b_. |
| `$i(a, b, step)`  | The integers in the range of _a_ to _b_ advanced by _step_, not including _b_. |
| `$f(a, b)`  | The floats in the range of _a_ to _b_ advanced by `1.0`, not including _b_. |
| `$f(a, b, step)`  | The floats in the range of _a_ to _b_ advanced by _step_, not including _b_. |
| `$p(:enumerate, map)`  | Returns integers in the range of `0` to `len map`. |
| `$p(:enumerate, vector)`  | Returns integers in the range of `0` to `len vector`. |
| `$p(:values, map)`  | Returns the values of the _map_ in undefined order. |
| `$p(:keys, map)`  | Returns the keys of the _map_ in undefined order. |
| `$p(int_a, int_b)` | The same as `$i(a, b)`. This makes it possible to write `$iter 0 => 10`. |
| `$p(iterator_a, iterator_b)` | Returns a zip operation of the elements returned by the iterator_a and iterator_b until one of both returns `$o()`. |
| `$p(iterator, x)` | Returns a zip operation of the elements returned by the iterator and the newly created iterator`$iter x`. |

#### <a name="3162-iterators-on-mutated-data"></a>3.16.2 - Iterators on mutated data

Iterators hold a reference to the collection values. That means, if you mutate
a vector while you iterate over it, it will not crash but it might produce
weird effects.

```wlambda
!v = $[1,2,3];
!it = $iter v;

iter i v {
    ? i <= 3 {
        std:push v i + 10;  # This is not recommended however...
    };
};

std:assert_eq (str v) (str $[1, 2, 3, 11, 12, 13]);
```

This will also work for maps, but as the order of the map entries
is undefined it will produce very indeterministic effects and it's really
not recommended.

#### <a name="3163-splicing-an-iterator"></a>3.16.3 - Splicing an Iterator

You can directly insert the values produced by an iterator into a vector or map:

```wlambda
!it = $iter $[1,2,3,4];

!v = $[10, 20, *it, 99];

std:assert_eq (str v) (str $[10, 20, 1, 2, 3, 4, 99]);
```

Same goes for maps:

```wlambda
!it = $iter ${a = 10, b = 20};

!m = ${ x = 99, *it };

std:assert_eq m.a 10;
std:assert_eq m.b 20;
std:assert_eq m.x 99;
```

#### <a name="3164-calling-an-iterator-with-a-function"></a>3.16.4 - Calling an Iterator with a Function

When an iterator gets called with a function as first argument
it will repeatedly call that function until no more values are
available:

```wlambda
!it = $iter $[1,2,3];

!sum = 0;

it { .sum = sum + _ };

std:assert_eq sum 6;
```

#### <a name="3165-zip-iterators"></a>3.16.5 - Zip Iterators

To highlight this feature from the table above: You can zip two iterators if
you pass an iterator as first part of a pair `$p(a, b)`:

```wlambda
!v = $["a", "b", "c"];

!elems = $@vec
    iter i $p($iter v, $iter $i(0, 10)) {
        $+ i;
    };

std:assert_eq
    (str elems)
    (str $[$p("a", 0), $p("b", 1), $p("c", 2)]);
```

### <a name="317-calling-semantics-of-data-types"></a>3.17 - Calling Semantics of Data Types

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
| $p(int_from, int_count) | string | Substring operation. (See also section about pairs) |
| $i(int_from, int_count, ...) | string | Substring operation. |
| string | $i(int_from, int_count, ...) | Substring operation. |
| $p(string, int)         | string | Split operation. (See also section about pairs) |
| string | $p(string, int) | Split operation. |
| $p(string, string)      | string | Replace all operation. (See also section about pairs) |
| string | $p(string, string) | Replace all operation. |
| $p(int_from, int_count) | byte_vec | Substring operation. (See also section about pairs) |
| $i(int_from, int_count, ...) | byte_vec | Substring operation on the byte vector. |
| byte_vec | $i(int_from, int_count, ...) | Substring operation on the byte vector. |
| $p(byte_vec, int)       | byte_vec | Split operation. (See also section about pairs) |
| byte_vec | $p(byte_vec, int) | Split operation. |
| $p(byte_vec, byte_vec)  | byte_vec | Replace all operation. (See also section about pairs) |
| byte_vec | $p(byte_vec, byte_vec) | Replace all operation. |
| $o()      | -                 | Returns $none. |
| $o(x)     | -                 | Returns _x_. |
| $o()      | *                 | Calls $none with arguments, leading to a panic. |
| $o(x)     | *                 | Calls _x_ with the given arguments. |
|           |                   | |

## <a name="4-conditional-execution---if--then--else"></a>4 - Conditional Execution - if / then / else

### <a name="41-if-condition-then-expr-else-expr"></a>4.1 - ?/if _condition_ _then-expr_ [_else-expr_]

The keyword for conditional execution is either `if` or just the question mark `?`.
Both are possible to use, while `?` is a bit more WLambda idiomatic.
It takes 3 arguments: The first is an expression that will be evaluated
and cast to a boolean. If the boolean is `$true`, the second argument is
evaluated. If the boolean is `$false` the thrid argument is evaluated.
The third argument is optional.

```wlambda
!x = 10;

!msg = "x is ";
? x > 4 {
    .msg = std:str:cat msg "bigger than 4";
} {
    .msg = std:str:cat msg "smaller than or equal to 4";
};

std:assert_eq msg "x is bigger than 4";

# You may also use `if` in case it suits your coding style better:
if x == 10 {
    std:assert $true;
} {
    std:assert $false;
};
```

The _condition_ can also be a function block, which will be evaluated:

```wlambda
!res =
    ? { !x = 2; x > 1 } "x > 1";

std:assert_eq res "x > 1";
```

### <a name="42-using-booleans-for-conditional-execution"></a>4.2 - Using Booleans for Conditional Execution

Conditional execution is also provided by the bool data type. As in WLambda
everything can be called like a function, you can just pass other functions as
arguments to `$true` and `$false`.  If you pass a function as first argument to
`$true`, it will be executed. If you pass a function as second argument to
`$false` then that will be executed.

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

#### <a name="421-pick-bool-a--b-"></a>4.2.1 - pick _bool_ _a_ -b-

Often, you may want to choose one variable (_a_) or another (_b_) based on some predicate (_bool_).
For these situations, the `pick` function is available.
For example, perhaps you want to make a function which can take any number of parameters,
or a single list parameter.

```wlambda
!sum = \|| std:fold 0 { _ + _1 } ~ pick (is_vec _) _ @;
```

#### <a name="422-indexing-by-booleans"></a>4.2.2 - Indexing by Booleans

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

### <a name="43-value-matching-with---match-value-expr-"></a>4.3 - Value matching with - match _value-expr_ ...

See also [7.1.1](#711-match-value-expr-match-pair1--default-expr) for a more
comprehensive discussion of `match` and structure matchers.

`match` allows for easily select from a set of values:

```wlambda
!check_fun = {
    match _
        20 =>   "It's 20"
        30 =>   "It's 20"
        "No idea?"
};

std:assert_eq check_fun[20] "It's 20";
std:assert_eq check_fun[34] "No idea?";
```

Also works for deeper data structures:

```wlambda
!val = $[1, ${a = 10}, ${a = 10}, ${a = 10}, ${a = 10}, 2, 2, 2, 10];

!res =
    match val
        $[1, _*, 3, 10] =>  :a
        $[1, a ~ _* ${ a = y }, b ~ _+ 2, 10] => {
            ${
                a_elems = $\.a,
                a_value = $\.y,
                b_vals  = $\.b,
            }
        };

std:assert_str_eq res.a_elems $[${ a = 10 }, ${ a = 10 }, ${ a = 10 }, ${ a = 10 }];
std:assert_str_eq res.a_value 10;
std:assert_str_eq res.b_vals  $[2,2,2];
```

## <a name="5-loops-and-iteration"></a>5 - Loops And Iteration

WLambda has many ways to loop and iterate:

- Counting loop with `range`
- While some condition is `$true` with the `while` special form.
- Over the items in a vector or map with the `iter` special form.
- Calling an `$iter` iterator value with a function as first argument.
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

### <a name="51-control-flow"></a>5.1 - Control Flow

#### <a name="511-while-predicate-body"></a>5.1.1 - while _predicate_ _body_

`while` will evaluate _body_ until the evaluation of _predicate_ function returns `$false`.
Or `break` is used to end the loop. The loop can be restarted using `next`.
This is the most basic loop for iteration:

```wlambda
!i   = 0;
!out = $[];

while i < 10 {
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

The first 

#### <a name="512-iter-var-iterable-body"></a>5.1.2 - iter _var_ _iterable_ _body_

This is the primary syntax of WLambda to iterate over collections,
numeric ranges and generally everything you can create an iterator from
using the `$iter` syntax.

The _var_ will be defined inside the _body_ and be filled with the
value that was generated for the current iteration.
And _iterable_ is everything that `$iter` can make an iterator from.
Please refer to the section `Iterator Kinds` for a listing of this.

Like usual, the control flow manipulators `next` and `break` also work
for this kind of loop.

##### <a name="5121-counting-loop-with-iter"></a>5.1.2.1 - Counting loop with _iter_

Here is an example how to iterate over a range from 1 to 9 and
collect the sum of those integers using an accumulator:

```wlambda
!sum = $@int iter i $i(1,10) ~ $+ i;

std:assert_eq sum 45;
```

Because `$iter $p(1, 10)` is the same as `$iter $i(1, 10)` and because
there is the pair constructor operator `a => b`, the above can also be written as:

```wlambda
!sum = $@int iter i 1 => 10 ~ $+ i;

std:assert_eq sum 45;
```

##### <a name="5122-vector-iteration-with-iter"></a>5.1.2.2 - Vector iteration with _iter_

Here is a simple example of how to iterate over all items of a vector
in order:

```wlambda
!sum = 0;

iter i $[1,2,3,4,5,6] {
    .sum = sum + i;
};

std:assert_eq sum 21;
```

Even if you pass the syntax for constructing a function to `iter` it will
create a block of statements from it. So this will work too (also for `while` above):

```wlambda
!sum = 0;

iter i $[1,2,3,4,5,6] \.sum = sum + i;

std:assert_eq sum 21;
```

However _body_ does not have to be a function definition or block, it can also
be just a regular call argument:

```wlambda
!sum = 0;

!inc = { .sum = sum + _; };

iter i $[1,2,3,4] inc[i];

std:assert_eq sum 10;
```

To iterate over a vector by index you can use this:

```wlambda
!v = $[1,2,3,4,5];
!sum = 0;

iter i $i(0, len v) {
    .sum = sum + v.(i);
};

std:assert_eq sum 15;
```

##### <a name="5123-map-iteration-with-iter"></a>5.1.2.3 - Map iteration with _iter_

Iteration over a map is also easy and concise. The map entry
will be represented using a pair value `$p(value, key)`.
You can access the first and second element of a pair using the `v`/`value`
and `k`/`key` keys of a pair (but also all other pair accessors defined
in the section for pairs):

```wlambda
!sum = 0;

iter i ${ a = 10, b = 20 } {
    .sum = sum + i.v;
};

std:assert_eq sum 30;
```

Very useful for iterating just over the keys or values of a map can also be the
special iterator values you get from the pair constructors:

```wlambda
!m = ${ a = 10, b = 20 };
!sum = 0;

iter v $p(:values, m) {
    .sum = sum + v;
};

std:assert_eq sum 30;
```

Or if you need the keys:

```wlambda
!m = ${ a = 10, b = 20 };
!sum = 0;

iter k $p(:keys, m) {
    std:displayln "FOO" k;
    .sum = sum + m.(k);
};

std:assert_eq sum 30;
```

##### <a name="5124-closures-and-iter-iter-i-"></a>5.1.2.4 - Closures and _iter_ `iter i ...`

If you need a new variable for capturing it in a closure on each
iteration you need to make a new variable binding for each iteration:

```wlambda
!closures = $[];

# Without the rebinding of the variable `i`, `i` would be captured as hidden
# reference and each iteration would update the contents of that reference.
iter i $i(0, 10) {
    !i = i;
    std:push closures { i * 10 };
};

std:assert_eq ($@i closures \$+ _[]) 450;
```

#### <a name="513-range-start-end-step-fun"></a>5.1.3 - range _start_ _end_ _step_ _fun_

`range` counts from _start_ to _end_ by increments of _step_ and calls _fun_
with the counter. The iteration is inclusive, this means if _start_ == _end_
the function _fun_ will be called once.

In contrast to `iter` this is not a special syntax, but just a regular function
that calls another function repeatedly. You can control it using the `break` and
`next` functions however.

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

#### <a name="514-break-value"></a>5.1.4 - break _value_

`break` stops the inner most iterative construct, which then will return _value_.
This should work for all repeatedly calling operations, such as
`for`, `while`, `iter` and when calling lists directly. Also most library functions
that iteratively call you react to it, like `std:re:map` and `std:re:replace_all`.
Be aware, that returning a value might not be supported by all iterative constructs.

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

#### <a name="515-next"></a>5.1.5 - next

`next` stops execution of the current function or statement block and continues
with the next iteration of the inner most iteration.

```wlambda
!sum = $@i $[1,2,3,4] {
    (_ == 3) next;
    $+ _;
};
std:assert_eq sum 7;
```

```wlambda
!sum = $@i range 1 10 1 {
    (_ % 2 == 0) next;
    $+ _;
};
std:assert_eq sum 25;
```

#### <a name="516-jump-index-val-branch1--last-branch"></a>5.1.6 - jump _index-val_ _branch1_ ... _last-branch_

This is a jump table operation, it's a building block for the more
sophisticated `match` operation. The first argument is an index into the table.
If the index is outside the table the _last-branch_ is jumped to.  The branches
are compiled like the bodies of `while`, `iter`, `match` and `?` into a runtime
evaluated block.

```wlambda
!x   = 10;
!idx = 2;

!res =
    jump idx
        { x + 3 }
        { x + 4 }
        { x + 5 };

std:assert_eq res 15;
```

The arms don't have to be in `{ ... }` because they are blocks
and the above could be written like this:

```wlambda
!x   = 10;
!idx = 2;

!res =
    jump idx
        x + 3
        x + 4
        x + 5;
std:assert_eq res 15;

# or even this:
!res = x + (jump idx 3 4 5);
std:assert_eq res 15;
```

### <a name="52-collection-iteration"></a>5.2 - Collection Iteration

#### <a name="521-iteration-over-vectors"></a>5.2.1 - Iteration over vectors

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

#### <a name="522-iteration-over-maps"></a>5.2.2 - Iteration over maps

Iterating over a map is as simple as iterating over a vector.
The map can be called with a function as first argument and it starts
iterating over its key/value pairs. The first argument of the
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

#### <a name="523-for-iteratable-value-function"></a>5.2.3 - for _iteratable-value_ _function_

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

### <a name="53-accumulation-and-collection"></a>5.3 - Accumulation and Collection

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

These syntaxes are not lexically scoped. That means `$+` and `$@@` can be used
in other functions:

```wlambda
!out_mul = { $+ _ * 20 };

!v = $@vec iter i $i(1,5) ~ out_mul i;

std:assert_eq (str v) (str $[20, 40, 60, 80]);
```

However, due to issues with coupling your functions to the usage
of accumulators this style is recommended:

```wlambda
!mul = { _ * 20 };

!v = $@vec iter i $i(1,5) ~ $+ mul[i];

std:assert_eq (str v) (str $[20, 40, 60, 80]);
```

#### <a name="531-transforming-a-vector"></a>5.3.1 - Transforming a vector

If you just want to do something with items in a vector and
construct a new one from the results:

```wlambda
!result = $@vec $[1,2,3,4] \$+ _ * 2;   # multiply each item by 2

std:assert_eq (str result)  "$[2,4,6,8]";
```

#### <a name="532-example-of-"></a>5.3.2 - Example of `$@@`

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

#### <a name="533-transforming-a-vector-to-a-map"></a>5.3.3 - Transforming a vector to a map

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

#### <a name="534-iteratively-concatenating-strings"></a>5.3.4 - Iteratively concatenating strings

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

#### <a name="535-accumulating-sums"></a>5.3.5 - Accumulating sums

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


### <a name="54-utilities"></a>5.4 - Utilities

#### <a name="541-stdaccum-collection-a-b-"></a>5.4.1 - std:accum _collection_ _a_ _b_ ...

This function accumulates all its arguments in the _collection_.
It does the same form of accumulation as `$+` does.

```wlambda
std:assert_eq (str ~ std:accum $[] 1 2 3)   "$[1,2,3]";
std:assert_eq (std:accum "" 1 2 3)          "123";
std:assert_eq (str ~ std:accum $b"" 1 2 3)  "\x01\x02\x03";
std:assert_eq (str ~ std:accum 10 1 2 3)    "16";
```

#### <a name="542-stdzip-vector-map-fn"></a>5.4.2 - std:zip _vector_ _map-fn_

Creates a generator that calls _map_fn_ with the consecutive elements of _vector_
as the last argument of _map-fn_. All arguments passed to std:zip
are appended to the argument list.

This is useful for combining the iteration over two vectors or collections.

```wlambda
!l = $@v $[13, 42, 97] ~ std:zip $["Foo", "Bar", "Baz"] { $+ @ };
std:assert_eq (str l) (str $[$[13, "Foo"], $[42, "Bar"], $[97, "Baz"]]);
```

#### <a name="543-stdfold-accumulator-func-iteratable"></a>5.4.3 - std:fold _accumulator_ _func_ _iteratable_

This function iterates over _iteratable_ while providing the current element
from _iteratable_ as first and the _accumulator_ variable to _func_ as second
argument.
The _accumulator_ for the next iteration is always the return value of the
previous execution of _func_.

This is a convenience function in cases where the accumulator syntax `$@`
does not fit the use-case.

Returns the most recently returned value from _func_.

Calculate the product of the first 5 integers.

```wlambda
!v = std:fold 1 {!(x, acc) = @;
    x * acc
} $[1,2,3,4,5];

std:assert_eq v 120;
```

Another contrived example:

```wlambda
!v = std:fold $[] {!(x, acc) = @;
    std:displayln @;
    ((std:cmp:str:asc "c" x) > 0) {
        std:push acc x;
    };
    acc
} "abcdef";

std:assert_eq (str v) (str $["d", "e", "f"]);
```

#### <a name="544-stdenumerate-map-fn"></a>5.4.4 - std:enumerate _map-fn_

Creates a generator that calls _map-fn_ with a counter that is incremented
after each call, starting with 0. The counter is appended to the
argument list after the regular arguments.

```wlambda
!l = $@v $["lo", "mid", "hi"] ~ std:enumerate { $+ $[_1, _] };
std:assert_eq (str l) (str $[$[0, "lo"], $[1, "mid"], $[2, "hi"]]);
```

## <a name="6-operators"></a>6 - Operators
### <a name="61-arithmetic"></a>6.1 - Arithmetic

The output type (float vs. integer) of the numerical arithmetic operators is defined
by the _first_ operand of the operation. Use the casting functions `float` or
`int` if you are unsure.

Please note that not all operators are available as plain identifiers and need
to be quoted when used in their prefix form or as functions, some of them are
`*`, `/`, `%` and some others.

#### <a name="611--operand-1-operand-2-"></a>6.1.1 - + _operand-1_ _operand-2_ ...

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

#### <a name="612---operand-1-operand-2-"></a>6.1.2 - - _operand-1_ _operand-2_ ...

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

#### <a name="613--op-a-op-b"></a>6.1.3 - * _op-a_ _op-b_

Returns the multiplication of the two operands.

```wlambda
std:assert 10   * 4 == 40;
std:assert 10.1 * 4 == 40.4;
std:assert "10" * 4 == 40;

std:assert (`*` 10 4) == 40;

std:assert (float "10.1") * 4 == 40.4;
```

#### <a name="614--op-a-op-b"></a>6.1.4 - / _op-a_ _op-b_

Returns the division of the two operands.

```wlambda
std:assert 10   / 4 == 2;
std:assert 10.0 / 4 == 2.5;
std:assert "10" / 2 == 5;

std:assert (`/` 10 4) == 2;

std:assert (float "10.1") * 4 == 40.4;
```

#### <a name="615--op-a-op-b"></a>6.1.5 - % _op-a_ _op-b_

Returns the remainder of the division of _op-a_ by _op-b_.

```wlambda
std:assert     5 % 4 == 1;
std:assert (`%` 5 4) == 1;
```

#### <a name="616--op-a-op-b"></a>6.1.6 - ^ _op-a_ _op-b_

Returns _op-a_ raised by the power of _op-b_.
Supports float and integers.

```wlambda
std:assert_eq 2 ^ 4     16;
std:assert_eq std:num:round[(2.0 ^ 2.1) * 1000] 4287.0;
std:assert_eq 2 ^ 2.1   4; # first arg type matters!
```

### <a name="62-comparison"></a>6.2 - Comparison

#### <a name="621--op-a-op-b"></a>6.2.1 - == _op-a_ _op-b_

Checks whether the two operands are equal to each other. Data types like
booleans, integers, floats, symbols and strings are compared by their contents.
Other types like vectors, maps, functions, errors or references are compared
by referential equality.

```wlambda
std:assert              $none == $none;
std:assert                  1 == 2 - 1;
std:assert               "aa" == ("a" "a");
std:assert               :xxy == :xxy;
std:assert       not ~ $[1,2] == $[1,2];
std:assert            $p(1,2) == $p(1,2);
std:assert            $i(1,2) == $i(1,2);
std:assert          $i(1,2,3) == $i(1,2,3);
std:assert    not ~ $i(1,2,3) == $f(1.0,2.0,3.0);
std:assert    $f(1.0,2.0,3.0) == $f(1.0,2.0,3.0);

std:assert ~ `==` 1 (2 - 1); # prefix form
```

#### <a name="622--op-a-op-b"></a>6.2.2 - != _op-a_ _op-b_

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

#### <a name="623--op-a-op-b"></a>6.2.3 - < _op-a_ _op-b_

Numerical comparison operator that checks whether _op-a_ is less than _op-b_

```wlambda
std:assert   10   < 11;
std:assert   10.1 < 10.2;
std:assert not[10 < 10.1];  # the type of the first argument decides return type!
```

#### <a name="624--op-a-op-b"></a>6.2.4 - <= _op-a_ _op-b_

Numerical comparison operator that checks whether _op-a_ is less or equal to _op-b_

```wlambda
std:assert 10   <= 11;
std:assert 10.1 <= 10.2;
std:assert 10   <= 10.1;  # integer <=, the type of the first argument decides return type!
```

#### <a name="625--op-a-op-b"></a>6.2.5 - > _op-a_ _op-b_

Numerical comparison operator that checks whether _op-a_ is greater than _op-b_

```wlambda
std:assert   11.1 > 11;
std:assert   11.1 > 11.0;
std:assert not[10 > 10.1];  # the type of the first argument decides return type!
```

#### <a name="626--op-a-op-b"></a>6.2.6 - >= _op-a_ _op-b_

Numerical comparison operator that checks whether _op-a_ is greater or equal to _op-b_

```wlambda
std:assert 11   >= 11;
std:assert 10.2 >= 10.1;
std:assert 10 >= 10.1;  # integer >=, the type of the first argument decides return type!
```

### <a name="63-bit-operations"></a>6.3 - Bit Operations

#### <a name="631--op-a-op-b"></a>6.3.1 - & _op-a_ _op-b_

Binary `and` operation between two integers.

```wlambda
std:assert (0b0011 & 0b1011) == 0b011;
std:assert (3      &     11) == 3;
```

#### <a name="632--op-a-op-b"></a>6.3.2 - &^ _op-a_ _op-b_

Binary `xor` operation between two integers.

```wlambda
std:assert (0b0011 &^ 0b1011) == 0b1000;
std:assert (3      &^     11) == 8;
```

#### <a name="633--op-a-op-b"></a>6.3.3 - &| _op-a_ _op-b_

Binary `or` operation between two integers.

```wlambda
std:assert (0b0011 &| 0b1000) == 0b1011;
std:assert (3      &|      8) == 11;
```

#### <a name="634--op-a-op-b"></a>6.3.4 - << _op-a_ _op-b_

Binary `left shift` operation of _op-a_ by _op-b_ bits.

```wlambda
std:assert (0b0011 << 3)   == 0b11000;
std:assert (`<<` 0b1011 2) == 0b101100
```

#### <a name="635--op-a-op-b"></a>6.3.5 - >> _op-a_ _op-b_

Binary `right shift` operation of _op-a_ by _op-b_ bits.

```wlambda
std:assert (0b0011 >> 2)      == 0b0;
std:assert (0b1100 >> 2)      == 0b11;
std:assert (`>>` 0b1011000 3) == 0b1011
```

## <a name="7-data-structure-matchers-selectors-and-string-patternsregex"></a>7 - Data Structure Matchers, Selectors and String Patterns/Regex

WLambda comes with a builtin DSL (domain specific language) for
shallow data structure matches and deep data structure selection and regular expression (regex) pattern
matching on strings. A _selector_ (structure selection) gives you the
ability to search deep into WLambda data structures like
[CSS Selectors](https://www.w3.org/TR/selectors-3/) into HTML DOM trees
or [XPath](https://www.w3.org/TR/xpath-31/) into XML.
While a _structure matcher_, as used by the `match` operation,
allows you to directly match a certain WLambda piece of data.

A subset of the _selectors_ are the _patterns_, which are able to
match strings like regular expressions. The syntax of _patterns_
is a bit different from normal regular expressions like Perl,
Python or JavaScript has. This is partly due to the fact that
these patterns aim to be easily used to match parts of a specific
string like filename globs `photo_???_*.jpg`.

For an in depth description of the _selector_ and _pattern_ syntax
please refer to the [Pattern and Selector Syntax](https://docs.rs/wlambda/newest/wlambda/selector/index.html)
in the wlambda::selector module.

### <a name="71-data-structure-matcher"></a>7.1 - Data Structure Matcher

This is probably one of the most convenient matching features of WLambda.
While selectors (`$S[a / * / b]`) allow searching deep into data structures,
the matches allow to efficient precise shallow selection and matching.
The `match` operation allows to match a value against multiple matchers,
while the `$M ...` syntax allows to define a matcher function for a single
match (commonly used in an if expression).

For a reference of the matcher syntax see below.

#### <a name="711-match-value-expr-match-pair1--default-expr"></a>7.1.1 - match _value-expr_ _match-pair1_ ... [_default-expr_]

The match operation is a very versatile control flow operation.


#### <a name="712-m-expr"></a>7.1.2 - $M _expr_

This is a structure matcher expression. It will compile _expr_ into a structure
matcher function. The reslting function will match it's first argument agianst
the match and return a map containing the capture variables (or just an empty map).

It will also bind the result map to `$\`. This makes it possible to easily match
a data structure in an if statement:

```wlambda
!some_struct = $[:TEST, ${ a = 10, b = 1442 }];

? some_struct &> ($M $[sym, ${ a = 10, b = x }]) {
    std:assert_eq $\.sym :TEST;
    std:assert_eq $\.x   1442;
} {
    panic "It should've matched!";
};
```

#### <a name="713-data-structure-matcher-syntax"></a>7.1.3 - Data Structure Matcher Syntax

This the the compiletime syntax that is understood by the
structure matchers that are used by `$M ...` and `match`.

- `$M`, `$M1`, `$M2`, ... in the following table stands for a structure matcher expression.
- All other tokens or values stand for themself.

| WLambda Value | Semantics |
|-|-|
| `x`                    | Matches any value and assigns it to the variable `x`. |
| `?`                    | Matches any value, but does not assign it. |
| `x $M $M1 ... $Mn`     | Assign the value that matched $M, $M1 or $Mn to the variable `x`. |
| `? $M $M1 ... $Mn`     | Matches if $M, $M1 or $Mn matches. |
| `_*`                   | Placeholder for 0 or N items that match any items in the vector. |
| `_+`                   | Placeholder for 1 or N items that match any items in the vector. |
| `_?`                   | Placeholder for 0 or 1 items that match any items in the vector. |
| `_* $M`                | Placeholder for 0 or N items that match $M in the vector. |
| `_+ $M`                | Placeholder for 1 or N items that match $M in the vector. |
| `_? $M`                | Placeholder for 0 or 1 items that match $M in the vector. |
| `_type? :integer ...`  | Matches an element of one of the given types.  Symbol names should have the same name as the type names returned by the `type` function. |
| `$M1 &or $M2`          | Matches if $M1 or $M2 matches. |
| `$M1 &and $M2`         | Matches if $M1 and $M2 matches. |
| `$[$M1, $M2, ...]`     | Matches a vector. |
| `${ $Mkey1 = $Mval1, ...}`| Matches a map. $Mkey1 can also be a $M match, but keep in mind that maps can only have symbols as keys. You can however match symbols using regex patterns for instance. If you only use symbols as keys in this match, the map access is optimized a bit, because there is no need to iterate over all keys then. |
| `$p($M1, $M2)`         | Matches a pair. |
| `$i($M1, ...)`         | Matches an integer vector. |
| `$f($M1, ...)`         | Matches a float vector. |
| `$o($M)`               | Matches an optional where the value matches $M. |
| `$e $M`                | Matches an error value that matches $M. |
| `$n`                   | Matches $none. |
| literal values         | Literal values like booleans, strings, symbols and numbers match their value. |

### <a name="72-data-structure-selectors-s"></a>7.2 - Data Structure Selectors `$S(...)`

This section shows how data structure selectors can be used.

TODO

#### <a name="721-selector-and-wlambda-regex-syntax"></a>7.2.1 - Selector and WLambda Regex Syntax:

```ebnf
    (* NOTE: Whitespace is not part of a pattern in most places. This means
             if you want to match whitespace, you will have to escape
             it either with a '\', with a [ ] character class or match
             one whitespace char with $s. *)

    class_char  = { ?any character except "]"? }
                  (* special sequence: "\^" => "^" and "\\" => "\"
                     and "\]" => "]" *)
                ;

    ident_char  = { ?any character except whitespace,
                    "!", "?", "/", "\", "|", "^", ",",
                    "'", "&", ":", ";", "$", "(", ")",
                    "{", "}", "[", "]", "*" or "="? }
                  (* allows the usual backslash escaping! *)
                ;

    ident       = ident_char, { ident_char }
                ;

    index       = digit, { digit }
                ;

    rx_atom     = pat_glob
                | ident_char
                ;

    glob_atom   = pat_glob
                | ident
                ;

    rx_match_mod = "L"             (* transforms the input string from the match
                                      position on to lower case. *)
                 | "U"             (* transforms the input string from the match
                                      position on to upper case. *)
                 ;

    pat_regex   = "*", rx_atom     (* matches sub pattern 0 or N times *)
                | "+", rx_atom     (* matches sub pattern 1 or N times *)
                | "<", [ ("*" | "+" | "?") ], rx_atom
                                   (* non greedy version of the above *)
                | "?", rx_atom     (* matches sub pattern 0 or 1 times *)
                | "!", rx_atom     (* matches (zero width) if next pattern does not match *)
                | "=", rx_atom     (* matches (zero width) if next pattern does match *)
                | "^"              (* matches (zero width) start of string *)
                | "$"              (* matches (zero width) end of string *)
                | "s"              (* matches one whitespace character *)
                | "S"              (* matches one non-whitespace character *)
                | "&", rx_match_mod
                ;

    glob_group  = "(", "^", pattern, ")"    (* capturing sub group *)
                | "(", pattern, ")"         (* sub group *)
                ;

    class_range = class_char, "-", class_char (* contains a range of chars, eg. [a-z] *)
                ;

    glob_cclass = "[",  { class_char | class_range }, "]" (* character class match for 1 char *)
                | "[^", { class_char | class_range }, "]" (* negated character class match for 1 char *)
                ;

    pat_glob    = "*"                       (* 0 or N any characters *)
                | "?"                       (* any character *)
                | "$", pat_regex
                | glob_cclass
                | glob_group
                ;

    pat_branch  = { glob_atom }
                ;

    pattern     = pat_branch, [ "|", pattern ]
                ;

    key         = index | pattern
                ;

    kv          = key, "=", pattern
                ;

    kv_item     = "{", kv, { ",", kv }, "}"
                ;

    node_match  = ":", ["!"], "(", selector, ")"
                | ":", ["!"], kv_item
                | ":", ["!"], "type", "=", pattern
                  (* pattern is matched against
                     vval type as returned by `type` *)
                | ":", ["!"], "str",  "=", pattern
                  (* pattern is matched against
                     the string contents or stringified
                     representation of the value *)
                ;

    node_cond   = node_match
                | node_match, "&", node_cond
                | node_match, "|", node_cond
                ;

    node        = key, { node_cond }
                  (* marks it for referencing it in the result set *)
                | "**", { node_cond }
                  (* deep expensive recursion *)
                | "^", node
                ;

    selector    = node, { "/", node }
                ;
```

### <a name="73-string-patterns-regex-r"></a>7.3 - String Patterns (Regex) `$r/.../`

This section shows how to use the builtin pattern regex engine
in WLambda. You can embed patterns directly in your WLambda source
with `$rQ...Q`. Where `Q` stands for the usual string quoting mechanism
in WLambda (like `$q/foo/`, `$q(foo bar)`, ...). This has the advantage
that the pattern syntax is checked on compile time of your WLambda program.

The result of the expression `$r/foo/` is a function, which takes as first
arguments a string and returns a vector of substrings of that input
string. First element of that vector is always the matched sub string
of the input string. All elements after that correspond to a pattern
capture `(^...)` like in `$r/foo(^bar)/`.
The function returns `$none` if the pattern could not be found in the input string.

Lets start off with a simple example:

```wlambda
# Please note: Whitespace inside the pattern is allowed and will not be matched!

!res = $r/a (^*) b/ "fooaxxxxbber";

std:assert_eq res.0 "axxxxbb";
std:assert_eq res.1 "xxxxb";
```

To match a whole string you can anchor using `$^` and `$$`:

```wlambda
!res = $r/$^ a (^*) b $$/ "axxxxbb";

std:assert_eq res.0 "axxxxbb";
std:assert_eq res.1 "xxxxb";
```

To match special the characters `$` you can use the backslash escaping `\$`:

```wlambda
std:assert_eq ($r/$+ \$/ "FF$$$FF").0   "$$$";
```

To access captured groups you can either use the return value of the
matcher function, or use the global variable `$\` which will contain
the results of the latest match that was exectuted:

```wlambda
# Notice the usage of the `<&` function call operator:
!res =
    ? "foo//\\/foo" &> $r| $<*? (^$+[\\/]) * | {
        std:assert_eq $\.0 "foo//\\/foo";

        $\.1
    };

std:assert_eq res "//\\/";
```

#### <a name="731-pattern-syntax-overview"></a>7.3.1 - Pattern Syntax Overview

While
[Selector and WLambda Regex Syntax](#721-selector-and-wlambda-regex-syntax)
describes the pattern syntax in detail,
here is the WLambda pattern regex syntax in a nutshell:

| Pattern Syntax | Semantics |
|-|-|
| `!?\|^,'&:;$(){}[]*=/\\` | Many special chars are reserved in WLambda patterns. Please escape then using `\\/` or `[/]`.|
| _whitespace_  | Please note, that whitespace to be matched must be escaped using '\' or inside a character calss `[ ]`. |
| `\.`          | Backslash escapes work the same as in regular WLambda strings. `\` escapes the following character to have no special syntactic meaning in a pattern except matching itself. While escape sequences like `\x41` match the character `A` or `\u{2211}` matches `∑`. These also work inside of character classes. |
| `*`           | Match 0 to N occurences of any character. |
| `?`           | Match 1 occurences of any character. |
| `(...)`       | A match group (does not capture). |
| `(^...)`      | A capturing match group. |
| `[abcA-Z]`    | A character class, matching the listed characters or ranges. |
| `[^abcA-Z]`   | A negative character class, matching all character except the listed characters or ranges. |
| `$^`          | String start anchor. Matches only the start of the string. Useful for specifying patterns that match the complete string (in combination with `$$`). |
| `$$`          | String end anchor. Matches only the end of the string. Useful for specifying patterns that must match the complete string. |
| `$*X`         | Greedly matches the pattern part `X` 0 or N times. For grouping pattern parts use `(...)` like in `$*(abc)`. |
| `$<*X`        | Non-greedly matches the pattern part `X` 0 or N times. |
| `$+X`         | Greedly matches the pattern part `X` 1 or N times. For grouping pattern parts use `(...)` like in `$+(abc)`. |
| `$<+X`        | Non-greedly matches the pattern part `X` 1 or N times. |
| `$?X`         | Greedly matches 0 or 1 occurences of the pattern part `X`. Like usual, you can group using `(...)`. |
| `$<?X`        | Non-greedly matches 0 or 1 occurences of the pattern part `X`. |
| `$!X`         | Zero-width negative look-ahead. `($^$!a*)` matches any string not starting with an `a`.  |
| `$=X`         | Zero-width positive look-ahead. `($^$=a*)` matches any string starting with an `a`. |
| `$s`          | Matches one (Unicode) whitespace character. |
| `$S`          | Matches one (Unicode) non-whitespace character. |
| `$&L`         | Transforms the input string for the following pattern matching parts to lowercase (attention: O(n) operation on the complete rest of the string!). Useful for matching case-insensitively. |
| `$&U`         | Transforms the input string for the following pattern matching parts to uppercase (attention: O(n) operation on the complete rest of the string!). Useful for matching case-insensitively. |

#### <a name="732-standard-regular-expressions"></a>7.3.2 - Standard Regular Expressions

Please note that WLambda can optionally be compiled with the `regex` crate,
which implements a more common syntax for regular expressions.
Please refer to the functions `std:re:match` in the WLambda standard library
for this.

#### <a name="733-stdpattern-string"></a>7.3.3 - std:pattern _string_

Compiles the regex pattern _string_ to a function just like `$r/.../` would do.
Useful for composing WLambda patterns at runtime:

```wlambda
!rx = std:pattern ~ std:str:cat "(^" "$+" "[a-z]" ")";

std:assert_eq (rx "foo").1 "foo";
```

Returns an error if the syntax failes to parse as pattern:

```wlambda
!err = unwrap_err ~ std:pattern "($+[a-z]";

std:assert_eq $i(0, 11)[err] "bad pattern";
```

## <a name="8-modules"></a>8 - Modules

### <a name="81-export"></a>8.1 - export

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

### <a name="82-import"></a>8.2 - import

```wlambda

!@import x = tests:test_mod; # prefixes everything from modixes with x:

std:assert ~ (x:symbol 10) == 40;

```

You can also skip the prefix:

```wlambda
!@import std;

!v = $[];

std:push v 10;
std:push v 20;

std:assert_eq (str v) "$[10,20]";
```

## <a name="9-core-library"></a>9 - Core Library

This library contains all the core functions which belong to the
core of the WLambda Programming Language. These functions can be seen
as keywords of WLambda. Some functions are also available as operators.

#### <a name="901-type-value"></a>9.0.1 - type _value_

Returns the name of the data type of _value_ as string.

```wlambda
std:assert_eq (type 10)         "integer";
std:assert_eq (type 10.0)       "float";
std:assert_eq (type {})         "function";
!y = $&&std:to_drop { };
std:assert_eq (type y)          "drop_function";
std:assert_eq (type :s)         "symbol";
std:assert_eq (type "s")        "string";
std:assert_eq (type $[])        "vector";
std:assert_eq (type ${})        "map";
std:assert_eq (type $b"")       "bytes";
std:assert_eq (type $n)         "none";
std:assert_eq (type $t)         "bool";
std:assert_eq (type $e $n)      "error";
std:assert_eq (type $&&10)      "ref_strong";
std:assert_eq (type $&10)       "ref_hidden";
!x = $&&10;
std:assert_eq (type ~ $w&x)     "ref_weak";
```

#### <a name="902-len-value"></a>9.0.2 - len _value_

Returns the length of _value_. Depending on the data type you will get
different semantics.

```wlambda
# Always zero for scalar non sequential/collection values:
std:assert_eq (len 10)              0;
std:assert_eq (len 10.1)            0;
std:assert_eq (len $t)              0;
std:assert_eq (len $f)              0;
std:assert_eq (len $n)              0;

std:assert_eq (len "\xFF")          2; # byte length of the UTF-8 string
std:assert_eq (len $b"\xFF")        1;
std:assert_eq (len $[1,2,3,4,5])    5;
std:assert_eq (len ${a=1, b=2})     2;
std:assert_eq (len ${a=1, b=2})     2;
```

#### <a name="903-panic-message"></a>9.0.3 - panic _message_

If your program runs into something that deserves a slap on the fingers
of the developer you can use `panic` to do that.

## <a name="10-standard-library"></a>10 - Standard Library

#### <a name="1001-stdshuffle-randfunc-vec"></a>10.0.1 - std:shuffle _rand\_func_ _vec_

Shuffles the _vec_ in place. The function _rand_func_ needs to return
a random 64 bit integer on each call. Here is an example:

```wlambda
!sm  = std:rand:split_mix64_new_from 1234;
!vec = $[1,2,3,4,5,6,7,8];
std:shuffle { std:rand:split_mix64_next sm } vec;

std:assert_eq (str vec) "$[2,1,7,4,8,5,3,6]";
```

#### <a name="1002-stddelete-vector-or-map-index-or-key"></a>10.0.2 - std:delete _vector-or-map_ _index-or-key_

This removes the designated element from the collection (either vector or map).
This works for:

- Vectors:
```wlambda
!v = $[1,2,3];

std:assert_eq (std:delete v 1) 2;
std:assert_eq (str v) (str $[1,3]);
```
- Maps:
```wlambda
!m = ${a = 10, b = 20};

std:assert_eq (std:delete m :a) 10;
std:assert_eq (str m) (str ${b = 20});
```

Please note that this operation is potentially O(n) on vectors.

#### <a name="1003-stdcopy-vecormap"></a>10.0.3 - std:copy _vec\_or\_map_

Makes a shallow copy of the given vector or map.

```wlambda
!a = $[1,2,3];
!b = std:copy a;
b.0 = 10;

std:assert_eq a.0 1;
std:assert_eq b.0 10;
```

#### <a name="1004-stdsort-comparefun-vec"></a>10.0.4 - std:sort [_compare\_fun_] _vec_

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

#### <a name="1005-stdcmpnumasc-a-b"></a>10.0.5 - std:cmp:num:asc _a_ _b_

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

#### <a name="1006-stdcmpnumdesc-a-b"></a>10.0.6 - std:cmp:num:desc _a_ _b_

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

#### <a name="1007-stddisplayln-arg1-"></a>10.0.7 - std:displayln _arg1_ ...

This function writes a humand readable version of all the arguments
(with a space inbetween) to the standard output. This means that:

```text
std:displayln "foo"
```

Will just print `foo` and a newline.

If you need a less ambigous form, use `std:writeln`, which
handles its argument like written via `std:ser:wlambda` instead of `str`.

#### <a name="1008-stdwriteln-arg1-"></a>10.0.8 - std:writeln _arg1_ ...

This function writes the WLambda representation of its arguments
(with a space inbetween) to standard output. This means that:

```text
std:displayln "foo"
```

Will print `"foo"` and a newline.

See also the description of `std:ser:wlambda`.

If you need a more human readable form use `std:displayln`.

#### <a name="1009-stdeval-code-string"></a>10.0.9 - std:eval _code-string_

Evaluates _code-string_ in the current global environment and returns
the generated value. If the code leads to any kind of evaluation error,
an error object is returned.

```wlambda
std:assert_eq (std:eval "1 + 2") 3;
!:global X = 20;
std:assert_eq (std:eval "1 + X") 21;
```

#### <a name="10010-stdassert-bool-message"></a>10.0.10 - std:assert _bool_ \[_message_]

Just a simple assertion function that panics if the first argument is not true.
Returns the passed value if it is a true value.
You can pass an optional message as second parameter.

```norun_wlambda
std:assert $false; #=> Panic
std:assert 120;    #=> 120
```

#### <a name="10011-stdasserteq-actual-expected-message"></a>10.0.11 - std:assert\_eq _actual_ _expected_ \[_message_]

This function checks if the _actual_ value is equal to the
_expected_ value and panics if not. The optional _message_ is
passed in the panic for reference.

```wlambda
!x = 30 * 2;
std:assert_eq x 60 "30 * 2 == 60";
```

#### <a name="10012-stdassertstreq-actual-expected"></a>10.0.12 - std:assert\_str\_eq _actual_ _expected_

This function stringifies _actual_ and _expected_ using the `str` function
and compares the resulting strings.

This is very useful to compare data structures, as map keys are sorted
if the maps are stringified using `str`:

```wlambda
std:assert_str_eq $[1, 2, 3]        $[1, 2, 3];
```

#### <a name="10013-stdassertreleq-l-r-epsilon-message"></a>10.0.13 - std:assert\_rel\_eq _l_ _r_ _epsilon_ \[_message_]

This function checks if `l` is within `epsilon` of `r`.
If the absolute value of the difference between `l` and `r` is greater than `epsilon`,
this function will panic, also displaying the optional message if present.

```wlambda
# these two are within 1 of each other
!x = 10.5;
!y = 11.3;
std:assert_rel_eq x y 1;

# but not within 0.5 of each other, so this line is commented out.
# std:assert_eq x y 0.5;
```

#### <a name="10014-stdwlambdaversion"></a>10.0.14 - std:wlambda:version

Returns the version number of the WLambda crate when called.

### <a name="101-io"></a>10.1 - I/O

#### <a name="1011-stdiofilereadtext-filename"></a>10.1.1 - std:io:file:read\_text _filename_

Opens the file _filename_ and returns its contents interpreted as UTF8
text as string.

```wlambda
std:io:file:write_safe "prelude_test.txt" "abcäöü";

!t = std:io:file:read_text "prelude_test.txt";
std:assert_eq t "abcäöü" "reading text from file works";
```

#### <a name="1012-stdiofileread-filename"></a>10.1.2 - std:io:file:read _filename_

Opens the file _filename_ and returns its contents as byte buffer.

```wlambda
std:io:file:write_safe "prelude_test.txt" "abcäöü";

!t = std:io:file:read "prelude_test.txt";
.t = std:str:from_utf8 t;
std:assert_eq t "abcäöü" "reading binary from file works";
```

#### <a name="1013-stdiofilewritesafe-filename-bytes-or-string"></a>10.1.3 - std:io:file:write\_safe _filename_ _bytes-or-string_

Creates a new file with the given filename but with a "~" appended
and writes the contents into it. After successful write, it renames
the file to the given filename.

#### <a name="1014-stdiofileappend-filename-bytes-or-string"></a>10.1.4 - std:io:file:append _filename_ _bytes-or-string_

Opens the given filename in append mode and appends _bytes-or-string_ to the
end of the file.

### <a name="102-threading"></a>10.2 - Threading

WLambda leverages the `std::thread` implementation of Rust's standard library
to provide safe threading. Threading works by spawning new threads that
get sent a piece of WLambda code (as string) and some arguments.

Most WLambda data can be shared between threads. An exception are
UserData values that are not thread safe. Also things like sharing
cyclic data structures are not possible, as the references are currently
broken up.

Sharing data is done by WLambda by transforming the _VVal_ data structures
into a thread safe shareable represenation called _AVal_. An AVal is a
deep copy of the original VVal and can additionally contain atoms (see `std:sync:atom:new`),
MPSC queues (see `std:sync:mpsc:new`) and value slots (see `std:sync:slot:new`).

## <a name="11-optional-standard-library"></a>11 - Optional Standard Library

### <a name="111-serialization"></a>11.1 - serialization

#### <a name="1111-stdserwlambda-arg"></a>11.1.1 - std:ser:wlambda _arg_

Returns the serialized WLambda representation of the value _arg_ as string.

Most values have the same represenation like a WLambda literal,
but there are other values that don't have a literal representation.

Warning: Consider all values that don't have a fixed literal representation
in the WLambda syntax as debug output that might change in future versions.

```wlambda
std:assert_eq (std:ser:wlambda "foo") $q|"foo"|;
std:assert_eq (std:ser:wlambda $none) $q|$n|;
std:assert_eq (std:ser:wlambda $[1,:a]) $q|$[1,:a]|;
```

#### <a name="1112-stdserjson-data-nopretty"></a>11.1.2 - std:ser:json _data_ \[_no\_pretty_]

Serializes the _data_ and returns a JSON formatted (and pretty printed) string.
Optionally not pretty printed if _no_pretty_ is a true value.

```wlambda
!str = std:ser:json $[1,2.3,${a=4}] $t;
std:assert_eq str "[1,2.3,{\"a\":4}]";
```

#### <a name="1113-stddeserjson-string"></a>11.1.3 - std:deser:json _string_

Deserializes the JSON formatted _string_ into a data structure.

```wlambda
!data = std:deser:json ~ std:ser:json $[1,2.3,${a=4}];
std:assert_eq data.0 1;
std:assert_eq data.1 2.3;
std:assert_eq data.(2).a 4;
```

#### <a name="1114-stdsercsv-fielddelim-rowseparator-escapeall-table"></a>11.1.4 - std:ser:csv _field\_delim_ _row\_separator_ _escape\_all_ _table_

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

#### <a name="1115-stddesercsv-fielddelim-rowseparator-data"></a>11.1.5 - std:deser:csv _field\_delim_ _row\_separator_ _data_

Parses the string _data_ as CSV. With the field delimiter _field_delim_
and the _row_separator_ for the data rows.

```wlambda
!table = std:deser:csv ";" "\r\n" "foo;bar\r\nx;y\r\n";
std:assert_eq table.0.0 "foo";
std:assert_eq table.0.1 "bar";
std:assert_eq table.1.1 "y";
```

#### <a name="1116-stdsermsgpack-data"></a>11.1.6 - std:ser:msgpack _data_

Serializes the _data_ and returns a msgpack bytes value.

```wlambda
std:assert_eq (std:ser:msgpack $b"abc") $b"\xC4\x03abc";
```

#### <a name="1117-stddesermsgpack-bytes"></a>11.1.7 - std:deser:msgpack _bytes_

Deserializes the msgpack bytes value into a data structure.

```wlambda
std:assert_eq (std:deser:msgpack $b"\xC4\x03abc") $b"abc";
```

### <a name="112-regex"></a>11.2 - regex


### <a name="113-chrono"></a>11.3 - chrono

#### <a name="1131-stdchronotimestamp-format"></a>11.3.1 - std:chrono:timestamp \[_format_]

For the documentation of _format_ please consule the
chrono Rust crate documentation: [chrono crate strftime format](https://docs.rs/chrono/latest/chrono/format/strftime/index.html#specifiers).

```wlambda
!year_str = std:chrono:timestamp "%Y";
std:displayln :XXXX ~ (year_str | int) == 2020;
std:assert ~ (year_str | int) == 2020;

!now_str = std:chrono:timestamp[];
```

### <a name="114-hash"></a>11.4 - hash

#### <a name="1141-stdhashfnv1a-arg1-"></a>11.4.1 - std:hash:fnv1a _arg1_ ...

Hashes all the arguments as FNV1a and returns an integer.

### <a name="115-rand"></a>11.5 - rand

#### <a name="1151-stdrandsplitmix64new"></a>11.5.1 - std:rand:split\_mix64\_new

Initializes the _sm_state_ from the current time (seconds) and returns it.
The time is retrieved in seconds, so don't expect different seed states
if you call this multiple times in the same wall clock second.
The returned value is supposed to be passed to `rand:split_mix64_next`
or `rand:split_mix64_next_open01`.

#### <a name="1152-stdrandsplitmix64newfrom-seed"></a>11.5.2 - std:rand:split\_mix64\_new\_from _seed_

Initializes the _sm_state_ from the given _seed_ and returns it.
The returned value is supposed to be passed to `rand:split_mix64_next`
or `rand:split_mix64_next_open01`.

#### <a name="1153-stdrandsplitmix64next-smstate-count"></a>11.5.3 - std:rand:split\_mix64\_next _sm\_state_ \[_count_]

Returns the _count_ next integer values generated from the given
_sm_state_.

#### <a name="1154-stdrandsplitmix64nextopen01-smstate-count"></a>11.5.4 - std:rand:split\_mix64\_next\_open01 _sm\_state_ \[_count_]

Returns the _count_ next float values (in an open [0, 1) interval)
generated from the given _sm_state_.

### <a name="116-utility-functions"></a>11.6 - Utility Functions

#### <a name="1161-stddumpupvals-function"></a>11.6.1 - std:dump\_upvals _function_

Returns a vector of all the upvalues of the _function_.
Please use this function for debugging purposes only, as the order of the
variables, while consistent for a specific WLambda version,
is not defined at this point.

```wlambda
!x = 1;
!y = 2;
!fun = { _ + x + y };

std:assert_eq fun[3]   6;
.x = 3;
std:assert_eq fun[3]   8;

!upvs = std:dump_upvals fun;
std:assert_eq (str upvs) "$[$&3,$&2]";
.y = 4;
std:assert_eq (str upvs) "$[$&3,$&4]";

std:assert_eq $*(upvs.0) 3;
std:assert_eq $*(upvs.1) 4;
```

## <a name="12-wlambda-lexical-syntax-and-grammar"></a>12 - WLambda Lexical Syntax and Grammar

White space is everything that satisfies `std::char::is_whitespace`,
so unicode white space is respected. Comments have the following syntax:

```ebnf
    comment = "#" ?anything except "\n"? "\n"
```

In the following grammar, white space and comments are omitted:

```ebnf

    ident_start   = ( ?alphabetic? | "_" | "@" | "?" )
    ident_end     = { ?any character?
                     - ( ?white space?
                         | "." | "," | ";"
                         | "{" | "}" | "[" | "]" | "(" | ")"
                         | "~" | "|" | "=" ) }
                  ;
    qident        = ident_end
                  (* a quoted identifier can not appear anywhere,
                     it's usually delimited or follows something that
                     makes sure we are now expecting an identifier *)
                  | "`", { ?any character except '`'? }, "`" (* quoted identifier *)
                  ;
    ident         = ident_start, [ ident_end ]
                  | "`", { ?any character except '`'? }, "`" (* quoted identifier *)
                  ;
    ref_specifier = ":", qident
                  ;

    digit         = "0" | "1" | "2" | "3" | "4" | "5"
                  | "6" | "7" | "8" | "9"
                  ;
    integer       = digit, { digit }
                  ;
    radix         = integer
                  ;
    radix_digits  = (* digits in the radix specified
                       earlier in the number.
                       Default radix is of course 10. *)
    number        = [ "-" | "+" ],
                    [ ( radix, "r"
                      | "0", "x"
                      | "0", "b"
                      | "0", "o"
                      ) ],
                    radix_digits,
                    [ ".", radix_digits ]
                  ;
    hexdigit      = ?hexdigit, upper or lower case?
                  ;
    string_escape = "x", hexdigit, hexdigit  (* byte/ascii escape *)
                  | "n"                      (* newline *)
                  | "r"                      (* carriage return *)
                  | "t"                      (* horizontal tab *)
                  | "0"                      (* nul byte/char *)
                  | "u", hexdigit, { hexdigit }
                                             (* unicode char, or in byte strings
                                                their utf-8 encoded form *)
                  | "\""
                  | "\'"
                  | "\\"
                  ;
    string        = "\"", { "\\", string_escape | ?any character? - "\\" },"\""
                  ;
    byte_string   = "b", string
                  ;
    quote_string  = "q", ?any character as quote?,
                         { ?any character? },
                         ?any character as quote?
                  | "Q", ?any character as quote?,
                         { ?any character? },
                         ?any character as quote?
                    (* but Q generates a byte string instead! *)
    selector      = "S", ?any character as quote?,
                         selector_rs_syntax,
                         ?any character as quote?
                    (* parses substring like 'q', but constructs a
                       selector_rs_syntax matcher at compile time *)
                  ;
    pattern       = "r", ?any character as quote?,
                         selector_rs_pattern_syntax,
                         ?any character as quote?
                    (* parses substring like 'q', but constructs a
                       pattern matcher at compile time *)
                  ;
    struct_match  = "M", expr   (* compiles expr as structure matcher function.
                                   If called, it matches the first argument against
                                   the literal structure and returns a map of
                                   matched variables. If nothing matches $none
                                   is returned. *)
    list_expr     = "*", expr   (* splices the vector result of 'expr'
                                   into the currently parsed list *)
                  | expr
                  ;
    list          = "[", [ list_expr, { ",", list_expr }, [ "," ] ],"]"
                  ;
    map_expr      = (ident | expr), "=", expr
                  | "*", expr   (* splices the map result of 'expr'
                                   into the currently parsed map *)
                  ;
    map           = "{", [ map_expr, { ",", map_expr }, [ "," ] ], "}"
                  ;
    self          = "s" | "self"
                  ;
    true          = "t" | "true"
                  ;
    false         = "f" | "false"
                  ;
    none          = "n" | "none"
                  ;
    code_string   = ("c" | "code" ), expr
                  ;
    pair          = "p", "(", expr, "," expr, ")"
                  ;
    err           = ("e" | "error"), expr
                  ;
    nvec          = ("i" | "f"), "(", expr, { ",", expr }, ")"
                  ;
    ref           = "&&", value
                  ;
    ref_hidden    = "&", value
                  ;
    ref_weak      = ("w&" | "weak&"), value
                  ;
    accumulator   = "@", ("i" | "int"
                         |"s" | "string"
                         |"f" | "float"
                         |"b" | "bytes"
                         |"v" | "vec"
                         |"m" | "map" ), expr
                    (* defines a new accumulator context *)
                  | "@@" (* returns the current accumulator value *)
                  | "+"  (* resolves to the current accumulator function *)
                  ;
    import        = "@import", ident, [ ident ]
                  ;
    export        = "@export", ident, [ "=" ], expr
                  ;
    capture_ref   = ":", var
                  ;
    deref         = "*", value
                  ;
    special_value = byte_string
                  | quote_string
                  | code_string
                  | list
                  | map
                  | none
                  | true
                  | false
                  | self
                  | err
                  | nvec
                  | pair
                  | ref
                  | ref_hidden
                  | ref_weak
                  | deref
                  | capture_ref
                  | accumulator
                  | selector
                  | pattern
                  | struct_match
                  | "\"             (* The global variable with the name "\" *)
                  ;
    arity_def     = "|", number, "<", number, "|" (* set min/max *)
                  | "|", number, "|"              (* set min and max *)
                  | "|", "|"                      (* no enforcement *)
                  ;
    function      = [ "\:", ident ], "{", [ arity_def ], block, "}"
                  | "\", [ arity_def ], statement
                  ;
    var           = ident
                  ;
    symbol        = ":", qident
                  | ":", "\"", (? any char, quoted \\ and \" ?), "\""
                  (*
                     symbols are usually used to specify
                     fields in literal map definitions
                     and lots of other places as stringy sentinel values
                  *)
                  ;
    value         = number
                  | string
                  | "$", special_value
                  | "(", expr, ")"
                  | function
                  | symbol
                  | var
                  ;
    op            = (* here all operators are listed line by line regarding
                       their precedence, top to bottom *)
                    "&>"              (* call rhs with lhs operator *)
                  | "<&"              (* call lhs with rhs operator *)
                  | "^"
                  | "*" | "/" | "%"
                  | "-" | "+"
                  | "<<" | ">>"       (* binary shift *)
                  | "<" | ">" | "<=" | ">="
                  | "==" | "!="
                  | "&"               (* binary and *)
                  | "&^"              (* binary xor *)
                  | "&|"              (* binary or *)
                  | "&and"            (* logical and, short circuit *)
                  | "&or"             (* logical or, short circuit *)
                  | "=>"              (* pair constructor *)
                  ;
    bin_op        = call_no_ops, { op, bin_op } (* precedence parsing is done
                                                   in a Pratt parser style *)
                  ;
    arg_list      = "[", [ expr, { ",", expr }, [ "," ] ], "]"
                  | "[[", expr, "]]"  (* apply result vector of expr as argument list *)
                  ;
    field         = ".", ( integer | ident | value ), [ field ]
                  ;
    field_access  = field, "=", expr
                  | field, arg_list
                  | field
                  (* please note, that a field access like:
                     `obj.field` is equivalent to the call:
                     `field[obj]`. That also means that
                     `obj.field[...]` is transformed into
                     `field[obj][...]`.
                     The exception is "=" which assigns
                     the field as specified.
                     BUT: There is a special case, when you specify
                     an `indent` it is quoted and interpreted as symbol. *)
                  ;
    call_no_ops   = value, { arg_list | field_access }
                  ;
    call          = value,
                    { arg_list | field_access | bin_op | value },
                    [ "~", expr ] (* this is a tail argument, if present the
                                     expr is appended to the argument list *)
                  ;
    expr          = call, { "|", call }
                  | call, { "|>", call }
                  | call, { "||", call }
                  ;
    simple_assign = qident, "=", expr
                  ;
    destr_assign  = "(", [ qident, { ",", qident } ], ")", "=" expr
                  ;
    definition    = [ ref_specifier ], ( simple_assign | destr_assign )
                  ;
    import        = "!", "@import", symbol, [ [ "=" ], symbol ]
                  | "!", "@wlambda"
                  ;
    export        = "!", "@export", symbol, [ "=" ], expr
                  ;
    statement     = "!" definition
                  | "." simple_assign
                  | "." destr_assign
                  | import
                  | export
                  | expr
                  ;
    block         = "{", { statement, ";", {";"}}, [ statement, {";"} ], "}"
                  | { statement, ";", {";"} }, [ statement, {";"} ]
                  ;
    code          = block
                  ;
```

### <a name="121-special-forms"></a>12.1 - Special Forms

There are certain calls that are handled by the compiler differently.

- `? _condition_ _then-block-or-expr_ [_else-block-or-expr_]`
- `while _condition_ _block-or-expr_`
- `iter _var_ _value-expr_ _block-or-expr_`
- `next _x_`
- `break`
- `match _value-expr_ $p(structure_pattern, branch_block) ... [ branch_block ]
- `jump _idx-expr_ _block1_ ...`



[]: ---- REFERENCE DOC END ----

*/

const VERSION: &str = env!("CARGO_PKG_VERSION");

use crate::compiler::*;
use crate::vval::*;
use crate::nvec::*;
use crate::util;
use std::rc::Rc;
use crate::threads::*;
use crate::selector::*;

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
            if argc <= 0 { return Ok(VVal::None); }
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
            if argc < 2 { return Ok(VVal::None); }
            let a = env.arg(0);
            if let VVal::Flt(af) = a { Ok(VVal::Bol(af $op env.arg(1).f())) }
            else { Ok(VVal::Bol(a.i() $op env.arg(1).i())) }
        }, Some(2), Some(2), false)
    }
}

macro_rules! add_fi_bin_op {
    ($g: ident, $op: tt, $a: ident, $b: ident, $ef: expr, $ei: expr) => {
        add_func!($g, $op, env, argc, {
            if argc < 2 { return Ok(VVal::None); }
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
            if argc < 2 { return Ok(VVal::None); }
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
                if argc < 2 { return Ok(VVal::None); }
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

macro_rules! sizeof_writeln {
    ($write: ident, $type: ty) => {
        writeln!($write, "sizeof {:40}:  {:2} bytes",
                 stringify!($type),
                 std::mem::size_of::<$type>()).expect("stdout access works");
    }
}

/// Returns a SymbolTable with all WLambda core language symbols.
#[allow(clippy::cast_lossless,clippy::assign_op_pattern)]
pub fn core_symbol_table() -> SymbolTable {
    let mut st = SymbolTable::new();

    // The implementations for +/- are essentially just like the `add_multi_op`
    // implementations, except for how they accept down to 1 parameter for
    // unary +/-.
    add_func!(st, +, env, argc, {
        Ok(match (argc, env.arg(0)) {
            (0, _) => VVal::None,
            (1, VVal::Flt(f)) => VVal::Flt(f),
            (1, VVal::Int(i)) => VVal::Int(i),
            (1, VVal::FVec(fv)) => VVal::FVec(fv),
            (1, VVal::IVec(iv)) => VVal::IVec(iv),
            (a, VVal::Flt(f)) => {
                let mut accum = f;
                for i in 1..a { accum = accum + env.arg(i).f() }
                VVal::Flt(accum)
            }
            (a, v) => {
                let mut accum = v.i();
                for i in 1..a { accum = accum + env.arg(i).i() }
                VVal::Int(accum)
            }
        })
    }, Some(1), None, false);
    add_func!(st, -, env, argc, {
        Ok(match (argc, env.arg(0)) {
            (0, _) => VVal::None,
            (1, VVal::Int(i))   => VVal::Int(-i),
            (1, VVal::Flt(f))   => VVal::Flt(-f),
            (1, VVal::FVec(fv)) => VVal::FVec(Box::new(-*fv)),
            (1, VVal::IVec(iv)) => VVal::IVec(Box::new(-*iv)),
            (a, VVal::Flt(f))   => {
                let mut accum = f;
                for i in 1..a { accum = accum - env.arg(i).f() }
                VVal::Flt(accum)
            }
            (a, v) => {
                let mut accum = v.i();
                for i in 1..a { accum = accum - env.arg(i).i() }
                VVal::Int(accum)
            }
        })
    }, Some(1), None, false);

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
            let mut label = VVal::None;
            let fn_arg_idx = if argc <= 1 { 0 } else { label = env.arg(0); 1 };
            match env.arg(fn_arg_idx).call_no_args(env) {
                Ok(v)   => Ok(v),
                Err(StackAction::Return(ret)) => {
                    if ret.0.eqv(&label) { Ok(ret.1) }
                    else { Err(StackAction::Return(ret)) }
                },
                Err(e)  => Err(e),
            }
        }, Some(1), Some(2), false);

    func!(st, "_?",
        |env: &mut Env, argc: usize| {
            let mut lbl = VVal::None;
            let err_val = if argc > 1 {
                lbl = env.arg(0);
                env.arg(1)
            } else { env.arg(0) };

            match err_val {
                VVal::Err(e) => Err(StackAction::Return(Box::new((lbl, VVal::Err(e))))),
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
                VVal::Opt(None) => {
                    Err(StackAction::panic_str(
                        "unwrap empty option!".to_string(), None))
                },
                VVal::Opt(Some(v)) => Ok((*v).clone()),
                v => Ok(v)
            }
        }, Some(1), Some(1), true);

    func!(st, "on_error",
        |env: &mut Env, _argc: usize| {
            let err_fn = env.arg(0);
            match env.arg(1) {
                VVal::Err(err_v) => {
                    env.with_restore_sp(|e: &mut Env| {
                        e.push(err_v.borrow().0.clone());
                        e.push(VVal::Int(err_v.borrow().1.line as i64));
                        e.push(VVal::Int(err_v.borrow().1.col as i64));
                        e.push(VVal::new_str(err_v.borrow().1.file.s()));
                        err_fn.call_internal(e, 4)
                    })
                },
                e => Ok(e)
            }
        }, Some(2), Some(2), true);

    func!(st, "return",
        |env: &mut Env, argc: usize| {
            if argc < 1 { return Err(StackAction::Return(Box::new((VVal::None, VVal::None)))); }
            if argc < 2 { return Err(StackAction::Return(Box::new((VVal::None, env.arg(0))))); }
            Err(StackAction::Return(Box::new((env.arg(0), env.arg(1)))))
        }, Some(1), Some(2), true);

    func!(st, "break",
        |env: &mut Env, argc: usize| {
            if argc < 1 { return Err(StackAction::Break(Box::new(VVal::None))); }
            Err(StackAction::Break(Box::new(env.arg(0))))
        }, Some(0), Some(1), true);

    func!(st, "next",
        |_env: &mut Env, _argc: usize| {
            Err(StackAction::Next)
        }, Some(0), Some(0), false);

    func!(st, "pick",
        |env: &mut Env, _argc: usize| Ok(if env.arg(0).b() { env.arg(1) } else { env.arg(2) }),
        Some(3), Some(3), false);

    func!(st, "ivec",
        |env: &mut Env, _argc: usize| Ok(VVal::IVec(Box::new(env.arg(0).nvec()))),
        Some(1), Some(1), false);
    func!(st, "ivec2",
        |env: &mut Env, _argc: usize| Ok(VVal::IVec(Box::new(env.arg(0).nvec().vec2()))),
        Some(1), Some(1), false);
    func!(st, "ivec3",
        |env: &mut Env, _argc: usize| Ok(VVal::IVec(Box::new(env.arg(0).nvec().vec3()))),
        Some(1), Some(1), false);
    func!(st, "ivec4",
        |env: &mut Env, _argc: usize| Ok(VVal::IVec(Box::new(env.arg(0).nvec().vec4()))),
        Some(1), Some(1), false);
    func!(st, "fvec",
        |env: &mut Env, _argc: usize| Ok(VVal::FVec(Box::new(env.arg(0).nvec()))),
        Some(1), Some(1), false);
    func!(st, "fvec2",
        |env: &mut Env, _argc: usize| Ok(VVal::FVec(Box::new(env.arg(0).nvec().vec2()))),
        Some(1), Some(1), false);
    func!(st, "fvec3",
        |env: &mut Env, _argc: usize| Ok(VVal::FVec(Box::new(env.arg(0).nvec().vec3()))),
        Some(1), Some(1), false);
    func!(st, "fvec4",
        |env: &mut Env, _argc: usize| Ok(VVal::FVec(Box::new(env.arg(0).nvec().vec4()))),
        Some(1), Some(1), false);
    func!(st, "is_nvec",
        |env: &mut Env, _argc: usize| Ok(VVal::Bol(env.arg(0).is_nvec())),
        Some(1), Some(1), false);
    func!(st, "is_ivec",
        |env: &mut Env, _argc: usize| Ok(VVal::Bol(env.arg(0).is_ivec())),
        Some(1), Some(1), false);
    func!(st, "is_fvec",
        |env: &mut Env, _argc: usize| Ok(VVal::Bol(env.arg(0).is_fvec())),
        Some(1), Some(1), false);
    func!(st, "nvec_len",
        |env: &mut Env, _argc: usize| Ok(VVal::Int(env.arg(0).nvec_len() as i64)),
        Some(1), Some(1), false);

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
        |env: &mut Env, _argc: usize|
            env.arg(0).with_s_ref(|s: &str| { Ok(VVal::new_sym(s)) }),
        Some(1), Some(1), false);
    func!(st, "is_some",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_some())) },
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
    func!(st, "is_pair",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_pair())) },
        Some(1), Some(1), true);
    func!(st, "is_optional",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_optional())) },
        Some(1), Some(1), true);
    func!(st, "is_iter",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_iter())) },
        Some(1), Some(1), true);
    func!(st, "is_int",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_int())) },
        Some(1), Some(1), true);

    func!(st, "len",
        |env: &mut Env, _argc: usize| { Ok(VVal::Int(env.arg(0).len() as i64)) },
        Some(1), Some(1), false);

    func!(st, "type",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::new_str(env.arg(0).type_name()))
        }, Some(1), Some(1), true);

    func!(st, "cons",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::pair(env.arg(0), env.arg(1)))
        }, Some(2), Some(2), true);

    func!(st, "for",
        |env: &mut Env, _argc: usize| {
            let val = env.arg(0);
            let f   = env.arg(1);

            let mut ret = VVal::None;
            for (v, k) in val.iter() {
                env.push(v);
                let n =
                    if let Some(k) = k { env.push(k); 2 }
                    else               { 1 };
                match f.call_internal(env, n) {
                    Ok(v)                      => { ret = v; },
                    Err(StackAction::Break(v)) => { env.popn(n); return Ok(*v); },
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

                let mut ret = VVal::None;
                #[allow(unused_must_use)]
                while from <= to {
                    ret = VVal::None;
                    env.push(VVal::Flt(from));
                    match f.call_internal(env, 1) {
                        Ok(v)                      => { ret = v; },
                        Err(StackAction::Break(v)) => { env.popn(1); return Ok(*v); },
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

                let mut ret = VVal::None;
                #[allow(unused_must_use)]
                while from <= to {
                    ret = VVal::None;
                    env.push(VVal::Int(from));
                    match f.call_internal(env, 1) {
                        Ok(v)                      => { ret = v; },
                        Err(StackAction::Break(v)) => { env.popn(1); return Ok(*v); },
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
        if raw {
            env.arg_ref(i).unwrap().with_s_ref(|s: &str| {
                if i == (argc - 1) {
                    if i > 0 { write!(write, " ").ok(); }
                    writeln!(write, "{}", s).ok();
                } else {
                    if i > 0 { write!(write, " ").ok(); }
                    write!(write, "{}", s).ok();
                }
            });
        } else {
            let s = env.arg_ref(i).unwrap().s();

            if i == (argc - 1) {
                if i > 0 { write!(write, " ").ok(); }
                writeln!(write, "{}", s).ok();
            } else {
                if i > 0 { write!(write, " ").ok(); }
                write!(write, "{}", s).ok();
            }
        }
    }
    if argc == 0 {
        writeln!(write).ok();
    }
    if argc > 0 {
        Ok(env.arg(argc - 1))
    } else {
        Ok(VVal::None)
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

    func!(st, "keys",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            Ok(v.keys())
        }, Some(1), Some(1), false);

    func!(st, "values",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            Ok(v.values())
        }, Some(1), Some(1), false);

    func!(st, "accum",
        |env: &mut Env, argc: usize| {
            let mut v = env.arg(0);
            for i in 1..argc {
                v.accum(&env.arg(i));
            }
            Ok(v)
        }, Some(2), None, false);

    func!(st, "delete",
        |env: &mut Env, _argc: usize| {
            let v   = env.arg(0);
            let key = env.arg(1);
            v.delete_key(&key)
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

    func!(st, "ser:wlambda",
        |env: &mut Env, _argc: usize| { Ok(VVal::new_str_mv(env.arg(0).s())) },
        Some(1), Some(1), true);
    func!(st, "str:len",
        |env: &mut Env, _argc: usize| { Ok(VVal::Int(env.arg(0).s_len() as i64)) },
        Some(1), Some(1), false);
    func!(st, "str:to_lowercase",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::new_str_mv(
                env.arg_ref(0).unwrap().with_s_ref(|s: &str| s.to_lowercase()))) },
        Some(1), Some(1), false);
    func!(st, "str:to_uppercase",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::new_str_mv(
                env.arg_ref(0).unwrap().with_s_ref(|s: &str| s.to_uppercase()))) },
        Some(1), Some(1), false);
    func!(st, "str:trim",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::new_str_mv(
                env.arg_ref(0).unwrap().with_s_ref(|s: &str| s.trim().to_string()))) },
        Some(1), Some(1), false);
    func!(st, "str:trim_start",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::new_str_mv(
                env.arg_ref(0).unwrap().with_s_ref(|s: &str| s.trim_start().to_string()))) },
        Some(1), Some(1), false);
    func!(st, "str:trim_end",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::new_str_mv(
                env.arg_ref(0).unwrap().with_s_ref(|s: &str| s.trim_end().to_string()))) },
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
            let mut s = String::from("");
            for i in 0..argc {
                let aref = env.arg_ref(i).unwrap();
                if let VVal::Lst(l) = aref {
                    for v in l.borrow().iter() {
                        v.with_s_ref(|vs: &str| s.push_str(vs));
                    }
                } else {
                    env.arg_ref(i).unwrap()
                       .with_s_ref(|vs: &str| s.push_str(vs))
                }
            }
            Ok(VVal::new_str_mv(s))
        }, None, None, false);
    func!(st, "str:replace_n",
        |env: &mut Env, _argc: usize| {
            let cnt  = env.arg(2).i() as usize;
            env.arg_ref(0).unwrap().with_s_ref(|pat: &str|
                env.arg_ref(1).unwrap().with_s_ref(|to: &str|
                    env.arg_ref(3).unwrap().with_s_ref(|data: &str|
                        Ok(VVal::new_str_mv(data.replacen(pat, to, cnt))))))
        }, Some(4), Some(4), false);
    func!(st, "str:replace",
        |env: &mut Env, _argc: usize| {
            env.arg_ref(0).unwrap().with_s_ref(|pat: &str|
                env.arg_ref(1).unwrap().with_s_ref(|to: &str|
                    env.arg_ref(2).unwrap().with_s_ref(|data: &str|
                        Ok(VVal::new_str_mv(data.replace(pat, to))))))
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
                    VVal::new_str_mv(String::from_utf8_lossy(u.as_ref()).to_string())
                } else {
                    VVal::None
                })
        }, Some(1), Some(1), false);
    func!(st, "str:from_utf8",
        |env: &mut Env, _argc: usize| {
            let b = env.arg(0);
            if let VVal::Byt(u) = b {
                match String::from_utf8(u.to_vec()) {
                    Ok(s) => Ok(VVal::new_str_mv(s)),
                    Err(e) => {
                        Ok(env.new_err(
                            format!("str:from_utf8 decoding error: {}", e)))
                    }
                }
            } else {
                Ok(VVal::None)
            }
        }, Some(1), Some(1), false);

    func!(st, "str:to_char_vec",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::vec_mv(
                env.arg_ref(0).unwrap().with_s_ref(|arg: &str|
                    arg.chars()
                    .map(|c| VVal::Int(i64::from(c as u32)))
                    .collect())))
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
                Ok(VVal::None)
            }
        }, Some(1), Some(1), false);

    func!(st, "bytes:to_vec",
        |env: &mut Env, _argc: usize| {
            if let VVal::Byt(u) = env.arg(0) {
                Ok(VVal::vec_mv(
                    u.iter()
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
            env.arg_ref(0).unwrap().with_s_ref(|s: &str| {
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
            })
        }, Some(1), Some(1), false);

    func!(st, "bytes:to_hex",
        |env: &mut Env, argc: usize| {
            static HEXCHARS : &[char] =
                &['0', '1', '2', '3', '4', '5', '6', '7',
                  '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'];

            if let VVal::Byt(u) = env.arg(0) {
                let mut out : String =
                    String::with_capacity(u.len() * 2);

                if argc == 1 {
                    for (a, b) in u.iter().map(|u|
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
                    for (a, b) in u.iter().map(|u|
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
                Ok(VVal::None)
            }
        }, Some(1), Some(3), false);

    func!(st, "to_no_arity",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            Ok(v.disable_function_arity())
        }, Some(1), Some(1), false);

    func!(st, "to_drop",
        |env: &mut Env, _argc: usize| {
            let fun = env.arg(0).disable_function_arity();

            Ok(VVal::DropFun(Rc::new(DropFun { fun })))
        }, Some(1), Some(1), false);

    func!(st, "fold",
        |env: &mut Env, _argc: usize| {
            let mut acc = env.arg(0);
            let f       = env.arg(1);
            let lst     = env.arg(2);

            for (i, _) in lst.iter() {
                env.push(i);
                env.push(acc.clone());
                let rv = f.call_internal(env, 2);
                env.popn(2);

                match rv {
                    Ok(v)                      => { acc = v;  },
                    Err(StackAction::Break(v)) => { acc = *v; break; },
                    Err(StackAction::Next)     => { },
                    Err(e)                     => { return Err(e); },
                }
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
                    env.push(o.at(*i.borrow()).unwrap_or(VVal::None));
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
    add_num_fun_flt!(st, "num:signum",     signum);
    add_num_fun_flt!(st, "num:abs",        abs);
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

    func!(st, "num:lerp",
        |env: &mut Env, _argc: usize| {
            let a = env.arg(0).f();
            let b = env.arg(1).f();
            let x = env.arg(2).f();
            Ok(VVal::Flt(a * (1.0 - x) + b * x))
        }, Some(3), Some(3), false);

    func!(st, "num:smoothstep",
        |env: &mut Env, _argc: usize| {
            let a = env.arg(0).f();
            let b = env.arg(1).f();
            let x = env.arg(2).f();
            let x = (x - a) / (b - a);
            let x = x.max(0.0).min(1.0);
            Ok(VVal::Flt(x * x * (3.0 - (2.0 * x))))
        }, Some(3), Some(3), false);

    func!(st, "fs:rename",
        |env: &mut Env, _argc: usize| {
            let from = env.arg(0);
            let to   = env.arg(1);
            from.with_s_ref(|from| to.with_s_ref(|to| {
                if let Err(e) = std::fs::rename(&from, &to) {
                    return Ok(env.new_err(
                        format!(
                            "Couldn't rename file '{}' to file '{}': {}",
                            from, to, e)));
                }

                Ok(VVal::Bol(true))
            }))
        }, Some(2), Some(2), false);

    func!(st, "io:lines",
        |env: &mut Env, _argc: usize| {
            let f = env.arg(0);
            let mut ret = VVal::None;
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
                    Err(StackAction::Break(v)) => { env.popn(1); return Ok(*v); },
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
            env.arg_ref(0).unwrap().with_s_ref(|vs: &str| {
                if let Err(e) = write!(*env.stdio.write.borrow_mut(), "{}", vs) {
                    Ok(env.new_err(
                       format!("IO-Error on std:io:stdout:print: {}", e)))
                } else {
                    Ok(v)
                }
            })
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
                VVal::Byt(b) => b.as_ref().clone(), // TODO: Remove clone
                v            => v.with_s_ref(|v: &str| v.as_bytes().to_vec()),
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
                VVal::Byt(b) => b.as_ref().clone(), // TODO: Remove clone
                v            => v.with_s_ref(|v: &str| v.as_bytes().to_vec()),
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

    func!(st, "dump_upvals",
        |env: &mut Env, _argc: usize| {
            if let VVal::Fun(f) = env.arg(0).deref() {
                return Ok(f.dump_upvals());
            }
            Ok(VVal::None)
        }, Some(1), Some(1), false);

    func!(st, "wlambda:sizes",
        |env: &mut Env, _argc: usize| {
            let mut write = env.stdio.write.borrow_mut();
            use crate::compiler::*;
            use crate::ops::*;
            use crate::vval::*;
            use crate::str_int::*;
            sizeof_writeln!(write, VVal);
            sizeof_writeln!(write, Op);
            sizeof_writeln!(write, Builtin);
            sizeof_writeln!(write, NVecPos);
            sizeof_writeln!(write, Prog);
            sizeof_writeln!(write, ResPos);
            sizeof_writeln!(write, (ResPos, Box<Symbol>, ResPos));
            sizeof_writeln!(write, (ResPos, Box<String>, u16, ResPos));
            sizeof_writeln!(write, (ResPos, ResPos, u16, ResPos));
            sizeof_writeln!(write, (ResPos, ResPos, ResPos, ResPos));
            sizeof_writeln!(write, (ResPos, DirectFun, ResPos));
            sizeof_writeln!(write, (DirectFun, ResPos, ResPos));
            sizeof_writeln!(write, (Box<String>, ResPos, ResPos, u16));
            sizeof_writeln!(write, SynPos);
            sizeof_writeln!(write, Symbol);
            sizeof_writeln!(write, Option<Rc<VVal>>);
            sizeof_writeln!(write, crate::nvec::NVec<f64>);
            sizeof_writeln!(write, Result<VVal, StackAction>);
            sizeof_writeln!(write, StackAction);
            sizeof_writeln!(write, Box<String>);
            sizeof_writeln!(write, Box<Vec<VVal>>);
            sizeof_writeln!(write, Vec<VVal>);
            Ok(VVal::None)
        }, Some(0), Some(0), false);

////    println!("sizeof OP:{} bytes", std::mem::size_of::<(ResPos, Box<String>, Box<String>, Box<String>, ResPos)>());

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

    func!(st, "assert_str_eq",
        |env: &mut Env, _argc: usize| {
            let a = env.arg(0).s();
            let b = env.arg(1).s();

            if a != b {
                if env.arg(2).is_none() {
                    Err(StackAction::panic_msg(
                        format!(
                            "assertion failed: expected: '{}', got: '{}'",
                            b, a)))
                } else {
                    Err(StackAction::panic_msg(
                        format!(
                            "assertion '{}' failed: expected: '{}', got: '{}'",
                            env.arg(2).s_raw(), b, a)))
                }
            } else {
                Ok(VVal::Bol(true))
            }
        }, Some(2), Some(3), true);

    func!(st, "assert_rel_eq",
        |env: &mut Env, _argc: usize| {
            let l = env.arg(0);
            let r = env.arg(1);
            let epsilon = env.arg(2).f();
            let delta = (l.f() - r.f()).abs();

            if delta < epsilon {
                Ok(VVal::Bol(true))
            } else {
                Err(StackAction::panic_msg(format!(
                    "assertion{}failed: delta[{}] was more than epsilon[{}],\
                        left['{}', f:'{}'], right['{}', f:'{}']",
                    if env.arg(3).is_none() {
                        " ".to_string()
                    } else {
                        format!(" '{}' ", env.arg(3).s_raw())
                    },
                    delta, epsilon, l.s(), l.f(), r.s(), r.f()
                )))
            }
        }, Some(3), Some(4), true);

    func!(st, "to_ref",
        |env: &mut Env, _argc: usize| {
            Ok(env.arg(0).to_ref())
        }, Some(1), Some(1), true);

    func!(st, "ref_id",
        |env: &mut Env, _argc: usize| {
            if let Some(id) = env.arg_ref(0).unwrap_or(&VVal::None).ref_id() {
                Ok(VVal::Int(id))
            } else {
                Ok(VVal::None)
            }
        }, Some(1), Some(1), true);

    func!(st, "ref:set",
        |env: &mut Env, _argc: usize| {
            Ok(env.arg(0).set_ref(env.arg(1)))
        }, Some(2), Some(2), false);

    func!(st, "ref:strengthen",
        |env: &mut Env, _argc: usize| {
            Ok(env.arg(0).upgrade())
        }, Some(1), Some(1), false);

    func!(st, "ref:hide",
        |env: &mut Env, _argc: usize| {
            Ok(env.arg(0).hide_ref())
        }, Some(1), Some(1), false);

    func!(st, "ref:weaken",
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

    func!(st, "pattern",
        |env: &mut Env, _argc: usize| {
            let pat_src = env.arg_ref(0).cloned().unwrap_or_else(|| VVal::None);
            let res_ref =
                env.global.borrow_mut()
                   .get_var_ref("\\")
                   .unwrap_or_else(|| VVal::None);
            pat_src.with_s_ref(|pat_src|
                match create_regex_find_function(pat_src, res_ref) {
                    Ok(fun) => Ok(fun),
                    Err(e) => {
                        Ok(env.new_err(
                            format!("bad pattern: {}, pattern was: /{}/",
                                    e, pat_src)))
                    }
                })
        }, Some(1), Some(1), false);

    func!(st, "selector",
        |env: &mut Env, _argc: usize| {
            let pat_src = env.arg_ref(0).cloned().unwrap_or_else(|| VVal::None);
            let res_ref =
                env.global.borrow_mut()
                   .get_var_ref("\\")
                   .unwrap_or_else(|| VVal::None);
            pat_src.with_s_ref(|sel_src|
                match create_selector_function(sel_src, res_ref) {
                    Ok(fun) => Ok(fun),
                    Err(e) => {
                        Ok(env.new_err(
                            format!("bad selector: {}, selector was: /{}/",
                                    e, sel_src)))
                    }
                })
        }, Some(1), Some(1), false);

//    func!(st, "tree_select",
//        |env: &mut Env, _argc: usize| {
//            let slct = env.arg(0);
//            let tree = env.arg(1);
//            Ok(util::tree_select(&slct, &tree))
//        }, Some(2), Some(2), false);

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
                delim.chars().next().unwrap_or(','),
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

            env.arg_ref(2).unwrap().with_s_ref(|data: &str| {
                match csv::parse_csv(
                        delim.chars().next().unwrap_or(','),
                        &row_sep,
                        data)
                {
                    Ok(v) => Ok(v),
                    Err(e) => Ok(env.new_err(e)),
                }
            })
        }, Some(3), Some(3), false);

    #[cfg(feature="regex")]
    func!(st, "re:replace_all",
        |env: &mut Env, _argc: usize| {
            use regex::Regex;
            let re   = env.arg(0);
            let f    = env.arg(1);
            let text = env.arg(2);

            let rx = re.with_s_ref(|re: &str| Regex::new(&re));
            if let Err(e) = rx {
                return Ok(env.new_err(
                    format!("Regex '{}' did not compile: {}", re.s_raw(), e)));
            }
            let rx = rx.unwrap();

            let mut finished = false;
            let mut ret = Ok(VVal::None);
            let ret_str = text.with_s_ref(|text: &str| {
                VVal::new_str_mv(String::from(
                    rx.replace_all(&text, |capts: &regex::Captures| {
                        let captures = VVal::vec();
                        for cap in capts.iter() {
                            match cap {
                                None    => { captures.push(VVal::None); },
                                Some(c) => {
                                    captures.push(VVal::new_str(c.as_str()));
                                }
                            }
                        }

                        let repl = captures.at(0).unwrap_or(VVal::None).s_raw();
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
                    })))
            });
            if ret.is_err() { return ret; }
            Ok(ret_str)
        }, Some(3), Some(3), false);

    #[cfg(feature="regex")]
    func!(st, "re:match_compile",
        |env: &mut Env, _argc: usize| {
            use regex::Regex;
            let re   = env.arg(0);

            let rx = re.with_s_ref(|re: &str| Regex::new(&re));
            if let Err(e) = rx {
                return Ok(env.new_err(
                    format!("Regex '{}' did not compile: {}", re.s_raw(), e)));
            }
            let rx = rx.unwrap();

            Ok(VValFun::new_fun(
                move |env: &mut Env, _argc: usize| {
                    let text = env.arg(0);
                    let f    = env.arg(1);

                    text.with_s_ref(|text: &str| {
                        match rx.captures(text) {
                            Some(c) => {
                                let captures = VVal::vec();
                                for cap in c.iter() {
                                    match cap {
                                        None    => { captures.push(VVal::None); },
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
                                Ok(VVal::None)
                            }
                        }
                    })
                }, Some(2), Some(2), false))
        }, Some(1), Some(1), false);

    #[cfg(feature="regex")]
    func!(st, "re:match",
        |env: &mut Env, _argc: usize| {
            use regex::Regex;
            let re   = env.arg(0);
            let text = env.arg(1);
            let f    = env.arg(2);

            let rx = re.with_s_ref(|re: &str| Regex::new(&re));
            if let Err(e) = rx {
                return Ok(env.new_err(
                    format!("Regex '{}' did not compile: {}", re.s_raw(), e)));
            }
            let rx = rx.unwrap();

            text.with_s_ref(|text: &str| {
                match rx.captures(text) {
                    Some(c) => {
                        let captures = VVal::vec();
                        for cap in c.iter() {
                            match cap {
                                None    => { captures.push(VVal::None); },
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
                        Ok(VVal::None)
                    }
                }
            })
        }, Some(3), Some(3), false);

    #[cfg(feature="regex")]
    func!(st, "re:map",
        |env: &mut Env, _argc: usize| {
            use regex::Regex;
            let re   = env.arg(0);
            let f    = env.arg(1);
            let text = env.arg(2);

            let rx = re.with_s_ref(|re: &str| Regex::new(&re));
            if let Err(e) = rx {
                return Ok(env.new_err(
                    format!("Regex '{}' did not compile: {}", re.s_raw(), e)));
            }
            let rx = rx.unwrap();

            let mut ret = VVal::None;
            text.with_s_ref(|text: &str| {
                for capts in rx.captures_iter(text) {
                    let captures = VVal::vec();
                    for cap in capts.iter() {
                        match cap {
                            None    => { captures.push(VVal::None); },
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
                        Err(StackAction::Break(v)) => { ret = *v; break; },
                        Err(StackAction::Next)     => { },
                        Err(e)                     => { return Err(e); },
                    }
                }
                Ok(ret)
            })
        }, Some(3), Some(3), false);

    #[cfg(feature="chrono")]
    func!(st, "chrono:timestamp",
        |env: &mut Env, _argc: usize| {
            use chrono::prelude::*;
            let dt = Local::now();

            let fmt = env.arg(0);
            if fmt.is_str() {
                fmt.with_s_ref(|fmt: &str|
                    Ok(VVal::new_str_mv(dt.format(fmt).to_string())))
            } else {
                Ok(VVal::new_str_mv(dt.format("%Y-%m-%d %H:%M:%S.%f").to_string()))
            }

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
            env.arg_ref(0).unwrap().with_s_ref(
                |json_txt: &str|
                    match VVal::from_json(json_txt) {
                        Ok(v) => Ok(v),
                        Err(e) => Ok(env.new_err(e)),
                    })
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
                match VVal::from_msgpack(&u[..]) {
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


    func!(st, "v:dims",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Int(match env.arg(0) {
                VVal::FVec(fv) => fv.dims(),
                v => v.nvec::<i64>().dims(),
            } as i64))
        }, Some(1), Some(1), false);

    func!(st, "v:mag2",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Flt(match env.arg(0) {
                VVal::FVec(fv) => fv.mag2(),
                v => v.nvec::<i64>().mag2(),
            }))
        }, Some(1), Some(1), false);

    func!(st, "v:mag",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Flt(match env.arg(0) {
                VVal::FVec(fv) => fv.mag(),
                v => v.nvec::<i64>().mag(),
            }))
        }, Some(1), Some(1), false);

    func!(st, "v:norm",
        |env: &mut Env, _argc: usize| {
            Ok(match env.arg(0) {
                VVal::FVec(fv) => VVal::FVec(Box::new(fv.norm())),
                v => VVal::IVec(Box::new(v.nvec::<i64>().norm())),
            })
        }, Some(1), Some(1), false);

    func!(st, "v:dot",
        |env: &mut Env, _argc: usize| {
            Ok(match env.arg(0) {
                VVal::FVec(fv) => VVal::Flt(fv.dot(env.arg(1).nvec())),
                v => VVal::Int(v.nvec::<i64>().dot(env.arg(1).nvec())),
            })
        }, Some(2), Some(2), false);

    func!(st, "v:cross",
        |env: &mut Env, _argc: usize| {
            Ok(match env.arg(0) {
                VVal::FVec(fv) => VVal::FVec(Box::new(fv.cross(env.arg(1).nvec()))),
                v => VVal::IVec(Box::new(v.nvec::<i64>().cross(env.arg(1).nvec()))),
            })
        }, Some(2), Some(2), false);

    func!(st, "v:lerp",
        |env: &mut Env, _argc: usize| {
            Ok(match env.arg(0) {
                VVal::FVec(fv) => VVal::FVec(Box::new(fv.lerp(env.arg(1).nvec(), env.arg(2).f()))),
                v => VVal::IVec(Box::new(v.nvec::<i64>().lerp(env.arg(1).nvec(), env.arg(2).f()))),
            })
        }, Some(3), Some(3), false);

    func!(st, "v:slerp",
        |env: &mut Env, _argc: usize| {
            Ok(match env.arg(0) {
                VVal::FVec(fv) => VVal::FVec(Box::new(fv.slerp(env.arg(1).nvec(), env.arg(2).f()))),
                v => VVal::IVec(Box::new(v.nvec::<i64>().slerp(env.arg(1).nvec(), env.arg(2).f()))),
            })
        }, Some(3), Some(3), false);

    func!(st, "v:vec2rad",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Flt(match env.arg(0) {
                VVal::FVec(fv) => fv.vec2rad(),
                v => v.nvec::<i64>().vec2rad(),
            }))
        }, Some(1), Some(1), false);

    func!(st, "v:rad2vec",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::FVec(Box::new(crate::nvec::NVec::rad2vec(env.arg(0).f()))))
        }, Some(1), Some(1), false);

    func!(st, "v:hex2hsva_i",
        |env: &mut Env, _argc: usize| {
            let hsvaf =
                env.arg_ref(0)
                   .unwrap()
                   .with_s_ref(|hex| util::hex2hsvaf(hex));
            Ok(VVal::ivec4(
                hsvaf.0.round() as i64,
                hsvaf.1.round() as i64,
                hsvaf.2.round() as i64,
                hsvaf.3.round() as i64))
        }, Some(1), Some(1), false);

    func!(st, "v:hex2hsva_f",
        |env: &mut Env, _argc: usize| {
            let hsvaf =
                env.arg_ref(0)
                   .unwrap()
                   .with_s_ref(|hex| util::hex2hsvaf(hex));
            Ok(VVal::fvec4(
                hsvaf.0,
                hsvaf.1,
                hsvaf.2,
                hsvaf.3))
        }, Some(1), Some(1), false);

    func!(st, "v:hex2rgba_i",
        |env: &mut Env, _argc: usize| {
            let (r, g, b, a) =
                env.arg(0).with_s_ref(|s| util::hex2rgba(s));
            Ok(VVal::IVec(Box::new(NVec::from_tpl(
                (r as i64, g as i64, Some(b as i64), Some(a as i64))).unwrap())))
        }, Some(1), Some(1), false);

    func!(st, "v:hex2rgba_f",
        |env: &mut Env, _argc: usize| {
            let (r, g, b, a) =
                env.arg(0).with_s_ref(|s| util::hex2rgbaf(s));
            Ok(VVal::FVec(Box::new(NVec::from_tpl((r, g, Some(b), Some(a))).unwrap())))
        }, Some(1), Some(1), false);

    func!(st, "v:rgba2hex",
        |env: &mut Env, _argc: usize| {
            let arg = env.arg_ref(0).unwrap().deref();
            match arg {
                VVal::FVec(fv) =>
                    Ok(VVal::new_str_mv(util::rgba2hexf((
                        fv.x_raw(),
                        fv.y_raw(),
                        fv.z_raw().unwrap_or(0.0),
                        fv.w_raw().unwrap_or(0.0))))),
                VVal::IVec(iv) =>
                    Ok(VVal::new_str_mv(util::rgba2hex((
                        iv.x_raw() as u8,
                        iv.y_raw() as u8,
                        iv.z_raw().unwrap_or(0) as u8,
                        iv.w_raw().unwrap_or(0) as u8)))),
                _ => {
                    Ok(env.new_err(
                        "v:rgba2hex expects float or int vectors"
                        .to_string()))
                },
            }
        }, Some(1), Some(1), false);

    func!(st, "v:hsv2rgb",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::None)
        }, Some(1), Some(1), false);

    func!(st, "v:rgb2hsv",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::None)
        }, Some(1), Some(1), false);

    func!(st, "sort",
        |env: &mut Env, argc: usize| {
            if argc == 1 {
                let list = env.arg(0);
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
                let list = env.arg(1);
                let mut ret = Ok(VVal::None);
                list.sort(|a: &VVal, b: &VVal| {
                    env.push(b.clone());
                    env.push(a.clone());
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
                fun.call_no_args(env).unwrap_or(VVal::None).i()
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
                        env.arg(i).with_s_ref(|s: &str|
                            hash.write(&s.as_bytes()[..]));
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

    func!(st, "symbols:collect", |_env: &mut Env, _argc: usize| {
        Ok(VVal::Int(crate::str_int::string_interner_collect()))
    }, Some(0), Some(0), false);

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

    func!(st, "sync:atom:new",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            let av = AtomicAVal::new();
            av.write(&v);
            Ok(VVal::Usr(Box::new(av)))
        }, Some(1), Some(1), false);

    func!(st, "sync:mpsc:new",
        |_env: &mut Env, _argc: usize| {
            Ok(AValChannel::new_vval())
        }, Some(0), Some(0), false);

    func!(st, "sync:slot:new",
        |_env: &mut Env, _argc: usize| {
            Ok(VVal::Usr(Box::new(AtomicAValSlot::new())))
        }, Some(0), Some(0), false);

    func!(st, "thread:sleep",
        |env: &mut Env, _argc: usize| {
            match env.arg(0).to_duration() {
                Ok(dur) => std::thread::sleep(dur),
                Err(v)  => { return Ok(v); },
            }
            Ok(VVal::Bol(true))
        }, Some(1), Some(1), false);

    func!(st, "thread:spawn",
        move |env: &mut Env, argc: usize| {
            let avs =
                if argc > 1 {
                    let mut avs = vec![];
                    for (i, (v, k)) in env.arg(1).iter().enumerate() {
                        let av = AtomicAVal::new();
                        av.write(&v);

                        if let Some(k) = k {
                            avs.push((k.s_raw(), av));
                        } else {
                            avs.push((format!("THREAD_ARG{}", i), av));
                        }
                    }
                    Some(avs)
                } else {
                    None
                };

            let tc = env.global.borrow().get_thread_creator();
            if let Some(tc) = &tc {
                let ntc = tc.clone();
                match tc.lock() {
                    Ok(mut tcg) => {
                        env.arg(0).with_s_ref(|code: &str|
                            Ok(tcg.spawn(ntc, code.to_string(), avs)))
                    },
                    Err(e) => {
                        Err(StackAction::panic_str(
                            format!("Couldn't create thread: {}", e),
                            None))
                    },
                }

            } else {
                Err(StackAction::panic_str(
                    "This global environment does not provide threads.".to_string(),
                    None))
            }
        }, Some(1), Some(2), false);

    st
}
