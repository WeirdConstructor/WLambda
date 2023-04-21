// Copyright (c) 2020-2022 Weird Constructor <weirdconstructor@gmail.com>
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
    - [2.4.1](#241-the-self-and-data-special-variables) The $self and $data special variables
    - [2.4.2](#242-object-oriented-programming-with-prototypes) Object Oriented Programming with Prototypes
    - [2.4.3](#243-object-oriented-with-prototypes-and-inheritance) Object Oriented with Prototypes and Inheritance
    - [2.4.4](#244-object-oriented-with-prototypes-and-self-references-and-data-references) Object Oriented with Prototypes and $self References and $data References
  - [2.5](#25-function-call-composition) Function call composition
    - [2.5.1](#251--argument-list-delimiter) '~' Argument List Delimiter
    - [2.5.2](#252--tail-argument-function-chaninig) '|' Tail Argument Function Chaninig
    - [2.5.3](#253--head-argument-function-chaining) '||' Head Argument Function Chaining
    - [2.5.4](#254--left-hand-function-chaining) '|>' Left Hand Function Chaining
    - [2.5.5](#255-forward-argument-pipe-arg--fun) Forward Argument Pipe `arg &> fun`
    - [2.5.6](#256-forward-argument-apply-pipe-list--fun) Forward Argument Apply Pipe `list &@> fun`
    - [2.5.7](#257-reverse-argument-pipe-fun--arg) Reverse Argument Pipe `fun <& arg`
    - [2.5.8](#258-reverse-argument-apply-pipe-list--fun) Reverse Argument Apply Pipe `list &@> fun`
  - [2.6](#26-control-flow---returning) Control Flow - Returning
    - [2.6.1](#261-return-label-value) return \[_label_\] _value_
    - [2.6.2](#262-block-label-function) block \[label\] _function_
    - [2.6.3](#263-stdtodrop-function-or-raii-destructors-or-drop-functions) std:to\_drop _function_ (or RAII, Destructors or Drop Functions)
    - [2.6.4](#264-stdtimenow-unit) std:time:now \[_unit_\]
    - [2.6.5](#265-stdsrand-seed) std:srand \[_seed_\]
    - [2.6.6](#266-stdrand-max-or-mode) std:rand [_max-or-mode_]
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
    - [3.3.1](#331--label-value) _? \[_label_\] _value_
    - [3.3.2](#332-unwrap-value) unwrap _value_
    - [3.3.3](#333-unwraperr-error-value) unwrap\_err _error-value_
    - [3.3.4](#334-onerror-handler-maybe-error-value) on\_error _handler_ _maybe-error-value_
    - [3.3.5](#335-iserr-value) is\_err _value_
    - [3.3.6](#336-stderrortostr-value) std:error\_to\_str _value_
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
    - [3.6.36](#3636-stdnumfract-float) std:num:fract _float_
  - [3.7](#37-numeric-functions) Numeric Functions
    - [3.7.1](#371-stdnumabs-number) std:num:abs _number_
    - [3.7.2](#372-stdnumsignum-number) std:num:signum _number_
    - [3.7.3](#373-stdnuminttoclosedopen01-integer) std:num:int\_to\_closed\_open01 _integer_
    - [3.7.4](#374-stdnuminttoopen01-integer) std:num:int\_to\_open01 _integer_
    - [3.7.5](#375-stdnuminttoopenclosed01-integer) std:num:int\_to\_open\_closed01 _integer_
  - [3.8](#38-numerical-mathematical-vectors) Numerical Mathematical Vectors
    - [3.8.1](#381-vector-conversions) Vector Conversions
    - [3.8.2](#382-vector-component-access) Vector Component Access
    - [3.8.3](#383-named-field-access-and-swizzling) Named Field Access and Swizzling
    - [3.8.4](#384-euler-additionsubtraction) Euler Addition/Subtraction
    - [3.8.5](#385-scalar-multiplicationdivision) Scalar Multiplication/Division
    - [3.8.6](#386-unary-vector-operations) Unary Vector Operations
    - [3.8.7](#387-isfvec-value) is\_fvec _value_
    - [3.8.8](#388-isivec-value) is\_ivec _value_
    - [3.8.9](#389-isnvec-value) is\_nvec _value_
    - [3.8.10](#3810-nveclen-value) nvec\_len _value_
    - [3.8.11](#3811-fvec-value) fvec _value_
    - [3.8.12](#3812-fvec2-value) fvec2 _value_
    - [3.8.13](#3813-fvec3-value) fvec3 _value_
    - [3.8.14](#3814-fvec4-value) fvec4 _value_
    - [3.8.15](#3815-ivec-value) ivec _value_
    - [3.8.16](#3816-ivec2-value) ivec2 _value_
    - [3.8.17](#3817-ivec3-value) ivec3 _value_
    - [3.8.18](#3818-ivec4-value) ivec4 _value_
    - [3.8.19](#3819-stdvdims-vec) std:v:dims _vec_
    - [3.8.20](#3820-stdvmag2-vec) std:v:mag2 _vec_
    - [3.8.21](#3821-stdvmag-vec) std:v:mag _vec_
    - [3.8.22](#3822-stdvnorm-vec) std:v:norm _vec_
    - [3.8.23](#3823-stdvdot-vec1-vec2) std:v:dot _vec1_ _vec2_
    - [3.8.24](#3824-stdvcross-vec1-vec2) std:v:cross _vec1_ _vec2_
    - [3.8.25](#3825-stdvlerp-vec1-vec2-t) std:v:lerp _vec1_ _vec2_ _t_
    - [3.8.26](#3826-stdvslerp-vec1-vec2-t) std:v:slerp _vec1_ _vec2_ _t_
    - [3.8.27](#3827-stdvvec2rad-vec) std:v:vec2rad _vec_
    - [3.8.28](#3828-stdvrad2vec-radians) std:v:rad2vec _radians_
  - [3.9](#39-characters-and-bytes) Characters and Bytes
    - [3.9.1](#391-byte-value) byte _value_
    - [3.9.2](#392-char-value) char _value_
    - [3.9.3](#393-isbyte-value) is\_byte _value_
    - [3.9.4](#394-ischar-value) is\_char _value_
    - [3.9.5](#395-stdchartolowercase-value) std:char:to\_lowercase _value_
    - [3.9.6](#396-stdchartouppercase-value) std:char:to\_uppercase _value_
  - [3.10](#310-strings) Strings
    - [3.10.1](#3101-string-literal-syntaxes) String Literal Syntaxes
    - [3.10.2](#3102-str-value) str _value_
    - [3.10.3](#3103-stdwritestr-value) std:write\_str _value_
    - [3.10.4](#3104-isstr-value) is\_str _value_
    - [3.10.5](#3105-stdstrcat-a-b-) std:str:cat _a_ _b_ ...
    - [3.10.6](#3106-stdstrjoin-sep-vector) std:str:join _sep_ _vector_
    - [3.10.7](#3107-stdstrlen-value) std:str:len _value_
    - [3.10.8](#3108-stdstrfind-pattern-string-offset---index--none) std:str:find _pattern_ _string_ \[_offset_\] -> _index_ | $none
    - [3.10.9](#3109-stdstrreplace-pattern-replacement-string) std:str:replace _pattern_ _replacement_ _string_
    - [3.10.10](#31010-stdstrreplacen-pattern-replacement-count-string) std:str:replace\_n _pattern_ _replacement_ _count_ _string_
    - [3.10.11](#31011-stdstrtrim-value) std:str:trim _value_
    - [3.10.12](#31012-stdstrtrimstart-value) std:str:trim\_start _value_
    - [3.10.13](#31013-stdstrtrimend-value) std:str:trim\_end _value_
    - [3.10.14](#31014-stdstrpadstart-len-pad-str-value) std:str:pad\_start _len_ _pad-str_ _value_
    - [3.10.15](#31015-stdstrpadend-len-pad-str-value) std:str:pad\_end _len_ _pad-str_ _value_
    - [3.10.16](#31016-stdstrtobytes-string) std:str:to\_bytes _string_
    - [3.10.17](#31017-stdstrtobyteslatin1-string) std:str:to\_bytes\_latin1 _string_
    - [3.10.18](#31018-stdstrfromlatin1-byte-vector) std:str:from\_latin1 _byte-vector_
    - [3.10.19](#31019-stdstrfromutf8-byte-vector) std:str:from\_utf8 _byte-vector_
    - [3.10.20](#31020-stdstrfromutf8lossy-byte-vector) std:str:from\_utf8\_lossy _byte-vector_
    - [3.10.21](#31021-stdstrtocharvec-string) std:str:to\_char\_vec _string_
    - [3.10.22](#31022-stdstrfromcharvec-vector) std:str:from\_char\_vec _vector_
    - [3.10.23](#31023-stdstrtolowercase-string) std:str:to\_lowercase _string_
    - [3.10.24](#31024-stdstrtouppercase-string) std:str:to\_uppercase _string_
    - [3.10.25](#31025-stdstrstriputf8bom-string-or-bytes) std:str:strip\_utf8\_bom _string-or-bytes_
    - [3.10.26](#31026-stdstreditdistance-str-a-strb) std:str:edit\_distance _str-a_ _str\_b
  - [3.11](#311-byte-vectors) Byte Vectors
    - [3.11.1](#3111-call-properties-of-bytes) Call Properties of Bytes
    - [3.11.2](#3112-byte-conversion-functions) Byte Conversion Functions
    - [3.11.3](#3113-isbytes-value) is\_bytes _value_
    - [3.11.4](#3114-stdbytesfind-pattern-string-offset---index--none) std:bytes:find _pattern_ _string_ \[_offset_\] -> _index_ | $none
    - [3.11.5](#3115-stdbytesreplace-byte-vector-pattern-replacement) std:bytes:replace _byte-vector_ _pattern_ _replacement_
    - [3.11.6](#3116-stdbytesfromhex-string-with-hex-chars) std:bytes:from\_hex _string-with-hex-chars_
    - [3.11.7](#3117-stdbytesfromvec-vector-of-ints) std:bytes:from\_vec _vector-of-ints_
    - [3.11.8](#3118-stdbytestohex-byte-vector) std:bytes:to\_hex _byte-vector_
    - [3.11.9](#3119-stdbytestobase64-byte-vector-config---string) std:bytes:to\_base64 _byte-vector_ \[_config_\] -> _string_
    - [3.11.10](#31110-stdbytesfrombase64-string-config---byte-vector) std:bytes:from\_base64 _string_ \[_config_\] -> _byte-vector_
    - [3.11.11](#31111-stdbytestovec-byte-vector) std:bytes:to\_vec _byte-vector_
    - [3.11.12](#31112-stdbytespack-pack-format-string-list-of-values) std:bytes:pack _pack-format-string_ _list-of-values_
    - [3.11.13](#31113-stdbytesunpack-pack-format-string-byte-vector) std:bytes:unpack _pack-format-string_ _byte-vector_
    - [3.11.14](#31114-stdbyteslzwencode-bytes-or-string-bitsize-bitorder---bytes--error) std:bytes:lzw:encode _bytes-or-string_ [_bitsize_ [_bitorder_]] -> _bytes_ | $error
    - [3.11.15](#31115-stdbyteslzwdecode-bytes-bitsize-bitorder---bytes--error) std:bytes:lzw:decode _bytes_ [_bitsize_ [_bitorder_]] -> _bytes_ | $error
  - [3.12](#312-symbols) Symbols
    - [3.12.1](#3121-sym-value) sym _value_
    - [3.12.2](#3122-issym-value) is\_sym _value_
    - [3.12.3](#3123-stdsymbolscollect) std:symbols:collect
  - [3.13](#313-syntax-block) Syntax `$%:Block`
    - [3.13.1](#3131-stdsynpos-syntax) std:syn:pos _syntax_
    - [3.13.2](#3132-stdsyntype-syntax) std:syn:type _syntax_
  - [3.14](#314-pairs-pa-b) Pairs `$p(a, b)`
    - [3.14.1](#3141-pair-operator-a--b) Pair Operator `a => b`
    - [3.14.2](#3142-cons-a-b) cons _a_ _b_
    - [3.14.3](#3143-pair-stringbyte-vector-operations) Pair string/byte vector operations
      - [3.14.3.1](#31431-p-from--count--string-or-byte-vec) $p( _from_ , _count_ ) _string-or-byte-vec_
      - [3.14.3.2](#31432-p-pattern--replacement--string-or-byte-vec) $p( _pattern_ , _replacement_ ) _string-or-byte-vec_
      - [3.14.3.3](#31433-p-split-pattern--max--string-or-byte-vec) $p( _split-pattern_ , _max_ ) _string-or-byte-vec_
    - [3.14.4](#3144-pair-to-iterator) Pair to Iterator
      - [3.14.4.1](#31441-iter---range) Iter - Range
      - [3.14.4.2](#31442-iter---enumerate) Iter - Enumerate
      - [3.14.4.3](#31443-iter---values) Iter - Values
      - [3.14.4.4](#31444-iter---keys) Iter - Keys
    - [3.14.5](#3145-ispair-value) is\_pair _value_
  - [3.15](#315-vectors-or-lists) Vectors (or Lists)
    - [3.15.1](#3151-stdpush-vector-item) std:push _vector_ _item_
    - [3.15.2](#3152-stdpop-vector) std:pop _vector_
    - [3.15.3](#3153-stdunshift-vector-item) std:unshift _vector_ _item_
    - [3.15.4](#3154-isvec-value) is\_vec _value_
    - [3.15.5](#3155-vector-splicing) Vector Splicing
    - [3.15.6](#3156-stdappend-vec-a-value-or-vec-) std:append _vec-a_ _value-or-vec_ ...
    - [3.15.7](#3157-stdprepend-vec-a-value-or-vec-) std:prepend _vec-a_ _value-or-vec_ ...
    - [3.15.8](#3158-stdtake-count-vector) std:take _count_ _vector_
    - [3.15.9](#3159-stddrop-count-vector) std:drop _count_ _vector_
  - [3.16](#316-associative-maps-or-string-to-value-mappings) Associative Maps (or String to Value mappings)
    - [3.16.1](#3161-map-splicing) Map Splicing
    - [3.16.2](#3162-ismap-value) is\_map _value_
  - [3.17](#317-references) References
    - [3.17.1](#3171-stdtoref-value) std:to\_ref _value_
    - [3.17.2](#3172-stdrefweaken-ref) std:ref:weaken _ref_
    - [3.17.3](#3173-stdrefhide-value) std:ref:hide _value_
    - [3.17.4](#3174-isref-value) is\_ref _value_
    - [3.17.5](#3175-iswref-value) is\_wref _value_
    - [3.17.6](#3176-stdrefstrengthen-ref) std:ref:strengthen _ref_
    - [3.17.7](#3177-stdrefset-ref-value) std:ref:set _ref_ _value_
  - [3.18](#318-iterators-iter-expression) Iterators $iter _expression_
    - [3.18.1](#3181-iterator-kinds) Iterator Kinds
    - [3.18.2](#3182-iterators-on-mutated-data) Iterators on mutated data
    - [3.18.3](#3183-splicing-an-iterator) Splicing an Iterator
    - [3.18.4](#3184-calling-an-iterator-with-a-function) Calling an Iterator with a Function
    - [3.18.5](#3185-zip-iterators) Zip Iterators
    - [3.18.6](#3186-isiter-value) is\_iter _value_
  - [3.19](#319-calling-semantics-of-data-types) Calling Semantics of Data Types
- [4](#4-conditional-execution---if--then--else) Conditional Execution - if / then / else
  - [4.1](#41-if-condition-then-expr-else-expr) if/? _condition_ _then-expr_ [_else-expr_]
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
    - [5.2.4](#524-map-function-iterable) map _function_ _iterable_
    - [5.2.5](#525-filter-function-iterable) filter _function_ _iterable_
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
  - [6.1](#61-operator-assignment) Operator Assignment
  - [6.2](#62-arithmetic) Arithmetic
    - [6.2.1](#621--operand-1-operand-2-) + _operand-1_ _operand-2_ ...
    - [6.2.2](#622---operand-1-operand-2-) - _operand-1_ _operand-2_ ...
    - [6.2.3](#623--op-a-op-b) * _op-a_ _op-b_
    - [6.2.4](#624--op-a-op-b) / _op-a_ _op-b_
    - [6.2.5](#625--op-a-op-b) % _op-a_ _op-b_
    - [6.2.6](#626--op-a-op-b) ^ _op-a_ _op-b_
  - [6.3](#63-comparison) Comparison
    - [6.3.1](#631--op-a-op-b) == _op-a_ _op-b_
    - [6.3.2](#632--op-a-op-b) != _op-a_ _op-b_
    - [6.3.3](#633--op-a-op-b) < _op-a_ _op-b_
    - [6.3.4](#634--op-a-op-b) <= _op-a_ _op-b_
    - [6.3.5](#635--op-a-op-b) > _op-a_ _op-b_
    - [6.3.6](#636--op-a-op-b) >= _op-a_ _op-b_
  - [6.4](#64-bit-operations) Bit Operations
    - [6.4.1](#641--op-a-op-b) & _op-a_ _op-b_
    - [6.4.2](#642--op-a-op-b) &^ _op-a_ _op-b_
    - [6.4.3](#643--op-a-op-b) &| _op-a_ _op-b_
    - [6.4.4](#644--op-a-op-b) << _op-a_ _op-b_
    - [6.4.5](#645--op-a-op-b) >> _op-a_ _op-b_
  - [6.5](#65-collection-addition-operators--and-) Collection Addition Operators +> and <+
    - [6.5.1](#651--collection-a-) +> _collection_ _a_ ...
    - [6.5.2](#652--collection-a-) <+ _collection_ _a_ ...
  - [6.6](#66-call-operators----and-) Call Operators &>, \<&, &@> and \<@&
  - [6.7](#67-default-value-operators---n-o-and-e) Default Value Operators //, /?, /$n, /$o and /$e
    - [6.7.1](#671--a-default-b) // _a_ _default-b_
    - [6.7.2](#672--a-default-b) /? _a_ _default-b_
    - [6.7.3](#673-n-a-default-b) /$n _a_ _default-b_
    - [6.7.4](#674-o-a-default-b) /$o _a_ _default-b_
    - [6.7.5](#675-e-a-default-b) /$e _a_ _default-b_
- [7](#7-string-and-byte-vector-formatting) String and Byte Vector Formatting
    - [7.0.1](#701-stdformatter-format-string) std:formatter _format-string_
  - [7.1](#71-formatting-numbers) Formatting Numbers
- [8](#8-data-structure-matchers-selectors-and-string-patternsregex) Data Structure Matchers, Selectors and String Patterns/Regex
  - [8.1](#81-data-structure-matcher) Data Structure Matcher
    - [8.1.1](#811-match-value-expr-match-pair1--default-expr) match _value-expr_ _match-pair1_ ... \[_default-expr_\]
    - [8.1.2](#812-m-expr) $M _expr_
    - [8.1.3](#813-data-structure-matcher-syntax) Data Structure Matcher Syntax
  - [8.2](#82-data-structure-selectors-s) Data Structure Selectors `$S(...)`
    - [8.2.1](#821-selector-and-wlambda-regex-syntax) Selector and WLambda Regex Syntax:
    - [8.2.2](#822-stdselector-string) std:selector _string_
  - [8.3](#83-string-patterns-regex-r) String Patterns (Regex) `$r/.../`
    - [8.3.1](#831-global-patterns-rg) Global Patterns `$rg/.../`
    - [8.3.2](#832-pattern-substitutions-rs) Pattern Substitutions `$rs/.../`
    - [8.3.3](#833-pattern-syntax-overview) Pattern Syntax Overview
    - [8.3.4](#834-standard-regular-expressions) Standard Regular Expressions
    - [8.3.5](#835-stdpattern-string-mode) std:pattern _string_ \[_mode_\]
- [9](#9-modules) Modules
  - [9.1](#91-export) export
  - [9.2](#92-import) import
- [10](#10-core-library) Core Library
    - [10.0.1](#1001-type-value) type _value_
    - [10.0.2](#1002-len-value) len _value_
    - [10.0.3](#1003-panic-message) panic _message_
- [11](#11-standard-library) Standard Library
    - [11.0.1](#1101-stdshuffle-randfunc-vec) std:shuffle _rand\_func_ _vec_
    - [11.0.2](#1102-stddelete-vector-or-map-index-or-key) std:delete _vector-or-map_ _index-or-key_
    - [11.0.3](#1103-stdrefid-value) std:ref\_id _value_
    - [11.0.4](#1104-stdcopy-vecormap) std:copy _vec\_or\_map_
    - [11.0.5](#1105-stdvalues-collection-or-iter) std:values _collection-or-iter_
    - [11.0.6](#1106-stdkeys-collection-or-iter) std:keys _collection-or-iter_
    - [11.0.7](#1107-stdsort-comparefun-vec) std:sort [_compare\_fun_] _vec_
    - [11.0.8](#1108-stdcmpnumasc-a-b) std:cmp:num:asc _a_ _b_
    - [11.0.9](#1109-stdcmpnumdesc-a-b) std:cmp:num:desc _a_ _b_
    - [11.0.10](#11010-stdcmpstrasc-a-b) std:cmp:str:asc _a_ _b_
    - [11.0.11](#11011-stdcmpstrdesc-a-b) std:cmp:str:desc _a_ _b_
    - [11.0.12](#11012-stdreverse-value) std:reverse _value_
    - [11.0.13](#11013-stddisplayln-arg1-) std:displayln _arg1_ ...
    - [11.0.14](#11014-debug-arg1-) $DEBUG _arg1_ ...
    - [11.0.15](#11015-stdwriteln-arg1-) std:writeln _arg1_ ...
    - [11.0.16](#11016-stdeval-code-string) std:eval _code-string_
    - [11.0.17](#11017-stdassert-bool-message) std:assert _bool_ \[_message_]
    - [11.0.18](#11018-stdasserteq-actual-expected-message) std:assert\_eq _actual_ _expected_ \[_message_]
    - [11.0.19](#11019-stdassertstreq-actual-expected) std:assert\_str\_eq _actual_ _expected_
    - [11.0.20](#11020-stdassertreleq-l-r-epsilon-message) std:assert\_rel\_eq _l_ _r_ _epsilon_ \[_message_]
    - [11.0.21](#11021-stdmeasuretime-unit-function) std:measure\_time _unit_ _function_
  - [11.1](#111-io) I/O
    - [11.1.1](#1111-stdioline) std:io:line
    - [11.1.2](#1112-stdiolines-function---value--vector) std:io:lines \[_function_\] -> _value_ | _vector_
    - [11.1.3](#1113-stdiofilereadtext-filename) std:io:file:read\_text _filename_
    - [11.1.4](#1114-stdiofileread-filename) std:io:file:read _filename_
    - [11.1.5](#1115-stdiofilewritesafe-filename-bytes-or-string) std:io:file:write\_safe _filename_ _bytes-or-string_
    - [11.1.6](#1116-stdiofileappend-filename-bytes-or-string) std:io:file:append _filename_ _bytes-or-string_
    - [11.1.7](#1117-stdiostdoutnewline) std:io:stdout:newline
    - [11.1.8](#1118-stdiostdoutflush) std:io:stdout:flush
    - [11.1.9](#1119-stdiostdoutprint-value) std:io:stdout:print _value_
    - [11.1.10](#11110-stdiostdoutwrite-value) std:io:stdout:write _value_
    - [11.1.11](#11111-stdioflush-handle) std:io:flush _handle_
    - [11.1.12](#11112-stdioreadsome-handle) std:io:read\_some _handle_
    - [11.1.13](#11113-stdiowrite-handle-data-offs---count--error--none) std:io:write _handle_ _data_ \[_offs_\] -> _count_ | $error | $none
    - [11.1.14](#11114-stdiowritesome-handle-data) std:io:write\_some _handle_ _data_
  - [11.2](#112-networking) Networking
    - [11.2.1](#1121-stdnettcpconnect-socket-addr-connect-timeout) std:net:tcp:connect _socket-addr_ [_connect-timeout_]
    - [11.2.2](#1122-stdnettcplisten-socket-addr-function) std:net:tcp:listen _socket-addr_ _function_
    - [11.2.3](#1123-stdnettcpsettimeouts-socket-read-timeout-duration-write-timeout-duration) std:net:tcp:set\_timeouts _socket_ _read-timeout-duration_ [_write-timeout-duration_]
    - [11.2.4](#1124-stdnetudpnew-socket-addr-connect-addr) std:net:udp:new _socket-addr_ [_connect-addr_]
    - [11.2.5](#1125-stdnetudpsend-socket-data-socket-addr) std:net:udp:send _socket_ _data_ [_socket-addr_]
    - [11.2.6](#1126-stdnetudprecv-socket-byte-count) std:net:udp:recv _socket_ [_byte-count_]
  - [11.3](#113-processes) Processes
    - [11.3.1](#1131-stdprocessrun-executable-path-arguments---map) std:process:run _executable-path_ \[_arguments_\] -> _map_
    - [11.3.2](#1132-stdprocessspawn-executable-path-arg-vector-inheritout--inheritall) std:process:spawn _executable-path_ _arg-vector_ [:inherit\_out | :inherit\_all]
    - [11.3.3](#1133-stdprocesstrywait-child-handle) std:process:try\_wait _child-handle_
    - [11.3.4](#1134-stdprocesskillwait-child-handle) std:process:kill\_wait _child-handle_
    - [11.3.5](#1135-stdprocesswait-child-handle) std:process:wait _child-handle_
  - [11.4](#114-file-system) File System
    - [11.4.1](#1141-stdfspathexists-string) std:fs:path:exists _string_
    - [11.4.2](#1142-stdfscanonicalize-string) std:fs:canonicalize _string_
    - [11.4.3](#1143-stdfsrename-file-path-new-file-name) std:fs:rename _file-path_ _new-file-name_
    - [11.4.4](#1144-stdfscopy-src-file-path-dst-file-path) std:fs:copy _src-file-path_ _dst-file-path_
    - [11.4.5](#1145-stdfsreaddir-path-function) std:fs:read\_dir _path_ _function_
    - [11.4.6](#1146-stdfsremovefile-file-path) std:fs:remove\_file _file-path_
    - [11.4.7](#1147-stdfsremovedir-dir-path) std:fs:remove\_dir _dir-path_
    - [11.4.8](#1148-stdfsremovedirall-dir-path) std:fs:remove\_dir\_all _dir-path_
  - [11.5](#115-system) System
    - [11.5.1](#1151-stdsysenvvar-variable-name) std:sys:env:var _variable-name_
    - [11.5.2](#1152-stdsysos) std:sys:os
  - [11.6](#116-threading) Threading
    - [11.6.1](#1161-stdthreadspawn-string-globals-map) std:thread:spawn _string_ [_globals-map_]
    - [11.6.2](#1162-stdthreadsleep-duration) std:thread:sleep _duration_
    - [11.6.3](#1163-thread-handle-api) Thread Handle API
      - [11.6.3.1](#11631-thdljoin) thdl.join
      - [11.6.3.2](#11632-thdlrecvready) thdl.recv\_ready
    - [11.6.4](#1164-atom-api) Atom API
      - [11.6.4.1](#11641-stdsyncatomnew-value) std:sync:atom:new _value_
      - [11.6.4.2](#11642-atomread) atom.read
      - [11.6.4.3](#11643-atomwrite-value) atom.write _value_
      - [11.6.4.4](#11644-atomswap-value) atom.swap _value_
    - [11.6.5](#1165-atom-value-slot-api) Atom Value Slot API
      - [11.6.5.1](#11651-stdsyncslotnew) std:sync:slot:new
      - [11.6.5.2](#11652-atomslotsend-value) atom\_slot.send _value_
      - [11.6.5.3](#11653-atomslotrecv) atom\_slot.recv
      - [11.6.5.4](#11654-atomslottryrecv) atom\_slot.try\_recv
      - [11.6.5.5](#11655-atomslotrecvtimeout-duration) atom\_slot.recv\_timeout _duration_
      - [11.6.5.6](#11656-atomslotcheckempty) atom\_slot.check\_empty
      - [11.6.5.7](#11657-atomslotwaitempty) atom\_slot.wait\_empty
      - [11.6.5.8](#11658-atomslotwaitemptytimeout-duration) atom\_slot.wait\_empty\_timeout _duration_
    - [11.6.6](#1166-channel-api) Channel API
      - [11.6.6.1](#11661-stdsyncmpscnew) std:sync:mpsc:new
      - [11.6.6.2](#11662-channelsend-value) channel.send _value_
      - [11.6.6.3](#11663-channelrecv) channel.recv
      - [11.6.6.4](#11664-channeltryrecv) channel.try\_recv
      - [11.6.6.5](#11665-channelrecvtimeout-duration) channel.recv\_timeout _duration_
- [12](#12-optional-standard-library) Optional Standard Library
  - [12.1](#121-serialization) serialization
    - [12.1.1](#1211-stdserwlambda-arg) std:ser:wlambda _arg_
    - [12.1.2](#1212-stdserjson-data-nopretty) std:ser:json _data_ \[_no\_pretty_]
    - [12.1.3](#1213-stddeserjson-string) std:deser:json _string_
    - [12.1.4](#1214-stdsertoml-data-nopretty) std:ser:toml _data_ \[no\_pretty\]
    - [12.1.5](#1215-stddesertoml-string) std:deser:toml _string_
    - [12.1.6](#1216-stdsercsv-fielddelim-rowseparator-escapeall-table) std:ser:csv _field\_delim_ _row\_separator_ _escape\_all_ _table_
    - [12.1.7](#1217-stddesercsv-fielddelim-rowseparator-data) std:deser:csv _field\_delim_ _row\_separator_ _data_
    - [12.1.8](#1218-stdsermsgpack-data) std:ser:msgpack _data_
    - [12.1.9](#1219-stddesermsgpack-bytes) std:deser:msgpack _bytes_
  - [12.2](#122-regular-expressions-more-classic-syntax) Regular Expressions (more classic syntax)
    - [12.2.1](#1221-stdrematch-regex-string-input-string-function) std:re:match _regex-string_ _input-string_ _function_
    - [12.2.2](#1222-stdrematchcompile-regex-string) std:re:match\_compile _regex-string_
    - [12.2.3](#1223-stdremap-regex-string-function-input-string) std:re:map _regex-string_ _function_ _input-string_
    - [12.2.4](#1224-stdrereplaceall-regex-string-replace-function-input-string) std:re:replace\_all _regex-string_ _replace-function_ _input-string_
  - [12.3](#123-xml) xml
    - [12.3.1](#1231-stdxmlreadsax-xml-string-event-callback-function-do-not-trim-text) std:xml:read\_sax _xml-string_ _event-callback-function_ [_do-not-trim-text_]
    - [12.3.2](#1232-stdxmlcreatesaxwriter-indent) std:xml:create\_sax\_writer \[_indent_\]
    - [12.3.3](#1233-stdxmlcreatetreebuilder) std:xml:create\_tree\_builder
  - [12.4](#124-chrono) chrono
    - [12.4.1](#1241-stdchronotimestamp-format) std:chrono:timestamp \[_format_]
    - [12.4.2](#1242-stdchronoformatutc-utc-timestamp-format---string) std:chrono:format\_utc _utc-timestamp_ \[_format_\] -> _string_
    - [12.4.3](#1243-stdchronoformatlocal-utc-timestamp-format---string) std:chrono:format\_local _utc-timestamp_ \[_format_\] -> _string_
  - [12.5](#125-color-conversion) color conversion
    - [12.5.1](#1251-stdvrgb2hsv-color-vector) std:v:rgb2hsv _color-vector_
    - [12.5.2](#1252-stdvhsv2rgb-color-vector) std:v:hsv2rgb _color-vector_
    - [12.5.3](#1253-stdvrgba2hex-color-vector) std:v:rgba2hex _color-vector_
    - [12.5.4](#1254-stdvhex2rgbaf-string) std:v:hex2rgba\_f _string_
    - [12.5.5](#1255-stdvhex2rgbai-string) std:v:hex2rgba\_i _string_
    - [12.5.6](#1256-stdvhex2hsvai-string) std:v:hex2hsva\_i _string_
    - [12.5.7](#1257-stdvhex2hsvaf-string) std:v:hex2hsva\_f _string_
  - [12.6](#126-hash) hash
    - [12.6.1](#1261-stdhashfnv1a-arg1-) std:hash:fnv1a _arg1_ ...
  - [12.7](#127-rand) rand
    - [12.7.1](#1271-stdrandsplitmix64new) std:rand:split\_mix64\_new
    - [12.7.2](#1272-stdrandsplitmix64newfrom-seed) std:rand:split\_mix64\_new\_from _seed_
    - [12.7.3](#1273-stdrandsplitmix64next-smstate-count) std:rand:split\_mix64\_next _sm\_state_ \[_count_]
    - [12.7.4](#1274-stdrandsplitmix64nextopen01-smstate-count) std:rand:split\_mix64\_next\_open01 _sm\_state_ \[_count_]
    - [12.7.5](#1275-stdrandsplitmix64nextopenclosed01-smstate-count) std:rand:split\_mix64\_next\_open\_closed01 _sm\_state_ \[_count_]
    - [12.7.6](#1276-stdrandsplitmix64nextclosedopen01-smstate-count) std:rand:split\_mix64\_next\_closed\_open01 _sm\_state_ \[_count_]
  - [12.8](#128-utility-functions) Utility Functions
    - [12.8.1](#1281-stddumpupvals-function) std:dump\_upvals _function_
    - [12.8.2](#1282-stdwlambdaversion) std:wlambda:version
    - [12.8.3](#1283-stdwlambdasizes) std:wlambda:sizes
    - [12.8.4](#1284-stdwlambdaparse-string) std:wlambda:parse _string_
  - [12.9](#129-http-client) HTTP Client
    - [12.9.1](#1291-stdhttpclientnew) std:http:client:new
    - [12.9.2](#1292-stdhttpget-http-client-url-string-headers-and-options-map) std:http:get _http-client_ _url-string_ [_headers-and-options-map_]
    - [12.9.3](#1293-stdhttppost-http-client-url-string-body-bytes-headers-and-options-map) std:http:post _http-client_ _url-string_ _body-bytes_ [_headers-and-options-map_]
    - [12.9.4](#1294-stdhttprequest-http-client-method-string-url-string-body-bytes-headers-and-options-map) std:http:request _http-client_ _method-string_ _url-string_ [_body-bytes_ [_headers-and-options-map_]]
  - [12.10](#1210-operating-system-utilities) Operating System Utilities
    - [12.10.1](#12101-stdosgetclipboardtext) std:os:get\_clipboard\_text
    - [12.10.2](#12102-stdossetclipboardtext-string) std:os:set\_clipboard\_text _string_
  - [12.11](#1211-mqtt-messaging) MQTT Messaging
    - [12.11.1](#12111-stdmqttbrokernew-config) std:mqtt:broker:new _config_
      - [12.11.1.1](#121111-brokerpublish-topic-string-payload-bytes) broker.publish _topic-string_ _payload-bytes_
    - [12.11.2](#12112-stdmqttclientnew-channel-client-id-broker-host-broker-port) std:mqtt:client:new _channel_ _client-id_ _broker-host_ _broker-port_
      - [12.11.2.1](#121121-mqttclientpublish-topic-string-payload-bytes) mqtt\_client.publish _topic-string_ _payload-bytes_
      - [12.11.2.2](#121122-mqttclientsubscribe-topic-string) mqtt\_client.subscribe _topic-string_
  - [12.12](#1212-cursive-consoleterminal-text-user-interface) Cursive Console/Terminal Text User Interface
    - [12.12.1](#12121-stdcursivenew---cursive) std:cursive:new -> $\<Cursive\>
    - [12.12.2](#12122-stdcursiveinstallcursivestdio) std:cursive:install\_cursive\_stdio
    - [12.12.3](#12123-cursive-object) $\<Cursive\> Object
      - [12.12.3.1](#121231-cursiverun---none--error) $\<Cursive\>.run -> $none | $error
      - [12.12.3.2](#121232-cursiveaddlayer-view-def) $\<Cursive\>.add\_layer _view-def_
      - [12.12.3.3](#121233-cursivepoplayer) $\<Cursive\>.pop\_layer
      - [12.12.3.4](#121234-cursiveget-name-of-view---none--namedviewnametype) $\<Cursive\>.get _name-of-view_ -> $none | $\<NamedView:Name:Type\>
      - [12.12.3.5](#121235-cursivepopup-popup-def-view-def) $\<Cursive\>.popup _popup-def_ _view-def_
      - [12.12.3.6](#121236-cursiveaddscreen-view-def---screen-id) $\<Cursive\>.add\_screen _view-def_ -> _screen-id_
      - [12.12.3.7](#121237-cursivesetscreen-screen-id) $\<Cursive\>.*set\_screen* _screen-id_
      - [12.12.3.8](#121238-cursiveactivescreen---screen-id) $\<Cursive\>.active\_screen -> _screen-id_
      - [12.12.3.9](#121239-cursivesender---cursivesendmsgcb) $\<Cursive\>.sender -> $\<Cursive:SendMsgCb\>
        - [12.12.3.9.1](#1212391-cursivesendmsgcb-event-tag-value---true--error) $\<Cursive:SendMsgCb\> _event-tag_ _value_ -> $true | $error
      - [12.12.3.10](#1212310-cursivedefaultcb-widget-name-function) $\<Cursive\>.default\_cb _widget-name_ _function_
      - [12.12.3.11](#1212311-cursivesetwindowtitle-string) $\<Cursive\>.set\_window\_title _string_
    - [12.12.4](#12124-cursiveinitconsolelogging) $\<Cursive\>.init\_console\_logging
    - [12.12.5](#12125-cursivetoggledebugconsole) $\<Cursive\>.toggle\_debug\_console
    - [12.12.6](#12126-cursiveshowdebugconsole) $\<Cursive\>.show\_debug\_console
      - [12.12.6.1](#121261-cursivequit) $\<Cursive\>.quit
      - [12.12.6.2](#121262-cursivecounter---cursivecounter) $\<Cursive\>.counter -> $\<Cursive:Counter\>
    - [12.12.7](#12127-cursivecounter-object) $\<Cursive:Counter\> Object
      - [12.12.7.1](#121271-cursivecounterget) $\<Cursive:Counter\>.get
      - [12.12.7.2](#121272-cursivecounterset-integer) $\<Cursive:Counter\>.set _integer_
      - [12.12.7.3](#121273-cursivecountersetupdate-integer) $\<Cursive:Counter\>.set\_update _integer_
      - [12.12.7.4](#121274-cursivecountertick-integer) $\<Cursive:Counter\>.tick _integer_
      - [12.12.7.5](#121275-cursivecountertickupdate-integer) $\<Cursive:Counter\>.tick\_update _integer_
- [13](#13-wlambda-lexical-syntax-and-grammar) WLambda Lexical Syntax and Grammar
  - [13.1](#131-special-forms) Special Forms
  - [13.2](#132-string-formatting-syntax) String Formatting Syntax
  - [13.3](#133-format-string-syntax-for-stdbytespack-and-stdbytesunpack) Format String Syntax for std:bytes:pack and std:bytes:unpack
- [14](#14-cursive-view-definition) Cursive View Definition
  - [14.1](#141-size-def-cursive-widthheight-size-definition) size-def Cursive Width/Height Size Definition
  - [14.2](#142-view-default-def-view-default-definitions) view-default-def View Default Definitions
    - [14.2.1](#1421-hideable-views) Hideable Views
      - [14.2.1.1](#14211-hideablesetvisible-bool) hideable.set\_visible _bool_
      - [14.2.1.2](#14212-hideableisvisible---bool) hideable.is\_visible -> _bool_
      - [14.2.1.3](#14213-hideablehide) hideable.hide
      - [14.2.1.4](#14214-hideableunhide) hideable.unhide
  - [14.3](#143-view-def-panel-grouping-views-in-a-panel) view-def `panel` Grouping Views in a Panel
  - [14.4](#144-view-def-list-a-list-view-with-labels) view-def `list` A List View with Labels
  - [14.5](#145-view-def-hbox-horizontal-layout) view-def `hbox` Horizontal Layout
  - [14.6](#146-view-def-vbox-vertical-layout) view-def `vbox` Vertical Layout
  - [14.7](#147-view-def-progress-progress-bar) view-def `progress` Progress Bar
- [15](#15-wlambda-interpreter-cli-command-line-interface) WLambda Interpreter (CLI) Command Line Interface
  - [15.1](#151-wlambda-script-to-executable-packing) WLambda Script To Executable Packing

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
assign to. They are resolved and inserted at compile time and offer a slight
performance advantage (roughly 3-4%) over (global or local) variables.

```wlambda
!:const X = 11;

std:assert_eq X 11;

## Destructuring works too, but only with compile time literal values
## in the vectors / maps:
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

## When imported the X will remain constant:
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

## function transforms a and b to hidden references
!add_a_and_b = { a + b };

std:assert_eq add_a_and_b[] 30;

## The assignment assigns to the hidden reference, so the closure add_a_and_b
## also receives the new value:
.a = 33;

std:assert_eq add_a_and_b[] 53;

## a and b are dereferenced on local variable access.
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
    ## Captures by closures upgrade the outer `self` variable to a _hidden_
    ## reference, which is then captured. As the closure is stored in
    ## `self`, this would create a ref cycle. This is why we needed
    ## to make a weak reference to self.

    ## Make an explicit hidden reference:
    !self_ = $& ${
        name = name,
    };

    ## Create a weak reference form the hidden reference:
    !self = $weak& $:self_;

    self.meow     = { std:displayln self.name " meows!"; };
    self.get_name = { self.name };

    ## To keep the object alive, we retrieve a strong reference
    ## from the hidden reference:
    $:self
};

!my_cat = new_Cat "Spot";

my_cat.meow[]; # Prints 'Spot meows!'

std:assert_eq my_cat.get_name[] "Spot";
```

Alternatively you can just make the cat name private:

```wlambda
!new_Cat = {!(name) = @;
    ## This does not make cycles, because `name` does not contain
    ## the closures in the end.
    !cat_name = name;

    !meow     = { std:displayln cat_name " meows!"; };
    !get_name = { $*cat_name };
    !set_name = { .*cat_name = _; };

    ## Just holds the methods
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

The fourth alternative is the `&>` and `<&` (and the apply variants `&@>` and
`<@&`) argument pipe operators which can be conveniently used in conjunction
with the first variant to prevent some parenthesis. Also belonging into the
category of function calling operators there is the collection addition operators
`+>` and `<+` which are described in their own section.

Here are examples:

```wlambda
## All the second variant:
std:assert_eq[std:str:cat[1, 2, 3], "123"];

## Can also be written as:
std:assert_eq (std:str:cat 1 2 3) "123";

## As the third variant:
!some_args = $[1, 2, 3];
std:assert_eq std:str:cat[[some_args]] "123";

## The fourth variant:
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

## You may also call them directly, notice the parenthesis ( ... ) syntax
## for delimiting the inner function call:
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
    ## Please note: We have to assign the
    ## parameters to named values here, because
    ## the arms of the conditional below have
    ## their own set of arguments.

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

## f keeps its arity checks, but f2 will
## call the same function, but without arity checks.
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

#### <a name="241-the-self-and-data-special-variables"></a>2.4.1 - The $self and $data special variables

If you call a method using the dot `.`, and the value on the left
side is a map or vector, you will get the map or vector by `$self`.
However, if you define a `_data` key on the map, or put something in
the second element of the vector, you can refer to it using `$data`.

You can use this to refer to the members and other functions of a structure:

```wlambda
!new_ab_struct = {
    ${
        _data = ${ a = 1, b = 2 },
        inc_a = { $data.a += 1; },
        inc_b = { $data.b += 1; },
        a = { $data.a },
        b = { $data.b },
        inc_both = {
            $self.inc_a[];
            $self.inc_b[];
        },
    }
};

!ab = new_ab_struct[];

ab.inc_a[];
std:assert_eq ab.a[] 2;

ab.inc_b[];
std:assert_eq ab.b[] 3;

ab.inc_both[];
std:assert_eq ab.a[] 3;
std:assert_eq ab.b[] 4;
```

The next seconds show how this can be used to do prototyped object
oriented programming.

#### <a name="242-object-oriented-programming-with-prototypes"></a>2.4.2 - Object Oriented Programming with Prototypes

Instead of using closures for OOP the preferred way is to use
maps of functions as classes and form an inheritance hierarchy
by using the `'_proto'` key of a map:

```wlambda
!class_a = ${
    ## $self is set by any key access using the '.' calling form:
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

#### <a name="243-object-oriented-with-prototypes-and-inheritance"></a>2.4.3 - Object Oriented with Prototypes and Inheritance

You can inherit functionality from a different class by assigning
it to the prototype of the class itself.

```wlambda
!SuperClass = ${
    init_super_class = {
        $data.inc = 0;
    },
    inc = {
        $data.inc += 1;
        $data.inc
    },
};
```

Please notice, that _SuperClass_ does not have it's own constructor,
instead you should define a custom init function like `init_super_class`,
to define the used fields. The _SuperClass_ will refer to the
`$data` of the object that is going to be created by _MyClass_ in the next
step.

```wlambda
!SuperClass = ${
    init_super_class = {
        $data.inc = 0;
    },
    inc = {
        $data.inc += 1;
        $data.inc
    },
};

!MyClass = ${
    _proto = SuperClass,
    new = {
        !self = ${
            _proto = $self,
            _data = ${ other = 10 },
        };
        self.init_super_class[];
        self
    },
    get_other = { $data.other },
    get_inc = { $data.inc },
};

!my_obj = MyClass.new[];

std:assert_eq my_obj.get_other[] 10;
std:assert_eq my_obj.inc[] 1;
std:assert_eq my_obj.inc[] 2;
std:assert_eq my_obj.inc[] 3;

std:assert_eq my_obj._data.other 10;
std:assert_eq my_obj._data.inc   3;
```

#### <a name="244-object-oriented-with-prototypes-and-self-references-and-data-references"></a>2.4.4 - Object Oriented with Prototypes and $self References and $data References

There might come a time, when you want to pass a reference of your
object around, but you want to prevent cyclic references.
For this you will need to return a strong reference `$&&` from your
constructor as `$self` and if you want to refer to `$data` from callback
functions, you are advised to also wrap it into a strong reference.

```wlambda
!destroyed = $false;

!MyClass = ${
    new = {
        $&& ${
            _proto = $self,
            _data  = $&& ${
                x = 1
            },
            dropper = std:to_drop { .destroyed = $t; },
        }
    },
    inc_x = { $data.x += 1 },
    install_on = {!(callchain) = @;
        !self = $w& $self;
        std:push callchain { self.inc_x[]; };
    },
    install_getter = {!(callchain) = @;
        !data = $w& $data;
        std:push callchain { data.x };
    },
};

## Create instance:
!my_obj = MyClass.new[];

my_obj.inc_x[];

!chain = $[];
my_obj.install_on     chain;
my_obj.install_getter chain;

## There are now 3 references to 'my_obj':
## - my_obj variable
## - first callback in chain
## - second callback in chain

std:assert_eq my_obj._data.x 2;
chain.0[]; # calls my_ocj.inc_x[];
std:assert_eq my_obj._data.x 3;

## Second callback gets x:
std:assert_eq chain.1[] 3;

!my_obj = $n; # destroy only strong reference
std:assert destroyed;
```

Of course the callbacks now refer to `$none` to call `inc_x`, a more
sophisticated way of cleanup is of course necessary. But this is just an
example.

### <a name="25-function-call-composition"></a>2.5 - Function call composition

For regular function calling please consult the section [2.2](#22-function-calling).

#### <a name="251--argument-list-delimiter"></a>2.5.1 - '~' Argument List Delimiter

The '~' character can be used to end argument lists in case of a function call without `[]`.
This means:

```text
    function arg1 arg2 ~ function2 arg2_1 arg2_2 ...
```

Is the same as:

```text
    (function arg1 arg2 (function2 arg2_1 arg2_2 ...))
```

or:

```text
    function[arg1, arg2, function2 arg2_1 arg2_2 ...]
```

Here is an example:

```wlambda
std:assert_eq (std:str:cat :a :b ~ std:str:to_lowercase "X") "abx";
```

#### <a name="252--tail-argument-function-chaninig"></a>2.5.2 - '|' Tail Argument Function Chaninig

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

An example with strings:

```wlambda
!res = "a" | "b" "x" | "c" "d";

std:assert_eq res "cdbxa";
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

#### <a name="253--head-argument-function-chaining"></a>2.5.3 - '||' Head Argument Function Chaining

This operator works similar to the '|' operator. But it inserts the function call at the front
of the function call on the right side of the '||' operator like this:

```text
    fn1 a1 a2 || fn2 (   ) b1 b2  =>  fn2 (fn1 a1 a2) b1 b2
    """""""""          ^
        v              |
        ---------------|
```

An example:

```wlambda
!res = "a" || "b" "x" || "c" "d";

std:assert_eq res "cbaxd";
```

#### <a name="254--left-hand-function-chaining"></a>2.5.4 - '|>' Left Hand Function Chaining

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

#### <a name="255-forward-argument-pipe-arg--fun"></a>2.5.5 - Forward Argument Pipe `arg &> fun`

This operator has the highest precedence over all other operators
and is used to be able to write this:

```wlambda
if "foob" &> $r/f(^*)b/ {
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

#### <a name="256-forward-argument-apply-pipe-list--fun"></a>2.5.6 - Forward Argument Apply Pipe `list &@> fun`

This operator is like `&>`. But it will call the function with the elements
in the given _list_ as arguments.

That means: `list &@> fun` is equivalent to `fun[[list]]`.

```wlambda
std:assert_eq $[2, 5] &@> `+`   7;
```

#### <a name="257-reverse-argument-pipe-fun--arg"></a>2.5.7 - Reverse Argument Pipe `fun <& arg`

Like the `&>` operator this operator, but it has a lower precedence (does not bind
as strongly as `&>`) and is right associative. That means you can write this:

```wlambda
!r = (\std:str:pad_start 10 "0" _) <& std:str:to_lowercase <& "ABC";

std:assert_eq r "0000000abc";
```

That means, writing `f <& a <& x` becomes `f[a[x]]` or `(f (a x))`.

#### <a name="258-reverse-argument-apply-pipe-list--fun"></a>2.5.8 - Reverse Argument Apply Pipe `list &@> fun`

This operator is like `<&`. But it will call the function with the elements
in the given _list_ as arguments.

That means: `fun <@& list` is equivalent to `fun[[list]]`.

```wlambda
std:assert_eq `+` <@& $[2, 5]   7;
```

### <a name="26-control-flow---returning"></a>2.6 - Control Flow - Returning

WLambda uses labelled blocks for control flow, as returning from the current function would not be
very helpful for the control flow in wlambda in case of conditional execution using the
boolean calling semantics.

```wlambda
!some_func = \:outer {
    !x = 10;

    ## does stuff...

    (x == 10) {
        return :outer 20
    };

    ## more stuff that is not executed if x == 10.
}
```

#### <a name="261-return-label-value"></a>2.6.1 - return \[_label_\] _value_

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

#### <a name="262-block-label-function"></a>2.6.2 - block \[label\] _function_

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

#### <a name="264-stdtimenow-unit"></a>2.6.4 - std:time:now \[_unit_\]

Returns the current system time since the UNIX epoch (1970-01-01 00:00:00 UTC).
If no _unit_ is specified, the default is `:ms`. Following units are available:

- seconds: `"s"`
- milliseconds: `"ms"`
- microseconds: `"us"`
- nanoseconds: `"ns"`

```wlambda
std:assert (std:time:now :s)  > 1000;
std:assert (std:time:now :ms) > 1000;
std:assert len[str[std:time:now :ns]] > 18;
```

#### <a name="265-stdsrand-seed"></a>2.6.5 - std:srand \[_seed_\]

With this function you can seed the internal pseudo random number
generator based on an unspecified PRNG algorithm, that might or might not
change in the next WLambda version.
If no _seed_ is provided, the current system time (in `ns` resolution) is used.
If _seed_ is provided, it is set to the integer value of that.

```wlambda
std:srand[];

std:srand 1000;
```

#### <a name="266-stdrand-max-or-mode"></a>2.6.6 - std:rand [_max-or-mode_]

Returns a random number between 0 and _max_.  The interval 0
to _max-or-mode_ is closed/open, that means 0 is included but _max-or-mode_
is not included.

If _max-or-mode_ is a string `"i64"` or symbol `:i64`, then std:rand will
return a random signed 64 bit integer.

If _max-or-mode_ is not provided, a float number between 0.0
and 1.0 (including 0.0 but not including 1.0) is returned.

```wlambda
std:srand 1234567890;

!zeros = $@i iter i 0 => 1000 {
    if std:rand[100] == 0 \$+ 1;
};

!count_100 = $@i iter i 0 => 1000 {
    if std:rand[100] == 100 \$+ 1;
};

std:assert zeros     >  0;
std:assert count_100 == 0;

std:assert std:rand[] < 1.0;
std:assert std:rand[] >= 0.0;
```

Please note: The PRNG algorithm used for `std:rand` may change
without further notice. If you require your project to have consistent
PRNG results across all WLambda versions use `std:rand:split_mix64_*`.

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

Note that there are two operators that interact with this type in a convenient
way. The optional or operator `/$o` and the default value operator `//` can be
used to provide default values and unwrap optionals. These operators also support
short circuit, that means you can write `x // return[$none]`.

The functions `is_none` and `is_some` like stated above work for these
optional values too:

```wlambda
std:assert ~ is_none $o();
std:assert ~ is_some $o(10);
std:assert ~ is_some $o($none);
std:assert ~ is_some $o($o());
```

Here is a demonstration of the very convenient optionals or operator
and the default value operator:

```wlambda
std:assert_eq $o()   /$o 11   11;
std:assert_eq $o(22) /$o 11   22;
std:assert_eq $none  /$o 11   $none;
std:assert (is_err ($e 10) /$o 20);

std:assert_eq $o()   // 11   11;
std:assert_eq $o(22) // 11   22;
std:assert_eq $none  // 11   11;
std:assert_eq 123    // 11   123;
std:assert (is_err ($e 10) // 20);

std:assert_eq $o()    /? 11   11;
std:assert_eq $o(22)  /? 11   22;
std:assert_eq $none   /? 11   11;
std:assert_eq 123     /? 11   123;
std:assert_eq ($e 10) /? 11   11;
```

Calling an optional value will return it's contents or `$none`:

```wlambda
std:assert_eq $o()[]     $none;
std:assert_eq $o(10)[]   10;

!do_something = {
    if _ == 0 {
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
!res1 = if x "something" "nothing";
std:assert_eq res1 "nothing";

.x = $o(30);
!res2 = if x "something" "nothing";
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
Alternatively you can use the optional or operator `/$o`
or even the default value operator `//`
for accessing the potentially wrapped value while providing a default value.

```wlambda
std:assert_eq unwrap[$o(10)] 10;

std:assert_eq $o(10) /$o 20  10;
std:assert_eq $o()   /$o 20  20;
std:assert_eq $none  /$o 20  $none;

std:assert_eq $o(10) //  20  10;
std:assert_eq $o()   //  20  20;
std:assert_eq $none  //  20  20;
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

You can also use the `match` statement to check return types of
functions:

```wlambda
!res = match (std:str:join "," "foo")
    ($e err) => { $\.err }
    r        => { $\.r };

std:assert_eq (res 0 8) "str:join";

## As comparison, the positive case for this construct:

!res = match 100
    ($e err) => { $\.err }
    r        => { $\.r };
std:assert_eq res 100;
```

There is also the error or operator `/$e` and the extended default value
operator `/?` that helps to shortcut any errors. These operators also support short
circuit, that means you can do `func_might_return_err[] // return[$n]`.
But use this with care, it hides errors all to easily!

```wlambda
std:assert_eq ($e 30) /$e 20    20;
std:assert_eq $n      /$e 10    $n;

## The extended default value operator also hides errors:
std:assert_eq ($e 30) /? 20    20;
std:assert_eq $n      /? 10    10;
std:assert_eq $o()    /? 10    10;
std:assert_eq $o(22)  /? 10    22;
std:assert_eq 22      /? 10    22;
std:assert_eq $f      /? 10    $false;

## You can combine this operator with the some or operator `//` or even `/?`:
std:assert_eq ($e 30) /$e 10 // 20   10;
std:assert_eq $n      /$e 10 // 20   20;

## To demonstrate short circuit:
!res = 0;
!x = ($e 10) /$e { .res = 10 }[];
std:assert_eq res 10;
std:assert_eq x   $none;
```

Most functions don't accept errors in their arguments.
If an error is encountered, a panic will occur. There are only
a few functions that accept error values in their arguments:

- panic
- `_?`
- unwrap_err
- std:error_to_str
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
- `//`, `/?`, `/$n`, `/$e`, `/$o`

All other functions don't accept errors as their argument.

#### <a name="331--label-value"></a>3.3.1 - _? \[_label_\] _value_

Unwind the call stack from the current function to a given _label_ if _value_ is an error value.
If no _label_ is given only the current function is returned from with the error value.  If there
is no error, the given value is returned.

The best usecase is, if you just want to hand any errors that might be returned
further upwards the call stack for the parent functions to handle.

```wlambda
!func = { $e "this failed!" };

!other = {
    ## some code ...

    _? func[]; ## If you would not catch the error value here,
               ## the program would panic, as an error value
               ## must not be ignored!

    ## other code ...

    panic "this will never be reached!";

    ## something here...
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

        ## The `then` branch we are currently in is a call frame.
        ## To jump further up the call stack, we need the label
        ## we defined for the function above.
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

#### <a name="332-unwrap-value"></a>3.3.2 - unwrap _value_

Unwraps the given _value_. If the _value_ is an error object it will panic.
Otherwise it will just return the given value. If the _value_ is an optional
value, it will return the value that is wrapped in the optional value.
If it is an empty optional, it will also panic.

Here an demonstration of the unwrap panic:

```wlambda
match (std:eval $code { unwrap $e XXX })
    ($e err) => {
        std:assert ~ std:str:find "Variable 'XXX' undefined" $\.err;
    }
    { std:assert $false };
```

And here how to unwrap optionals:

```wlambda
std:assert_eq (unwrap $o(123))  123;

match (std:eval $code { unwrap $o() })
    ($e err) => {
        std:assert ~ std:str:find "unwrap empty option" $\.err;
    }
    { std:assert $false };
```

#### <a name="333-unwraperr-error-value"></a>3.3.3 - unwrap\_err _error-value_

Unwraps an error value. Does panic if _error-value_ is not an error value.
If it is an error value, the inner wrapped value is returned.

```wlambda
!v = unwrap_err $e "Some Error";

std:assert_eq v "Some Error";
```

#### <a name="334-onerror-handler-maybe-error-value"></a>3.3.4 - on\_error _handler_ _maybe-error-value_

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
    ## ...
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

## The first function of on_error will be called with the unwrapped
## error if an error occured.
on_error {|4| .x = _; } ~ func 13;
std:assert_eq x "this failed!";

!ret = on_error {|4| .x = _; } ~ func 1;
std:assert_eq ret "all ok!";
```

#### <a name="335-iserr-value"></a>3.3.5 - is\_err _value_

Returns `$true` if _value_ is an error value.

```wlambda
std:assert ~ is_err $e "foo";
std:assert ~ not ~ is_err $none;
std:assert ~ not ~ is_err 10;
```

#### <a name="336-stderrortostr-value"></a>3.3.6 - std:error\_to\_str _value_

This function accepts an error value in contrast to `str`, but does
not panic but transform the error value into its string representation.

```wlambda
!r = std:error_to_str $e "TEST";

std:assert_eq r "$e \"TEST\" [@ <wlambda::eval>:1:26 Err]";
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
## Decimal:
std:assert_eq 10r99         99;

## Hexadecimal:
std:assert_eq 0xFF01        65281;

## Binary:
std:assert_eq  0b1011       11;
std:assert_eq -0b1011      -11;

## Radix 4:
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
## Decimal:
std:assert_eq 10r9.92       9.92;

## Hexadecimal:
std:assert_eq 0xFF.1        255.0625;

## Binary:
std:assert_eq 0b1011.101    11.625;

## Radix 4:
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

#### <a name="3636-stdnumfract-float"></a>3.6.36 - std:num:fract _float_

Returns the fractional part of the floating point number _float_.

```wlambda
std:assert ((std:num:fract 4.25) - 0.25) < 0.00001
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

#### <a name="372-stdnumsignum-number"></a>3.7.2 - std:num:signum _number_

Returns either 1 or -1, depending on the sign of the given _number_.

```wlambda
std:assert_eq (std:num:signum -4)  -1;
std:assert_eq (std:num:signum  4)   1;

std:assert_eq (std:num:signum -4.0)  -1.0;
std:assert_eq (std:num:signum  4.0)   1.0;
```

#### <a name="373-stdnuminttoclosedopen01-integer"></a>3.7.3 - std:num:int\_to\_closed\_open01 _integer_

Transforms the given 64-Bit _integer_ into a number in the range `0.0` to `1.0`.
Inclusive `0.0`, exclusive `1.0`. This function is mainly useful if you generated
the integer from a random number generator.

```wlambda
std:assert_rel_eq (std:num:int_to_closed_open01 0)  0.0         0.00000001;
std:assert_rel_eq (std:num:int_to_closed_open01 -1) 0.999999999 0.00000001;
```

#### <a name="374-stdnuminttoopen01-integer"></a>3.7.4 - std:num:int\_to\_open01 _integer_

Transforms the given 64-Bit _integer_ into a number in the range `0.0` to `1.0`.
Exclusive `0.0`, exclusive `1.0`. This function is mainly useful if you generated
the integer from a random number generator.

```wlambda
std:assert (std:num:int_to_open01 0)  > 0.0;
std:assert (std:num:int_to_open01 -1) < 1.0;
```

#### <a name="375-stdnuminttoopenclosed01-integer"></a>3.7.5 - std:num:int\_to\_open\_closed01 _integer_

Transforms the given 64-Bit _integer_ into a number in the range `0.0` to `1.0`.
Inclusive `0.0`, inclusive `1.0`. This function is mainly useful if you generated
the integer from a random number generator.

```wlambda
std:assert (std:num:int_to_open_closed01 0)  > 0.0;
std:assert (std:num:int_to_open_closed01 -1) == 1.0;
```

### <a name="38-numerical-mathematical-vectors"></a>3.8 - Numerical Mathematical Vectors

In order to aid in the development of GUIs, games, and other physics/geometry adjacent software,
WLambda comes with a built in datatype for mathematical vectors, which can contain floats and integers
and have between two and four dimensions.

```wlambda
## integer vectors
std:assert ~ $i(-1, 2).y                == 2;
std:assert ~ (ivec ${z=3})              == $i(0,0,3);
std:assert ~ (ivec4 $[])                == $i(0,0,0,0);
std:assert ~ $i(1.49, -2.72)            == $i(1,-2);
## float vectors
std:assert ~ $f(1.00, -33).x            == $f(1, 200).first;
std:assert ~ $f(-0, 2.4).y              == $f(1.6, 2.4).second;
std:assert ~ (fvec3 ${w=0.1})           == $f(0,0,0);
## conversion
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
## adding something to its inverse yields all 0s
std:assert_eq[ my_vec + (-my_vec), my_vec * 0 ];
```

#### <a name="387-isfvec-value"></a>3.8.7 - is\_fvec _value_

Returns `$true` if _value_ is a float vector.

```wlambda
std:assert_eq   (is_fvec $f(1,2))       $true;
std:assert_eq   (is_fvec $f(1,2,3))     $true;
std:assert_eq   (is_fvec $f(1,2,3,4))   $true;
std:assert_eq   (is_fvec $none)         $false;
std:assert_eq   (is_fvec $i(1,2))       $false;
std:assert_eq   (is_fvec $[3.4, 4.5])   $false;

std:assert_eq   (is_fvec fvec <& $[3.4, 4.5])   $true;

## References are not dereferenced:
std:assert_eq   (is_fvec $&&$f(3,4))    $false;
std:assert_eq   (is_fvec $*$&&$f(3,4))  $true;
```

#### <a name="388-isivec-value"></a>3.8.8 - is\_ivec _value_

Returns `$true` if _value_ is an integer vector.

```wlambda
std:assert_eq   (is_ivec $i(1,2))       $true;
std:assert_eq   (is_ivec $i(1,2,3))     $true;
std:assert_eq   (is_ivec $i(1,2,3,4))   $true;
std:assert_eq   (is_ivec $none)         $false;
std:assert_eq   (is_ivec $[3, 4])       $false;

std:assert_eq   (is_ivec ivec <& $[3.4, 4.5])   $true;

## References are not dereferenced:
std:assert_eq   (is_ivec    $&& $i(3,4))  $false;
std:assert_eq   (is_ivec $* $&& $i(3,4))  $true;
```

#### <a name="389-isnvec-value"></a>3.8.9 - is\_nvec _value_

Returns `$true` if _value_ is either a numerical float or integer vector.

```wlambda
std:assert_eq   (is_nvec $i(1,2))       $true;
std:assert_eq   (is_nvec $i(1,2,3))     $true;
std:assert_eq   (is_nvec $i(1,2,3,4))   $true;
std:assert_eq   (is_nvec $f(1,2))       $true;
std:assert_eq   (is_nvec $f(1,2,3))     $true;
std:assert_eq   (is_nvec $f(1,2,3,4))   $true;
std:assert_eq   (is_nvec $none)         $false;
std:assert_eq   (is_nvec $[3, 4])       $false;

std:assert_eq   (is_nvec fvec <& $[3.4, 4.5])   $true;
std:assert_eq   (is_nvec ivec <& $[3.4, 4.5])   $true;
```

#### <a name="3810-nveclen-value"></a>3.8.10 - nvec\_len _value_

Returns the length of a numerical vector, commonly known as the dimension.
Either 2, 3 or 4.

```wlambda
std:assert_eq (nvec_len $i(1, 2))       2;
std:assert_eq (nvec_len $i(1, 2, 3))    3;
std:assert_eq (nvec_len $i(1, 2, 3, 4)) 4;

std:assert_eq (nvec_len $f(1.1, 2.2))           2;
std:assert_eq (nvec_len $f(1.1, 2.2, 3.3))      3;
std:assert_eq (nvec_len $f(1.1, 2.2, 3.3, 4.4)) 4;
```

#### <a name="3811-fvec-value"></a>3.8.11 - fvec _value_

Will cast _value_ into a float vector. You can cast a multitude of data types
into a float vector:

```wlambda
std:assert_eq   (fvec  $[1,2,3,4])      $f(1,2,3,4);
std:assert_eq   (fvec  $[1,2,3])        $f(1,2,3);
std:assert_eq   (fvec  $[1,2])          $f(1,2);

std:assert_eq   (fvec $i(1,2))          $f(1,2);
std:assert_eq   (fvec $i(1,2,3))        $f(1,2,3);
std:assert_eq   (fvec $i(1,2,3,4))      $f(1,2,3,4);

std:assert_eq   (fvec $p("2", "3.4"))   $f(2,3.4);

!i = $iter $[] +> $p(3,4) +> $[5,6];
std:assert_eq   (fvec i)    $f(3,4);
std:assert_eq   (fvec i)    $f(5,6);

std:assert_eq   (fvec ${x = 1, y = 2})                 $f(1,2);
std:assert_eq   (fvec ${x = 1, y = 2, z = 3})          $f(1,2,3);
std:assert_eq   (fvec ${x = 1, y = 2, z = 3, w = 4})   $f(1,2,3,4);
```

#### <a name="3812-fvec2-value"></a>3.8.12 - fvec2 _value_

Like `fvec` but always returns a 2 dimensional vector.

```wlambda
std:assert_eq  (fvec2 $i(3,4,5))    $f(3,4);
std:assert_eq  (fvec2 $[4,5,6,7,8]) $f(4,5);

std:assert_eq  (fvec2 ${x = 1, y = 2, z = 3, w = 4})   $f(1,2);
```

#### <a name="3813-fvec3-value"></a>3.8.13 - fvec3 _value_

Like `fvec` but always returns a 3 dimensional vector.

```wlambda
std:assert_eq  (fvec3 $i(3,4,5))    $f(3,4,5);
std:assert_eq  (fvec3 $[4,5,6,7,8]) $f(4,5,6);

std:assert_eq  (fvec3 ${x = 1, y = 2, z = 3, w = 4})   $f(1,2,3);
```

#### <a name="3814-fvec4-value"></a>3.8.14 - fvec4 _value_

Like `fvec` but always returns a 4 dimensional vector.

```wlambda
std:assert_eq  (fvec4 $i(3,4,5))    $f(3,4,5,0);
std:assert_eq  (fvec4 $[4,5,6,7,8]) $f(4,5,6,7);

std:assert_eq  (fvec4 ${x = 1, y = 2, z = 3, w = 4})   $f(1,2,3,4);
```

#### <a name="3815-ivec-value"></a>3.8.15 - ivec _value_

Will cast _value_ into a float vector. You can cast a multitude of data types
into a float vector:

```wlambda
std:assert_eq   (ivec  $[1,2,3,4])      $i(1,2,3,4);
std:assert_eq   (ivec  $[1,2,3])        $i(1,2,3);
std:assert_eq   (ivec  $[1,2])          $i(1,2);

std:assert_eq   (ivec $f(1.1,2.1))          $i(1,2);
std:assert_eq   (ivec $f(1.1,2.1,3.2))      $i(1,2,3);
std:assert_eq   (ivec $f(1.1,2.1,3.2,4))    $i(1,2,3,4);

std:assert_eq   (ivec $p("2", "3"))   $i(2,3);

!i = $iter $[] +> $p(3,4) +> $[5,6];
std:assert_eq   (ivec i)    $i(3,4);
std:assert_eq   (ivec i)    $i(5,6);

std:assert_eq   (ivec ${x = 1, y = 2})                 $i(1,2);
std:assert_eq   (ivec ${x = 1, y = 2, z = 3})          $i(1,2,3);
std:assert_eq   (ivec ${x = 1, y = 2, z = 3, w = 4})   $i(1,2,3,4);
```

#### <a name="3816-ivec2-value"></a>3.8.16 - ivec2 _value_

Like `ivec` but always returns a 2 dimensional vector.

```wlambda
std:assert_eq  (ivec2 $f(3,4,5))    $i(3,4);
std:assert_eq  (ivec2 $[4,5,6,7,8]) $i(4,5);

std:assert_eq  (ivec2 ${x = 1, y = 2, z = 3, w = 4})   $i(1,2);
```

#### <a name="3817-ivec3-value"></a>3.8.17 - ivec3 _value_

Like `ivec` but always returns a 3 dimensional vector.

```wlambda
std:assert_eq  (ivec3 $f(3,4,5))    $i(3,4,5);
std:assert_eq  (ivec3 $[4,5,6,7,8]) $i(4,5,6);

std:assert_eq  (ivec3 ${x = 1, y = 2, z = 3, w = 4})   $i(1,2,3);
```

#### <a name="3818-ivec4-value"></a>3.8.18 - ivec4 _value_

Like `ivec` but always returns a 4 dimensional vector.

```wlambda
std:assert_eq  (ivec4 $f(3,4,5))    $i(3,4,5,0);
std:assert_eq  (ivec4 $[4,5,6,7,8]) $i(4,5,6,7);

std:assert_eq  (ivec4 ${x = 1, y = 2, z = 3, w = 4})   $i(1,2,3,4);
```

#### <a name="3819-stdvdims-vec"></a>3.8.19 - std:v:dims _vec_

You can use this function to retrieve the number of dimensions in _vec_.

Like most other std:v functions,
it will coerce whatever value is passed into it into a `ivec`,
if that value is not a `fvec`.

This function always returns an integer, regardless of whether an `ivec` or `fvec` is passed in.

```wlambda
## the least number of dimensions a vector can have is 2.
std:assert_eq (std:v:dims $[]) 2;
## while the most is 4.
std:assert_eq (std:v:dims ${w=0}) 4;
std:assert_eq (std:v:dims $f(1,2)) (std:v:dims $i(1,2));
```

#### <a name="3820-stdvmag2-vec"></a>3.8.20 - std:v:mag2 _vec_

Returns the magnitude of _vec_, squared.

Calculating the squared magnitude is a little bit faster,
so you should prefer this method where performance is paramount.

The magnitude is always a float, regardless of whether the parameter is an `ivec` or `fvec`.

```wlambda
std:assert_eq (std:v:mag2 ${w=4}) 16.0;
```

#### <a name="3821-stdvmag-vec"></a>3.8.21 - std:v:mag _vec_

Returns the magnitude (also known as the length) of _vec_.

The magnitude is always a float, regardless of whether the parameter is an `ivec` or `fvec`.

```wlambda
std:assert_eq (std:v:mag ${w=4}) 4.0;
```

#### <a name="3822-stdvnorm-vec"></a>3.8.22 - std:v:norm _vec_

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

## get the delta representing how far you'd have to travel to get from p1 to p2
!delta = p2 - p1;
## the normalized delta represents a single 1 sized step you could take to get to p2 from p1.
!n = std:v:norm delta;

## the length of this step is reflected in the magnitude of the vectors
std:assert_eq[ (std:v:mag delta) - 1, std:v:mag (p1 + n) - p2 ];
```

#### <a name="3823-stdvdot-vec1-vec2"></a>3.8.23 - std:v:dot _vec1_ _vec2_

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

## do you need to turn left or right to look at `goal`,
## if you're standing at `at` looking in `looking`?

## find the unit vector representing the space between where you want to look and where you're at.
!delta = std:v:norm goal - at;

## the direction you need to turn in can be found by checking the sign of
## the dot product of where you're currently looking and where you're at.
!dir = std:v:dot delta looking;

std:assert_eq[ (dir < 0) "left" "right", "left" ];
```

#### <a name="3824-stdvcross-vec1-vec2"></a>3.8.24 - std:v:cross _vec1_ _vec2_

Returns a vector perpendicular to _vec1_ and _vec2_.

Similar to the dot product, but instead of returning a single value it returns another vector,
and is only useful in three (and seven, but WLambda's vectors don't support so many) dimensions.

Regardless of the number of dimensions in the input vectors, this function will return a 3d vector.

```wlambda
!x = fvec ${x=1};
!y = fvec ${y=1};

## the cross product of these two values will represent the third axis, z, and will be
## perpendicular to both other vectors.

!z = std:v:cross x y;

std:assert_eq z (fvec ${z=1});

## because all three vectors are perpindicular, they'll all have the same dot product from each other.
std:assert_eq[(std:v:dot x y), (std:v:dot y z)];
std:assert_eq[(std:v:dot y z), (std:v:dot z x)];
```

#### <a name="3825-stdvlerp-vec1-vec2-t"></a>3.8.25 - std:v:lerp _vec1_ _vec2_ _t_

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

#### <a name="3826-stdvslerp-vec1-vec2-t"></a>3.8.26 - std:v:slerp _vec1_ _vec2_ _t_

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
## compare this to the one for std:v:lerp! note that the length of this one is almost 1.
## this is definitely not the case for std:v:lerp's output with the same input.
!v = std:v:slerp $f(1,0) $f(0,1) 0.5;
## the values may not be exact because of floating point rounding errors,
## but they should be pretty close.
std:assert_rel_eq v.x 0.7071067811865476 0.000001;
std:assert_rel_eq v.y 0.7071067811865476 0.000001;

## The values are interpolated around a circle, so if you raise t high enough you'll start
## getting the same values as you get with a lower t, although not quite because of float rounding.
!half = (std:v:slerp $f(1,0) $f(0,1) 0.5);
!four = (std:v:slerp $f(1,0) $f(0,1) 4.5);
std:assert_rel_eq half.x four.x 0.000001;
std:assert_rel_eq half.y four.y 0.000001;
```

#### <a name="3827-stdvvec2rad-vec"></a>3.8.27 - std:v:vec2rad _vec_

Creates a rotation in radians from the x and y components of _vec_.

Always returns a float.

Coerces the argument into an `ivec` unless it's a `fvec`.

```wlambda
std:assert_eq[ std:num:to_degrees (std:v:vec2rad ${x=1}) , 0.0 ];
std:assert_eq[ std:num:to_degrees (std:v:vec2rad ${y=1}) , 90.0 ];

## halfway in between 0.0 and 90.0 should be 45.
## note that lerp would work here as well
!h = std:v:slerp $f(1, 0) $f(0, 1) 0.5;
std:assert_eq[ std:num:to_degrees (std:v:vec2rad h) , 45.0 ];
```

#### <a name="3828-stdvrad2vec-radians"></a>3.8.28 - std:v:rad2vec _radians_

Creates a unit vector from _radians_.

Always returns an `fvec`.

```wlambda
std:assert_eq[ std:v:rad2vec (std:num:to_radians 0.0) , $f(1, 0)];
std:assert_eq[ ivec (std:v:rad2vec (std:num:to_radians 90.0)), $i(0, 1)];

## halfway in between 0.0 and 90.0 should be 45.
## note that lerp would NOT work here as well, rad2vec returns a unit vector.
!h = std:v:slerp $f(1, 0) $f(0, 1) 0.5; # slerp because rotations
!r = std:v:rad2vec (std:num:to_radians 45.0);
std:assert_rel_eq r.x h.x 0.0001;
std:assert_rel_eq r.y h.y 0.0001;
```

### <a name="39-characters-and-bytes"></a>3.9 - Characters and Bytes

WLambda has a data type for single characters and bytes. The lexical syntax is
a character or escape sequence delimited by `'`:

```wlambda
std:assert_eq (type 'a')   "char";
std:assert_eq (type $b'a') "byte";

std:assert_eq (type '\u{40}') "char";
std:assert_eq (type $b'\x40') "byte";

## You can use the unicode escapes up to the first 256 code points in bytes too:
std:assert_eq (char $b'\u{40}') '\u{40}';

## Beyond that, you will get the byte '?':
std:assert_eq (char $b'\u{3131}') '?';
```

The can be used interchangeably almost everywhere. They can often also be used
instead of a string, because they are handled like a single character long string.

They are useful because they do not require an extra allocation in the background.
They are not boxed like strings:

```wlambda
std:assert_eq ("foo" $p(0, 1))  "f"; # requires allocation
std:assert_eq ("foo".0)         'f'; # requires NO allocation
```

#### <a name="391-byte-value"></a>3.9.1 - byte _value_

Converts the _value_ to a byte. If _value_ is a number, it must be
below or equal to 255, otherwise it will result in the byte `'?'`.

```wlambda
std:assert_eq (byte 64)           $b'@';
std:assert_eq (byte 300)          $b'?';
std:assert_eq (byte "ABC")        $b'A';
std:assert_eq (byte $b"\xFF\xF0") $b'\xFF';
std:assert_eq (byte "\xFF\xF0")   $b'\xC3'; # first byte of an utf-8 sequence!
```

#### <a name="392-char-value"></a>3.9.2 - char _value_

Converts the _value_ to a Unicode character.

```wlambda
std:assert_eq (char $b'\xFF') 'ÿ';
std:assert_eq (char 0x262F)   '☯';
std:assert_eq (char "☯xyz")   '☯';
```

#### <a name="393-isbyte-value"></a>3.9.3 - is\_byte _value_

Checks if _value_ is of the byte data type.

```wlambda
std:assert (is_byte $b'X');
std:assert not[is_byte 'X'];
std:assert not[is_byte 123];
std:assert (is_byte $b"abc".0);
std:assert not[is_byte "abc".0];
std:assert not[is_byte $b"abc"];
```

#### <a name="394-ischar-value"></a>3.9.4 - is\_char _value_

Check if _value_ is of the Unicode character data type.

```wlambda
std:assert (is_char 'X');
std:assert not[is_char $b'X'];
std:assert not[is_char 123];
std:assert (is_char "abc".0);
std:assert not[is_char $b"abc".0];
std:assert not[is_char $b"abc"];
std:assert not[is_char "abc"];
```

#### <a name="395-stdchartolowercase-value"></a>3.9.5 - std:char:to\_lowercase _value_

Turns the _value_ into a lower case Unicode character.

```wlambda
std:assert_eq (std:char:to_lowercase 'A') 'a';
std:assert_eq (std:char:to_lowercase 65)  'a';
```

#### <a name="396-stdchartouppercase-value"></a>3.9.6 - std:char:to\_uppercase _value_

Turns the _value_ into an upper case Unicode character.

```wlambda
std:assert_eq (std:char:to_uppercase 'a') 'A';
std:assert_eq (std:char:to_uppercase 97)  'A';
```

### <a name="310-strings"></a>3.10 - Strings

Strings in WLambda are like Rust UTF-8 encoded immutable Unicode strings.
There are two types of literal forms for strings:

```wlambda
"abc def \"foo\"";
std:assert_eq $q/any delimiter may be used instead of/
    "any delimiter may be used instead of";
## Unicode escapes are also working:
std:assert_eq "\u{2211}" "∑";
```

#### <a name="3101-string-literal-syntaxes"></a>3.10.1 - String Literal Syntaxes

There are multiple kinds of syntax constructs you can use to
notate string (and byte vector) literals:

- Regular strings
```wlambda
!s = "a b c";

std:assert_eq s "a b c";
```
- Byte vectors `$b"\x02FOO\x03"`
- Quoted strings `$q(123433)`
- Quoted byte vectors `$Q(XZY)`
- WLambda code strings
```wlambda
## Short form $c works too.
!code = $code {
    !this = is a block;
    It just needs to be in valid WLambda[:Syntax];
    .x = But it does not need to pass the compiler
        phase.x;
};

## Primary use case is `eval` and `std:thread:spawn`:
!v = (std:thread:spawn $code {
    !@import std std;
    !res = "x" "y" "z";
    std:str:cat res 33;
}).join[];

std:assert_eq v "xyz33";
```

#### <a name="3102-str-value"></a>3.10.2 - str _value_

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

#### <a name="3103-stdwritestr-value"></a>3.10.3 - std:write\_str _value_

Writes a WLambda syntax representation of the given _value_ to a string.
This is useful for debugging purposes or in combination with `std:eval`.


```wlambda
std:assert_eq (std:write_str "foo")         "\"foo\"";
std:assert_eq (std:write_str $&&10)         "$&&10";
std:assert_eq (std:write_str $[1, 2, 3])    "$[1,2,3]"
```

Here an example in combination with `std:eval`:

```wlambda
!code =
    std:str:cat
        "$@i iter i "
        (std:write_str $[1, 2, 3, 4])
        " { $+ i }";

std:assert_eq (std:eval code) 10;
```

#### <a name="3104-isstr-value"></a>3.10.4 - is\_str _value_

Returns `$true` if _value_ is a string.

```wlambda
std:assert ~ is_str "foo";

std:assert ~ not ~ is_str $b"foo";
std:assert ~ not ~ is_str :foo;
std:assert ~ not ~ is_str 324;

std:assert ~ not ~ is_str $&&"foo";
std:assert ~ is_str $*$&&"foo";
```

#### <a name="3105-stdstrcat-a-b-"></a>3.10.5 - std:str:cat _a_ _b_ ...

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

#### <a name="3106-stdstrjoin-sep-vector"></a>3.10.6 - std:str:join _sep_ _vector_

Join's the stringified elements of _vector_ with the _sep_ string.
Will return an error if _vector_ is not a vector.

```wlambda
std:assert_eq
    (std:str:join "::" $[1,2,3])
    "1::2::3";
```

#### <a name="3107-stdstrlen-value"></a>3.10.7 - std:str:len _value_

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

#### <a name="3108-stdstrfind-pattern-string-offset---index--none"></a>3.10.8 - std:str:find _pattern_ _string_ \[_offset_\] -> _index_ | $none

Searches for the string _pattern_ in the _string_ and returns the 0 based position
in the string the given _pattern_ starts.
If no pattern was found `$none` is returned.

```wlambda
std:assert_eq (std:str:find "xyz" "abcxyz")         3;
std:assert_eq (std:str:find "xyz" "abcxyzxyz" 6)    6;
std:assert_eq (std:str:find "xyz" "abcxyzfooxyz" 6) 9;
```

#### <a name="3109-stdstrreplace-pattern-replacement-string"></a>3.10.9 - std:str:replace _pattern_ _replacement_ _string_

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

#### <a name="31010-stdstrreplacen-pattern-replacement-count-string"></a>3.10.10 - std:str:replace\_n _pattern_ _replacement_ _count_ _string_

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

#### <a name="31011-stdstrtrim-value"></a>3.10.11 - std:str:trim _value_

Trims off any (unicode) white space from the start and end of the
stringified _value_.

```wlambda
std:assert_eq
    (std:str:trim "\nfooo bar ")
    "fooo bar";
```

#### <a name="31012-stdstrtrimstart-value"></a>3.10.12 - std:str:trim\_start _value_

Trims off any (unicode) white space from the start of the stringified _value_.

```wlambda
std:assert_eq
    (std:str:trim_start "  \nfooo bar \n")
    "fooo bar \n";
```

#### <a name="31013-stdstrtrimend-value"></a>3.10.13 - std:str:trim\_end _value_

Trims off any (unicode) white space from the end of the stringified _value_.

```wlambda
std:assert_eq
    (std:str:trim_end "  \nfooo bar \n")
    "  \nfooo bar";
```

#### <a name="31014-stdstrpadstart-len-pad-str-value"></a>3.10.14 - std:str:pad\_start _len_ _pad-str_ _value_

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

## Empty _pad-str_ is not an error but a nop:
std:assert_eq
    (std:str:pad_start 8 "" "∑∑")
    "∑∑";

## also works with characters
std:assert_eq
    (std:str:pad_start 3 'x' "0")
    "xx0";
## also works with bytes
std:assert_eq
    (std:str:pad_start 3 $b'x' "0")
    "xx0";
```

#### <a name="31015-stdstrpadend-len-pad-str-value"></a>3.10.15 - std:str:pad\_end _len_ _pad-str_ _value_

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

## Empty _pad-str_ is not an error but a nop:
std:assert_eq
    (std:str:pad_end 8 "" "∑∑")
    "∑∑";

## also works with characters
std:assert_eq
    (std:str:pad_end 3 'x' "0")
    "0xx";
## also works with bytes
std:assert_eq
    (std:str:pad_end 3 $b'x' "0")
    "0xx";
```

#### <a name="31016-stdstrtobytes-string"></a>3.10.16 - std:str:to\_bytes _string_

Encodes _string_ in UTF-8 and returns a byte vector containing all it's bytes.

```wlambda
!b = std:str:to_bytes "1234";
std:assert_eq b $b"1234";

!b = std:str:to_bytes "Äß日本人";
std:assert_eq b $b"\xC3\x84\xC3\x9F\xE6\x97\xA5\xE6\x9C\xAC\xE4\xBA\xBA";
```

#### <a name="31017-stdstrtobyteslatin1-string"></a>3.10.17 - std:str:to\_bytes\_latin1 _string_

Encodes _string_ as bytes in Latin1 (ISO-8859-1) encoding and returns
a byte vector containing all it's bytes. If a character is outside the Latin1 Unicode
range, it will be replaced by a "?".

```wlambda
!b = std:str:to_bytes_latin1 "\u{FF}\u{F0}";
std:assert_eq b $b"\xFF\xF0";
!b = std:str:to_bytes_latin1 "\u{FE00}\u{FF}\u{3232}";
std:assert_eq b $b"?\xFF?";
```

#### <a name="31018-stdstrfromlatin1-byte-vector"></a>3.10.18 - std:str:from\_latin1 _byte-vector_

Converts the _byte-vector_ to a Unicode string, assuming Latin 1 (ISO-8859-1) encoding
and returns it.

```wlambda
!s = std:str:from_latin1 $b"Ä";
std:assert_eq s "\u{C3}\u{84}";

!s = std:str:from_latin1 $b"\xFF\xF0";
std:assert_eq s "\u{FF}\u{F0}";
```

#### <a name="31019-stdstrfromutf8-byte-vector"></a>3.10.19 - std:str:from\_utf8 _byte-vector_

Converts the _byte-vector_ to a Unicode string and returns it.
If the _byte-vector_ contains invalid UTF-8 sequences an
error value is returned.

```wlambda
!s = _? ~ std:str:from_utf8 $b"\xC3\x84\xC3\x9F\xE6\x97\xA5\xE6\x9C\xAC\xE4\xBA\xBA";
std:assert_eq s "Äß日本人";

!r = on_error {|| _ } ~ std:str:from_utf8 $b"\xFF\xFF";
std:assert_eq r "str:from_utf8 decoding error: invalid utf-8 sequence of 1 bytes from index 0";
```

#### <a name="31020-stdstrfromutf8lossy-byte-vector"></a>3.10.20 - std:str:from\_utf8\_lossy _byte-vector_

Converts the _byte-vector_ to a Unicode string and returns it.
If the _byte-vector_ contains invalid UTF-8 sequences a `"�"` will be
inserted.

```wlambda
!s = _? ~ std:str:from_utf8_lossy
    $b"\xC3\x84\xFF\xC3\x9F\xE6\x97\xA5\xE6\x9C\xAC\xE4\xBA\xBA\xFF\xFF\x00";
std:assert_eq s "Ä�ß日本人��\0";
```

#### <a name="31021-stdstrtocharvec-string"></a>3.10.21 - std:str:to\_char\_vec _string_

Converts the _string_ into a vector of integers which represent the Unicode
character number.

```wlambda
!v = std:str:to_char_vec "1234";
std:assert_eq (str v) ~ str $[49,50,51,52];

!v = std:str:to_char_vec "Äß日本人";
std:assert_eq (str v) ~ str $[196,223,0x65E5,0x672C,0x4EBA];
```

#### <a name="31022-stdstrfromcharvec-vector"></a>3.10.22 - std:str:from\_char\_vec _vector_

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

#### <a name="31023-stdstrtolowercase-string"></a>3.10.23 - std:str:to\_lowercase _string_

Swaps all (Unicode) characters in _string_ to their lowercase version.

```wlambda
std:assert_eq (std:str:to_lowercase "ZABzabÄßÜÖ") "zabzabäßüö";
```

#### <a name="31024-stdstrtouppercase-string"></a>3.10.24 - std:str:to\_uppercase _string_

Swaps all (Unicode) characters in _string_ to their lowercase version.

```wlambda
std:assert_eq (std:str:to_uppercase "ZABzabäßüö") "ZABZABÄSSÜÖ";
```

#### <a name="31025-stdstrstriputf8bom-string-or-bytes"></a>3.10.25 - std:str:strip\_utf8\_bom _string-or-bytes_

Strips the sequence `$b"\xEF\xBB\xBF"` from the start of either the _string_ or the _bytes_
vector given at the input and returns it as utf8 encoded string.

This is useful in combination with eg. `std:deser:csv`.

```wlambda
std:assert_eq (std:str:strip_utf8_bom $b"\xEF\xBB\xBF") "";
std:assert_eq (std:str:strip_utf8_bom (std:str:from_utf8 $b"\xEF\xBB\xBF")) "";
```

#### <a name="31026-stdstreditdistance-str-a-strb"></a>3.10.26 - std:str:edit\_distance _str-a_ _str\_b

Calculates the Levenshtein distance between two (Unicode) strings.

```wlambda
std:assert_eq (std:str:edit_distance "aaa" "aba") 1;
```

### <a name="311-byte-vectors"></a>3.11 - Byte Vectors

Bytes (plural of Byte) are a vector of bytes. Unlike strings they don't have any encoding.
Literal syntax however supports inserting unicode characters:


```wlambda
$b"abc";
$b"\xFF\xFD\x00";
$Q/ABCDEF\xFD/;      # \xFD is not an escape sequence here!
```

#### <a name="3111-call-properties-of-bytes"></a>3.11.1 - Call Properties of Bytes

You can index inside a byte array by calling it with an integer:

```wlambda
std:assert_eq ($b"ABC" 1) $b'B';
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

If you call bytes with a pair as argument, you can do a multitude of
operations, from replacement to finding a byte:

```wlambda
## replacing substrings:
std:assert_eq ($b"a,b,c,d" $p($b',', $b';')) $b"a;b;c;d";
std:assert_eq ($b"a,b,c,d" $p($b"a,", $b"XXX")) $b"XXXb,c,d";
## also works with strings and chars:
std:assert_eq ($b"a,b,c,d" $p("a,", "XXX")) $b"XXXb,c,d";
std:assert_eq ($b"a,b,c,d" $p("a,", 'O')) $b"Ob,c,d";

## finding a character/byte:
std:assert_eq ($b"a,b,c,d" $p(0, $b'c')) 4;
std:assert_eq ($b"a,b,c,d" $p(0,   'c')) 4;

## splitting:
std:assert_str_eq ($b"A\<SOH>B\<SOH>C" $p($b'\<SOH>', 0)) $[$b"A", $b"B", $b"C"];
```

See also the section [Calling Semantics of Data Types](#319-calling-semantics-of-data-types).

#### <a name="3112-byte-conversion-functions"></a>3.11.2 - Byte Conversion Functions

You can convert bytes to strings in a multitude of ways:

- str _bytes_
  ```wlambda
  std:assert_eq (str $b"abc")        "abc";
  std:assert_eq (str $b"abc\xFF")    "abcÿ";
  std:assert_eq (str $Q/ABCDEF\xFD/) "ABCDEF\\xFD";
  ```
- std:bytes:to\_hex _bytes_ \[_group-len_ \[_group-sep_]]
  ```wlambda
  std:assert_eq (std:bytes:to_hex $b"\xFF\x0A\xBE\xEF")
                "FF0ABEEF";
  std:assert_eq (std:bytes:to_hex $b"\xFF\x0A\xBE\xEF" 2)
                "FF 0A BE EF";
  std:assert_eq (std:bytes:to_hex $b"\xFF\x0A\xBE\xEF" 2 ":")
                "FF:0A:BE:EF";
  ```
- std:str:from\_latin1 _bytes_
  ```wlambda
  std:assert_eq (std:str:from_latin1 $b"\xFF\xF0") "\u{FF}\u{F0}";
  ```
- std:str:from\_utf8 _bytes_
  ```wlambda
  std:assert_eq (std:str:from_utf8 $b"\xC3\xA4\xC3\x9F\xC3\xBF") "äßÿ";
  std:assert_eq (std:str:from_utf8 [std:str:to_bytes "äßÿ"])         "äßÿ";
  ## broken UTF8 will result in an error:
  std:assert ~ is_err (std:str:from_utf8 $b"\xC3\xC3\xA4\xC3\x9F\xC3\xBF");
  ```
- std:str:from\_utf8\_lossy _bytes_
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

#### <a name="3113-isbytes-value"></a>3.11.3 - is\_bytes _value_

Returns `$true` if _value_ is a byte vector.

```wlambda
std:assert ~ is_bytes $b"ABC";
std:assert ~ not ~ is_bytes "ABC";
```

#### <a name="3114-stdbytesfind-pattern-string-offset---index--none"></a>3.11.4 - std:bytes:find _pattern_ _string_ \[_offset_\] -> _index_ | $none

Searches for the string _pattern_ in the _string_ and returns the 0 based position
in the string the given _pattern_ starts.
If no pattern was found `$none` is returned.

```wlambda
std:assert_eq (std:bytes:find $b"xyz" $b"abcxyz")         3;
std:assert_eq (std:bytes:find $b"xyz" $b"abcxyzxyz" 6)    6;
std:assert_eq (std:bytes:find $b"xyz" $b"abcxyzfooxyz" 6) 9;
```

#### <a name="3115-stdbytesreplace-byte-vector-pattern-replacement"></a>3.11.5 - std:bytes:replace _byte-vector_ _pattern_ _replacement_

Replaces all occurences of _pattern_ in _byte-vector_ with _replacement_.

```wlambda
std:assert_eq
    (std:bytes:replace $b"XXX\x01\x02\x03OOO" $b"\x01\x02\x03" $b"---")
    $b"XXX---OOO";

std:assert_eq
    (std:bytes:replace $b"XXX\x01\x02\x03OOO" $b"\x01\x02\x03" $b"")
    $b"XXXOOO";

std:assert_eq
    (std:bytes:replace $b"XXX\x01\x02\x03OOO" $b"\x01\x02\x03" $b"\xFF\xFF\xFF\xFF")
    $b"XXX\xFF\xFF\xFF\xFFOOO";
```

#### <a name="3116-stdbytesfromhex-string-with-hex-chars"></a>3.11.6 - std:bytes:from\_hex _string-with-hex-chars_

This function decodes a string of hex characters into a byte vector.

```wlambda
!bv = std:bytes:from_hex "62797465";
std:assert_eq bv $b"byte";
```

#### <a name="3117-stdbytesfromvec-vector-of-ints"></a>3.11.7 - std:bytes:from\_vec _vector-of-ints_

Decodes a vector of integers in the range 0-255 into a byte vector. If an
integer is larger than 255 don't expect a sensible result. But it will most
likely just wrap around.

```wlambda
std:assert_eq
    (std:bytes:from_vec $[1,2,3,0x62,0x79,0x74,0x65])
    $b"\x01\x02\x03byte";
```

#### <a name="3118-stdbytestohex-byte-vector"></a>3.11.8 - std:bytes:to\_hex _byte-vector_

Converts the given byte vector to a string of hex encoded bytes.

```wlambda
std:assert_eq
    (std:bytes:to_hex $b"byte")
    "62797465";
```

#### <a name="3119-stdbytestobase64-byte-vector-config---string"></a>3.11.9 - std:bytes:to\_base64 _byte-vector_ \[_config_\] -> _string_

Converts the given byte vector to a Base64 encoded string. With _config_ you can
define the encoding style:

- `:std`
- `:std_no_pad`
- `:url`
- `:url_no_pad`

```wlambda
std:assert_eq
    (std:bytes:to_base64 $b"\x00\xFF")
    "AP8=";
std:assert_eq
    (std:bytes:to_base64 "test")
    "dGVzdA==";
std:assert_eq
    (std:bytes:to_base64 "test" :std_no_pad)
    "dGVzdA";
std:assert_eq
    (std:bytes:from_base64 (std:bytes:to_base64 "test"))
    $b"test";
std:assert_eq
    (std:bytes:from_base64 (std:bytes:to_base64 "test" :url) :url)
    $b"test";
```

#### <a name="31110-stdbytesfrombase64-string-config---byte-vector"></a>3.11.10 - std:bytes:from\_base64 _string_ \[_config_\] -> _byte-vector_

Converts the given Base64 encoded string to a byte vector. With _config_ you can
define the encoding style, see also `to_base64` for a list of possible values.

```wlambda
std:assert_eq
    (std:bytes:from_base64 "AP8=")
    $b"\x00\xFF";
std:assert_eq
    (std:bytes:from_base64 "dGVzdA")
    $b"test";
```

#### <a name="31111-stdbytestovec-byte-vector"></a>3.11.11 - std:bytes:to\_vec _byte-vector_

Converts the given byte vector to a vector of integers in the range 0-255.

```wlambda
std:assert_str_eq
    (std:bytes:to_vec $b"byte")
    $[98, 121, 116, 101];
```

#### <a name="31112-stdbytespack-pack-format-string-list-of-values"></a>3.11.12 - std:bytes:pack _pack-format-string_ _list-of-values_

Returns a byte vector containing the values of _list-of-values_
serialized in binary form (packed) according to the given _pack-format-string_.

If the syntax of the _pack-format-string_ has errors, an error value is returned.

See also the section [Format String Syntax](#133-format-string-syntax-for-stdbytespack-and-stdbytesunpack)
for a description of the syntax for _pack-format-string_.

This function is very useful for constructing binary data for file formats and
network protocols.

```wlambda
std:assert_eq
    (std:bytes:pack "> i16 x s8 x f" $[1, "test", 0.5])
    $b"\0\x01\0\x04test\0?\0\0\0";
```

#### <a name="31113-stdbytesunpack-pack-format-string-byte-vector"></a>3.11.13 - std:bytes:unpack _pack-format-string_ _byte-vector_

Decodes the given _byte-vector_ according to the _pack-format-string_ and returns it
as list of values.

If the syntax of the _pack-format-string_ has errors or the given _byte-vector_
is too short, an error value is returned.

See also the section [Format String Syntax](#133-format-string-syntax-for-stdbytespack-and-stdbytesunpack)
for a description of the syntax for _pack-format-string_.

```wlambda
std:assert_str_eq
    (std:bytes:unpack
        "< i16 x c3 s16 x y"
        $b"\x10\x00\x00ABC\x02\x00XY\x00This is the rest")
    $[16, $b"ABC", $b"XY", $b"This is the rest"];
```

#### <a name="31114-stdbyteslzwencode-bytes-or-string-bitsize-bitorder---bytes--error"></a>3.11.14 - std:bytes:lzw:encode _bytes-or-string_ [_bitsize_ [_bitorder_]] -> _bytes_ | $error

This function encodes the given _bytes-or-string_ using the LZW algorithm. You can
specify a _bitsize_ which is by default 9. It has to be within (or equal) 2 and 12.
You can also specific the bitorder (`:msb` or `:lsb`).

If the _bitsize_ is not sufficient for encoding the given data, an `$error` is returned.

```wlambda
std:assert_eq len[std:bytes:lzw:encode $b"123123123123123123123123123123123123"] 20;
std:assert_eq len[std:bytes:lzw:encode $b"123123123123123123123123123123123123" 6] 14;

!text = "123123123123123123123123123123123123";
!data = std:bytes:lzw:encode text;

std:assert_eq std:str:from_utf8[std:bytes:lzw:decode[data]] text;
```

#### <a name="31115-stdbyteslzwdecode-bytes-bitsize-bitorder---bytes--error"></a>3.11.15 - std:bytes:lzw:decode _bytes_ [_bitsize_ [_bitorder_]] -> _bytes_ | $error

This function decodes the given _bytes_ and decodes them according to the LZW algorithm.
Make sure to set the right _bitsize_ and _bitorder_!

If there is some error in the data, an `$error` is returned.

The decoded data as bytes is returned otherwise. Here is an example:

```wlambda
!text = "123123123123123123123123123123123123";
!data = std:bytes:lzw:encode text 6;
std:assert_eq len[data] 14;

std:assert_eq std:str:from_utf8[std:bytes:lzw:decode[data, 6]] text;
```

### <a name="312-symbols"></a>3.12 - Symbols

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

## They don't match with strings:
std:assert_eq ((x == "ON") { 10 } { 20 }) 20;

## Work together nicely with `match`:

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

The collection of dead symbols is also run automatically for every 100th newly
allocated symbol.

#### <a name="3121-sym-value"></a>3.12.1 - sym _value_

Casts the given _value_ into a symbol.

```wlambda
std:assert_eq (sym "a")     :a;
std:assert_eq (sym $b"a")   :a;
std:assert_eq (sym $[])     :"$[]";
std:assert_eq (sym 10)      :10;
```

#### <a name="3122-issym-value"></a>3.12.2 - is\_sym _value_

Returns `$true` if the _value_ is symbol.

```wlambda
std:assert ~ is_sym :a;
std:assert ~ is_sym ~ sym "a";
std:assert ~ is_sym ~ sym "a";
std:assert ~ is_sym ~ sym $b"a";

std:assert ~ not ~ is_sym "a";
std:assert ~ not ~ is_sym $b"a";
std:assert ~ not ~ is_sym $&&:a;
std:assert ~ not ~ is_sym ${};
std:assert ~ not ~ is_sym $none;
std:assert ~ not ~ is_sym $true;
```

#### <a name="3123-stdsymbolscollect"></a>3.12.3 - std:symbols:collect

Collect and remove all interned symbols in the current thread that are no
longer used. Returns the number of freed symbols. Please keep in mind, that
the `std:ref_id` of any collected symbol will be different from a symbol that
is created later with the same characters.

The collection of dead symbols is also run automatically for every 100th newly
allocated symbol.

If you rely on the reference ID of a symbol, you should make sure to keep it
around. Literal symbols are always kept around as long as the code is running
or referenced somewhere (eg. by a function).


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

### <a name="313-syntax-block"></a>3.13 - Syntax `$%:Block`

A syntax element is an element of an abstract syntax tree as it is returned
by `std:wlambda:parse` for instance. They carry the type of syntax node
and debug information with them.

#### <a name="3131-stdsynpos-syntax"></a>3.13.1 - std:syn:pos _syntax_

Returns the position of the syntax element in the source code.

```wlambda
!ast = std:wlambda:parse "1 + 2";
!add_syntax = ast.1.0;

std:assert_str_eq
    (std:syn:pos add_syntax)
    $["<wlambda:parse:input>", 1, 3];
```

#### <a name="3132-stdsyntype-syntax"></a>3.13.2 - std:syn:type _syntax_

Converts the syntax element to a symbol, so you can determine it's type:

```wlambda
!ast = std:wlambda:parse "1 + 2";
!add_syntax = ast.1.0;

std:assert_str_eq
    (std:syn:type add_syntax)
    :BinOpAdd;
```

### <a name="314-pairs-pa-b"></a>3.14 - Pairs `$p(a, b)`

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

## Pairs are often used to represent map entries,
## so you can use `key` and `value`
## and the short forms `k` and `v` too:
std:assert_eq v.value 11;
std:assert_eq v.key   12;

std:assert_eq v.v     11;
std:assert_eq v.k     12;
```

Comparison does happen by their contents:

```wlambda
std:assert $p(1, 2) == $p(1, 2);
std:assert $p(2, 2) != $p(1, 2);

## In contrast to vectors:
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

#### <a name="3141-pair-operator-a--b"></a>3.14.1 - Pair Operator `a => b`

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

#### <a name="3142-cons-a-b"></a>3.14.2 - cons _a_ _b_

Creates a new pair from the values _a_ and _b_.

```wlambda
!p = cons 3 4;

std:assert_eq p $p(3, 4);
```

#### <a name="3143-pair-stringbyte-vector-operations"></a>3.14.3 - Pair string/byte vector operations

If you call a pair with a string or byte vector as argument, there are some
operations that can be done:

##### <a name="31431-p-from--count--string-or-byte-vec"></a>3.14.3.1 - $p( _from_ , _count_ ) _string-or-byte-vec_

Returns a substring starting at _from_ with the length _count_.

```wlambda
std:assert_eq ($p(2, 4) "abcdefgh") "cdef";
```

The same works for byte vectors:

```wlambda
std:assert_eq ($p(2, 4) $b"abcdefgh") $b"cdef";
```

##### <a name="31432-p-pattern--replacement--string-or-byte-vec"></a>3.14.3.2 - $p( _pattern_ , _replacement_ ) _string-or-byte-vec_

Replaces all _pattern_ occurences in _string_ by _replacement_.

```wlambda
std:assert_eq ($p(";", "_") "A;B;D;EFG;HI") "A_B_D_EFG_HI";
```

The same works for byte vectors:

```wlambda
std:assert_eq ($p($b";", $b"_") $b"A;B;D;EFG;HI") $b"A_B_D_EFG_HI";
```

##### <a name="31433-p-split-pattern--max--string-or-byte-vec"></a>3.14.3.3 - $p( _split-pattern_ , _max_ ) _string-or-byte-vec_

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

#### <a name="3144-pair-to-iterator"></a>3.14.4 - Pair to Iterator

Pairs play a special role if you make an iterator from it.
It can be used to create a specialized iterator that only
iterates over keys or values of a map. Or that enumerates
a vector or map.

##### <a name="31441-iter---range"></a>3.14.4.1 - Iter - Range

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

##### <a name="31442-iter---enumerate"></a>3.14.4.2 - Iter - Enumerate

If the first value of the pair is `:enumerate`
it will enumerate entries in a map or values in a vector.

```wlambda
!v = $[];

## $iter is only explicit here for demonstration
## purposes! `iter` will make an iter from the pair
## if you don't pass one!
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

##### <a name="31443-iter---values"></a>3.14.4.3 - Iter - Values

This is useful for iterating over the values in a map in an undefined order:

```wlambda
!m = ${ a = 10, b = 20, c = 33 };

!sum = $@i iter v $p(:values, m) ~ $+ v;

std:assert_eq sum 63;
```

##### <a name="31444-iter---keys"></a>3.14.4.4 - Iter - Keys

You can also iterate over map keys in an undefined order:

```wlambda
!m = ${ :10 = :c, :20 = :b, :30 = :a };

!sum = $@i iter v $p(:keys, m) ~ $+ v;

std:assert_eq sum 60;
```

#### <a name="3145-ispair-value"></a>3.14.5 - is\_pair _value_

Checks if _value_ is a pair.

```wlambda
std:assert ~ is_pair $p(1, 2);
std:assert not ~ is_pair $[1, 2];
std:assert not ~ is_pair $i(1, 2);
```

### <a name="315-vectors-or-lists"></a>3.15 - Vectors (or Lists)

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

## Index calling:
std:assert_eq (0 some_vec) 1;
std:assert_eq (1 some_vec) 20;
std:assert_eq (2 some_vec) 30;

## Field syntax:
std:assert_eq some_vec.0 1;
std:assert_eq some_vec.1 20;
std:assert_eq some_vec.2 30;
```

To add elements to a vector, you can use the prepend and append operators `+>`
and `<+` too:

```wlambda
!v = $[1];

0 <+ v;
v +> 2;

std:assert_str_eq v $[0,1,2];
```

#### <a name="3151-stdpush-vector-item"></a>3.15.1 - std:push _vector_ _item_

Pushes _item_ to the end of _vector_. Returns _item_.
Be aware, that there is also the `+>` operator, that will append elements
to a vector.

```wlambda
!v = $[1,2];

std:push v 3;

std:assert_eq (str v) (str $[1,2,3]);
```

#### <a name="3152-stdpop-vector"></a>3.15.2 - std:pop _vector_

Pops off the last element of _vector_. Returns `$none` if the vector is empty
or if _vector_ is not a vector.

```wlambda
!v = $[1,2,3];

std:assert_eq (std:pop v) 3;
std:assert_eq (str v) (str $[1,2]);
```

#### <a name="3153-stdunshift-vector-item"></a>3.15.3 - std:unshift _vector_ _item_

Inserts _item_ at the front of _vector_. Returns _item_ and mutates _vector_
inplace. Be aware that this operation is of O(n) complexity.
Be aware, that there is also the `<+` operator, that will prepend elements
to a vector (with O(n) complexity however).

```wlambda
!v = $[1,2];

std:unshift v 3;

std:assert_eq (str v) (str $[3,1,2]);
```

#### <a name="3154-isvec-value"></a>3.15.4 - is\_vec _value_

Returns `$true` if _value_ is a vector.

```wlambda
std:assert ~ is_vec $[];
std:assert ~ is_vec $[1,2,3];

std:assert ~ not ~ is_vec 0;
std:assert ~ not ~ is_vec $none;
std:assert ~ not ~ is_vec $true;
std:assert ~ not ~ is_vec $p(1,2);
std:assert ~ not ~ is_vec $i(1,2);
std:assert ~ not ~ is_vec $f(1,2);
std:assert ~ not ~ is_vec ${a = 10};
```

#### <a name="3155-vector-splicing"></a>3.15.5 - Vector Splicing

You can splice vectors directly into their literal form with the `$[..., * vec_expr, ...]`
syntax. Here is an example:

```wlambda
!make_some = { $[_ + 1, _ + 2] };

!some_vec = $[ 0, *make_some 1 ];

std:assert_eq some_vec.1 2;
std:assert_eq some_vec.2 3;

## There can be any expression after the `.` if you wrap it into `(...)`:
std:assert_eq some_vec.(1 + 1) 3;

## A more direct example:
std:assert_eq (str $[1,2,*$[3,4]]) "$[1,2,3,4]";
```

#### <a name="3156-stdappend-vec-a-value-or-vec-"></a>3.15.6 - std:append _vec-a_ _value-or-vec_ ...

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

#### <a name="3157-stdprepend-vec-a-value-or-vec-"></a>3.15.7 - std:prepend _vec-a_ _value-or-vec_ ...

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

#### <a name="3158-stdtake-count-vector"></a>3.15.8 - std:take _count_ _vector_

Takes and returns the first _count_ elements of _vector_. Does not
mutate _vector_.

```wlambda
!v = $[1,2,3,4,5,6];

!t = std:take 4 v;

std:assert_eq (str v) "$[1,2,3,4,5,6]";
std:assert_eq (str t) "$[1,2,3,4]";
```

#### <a name="3159-stddrop-count-vector"></a>3.15.9 - std:drop _count_ _vector_

Drops _count_ elements from _vector_ and returns them as new vector.
Does not mutate _vector_.

```wlambda
!v = $[1,2,3,4,5,6];

!t = std:drop 4 v;

std:assert_eq (str v) "$[1,2,3,4,5,6]";
std:assert_eq (str t) "$[5,6]";
```

### <a name="316-associative-maps-or-string-to-value-mappings"></a>3.16 - Associative Maps (or String to Value mappings)

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

## Symbol calling:
std:assert_eq (:a some_map) 1;
std:assert_eq (:b some_map) 2;
std:assert_eq ("a" some_map) 1;
std:assert_eq ("b" some_map) 2;

## Field syntax:
std:assert_eq some_map.a 1;
std:assert_eq some_map.b 2;

## There can be any expression after the `.` if you wrap it into `(...)`,
## also strings:
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

#### <a name="3161-map-splicing"></a>3.16.1 - Map Splicing

Like vectors you can splice map values directly into map literals:

```wlambda
!map_gen = { ${ (std:str:cat "_" _) = _ } };

!some_map = ${ a = 10, *map_gen "x" };

std:assert_eq some_map.a 10;
std:assert_eq some_map._x "x";

std:assert_eq (str ${*${a=10}}) "${a=10}";

## As a reminder, a full expression can come after the '*':

std:assert_eq (str ${*map_gen "y"}) $q/${_y="y"}/;
```

#### <a name="3162-ismap-value"></a>3.16.2 - is\_map _value_

Returns `$true` if _value_ is a map.

```wlambda
std:assert ~ is_map ${};
std:assert ~ is_map ${a = 10};

std:assert ~ not ~ is_map $&&${};
std:assert ~ not ~ is_map $&${};
std:assert ~ not ~ is_map $[:a, 10];
std:assert ~ not ~ is_map $p(:a, 10);
std:assert ~ not ~ is_map $none;
std:assert ~ not ~ is_map $true;
```

### <a name="317-references"></a>3.17 - References

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

#### <a name="3171-stdtoref-value"></a>3.17.1 - std:to\_ref _value_

Creates a new strong reference that refers to a cell that stores _value_.

```wlambda
!x = std:to_ref 10;

std:assert_eq (std:ser:wlambda x) "$&&10";

std:assert_eq $*x 10;
```

#### <a name="3172-stdrefweaken-ref"></a>3.17.2 - std:ref:weaken _ref_

You can weaken any of those two types of references manually using the
`std:ref:weaken` function.

```wlambda
!drop_check = $& $f;

## Set `drop_check` to $true when all (non weak) references to it are gone.
!x = $&& (std:to_drop {|| .drop_check = $true });

## Create a weakened reference to the value referred to by x:
!y = std:ref:weaken x;

## The reference to the drop function is removed and this means
## that the weak reference in y is invalidated and returns $n in future.
.x = $n;

## Deref y now gives you $n:
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

#### <a name="3173-stdrefhide-value"></a>3.17.3 - std:ref:hide _value_

Creates a hidden reference from a given value or reference.

```wlambda
!r = $&& 10;            # strong ref to value 10

## hide the reference for direct access via local variables
!h = std:ref:hide r;

.h += 11;

std:assert_eq $*r       21;
std:assert_eq h         21;
std:assert_eq (std:write_str $:h) "$&&21";

std:assert_eq (std:write_str $[r, $:h]) "$[$<1=>$&&21,$<1>]";
```

#### <a name="3174-isref-value"></a>3.17.4 - is\_ref _value_

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

#### <a name="3175-iswref-value"></a>3.17.5 - is\_wref _value_

Returns `$true` if _value_ is a weak reference.

```wlambda
!x = $&& 10;
!y = std:ref:weaken x;
std:assert ~ is_wref y;
std:assert ~ not ~ is_wref x;
```

#### <a name="3176-stdrefstrengthen-ref"></a>3.17.6 - std:ref:strengthen _ref_

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

#### <a name="3177-stdrefset-ref-value"></a>3.17.7 - std:ref:set _ref_ _value_

Sets the value of the reference _ref_ to _value_.
If _ref_ is not a strong, hidden or weak reference nothing happens.

Returns _value_ or `$none`.

```wlambda
!r1 = $&&1;
std:ref:set r1 10;
std:assert_eq $*r1 10;

## Note that $& references in local variables are
## automatically dereferenced. Because of that we need to wrap it into
## an extra reference.
!r2 = $& $& 1;
std:ref:set r2 11;
std:assert_eq $*r2 11;

!r3 = $& $& 1;
!w3 = std:ref:weaken r3;
std:ref:set w3 14;      # Set reference via the weak reference in w3 to r3.
std:assert_eq $*r3 14;
```

### <a name="318-iterators-iter-expression"></a>3.18 - Iterators $iter _expression_

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

By passing an iterator function to `$iter` or `iter` you can
iterate over returned values from a WLambda function/closure:

```wlambda
!counter = 0;
!generator = {
    .counter = counter + 1;
    if counter > 10
        { $o() }
        { $o(counter) }
};

!sum = 0;
iter i generator {
    .sum = sum + i;
};

std:assert_eq sum 55;
```

#### <a name="3181-iterator-kinds"></a>3.18.1 - Iterator Kinds

Here is a table of the behaviour of iterators created from WLambda data.

| Data      | Iterator return values |
|-----------|-----------|
| vector  | Each element of the vector. |
| map  | Each key/value pair of the map in undefined order. |
| `$none`   | Returns nothing  |
| optional | Returns the optional value on first invocation. |
| `$o()` | Returns nothing. |
| function | Treats the given function as generator: The function should return a value wrapped in an optional value `$o(...)`. If no further values can be generated `$o()` should be returned. |
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
| `$p(iterator, x)` | Returns a zip operation of the elements returned by the iterator and the newly created iterator `$iter x`. |

#### <a name="3182-iterators-on-mutated-data"></a>3.18.2 - Iterators on mutated data

Iterators hold a reference to the collection values. That means, if you mutate
a vector while you iterate over it, it will not crash but it might produce
weird effects.

```wlambda
!v = $[1,2,3];
!it = $iter v;

iter i v {
    if i <= 3 {
        std:push v i + 10;  # This is not recommended however...
    };
};

std:assert_eq (str v) (str $[1, 2, 3, 11, 12, 13]);
```

This will also work for maps, but as the order of the map entries
is undefined it will produce very indeterministic effects and it's really
not recommended.

#### <a name="3183-splicing-an-iterator"></a>3.18.3 - Splicing an Iterator

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

#### <a name="3184-calling-an-iterator-with-a-function"></a>3.18.4 - Calling an Iterator with a Function

When an iterator gets called with a function as first argument
it will repeatedly call that function until no more values are
available:

```wlambda
!it = $iter $[1,2,3];

!sum = 0;

it { .sum = sum + _ };

std:assert_eq sum 6;
```

#### <a name="3185-zip-iterators"></a>3.18.5 - Zip Iterators

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

#### <a name="3186-isiter-value"></a>3.18.6 - is\_iter _value_

Returns `$true` if _value_ is an iterator.

```wlambda
std:assert   (is_iter $iter $n);
std:assert   (is_iter $iter 0 => 30);
std:assert   not <& (is_iter $[1,2,3]);
std:assert   (is_iter $iter $[1,2,3]);

std:assert   (not <& is_iter <& $true);
std:assert   (not <& is_iter <& $false);
std:assert   (not <& is_iter <& 4);
std:assert   (not <& is_iter <& $p(1, 2));
```

### <a name="319-calling-semantics-of-data-types"></a>3.19 - Calling Semantics of Data Types

You can call almost all basic data types of WLambda.
Here is an overview of the data type calling semantics:

| Type                         | Args                         | Semantics |
|------------------------------|------------------------------|-----------|
| `$none`                      | -                            | Any call to `$none` will result in a panic. |
| `$error`                     | -                            | Any call to `$error` will result in a panic. |
| function                     | *                            | Will call the function with the specified arguments. |
| `$true`                      | `f1, f2`                     | Will call `f1`.          |
| `$false`                     | `f1, f2`                     | Will call `f2` or return `$n` if `f2` is not provided.          |
| `$true`                      | `$[1,2]`                     | Will return the second element `2` of the list. |
| `$false`                     | `$[1,2]`                     | Will return the first element `1` of the list. |
| integer                      | vector, string, byte_vec, iterator | Will return the element at the given integer index. |
| symbol                       | map, userval                 | Will retrieve the value in the map at the key equal to the symbol. |
| map                          | anything                     | Will call `anything` for each value and key in the map and return a list with the return values. |
| string                       | string, byte_vec, char or byte | Append operation, works with multiple arguments. |
| byte_vec                     | string, byte_vec, char or byte | Append operation, works with multiple arguments. |
| string                       | `$p(int_offs, string/byte_vec/char/byte)` | Find operation, search from int_offs for the given string, byte_vec, char or byte. See also `std:str:find` and `std:bytes:find`. |
| byte_vec                     | `$p(int_offs, string/byte_vec/char/byte)` | Find operation, search from int_offs for the given string, byte_vec, char or byte. See also `std:str:find` and `std:bytes:find`. |
| `$p(char or byte, int_count)`| -                            | Create a string or byte_vec containing int_count of copies. |
| `$p(int_offs, string/byte_vec/char/byte)` | string          | Find operation, search from int_offs for the given string, byte_vec, char or byte. |
| `$p(int_offs, string/byte_vec/char/byte)` | byte_vec        | Find operation, search from int_offs for the given string, byte_vec, char or byte. |
| `$p(int_from, int_count)`     | vector                      | Vector slice operation. |
| `$i(int_from, int_count)`     | vector                      | Vector slice operation. |
| `$p(int_from, int_count)`     | numeric vector              | Creates a vector slice from a numeric vector. |
| `$i(int_from, int_count)`     | numeric vector              | Creates a vector slice from a numeric vector. |
| `$p(int_from, int_count)`     | iterator                    | Iterator result list slice operation. |
| `$i(int_from, int_count)`     | iterator                    | Iterator result list slice operation. |
| `$p(int_from, int_count)`     | string                      | Substring operation. (See also section about pairs) |
| `$i(int_from, int_count, ...)`| string                      | Substring operation. |
| `$p(int_from, int_count)`     | byte_vec                    | Substring operation. (See also section about pairs) |
| `$i(int_from, int_count, ...)`| byte_vec                    | Substring operation on the byte vector. |
| string                       |`$i(int_from, int_count, ...)`| Substring operation. |
| byte_vec                     |`$i(int_from, int_count, ...)`| Substring operation on the byte vector. |
|`$p(string, int)`             | string                       | Split operation. (See also section about pairs) |
|`$p(byte_vec, int)`           | byte_vec                     | Split operation. (See also section about pairs) |
| string                       |`$p(string, int)`             | Split operation. |
| byte_vec                     |`$p(byte_vec, int)`           | Split operation. |
| string                       |`$p(string, string)`          | Replace all operation. |
| byte_vec                     |`$p(byte_vec, byte_vec)`      | Replace all operation. |
| `$p(string, string)`         | string                       | Replace all operation. (See also section about pairs) |
| `$p(byte_vec, byte_vec)`     | byte_vec                     | Replace all operation. (See also section about pairs) |
| `$p(pat_char, repl_char)`    | string or byte_vec           | Replace all pat_char in string or byte_vec with repl_char. |
| `$p(pat_byte, repl_byte)`    | string or byte_vec           | Replace all pat_char in string or byte_vec with repl_char. |
| `$p(char, char)`             | char or byte                 | Range check operation, whether char or byte is inside to/from range. |
| `$p(byte, byte)`             | char or byte                 | Range check operation, whether char or byte is inside to/from range. |
| `$p(int_a, int_b)`           | char or byte                 | Range check operation, whether char or byte is inside to/from range. |
| `$i(int_a, int_b)`           | char or byte                 | Range check operation, whether char or byte is inside to/from range. |
| char or byte                 |`$p(char, char)`              | Range check operation, whether char or byte is inside to/from range. |
| char or byte                 |`$p(byte, byte)`              | Range check operation, whether char or byte is inside to/from range. |
| char or byte                 |`$p(int_a, int_b)`            | Range check operation, whether char or byte is inside to/from range. |
| char or byte                 |`$i(int_a, int_b)`            | Range check operation, whether char or byte is inside to/from range. |
| `$o()`                       | -                            | Returns $none. |
| `$o(x)`                      | -                            | Returns _x_. |
| `$o()`                       | *                            | Calls $none with arguments, leading to a panic. |
| `$o(x)`                      | *                            | Calls _x_ with the given arguments. |
|                              |                              | |

## <a name="4-conditional-execution---if--then--else"></a>4 - Conditional Execution - if / then / else

### <a name="41-if-condition-then-expr-else-expr"></a>4.1 - if/? _condition_ _then-expr_ [_else-expr_]

The keyword for conditional execution is either `if` or just the question mark `?`.
It takes 3 arguments: The first is an expression that will be evaluated
and cast to a boolean. If the boolean is `$true`, the second argument is
evaluated. If the boolean is `$false` the thrid argument is evaluated.
The third argument is optional.

```wlambda
!x = 10;

!msg = "x is ";
if x > 4 {
    .msg = std:str:cat msg "bigger than 4";
} {
    .msg = std:str:cat msg "smaller than or equal to 4";
};

std:assert_eq msg "x is bigger than 4";

## You may also use `if` in case it suits your coding style better:
if x == 10 {
    std:assert $true;
} {
    std:assert $false;
};
```

The _condition_ can also be a function block, which will be evaluated:

```wlambda
!res =
    if { !x = 2; x > 1 } "x > 1";

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

See also [8.1.1](#811-match-value-expr-match-pair1--default-expr) for a more
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

## Without the rebinding of the variable `i`, `i` would be captured as hidden
## reference and each iteration would update the contents of that reference.
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

## 40 is not in the set because the accumulation of 0.01 results
## in a value slightly above 0.4 and ends the range iteration:
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
are compiled like the bodies of `while`, `iter`, `match` and `if` into a runtime
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

## or even this:
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

std:assert_eq (str str_chars) (str $['a', 'b', 'c']);
```
- Symbols
```wlambda
!str_chars = $[];

for :abc {
    std:push str_chars _;
};

std:assert_eq (str str_chars) (str $['a', 'b', 'c']);
```

#### <a name="524-map-function-iterable"></a>5.2.4 - map _function_ _iterable_

Maps anything that is _iterable_ by calling _function_ with each item as
first argument and collecting the return values in a vector.

If a map is passed as _iterable_ then _function_ is called with two arguments,
the first being the map entry value and the second the key.
Note: When iterating over maps, don't assume any order.

It is very similar to `$@vec iter i <iterable> { $+ ... }`.

```wlambda
## Lists:

std:assert_str_eq
    (map { float[_] / 2.0 } $[1,2,3,4,5])
    $[0.5, 1, 1.5, 2, 2.5];

std:assert_str_eq
    (map { _ * 10 } $[$b'a', $b'b', $b'c', 10, 20])
    ($@vec
        iter i $[$b'a', $b'b', $b'c', 10, 20] {
            $+ i * 10
        });

## Great for working with strings too:

std:assert_str_eq
    (map std:str:to_uppercase
        $["abc", "bcbc", "aaad", "afoo", "foo"])
    $["ABC", "BCBC", "AAAD", "AFOO", "FOO"];

## Maps:

std:assert_str_eq
    (std:sort ~ map { @ } ${a = 10, b = 20})
    $[$[10, "a"], $[20, "b"]];

## Generally anything that you can pass into `$iter`:

std:assert_str_eq
    (map { _ * 2 } 0 => 10)
    $[0,2,4,6,8,10,12,14,16,18];

```

#### <a name="525-filter-function-iterable"></a>5.2.5 - filter _function_ _iterable_

Filters anything that is _iterable_ by the given _function_.
The _function_ is called with each item and if it returns a `$true` value,
the item will be collected into a vector that is returned later.

If a map is passed as _iterable_ then _function_ is called with two arguments,
the first being the map entry value and the second the key.

It is very similar to `$@vec iter i <iterable> { if some_function[_] { $+ ... } }`.

```wlambda
## Lists:

std:assert_str_eq
    (filter { (_ 0 1) == "a" } $["abc", "bcbc", "aaad", "afoo", "foo"])
    $["abc","aaad","afoo"];

## Good in combination with `map` too:

std:assert_str_eq
    (map std:str:to_uppercase
        ~ filter { (_ 0 1) == "a" }
            $["abc", "bcbc", "aaad", "afoo", "foo"])
    $["ABC","AAAD","AFOO"];

## Also like `map` works fine with maps, but the function
## needs to take two arguments and returns a pair:

std:assert_str_eq
    (std:sort
        ~ filter { @.0 % 2 == 0 }
            ${a = 2, b = 43, c = 16, d = 13 })
    $[16 => "c", 2 => "a"];

## Generally anything that you can pass into `$iter`:

std:assert_str_eq
    (filter { _ % 2 == 0 } 0 => 10)
    $[0,2,4,6,8];

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
## Inefficient example:

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

std:assert_eq (str v) (str $['d', 'e', 'f']);
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

### <a name="61-operator-assignment"></a>6.1 - Operator Assignment

Please note, that you can use all these operators, as well as special operators
like `=>`, `&>` and `<&` with assignment operations:

```wlambda
!x = 10;
.x += 3;
std:assert_eq x 13;

## also comparison operators work
!y = 10;
.y < = 10;
std:assert_eq y $false;

## function argument pipelining also works in this context
!f = \_ * 10;
.f <&= 10;
std:assert_eq f 100;

!x = 10;
.x &>= \_ * 10;
std:assert_eq f 100;
```

### <a name="62-arithmetic"></a>6.2 - Arithmetic

The output type (float vs. integer) of the numerical arithmetic operators is defined
by the _first_ operand of the operation. Use the casting functions `float` or
`int` if you are unsure.

Please note that not all operators are available as plain identifiers and need
to be quoted when used in their prefix form or as functions, some of them are
`*`, `/`, `%` and some others.

#### <a name="621--operand-1-operand-2-"></a>6.2.1 - + _operand-1_ _operand-2_ ...

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

#### <a name="622---operand-1-operand-2-"></a>6.2.2 - - _operand-1_ _operand-2_ ...

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

#### <a name="623--op-a-op-b"></a>6.2.3 - * _op-a_ _op-b_

Returns the multiplication of the two operands.

```wlambda
std:assert 10   * 4 == 40;
std:assert 10.1 * 4 == 40.4;
std:assert "10" * 4 == 40;

std:assert (`*` 10 4) == 40;

std:assert (float "10.1") * 4 == 40.4;
```

#### <a name="624--op-a-op-b"></a>6.2.4 - / _op-a_ _op-b_

Returns the division of the two operands.

```wlambda
std:assert 10   / 4 == 2;
std:assert 10.0 / 4 == 2.5;
std:assert "10" / 2 == 5;

std:assert (`/` 10 4) == 2;

std:assert (float "10.1") * 4 == 40.4;
```

#### <a name="625--op-a-op-b"></a>6.2.5 - % _op-a_ _op-b_

Returns the remainder of the division of _op-a_ by _op-b_.

```wlambda
std:assert     5 % 4 == 1;
std:assert (`%` 5 4) == 1;
```

#### <a name="626--op-a-op-b"></a>6.2.6 - ^ _op-a_ _op-b_

Returns _op-a_ raised by the power of _op-b_.
Supports float and integers.

```wlambda
std:assert_eq 2 ^ 4     16;
std:assert_eq std:num:round[(2.0 ^ 2.1) * 1000] 4287.0;
std:assert_eq 2 ^ 2.1   4; # first arg type matters!
```

### <a name="63-comparison"></a>6.3 - Comparison

#### <a name="631--op-a-op-b"></a>6.3.1 - == _op-a_ _op-b_

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

#### <a name="632--op-a-op-b"></a>6.3.2 - != _op-a_ _op-b_

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

#### <a name="633--op-a-op-b"></a>6.3.3 - < _op-a_ _op-b_

Numerical comparison operator that checks whether _op-a_ is less than _op-b_

```wlambda
std:assert   10   < 11;
std:assert   10.1 < 10.2;
std:assert not[10 < 10.1];  # the type of the first argument decides return type!
```

#### <a name="634--op-a-op-b"></a>6.3.4 - <= _op-a_ _op-b_

Numerical comparison operator that checks whether _op-a_ is less or equal to _op-b_

```wlambda
std:assert 10   <= 11;
std:assert 10.1 <= 10.2;
std:assert 10   <= 10.1;  # integer <=, the type of the first argument decides return type!
```

#### <a name="635--op-a-op-b"></a>6.3.5 - > _op-a_ _op-b_

Numerical comparison operator that checks whether _op-a_ is greater than _op-b_

```wlambda
std:assert   11.1 > 11;
std:assert   11.1 > 11.0;
std:assert not[10 > 10.1];  # the type of the first argument decides return type!
```

#### <a name="636--op-a-op-b"></a>6.3.6 - >= _op-a_ _op-b_

Numerical comparison operator that checks whether _op-a_ is greater or equal to _op-b_

```wlambda
std:assert 11   >= 11;
std:assert 10.2 >= 10.1;
std:assert 10 >= 10.1;  # integer >=, the type of the first argument decides return type!
```

### <a name="64-bit-operations"></a>6.4 - Bit Operations

#### <a name="641--op-a-op-b"></a>6.4.1 - & _op-a_ _op-b_

Binary `and` operation between two integers.

```wlambda
std:assert (0b0011 & 0b1011) == 0b011;
std:assert (3      &     11) == 3;
```

#### <a name="642--op-a-op-b"></a>6.4.2 - &^ _op-a_ _op-b_

Binary `xor` operation between two integers.

```wlambda
std:assert (0b0011 &^ 0b1011) == 0b1000;
std:assert (3      &^     11) == 8;
```

#### <a name="643--op-a-op-b"></a>6.4.3 - &| _op-a_ _op-b_

Binary `or` operation between two integers.

```wlambda
std:assert (0b0011 &| 0b1000) == 0b1011;
std:assert (3      &|      8) == 11;
```

#### <a name="644--op-a-op-b"></a>6.4.4 - << _op-a_ _op-b_

Binary `left shift` operation of _op-a_ by _op-b_ bits.

```wlambda
std:assert (0b0011 << 3)   == 0b11000;
std:assert (`<<` 0b1011 2) == 0b101100
```

#### <a name="645--op-a-op-b"></a>6.4.5 - >> _op-a_ _op-b_

Binary `right shift` operation of _op-a_ by _op-b_ bits.

```wlambda
std:assert (0b0011 >> 2)      == 0b0;
std:assert (0b1100 >> 2)      == 0b11;
std:assert (`>>` 0b1011000 3) == 0b1011
```

### <a name="65-collection-addition-operators--and-"></a>6.5 - Collection Addition Operators +> and <+

`+>` and `<+` are special operators for convenient and quick collection creation.
You can use them also to call a function with multiple arguments.

```wlambda
!vec = $[] +> 1 +> 2 +> 3;

std:assert_str_eq vec $[1, 2, 3];

!map = ${}
    +> (:a => 1)
    +> (:b => 2);

map +> (:c => 3);

std:assert_str_eq map ${a=1,b=2,c=3};
```

Usually these operators just append (`+>`) or prepend (`<+`) the right/left hand
side to the collection. But there are some special values which do special things.

First and foremost the iterator data type. If you pass an iterator to this
operator, the iterator will be iterated and all returned elements are added
to the collection in the order of the operator:

```wlambda
!v = $[] +> ($iter 0 => 4) +> ($iter "abc");

std:assert_str_eq v $[0,1,2,3,'a','b','c'];
```

As the `<+` operator prepends the individual elements, the
same is happening with the iterated elements. Which means
that their order is reversed:

```wlambda
!v = ($iter 0 => 4) <+ ($iter "abc") <+ $[];

std:assert_str_eq v $[3,2,1,0,'c','b','a'];
```

The following data types can be used as collection for
these operators:

- Vectors `$[]`
- Maps `${}`
- Strings `""`
- Byte vectors `$b""`
- Functions

The most special cases are maps and functions, which are described
in more detail in the next sections.

#### Collection Addition with Maps

If you add to a map, there is some special behavior for some data types.

Adding a key value pair is done with pairs:

```wlambda
!m = ${}
    +> $p(:a, 10)
    +> :b => 20;

std:assert_str_eq m ${a=10,b=20};
```

If you add an iterator, the iterator is walked and the given keys are
added if present:

```wlambda
!m = ${}
    +> ($iter $[1, 2, 3])
    +> ($iter ${ a = 10, b = 20 });

std:assert_str_eq m ${1=1,2=2,3=3,a=10,b=20};
```

If you add a list to a map, the first element of that list is used
as key, and the list as value:

```wlambda
!m = ${}
    +> $[:a, 1, 2]
    +> $[:b, 3, 4];

std:assert_str_eq m ${a=$[:a,1,2],b=$[:b,3,4]};
```

#### Collection Addition with Function

If you pass a function as collection to either `+>` or `<+` the function
is called for each added element. The return value of the expression
is the return value of the most recent function call.

```wlambda
!v = $[];

!v2 = { std:push v _; v } +> 1 +> 2 +> ${ a = 3, b = 4 };

std:assert_str_eq v  $[1,2,${a=3,b=4}];
std:assert_str_eq v2 $[1,2,${a=3,b=4}];
```

#### <a name="651--collection-a-"></a>6.5.1 - +> _collection_ _a_ ...

Append to collection operator.

```wlambda
!v  = $[] +> 1 +> "x" +> $b"y" +> ($iter 0 => 3);

!v2 = `+>` $[] 1 "x" $b"y" ($iter 0 => 3);

std:assert_str_eq v v2;
```

#### <a name="652--collection-a-"></a>6.5.2 - <+ _collection_ _a_ ...

Prepend to collection operator. Please note that the arguments are
reversed to the order in an operator expression.

```wlambda
!v  = ($iter 0 => 3) <+ 1 <+ "x" <+ $b"y" <+ $[];

!v2 = `<+` $[] $b"y" "x" 1 ($iter 0 => 3);

std:assert_str_eq v v2;
```

### <a name="66-call-operators----and-"></a>6.6 - Call Operators &>, \<&, &@> and \<@&

See also:

- [Forward Argument Pipe `arg &> fun`](#255-forward-argument-pipe-arg--fun)
- [Forward Argument Apply Pipe `list &@> fun`](256-forward-argument-apply-pipe-list--fun)
- [Reverse Argument Pipe `fun <& arg`](#257-reverse-argument-pipe-fun--arg)
- [Reverse Argument Apply Pipe `list &@> fun`](#258-reverse-argument-apply-pipe-list--fun)

### <a name="67-default-value-operators---n-o-and-e"></a>6.7 - Default Value Operators //, /?, /$n, /$o and /$e

There is a set of convenient default value operators that allow quick unwrapping
of optional value, `$none` and even `$error` values.

The most safe to use default value operator is the `//` operator, which returns
an alternative value in case `$none` or `$o()` is provided and even unwraps
optional values like `$o(10)`.  It does not do anything if an `$error` value is
encountered.

```wlambda
!mul = {|1<2| !(a, b) = @;
    a * b // 1
};

std:assert_eq mul[10]     10;
std:assert_eq mul[10, 20] 200;
```

For handling the error value case you can either explicitly combine it with
a `/$e` operator, which is the most explicit thing to use. Or use
the extended default value operator `/?` which also provides the default
value if an error is encountered.

Please note that the operator versions (in contrast to the function versions
in backticks) is short circuit just like the `&or` operator:

```wlambda
!mul = {|1<2| !(a, b) = @;
    a   // return[-1]
    * b // return[-1]
};

std:assert_eq mul[10]        -1;
std:assert_eq mul[$none, 20] -1;
std:assert_eq mul[10, 20]    200;
```

For more details see the following sections.

#### <a name="671--a-default-b"></a>6.7.1 - // _a_ _default-b_

The default value operator is the `//` operator, which returns an alternative
value in case `$none` or `$o()` is provided on the left hand side and even
unwraps optional values like `$o(10)`.  It does not do anything if an `$error`
value is encountered. For an operator that also defaults `$error` values see
the extended default value operator `/?`.

```wlambda
std:assert_eq   $n     // 10           10;
std:assert_eq   $o()   // 10           10;
std:assert_eq   $o(20) // 10           20;
std:assert_eq   $false // 10           $false;
std:assert_eq   (is_err ($e 1) // 10)  $true;
```

Please note you can combine and chain these operators: `a_func[] /$e -1 // 10`.

#### <a name="672--a-default-b"></a>6.7.2 - /? _a_ _default-b_

The extended default value operator is the `/?` operator, which returns an
alternative value in case `$none`, `$error` or `$o()` is provided on the left
hand side and even unwraps optional values like `$o(10)`. It's the extended
version of the default value operator `//`.

```wlambda
std:assert_eq   $n     /? 10  10;
std:assert_eq   $o()   /? 10  10;
std:assert_eq   $o(20) /? 10  20;
std:assert_eq   $false /? 10  $false;
std:assert_eq   ($e 1) /? 10  10;
```

#### <a name="673-n-a-default-b"></a>6.7.3 - /$n _a_ _default-b_

The `$none` default value operator returns it's right hand side if the
left hand side is a `$none` value.

```wlambda
std:assert_eq   $n     /$n 10           10;
std:assert_eq   $o()   /$n 10           $o();
std:assert_eq   $o(20) /$n 10           $o(20);
std:assert_eq   $false /$n 10           $false;
std:assert_eq   (is_err ($e 1) /$n 10)  $true;
```

Please note you can combine and chain these operators: `a_func[] /$n -1 /$o -2`.

#### <a name="674-o-a-default-b"></a>6.7.4 - /$o _a_ _default-b_

The optionals default value operator returns it's right hand side if the
left hand side is a `$o()` value. And it unwraps it's left hand side
if it is an non empty optional value like eg. `$o(10)`.

```wlambda
std:assert_eq   $n     /$o 10           $n;
std:assert_eq   $o()   /$o 10           10;
std:assert_eq   $o(20) /$o 10           20;
std:assert_eq   $false /$o 10           $false;
std:assert_eq   (is_err ($e 1) /$o 10)  $true;
```

Please note you can combine and chain these operators: `a_func[] /$e -1 /$n -2 /$o -3`.

#### <a name="675-e-a-default-b"></a>6.7.5 - /$e _a_ _default-b_

The error default value operator returns it's right hand side if the
left hand side is an `$error` value. It's convenient to provide default
values only in case an error is returned. It can also be used
to ignore errors more conveniently.

```wlambda
std:assert_eq   $n     /$e 10           $n;
std:assert_eq   $o()   /$e 10           $o();
std:assert_eq   $o(20) /$e 10           $o(20);
std:assert_eq   $false /$e 10           $false;
std:assert_eq   (is_err ($e 1) /$e 10)  $false;
```

Please note you can combine and chain these operators: `a_func[] /$e -1 /$o -2 // 0`.

## <a name="7-string-and-byte-vector-formatting"></a>7 - String and Byte Vector Formatting

WLambda comes with a built in functionality for string (and byte vector)
formatting.  It works by creating a specialized formatting function from a
given string literal at compile time with the `$F"..."` syntax, or a string at
runtime with the `std:formatter _str_` function.

The formatter syntax is documented in detail at [12.2 String Formatting
Syntax](#132-string-formatting-syntax). It is basically the Rust `std::fmt`
Syntax with a few extensions for WLambda data types and the dynamically typed
nature of WLambda.

The WLambda syntax for `$F` is: `$F string-literal`. This means, you can
use any WLambda string literal after `$F`:

```wlambda
$F"...";        # normal string
$F$b"...";      # byte vector
$F$q/.../;      # normal string, quote syntax
$F$Q"...";      # byte vector quote syntax
$F$code{ };     # code block string
```

(Please note, that `$code{ ... }` is not as useful in this context, because
the formatter placeholders usually are not valid WLambda syntax.)

This is a very simple example:

```wlambda
!x = "abc";
!s = $F"x = {}" x;

std:assert_eq s "x = abc";
```

You can also use string formatting to generate byte vectors:

```wlambda
!bv = $F$b"x={}" $b"\xFF";

std:assert_eq bv $b"x=\xFF";
```

If you want to generate the WLambda written representation of a piece of
data like `std:write_str` would return it, you have to specify the
special formatting syntax `{:...!w}`:

```wlambda
std:assert_eq ($F"x={:!w}" $&&$[1, 2, 3, 4]) "x=$&&$[1,2,3,4]";

## Without the `!w` the reference would just
## be auto dereferenced like `str` would do it:
std:assert_eq ($F"x={}"   $&&$[1, 2, 3, 4]) "x=$[1,2,3,4]";
```

#### <a name="701-stdformatter-format-string"></a>7.0.1 - std:formatter _format-string_

Returns a formatting function that takes exactly the arguments specified
in the _format-string_. If the format syntax is wrong, an error is returned.

This is useful, if you need to build a format string at runtime,
because `$F` only allows string/byte vector literals.

```wlambda
!fmt = ">6.2";
!fmt_fun = (std:formatter (std:str:cat "{1} [{0:" fmt "}]"));

std:assert_eq (fmt_fun 3.43554 1.2323) "1.2323 [  3.44]";
```

### <a name="71-formatting-numbers"></a>7.1 - Formatting Numbers

Number formatting, that is integers, float and numerical vectors, require an
extension of the formatting syntax. You need to specify whether an integer
`"{:...!i}"` or a float `{:...!f}` is formatted. Otherwise WLambda will cast
everything to a string for formatting.

Here are some examples:

```wlambda
std:assert_eq ($F "{:8!i}"  123)   "     123";
std:assert_eq ($F "{:08!i}" 123)   "00000123";
std:assert_eq ($F "{:<8!i}" 123)   "123     ";
std:assert_eq ($F "{:^8!i}" 123)   "  123   ";
std:assert_eq ($F "{:>8!i}" 123)   "     123";

std:assert_eq ($F "{:8.2!f}"  123.567)   "  123.57";
std:assert_eq ($F "{:08.2!f}" 123.567)   "00123.57";
std:assert_eq ($F "{:<8.2!f}" 123.567)   "123.57  ";
std:assert_eq ($F "{:^8.2!f}" 123.567)   " 123.57 ";
std:assert_eq ($F "{:>8.2!f}" 123.567)   "  123.57";

## Note: For floats, the "!f" is implicit if you specify a precision:
std:assert_eq ($F "{:8.2}"  123.567)   "  123.57";
std:assert_eq ($F "{:08.2}" 123.567)   "00123.57";
std:assert_eq ($F "{:<8.2}" 123.567)   "123.57  ";
std:assert_eq ($F "{:^8.2}" 123.567)   " 123.57 ";
std:assert_eq ($F "{:>8.2}" 123.567)   "  123.57";
```

You can even format numbers in numerical vectors, data vector, pairs or maps:

```wlambda
std:assert_eq ($F "{:8.2}" $f(1.2, 3.456, 8.232)) "(    1.20,    3.46,    8.23)";
std:assert_eq ($F "{:8.2}" $[1.2, 3.456, 8.232])  "[    1.20,    3.46,    8.23]";
std:assert_eq ($F "{:8.2}" $p(1.2, 3.456))        "(    1.20,    3.46)";
std:assert_eq ($F "{:8.2}" ${x = 1.2, y = 3.456, z = 8.232})
              "{x:    1.20, y:    3.46, z:    8.23}";

std:assert_eq
    ($F "{:>8!i}" $i(1.2, 3.456, 8.232))
    "(       1,       3,       8)";
```

Also hexadecimal, octal and binary are supported for integers, they come after the `!i`:

```wlambda
std:assert_eq ($F "{:5!ix}" 321)    "  141";
std:assert_eq ($F "{:5!io}" 321)    "  501";
std:assert_eq ($F "{:<11!ib}" 321)  "101000001  ";
std:assert_eq ($F "{:011!ib}" 321)  "00101000001";
```

## <a name="8-data-structure-matchers-selectors-and-string-patternsregex"></a>8 - Data Structure Matchers, Selectors and String Patterns/Regex

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
please refer to the [Pattern and Selector Syntax](#821-selector-and-wlambda-regex-syntax).

### <a name="81-data-structure-matcher"></a>8.1 - Data Structure Matcher

This is probably one of the most convenient matching features of WLambda.
While selectors (`$S[a / * / b]`) allow searching deep into data structures,
the matches allow to efficient precise shallow selection and matching.
The `match` operation allows to match a value against multiple matchers,
while the `$M ...` syntax allows to define a matcher function for a single
match (commonly used in an if expression).

For a reference of the matcher syntax see below.

#### <a name="811-match-value-expr-match-pair1--default-expr"></a>8.1.1 - match _value-expr_ _match-pair1_ ... \[_default-expr_\]

The match operation is a very versatile control flow operation.

#### <a name="812-m-expr"></a>8.1.2 - $M _expr_

This is a structure matcher expression. It will compile _expr_ into a structure
matcher function. The reslting function will match it's first argument agianst
the match and return a map containing the capture variables (or just an empty map).

It will also bind the result map to `$\`. This makes it possible to easily match
a data structure in an if statement:

```wlambda
!some_struct = $[:TEST, ${ a = 10, b = 1442 }];

if some_struct &> ($M $[sym, ${ a = 10, b = x }]) {
    std:assert_eq $\.sym :TEST;
    std:assert_eq $\.x   1442;
} {
    panic "It should've matched!";
};
```

#### <a name="813-data-structure-matcher-syntax"></a>8.1.3 - Data Structure Matcher Syntax

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
| `$r/.../`              | Matches any element, where it's string contents matches the given pattern. |
| `$rg/.../`             | Matches any element, where it's string contents matches the given pattern. Returns a list with all global matches. |
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

### <a name="82-data-structure-selectors-s"></a>8.2 - Data Structure Selectors `$S(...)`

This section shows how data structure selectors can be used.

TODO

#### <a name="821-selector-and-wlambda-regex-syntax"></a>8.2.1 - Selector and WLambda Regex Syntax:

```ebnf
    (* NOTE: Whitespace is not part of a pattern in most places. This means
             if you want to match whitespace, you will have to escape
             it either with a '\', with a [ ] character class or match
             one whitespace char with $s. *)

    class_char  = { ?any character except "]"? }
                  (* special sequence: "\^" => "^" and "\\" => "\"
                     and "\]" => "]" *)
                ;

    ident_char_in_selector =
                  (* if regex is used inside a selector: *)
                  { ?any character except whitespace,
                    "!", "?", "/", "\", "|", "^", ",",
                    "'", "&", ":", ";", "$", "(", ")",
                    "{", "}", "[", "]", "*" and "="? }
                  (* allows the usual backslash escaping from strings! *)
                ;

    ident_char_in_direct_pattern =
                | (* if regex is used as pattern directly: *)
                  { ?any character except whitespace,
                    "?", "|", "$", "(", ")", "[", "]" and "*"? }
                  (* allows the usual backslash escaping from strings! *)
                ;
    ident_char  = ident_char_in_direct_pattern
                | ident_char_in_selector
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

    reckey_cond = "!", "key", "=", pattern
                  (* recurse only into values if they are not referred
                     to by a key matching the given pattern. *)
                ;
    recval_cond = "=", node_cond
                  (* recurse only into values if they match the given
                     condition *)
                ;

    node        = key, { node_cond }
                  (* marks it for referencing it in the result set *)
                | "**", [ reckey_cond ], [ recval_cond ], { node_cond }
                  (* deep expensive recursion *)
                | "^", node
                ;

    selector    = node, { "/", node }
                ;
```

#### <a name="822-stdselector-string"></a>8.2.2 - std:selector _string_

Parses the given _string_ as WLambda data structure selector and returns
a function that takes a data structure as first argument. That function will
then query the data structure according to the given selector.
That function will also set the global variable `$\` to the result.

The main usage of this function is, when you want to define the selector
at runtime. Otherwise WLambda provides the handy `$S(...)` syntax for
generating the structure pattern function at compile time.

```wlambda
!runtime_name = "foo";
!sel = std:selector (" * / " runtime_name);

if sel <& $[${foo = 1}, ${foo = 2}, ${foo = 3}] {
    std:assert_str_eq $\ $[1,2,3];
} {
    std:assert $false
};
```


### <a name="83-string-patterns-regex-r"></a>8.3 - String Patterns (Regex) `$r/.../`

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
## Please note: Whitespace inside the pattern is allowed and will not be matched!

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
the results of the latest match that was executed:

```wlambda
## Notice the usage of the `<&` function call operator:
!res =
    if "foo//\\/foo" &> $r| $<*? (^$+[\\/]) * | {
        std:assert_eq $\.0 "foo//\\/foo";

        $\.1
    };

std:assert_eq res "//\\/";
```

#### <a name="831-global-patterns-rg"></a>8.3.1 - Global Patterns `$rg/.../`

With the `g` modifier the regex can be modified and will match the input
string with the given pattern repeatedly and call a given function
for each match.

The match function will receive the input string as first argument
and a function that will be called for each match as second argument.

Inside the match function, you can use the control flow functions `break`
and `next` to skip ahead.

The match function receives the contents of `$\` as first argument,
the offset of the match in the input string as second argument
and the length of the match as third argument:

```wlambda
!found = $@vec $rg/x(^?)y/ "aax9yaaxcy" {!(match, offs, len) = @;
    $+ $[match.1, offs, len]
};

std:assert_str_eq found $[$["9", 2, 3], $["c", 7, 3]];
```

#### <a name="832-pattern-substitutions-rs"></a>8.3.2 - Pattern Substitutions `$rs/.../`

The `s` modifier creates a substitution that will substitute each match
of the pattern in the given input string with the return value of the
match function. The match function is called with the same values as `$rg`
does.

```wlambda
!digits =
    $["zero", "one", "two", "three", "four",
      "five", "six", "seven", "eight", "nine"];

!ret = $rs/[0-9]/ "on 1 at 0 of 8" {!(match, offs, len) = @;
    digits.(int match.0)
};

std:assert_eq ret "on one at zero of eight";
```

Inside the match function, you can use the control flow functions `break`
and `next`. You can use that to control which occurence within the string to
replace:

```wlambda
!res = $rs/xxx/ "fxxxfxxxfxxxf" { break "O" };

std:assert_eq res "fOfxxxfxxxf";
```

#### <a name="833-pattern-syntax-overview"></a>8.3.3 - Pattern Syntax Overview

While
[Selector and WLambda Regex Syntax](#821-selector-and-wlambda-regex-syntax)
describes the pattern syntax in detail,
here is the WLambda pattern regex syntax in a nutshell:

| Pattern Syntax | Semantics |
|-|-|
| `?|$()[]*`    | Many special chars are reserved in WLambda patterns. Be aware that more characters are reserved if you use the patterns in a data structure selector, instead of a single pattern. Please escape then using backslash like `\\/` or `[/]`.|
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

#### <a name="834-standard-regular-expressions"></a>8.3.4 - Standard Regular Expressions

Please note that WLambda can optionally be compiled with the `regex` crate,
which implements a more common syntax for regular expressions.
Please refer to the functions `std:re:match` in the WLambda standard library
for this.

#### <a name="835-stdpattern-string-mode"></a>8.3.5 - std:pattern _string_ \[_mode_\]

Compiles the regex pattern _string_ to a function just like `$r/.../` would do.
The _mode_ can either be `:g` (global match like `$rg...`), `:s` (substitution
like `$rs...`) or `$none`.  Useful for composing WLambda patterns at runtime:

```wlambda
!rx = std:pattern ~ std:str:cat "(^" "$+" "[a-z]" ")";

std:assert_eq (rx "foo").1 "foo";
```

Returns an error if the syntax failes to parse as pattern:

```wlambda
!err = unwrap_err ~ std:pattern "($+[a-z]";

std:assert_eq $i(0, 11)[err] "bad pattern";
```

Here an example of substitution:

```wlambda
!subs = std:pattern "$+x" :s;
std:assert_eq subs["fooxxxoxx", \"a"] "fooaoa";
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

std:push v 10;
std:push v 20;

std:assert_eq (str v) "$[10,20]";
```

## <a name="10-core-library"></a>10 - Core Library

This library contains all the core functions which belong to the
core of the WLambda Programming Language. These functions can be seen
as keywords of WLambda. Some functions are also available as operators.

#### <a name="1001-type-value"></a>10.0.1 - type _value_

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

#### <a name="1002-len-value"></a>10.0.2 - len _value_

Returns the length of _value_. Depending on the data type you will get
different semantics.

```wlambda
## Always zero for scalar non sequential/collection values:
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

#### <a name="1003-panic-message"></a>10.0.3 - panic _message_

If your program runs into something that deserves a slap on the fingers
of the developer you can use `panic` to do that.

## <a name="11-standard-library"></a>11 - Standard Library

#### <a name="1101-stdshuffle-randfunc-vec"></a>11.0.1 - std:shuffle _rand\_func_ _vec_

Shuffles the _vec_ in place. The function _rand_func_ needs to return
a random 64 bit integer on each call. Here is an example:

```wlambda
std:srand 1234;
!vec = $[1,2,3,4,5,6,7,8];
std:shuffle { std:rand :i64 } vec;

std:assert_eq (str vec) "$[2,1,7,4,8,5,3,6]";
```

An Example with std:rand:split\_mix64\_next:

```wlambda
!sm  = std:rand:split_mix64_new_from 1234;
!vec = $[1,2,3,4,5,6,7,8];
std:shuffle { std:rand:split_mix64_next sm } vec;

std:assert_eq (str vec) "$[2,1,7,4,8,5,3,6]";
```

#### <a name="1102-stddelete-vector-or-map-index-or-key"></a>11.0.2 - std:delete _vector-or-map_ _index-or-key_

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

#### <a name="1103-stdrefid-value"></a>11.0.3 - std:ref\_id _value_

Returns an integer identifier for a given referential value.
The ID will stay the same as long as the reference is allocated.
This returns a value for all data types that have some form
of internal reference to a value on the heap.

The main usage of this function is to get a pre process unique ID
for an allocated value. But be aware, that once the value is deallocated,
the reference ID does not belong to that value anymore.

```wlambda
!v = $[1,2,3];

!v_id1 = std:ref_id v;
std:push v 4;

!v_id2 = std:ref_id v;

std:assert_eq v_id1 v_id2;
```

#### <a name="1104-stdcopy-vecormap"></a>11.0.4 - std:copy _vec\_or\_map_

Makes a shallow copy of the given vector or map.

```wlambda
!a = $[1,2,3];
!b = std:copy a;
b.0 = 10;

std:assert_eq a.0 1;
std:assert_eq b.0 10;
```

#### <a name="1105-stdvalues-collection-or-iter"></a>11.0.5 - std:values _collection-or-iter_

This function returns all values in the given collection or iterator
as vector. _collection-or-iter_ can have be one of the following data
types:

- vector
- numerical float or integer vector
- map
- iterator `$iter`

```wlambda
std:assert_str_eq (std:values $iter 0 => 5)      $[0,1,2,3,4];
std:assert_str_eq (std:values ${a = 10})         $[10];
std:assert_str_eq (std:values $iter ${a = 10})   $[10];
std:assert_str_eq (std:values $[1,2,3])          $[1,2,3];
std:assert_str_eq (std:values $i(1,2,3))         $[1,2,3];
```

#### <a name="1106-stdkeys-collection-or-iter"></a>11.0.6 - std:keys _collection-or-iter_

This function returns all keys in the given _collection_ or _iterator_.
It's most useful for the map data type, but also returns the indices in
a vector or numerical vector.

```wlambda
std:assert_str_eq (std:keys ${a = 10})           $["a"];
std:assert_str_eq (std:keys $iter ${a = 10})     $["a"];
std:assert_str_eq (std:keys $[3,3,3])            $[0,1,2];
std:assert_str_eq (std:keys $i(4,4,4))           $[0,1,2];
std:assert_str_eq (std:keys $i(4,4))             $[0,1];
```

#### <a name="1107-stdsort-comparefun-vec"></a>11.0.7 - std:sort [_compare\_fun_] _vec_

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

#### <a name="1108-stdcmpnumasc-a-b"></a>11.0.8 - std:cmp:num:asc _a_ _b_

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

#### <a name="1109-stdcmpnumdesc-a-b"></a>11.0.9 - std:cmp:num:desc _a_ _b_

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

#### <a name="11010-stdcmpstrasc-a-b"></a>11.0.10 - std:cmp:str:asc _a_ _b_

Compares _a_ and _b_ lexicographically by their byte values. This orders
Unicode code points based on their positions in the code charts.

| Cases         | Return Value |
|---------------|--------------|
| _a_ > _b_     | -1           |
| _a_ == _b_    | 0            |
| _a_ < _b_     | 1            |

```wlambda
std:assert_eq (std:cmp:str:asc "abc" "aba") -1;
std:assert_eq (std:cmp:str:asc "abc" "abc")  0;
std:assert_eq (std:cmp:str:asc "abc" "abd")  1;
```

#### <a name="11011-stdcmpstrdesc-a-b"></a>11.0.11 - std:cmp:str:desc _a_ _b_

Compares _a_ and _b_ lexicographically by their byte values. This orders
Unicode code points based on their positions in the code charts.

| Cases         | Return Value |
|---------------|--------------|
| _a_ > _b_     | 1            |
| _a_ == _b_    | 0            |
| _a_ < _b_     | -1           |

```wlambda
std:assert_eq (std:cmp:str:desc "abc" "aba")  1;
std:assert_eq (std:cmp:str:desc "abc" "abc")  0;
std:assert_eq (std:cmp:str:desc "abc" "abd") -1;
```

#### <a name="11012-stdreverse-value"></a>11.0.12 - std:reverse _value_

Reverses the given sequence of values. This works for following data types:

- Strings
- Byte vectors
- Vectors
- Numeric vectors
- Iterators

```wlambda
std:assert_str_eq (std:reverse $[1, 2, 3, 4])       $[4,3,2,1];
std:assert_str_eq (std:reverse "ABC")               "CBA";
std:assert_str_eq (std:reverse $b"ABC")             $b"CBA";
std:assert_str_eq (std:reverse $i(1,2,3,4))         $i(4,3,2,1);
std:assert_str_eq (std:reverse $f(1.1,2.2,3.3,4.4)) $f(4.4,3.3,2.2,1.1);
```

#### <a name="11013-stddisplayln-arg1-"></a>11.0.13 - std:displayln _arg1_ ...

This function writes a humand readable version of all the arguments
(with a space inbetween) to the standard output. This means that:

```text
std:displayln "foo"
```

Will just print `foo` and a newline.

If you need a less ambigous form, use `std:writeln`, which
handles its argument like written via `std:ser:wlambda` instead of `str`.

#### <a name="11014-debug-arg1-"></a>11.0.14 - $DEBUG _arg1_ ...

This is a special value that evaluates to a print function that supplies the
current position in the source code. For example this:

```wlambda
!k = $[1, 2, 3];

$DEBUG "I got values:" k 99;
```

Will print this (assuming it's at line 1 col 3 in file `file_foo.wl`):

```text
[1,3:<file_foo.wl>] DEBUG: "I got values:"(string) $[1,2,3](vector) 99(integer)
```

In case you want to directly write a string or some value, you will have
to prefix the argument with the symbol `:\`:

```wlambda
!k = 30;
$DEBUG :\ "k =" :\ k;
```

Will print like this:

```text
[1,11:<wlambda::eval>] DEBUG: k = 30
```

#### <a name="11015-stdwriteln-arg1-"></a>11.0.15 - std:writeln _arg1_ ...

This function writes the WLambda representation of its arguments
(with a space inbetween) to standard output. This means that:

```text
std:displayln "foo"
```

Will print `"foo"` and a newline.

See also the description of `std:ser:wlambda`.

If you need a more human readable form use `std:displayln`.

#### <a name="11016-stdeval-code-string"></a>11.0.16 - std:eval _code-string_

Evaluates _code-string_ in the current global environment and returns
the generated value. If the code leads to any kind of evaluation error,
an error object is returned.

```wlambda
std:assert_eq (std:eval "1 + 2") 3;
!:global X = 20;
std:assert_eq (std:eval "1 + X") 21;
```

#### <a name="11017-stdassert-bool-message"></a>11.0.17 - std:assert _bool_ \[_message_]

Just a simple assertion function that panics if the first argument is not true.
Returns the passed value if it is a true value.
You can pass an optional message as second parameter.

```norun_wlambda
std:assert $false; #=> Panic
std:assert 120;    #=> 120
```

#### <a name="11018-stdasserteq-actual-expected-message"></a>11.0.18 - std:assert\_eq _actual_ _expected_ \[_message_]

This function checks if the _actual_ value is equal to the
_expected_ value and panics if not. The optional _message_ is
passed in the panic for reference.

```wlambda
!x = 30 * 2;
std:assert_eq x 60 "30 * 2 == 60";
```

#### <a name="11019-stdassertstreq-actual-expected"></a>11.0.19 - std:assert\_str\_eq _actual_ _expected_

This function stringifies _actual_ and _expected_ using the `str` function
and compares the resulting strings.

This is very useful to compare data structures, as map keys are sorted
if the maps are stringified using `str`:

```wlambda
std:assert_str_eq $[1, 2, 3]        $[1, 2, 3];
```

#### <a name="11020-stdassertreleq-l-r-epsilon-message"></a>11.0.20 - std:assert\_rel\_eq _l_ _r_ _epsilon_ \[_message_]

This function checks if `l` is within `epsilon` of `r`.
If the absolute value of the difference between `l` and `r` is greater than `epsilon`,
this function will panic, also displaying the optional message if present.

```wlambda
## these two are within 1 of each other
!x = 10.5;
!y = 11.3;
std:assert_rel_eq x y 1;

## but not within 0.5 of each other, so this line is commented out.
## std:assert_eq x y 0.5;
```

#### <a name="11021-stdmeasuretime-unit-function"></a>11.0.21 - std:measure\_time _unit_ _function_

This function measures the time the given _function_ took to execute.
The _unit_ defines how precisely the time is measured. Following strings are supported
units:

- `s` - seconds
- `ms` - milliseconds
- `us` - microseconds
- `ns` - nanoseconds

The return value is a vector where the first element is the
time it took to execute the function, and the second element is the
return value of that function.

```wlambda
!res = std:measure_time :ns { $@i iter i 0 => 100000 { $+ i } };
std:assert res.0 > 100;
std:assert_eq res.1 4999950000;
```

### <a name="111-io"></a>11.1 - I/O

#### <a name="1111-stdioline"></a>11.1.1 - std:io:line

Reads a line from standard input and returns it. Returns an error if something
went wrong.

```text
!line = unwrap std:io:line[];
std:displayln "you entered: " std:str:trim_end[line];
```

#### <a name="1112-stdiolines-function---value--vector"></a>11.1.2 - std:io:lines \[_function_\] -> _value_ | _vector_

Calls the given _function_ for each line in standard input until EOF and returns
the last returned value from that call.  If _function_ is not given, all lines
will be appended to a new vector and returned.  Returns an error if some IO
error occured.

```text
!lines = std:io:lines[];

!lines = $@v std:io:lines $+;

std:io:lines {!(line) = @;
    std:displayln "You entered: [" std:str:trim[line] "]";
};
```

#### <a name="1113-stdiofilereadtext-filename"></a>11.1.3 - std:io:file:read\_text _filename_

Opens the file _filename_ and returns its contents interpreted as UTF8
text as string.

```wlambda
std:io:file:write_safe "prelude_test.txt" "abcäöü";

!t = std:io:file:read_text "prelude_test.txt";
std:assert_eq t "abcäöü" "reading text from file works";
```

#### <a name="1114-stdiofileread-filename"></a>11.1.4 - std:io:file:read _filename_

Opens the file _filename_ and returns its contents as byte buffer.

```wlambda
std:io:file:write_safe "prelude_test.txt" "abcäöü";

!t = std:io:file:read "prelude_test.txt";
.t = std:str:from_utf8 t;
std:assert_eq t "abcäöü" "reading binary from file works";
```

#### <a name="1115-stdiofilewritesafe-filename-bytes-or-string"></a>11.1.5 - std:io:file:write\_safe _filename_ _bytes-or-string_

Creates a new file with the given filename but with a "~" appended
and writes the contents into it. After successful write, it renames
the file to the given filename.

#### <a name="1116-stdiofileappend-filename-bytes-or-string"></a>11.1.6 - std:io:file:append _filename_ _bytes-or-string_

Opens the given filename in append mode and appends _bytes-or-string_ to the
end of the file.

#### <a name="1117-stdiostdoutnewline"></a>11.1.7 - std:io:stdout:newline

Writes a newline to standard output. Returns an error if an error occured or
`$true` otherwise.

#### <a name="1118-stdiostdoutflush"></a>11.1.8 - std:io:stdout:flush

Flushes the standard output buffer. Returns an error if an error occured or
`$true` otherwise.

#### <a name="1119-stdiostdoutprint-value"></a>11.1.9 - std:io:stdout:print _value_

Writes the given _value_ to standard output in a human readable form like `std:displayln`
does. Returns an error if an error occured. `$true` if everything is fine.

```text
std:io:stdout:write "xxx"; # => Writes `xxx` to standard output
```

#### <a name="11110-stdiostdoutwrite-value"></a>11.1.10 - std:io:stdout:write _value_

Writes a WLambda representation of _value_ to standard output.
Returns an error if an error occured. `$true` if everything is fine.

```text
std:io:stdout:write "xxx"; # => Writes `"xxx"` to standard output
```

#### <a name="11111-stdioflush-handle"></a>11.1.11 - std:io:flush _handle_

Flushes the internal buffers of _handle_. _handle_ can be any kind of IO handle,
like a file handle or networking socket.

```text
!socket = unwrap ~ std:net:tcp:connect "127.0.0.1:80";

std:io:write socket $b"GET / HTTP/1.0\r\n\r\n";
std:io:flush socket;
```

#### <a name="11112-stdioreadsome-handle"></a>11.1.12 - std:io:read\_some _handle_

Reads some amount of data from _handle_. The default maximum amount
of bytes read is 4096. This function returns `$o(bytes)` if something
was read. It returns `$o()` when EOF is encountered. `$none` is
returned when the IO operation was interrupted or did timeout.
An `$error` is returned if some kind of error happened, like loss of
TCP connection.

Here is an example how to read everything from a socket until EOF is
encountered:

```text
!socket = unwrap ~ std:net:tcp:connect "127.0.0.1:80";

std:io:write socket $b"GET / HTTP/1.0\r\n\r\n";
std:io:flush socket;

!buf = $b"";
!done = $f;
while not[done] {
    match std:io:read_some[socket]
        $o(buf) => { .buf = buf +> $\.buf; }
        $o()    => { .done = $t; }
        ($e _)  => { .done = $t; };
};
```

#### <a name="11113-stdiowrite-handle-data-offs---count--error--none"></a>11.1.13 - std:io:write _handle_ _data_ \[_offs_\] -> _count_ | $error | $none

Write all data as byte vector to the IO _handle_ (socket, file handle, ...),
starting at _offs_ (0 default).
Returns the number of bytes written, an error or `$none` if interrupted.
Note: This function does not respect the underlying write timeout in _handle_ properly.

```text
!socket = unwrap ~ std:net:tcp:connect "127.0.0.1:80";

std:io:write socket $b"GET / HTTP/1.0\r\n\r\n";
```

#### <a name="11114-stdiowritesome-handle-data"></a>11.1.14 - std:io:write\_some _handle_ _data_

Try to write some data as byte vector to the IO _handle_ (socket, file handle,
...), starting at _offs_ (0 default).
Returns the number of bytes written, an error or `$none` if a timeout or interrupt
ended the write operation.

```text
!socket = unwrap ~ std:net:tcp:connect "127.0.0.1:80";

!bytes = $b"GET / HTTP/1.0\r\n\r\n";
!written = 0;

while len[bytes] > written {
    match (std:io:write_some socket bytes written)
        $o(n)  => { .written += n; }
        $none  => {}
        ($e _) => { break[] };
}
```

### <a name="112-networking"></a>11.2 - Networking

#### <a name="1121-stdnettcpconnect-socket-addr-connect-timeout"></a>11.2.1 - std:net:tcp:connect _socket-addr_ [_connect-timeout_]

This tries to connect to _socket-addr_. If a _connect-timeout_ is given, it
will try to connect within that time. Please note that the returned sockets are
thread safe and can be passed to another thread via an _Atom_, _Atom Value
Slot_ or _Channel_ for instance.

_socket-addr_ can be:

- A pair `$p(host, port)`
- A string like "host:port".
- A pair within a pair to specify whether to use IPv4 or IPv6
addresses only:
    - `$p(:v4, "host:port")`
    - `$p(:v4, $p(host, port))`
    - `$p(:v6, "host:port")`
    - `$p(:v6, $p(host, port))`

About _connect-timeout_ see std:thread:sleep.

```test
!socket =
    match (std:net:tcp:connect "127.0.0.1:8328")
        ($e err) => { panic ("Couldn't connect: " + str[$\.err]) }
        socket   => $\.socket;
```

#### <a name="1122-stdnettcplisten-socket-addr-function"></a>11.2.2 - std:net:tcp:listen _socket-addr_ _function_

Tries to bind a local port to _socket-addr_ (see std:net:tcp:connect about
_socket-addr_.  Note: you can't use `$p(:v4 / :v6, ...)`).
Returns an error if something bad happened.

For every new connection _function_ is called with the socket
as first argument:

```text
unwrap ~ std:net:tcp:listen "0.0.0.0:8292" {!(socket) = @;
    std:io:write socket "Hello!\r\n";
};
```

Please note that you can share the socket with other threads, see also `std:net:tcp:connect`.

#### <a name="1123-stdnettcpsettimeouts-socket-read-timeout-duration-write-timeout-duration"></a>11.2.3 - std:net:tcp:set\_timeouts _socket_ _read-timeout-duration_ [_write-timeout-duration_]

Sets the read/write timeout of that TCP _socket_. The duration can be specified as described for `std:thread:sleep`.

```text
!socket =
    match (std:net:tcp:connect "127.0.0.1:8328")
        ($e err) => { panic ("Couldn't connect: " + str[$\.err]) }
        socket   => $\.socket;

std:net:tcp:set\_timeouts socket :ms => 250 :ms => 100;
```

#### <a name="1124-stdnetudpnew-socket-addr-connect-addr"></a>11.2.4 - std:net:udp:new _socket-addr_ [_connect-addr_]

Creates a new UDP socket and binds it to an endpoint.  The arguments
_socket-addr_ and _connect-addr_ have the same properties as the _socket-addr_
that `std:net:tcp:connect` receives.

If _connect-addr_ is given, a connected UDP port is created and
`std:net:udp:send` does not need to pass a _socket-addr_.

Returns a socket or an error.

The socket can be shared between threads, so you can have a receiving
thread and a sending one.

```wlambda
!socket = std:net:udp:new "0.0.0.0:31889";

std:net:udp:send socket $b"TEST" "127.0.0.1:31888";
```

Here is a more elaborate example using threads:

```wlambda
!hdl = std:thread:spawn $code {
    !socket = std:net:udp:new "0.0.0.0:31889";
    _READY.send :ok;

    !(data, addr) = std:net:udp:recv socket;
    std:displayln "PING" data;
    unwrap ~ std:net:udp:send socket ("Test:" data) addr;
};

hdl.recv_ready[];

!socket = std:net:udp:new "0.0.0.0:31888" "127.0.0.1:31889";
unwrap ~ std:net:udp:send socket $b"XYB123";

!(data, addr) = unwrap ~ std:net:udp:recv socket;

std:displayln "PONG" data;

std:assert_eq data $b"Test:XYB123";

hdl.join[];
```

#### <a name="1125-stdnetudpsend-socket-data-socket-addr"></a>11.2.5 - std:net:udp:send _socket_ _data_ [_socket-addr_]

Sends the _data_ to the given _socket-addr_ or to the connected
address of the _socket_.

Returns the number of bytes sent or an error.

```wlambda
!socket = std:net:udp:new "0.0.0.0:31889";

std:net:udp:send socket $b"TEST" "127.0.0.1:31888";
```

#### <a name="1126-stdnetudprecv-socket-byte-count"></a>11.2.6 - std:net:udp:recv _socket_ [_byte-count_]

Receives _byte-count_ number of bytes from the given _socket_.
If _byte-count_ is omitted 512 is assumed.

Returns the byte vector with the data and the endpoint address
that it was received from. Or an error.

```text
!socket = std:net:udp:new "0.0.0.0:31889";

!(buf, addr) = std:net:udp:recv socket;
```

### <a name="113-processes"></a>11.3 - Processes

This chapter documents how to execute new processes.

#### <a name="1131-stdprocessrun-executable-path-arguments---map"></a>11.3.1 - std:process:run _executable-path_ \[_arguments_\] -> _map_

Starts the given _executable-path_ with the given (but optional) _arguments_.
_arguments_ can be a vector or an iterator. A data structure containing
information about the finished child process or an error is returned if something
went wrong.

This function will block the current thread while the child process
is executed. It collects the output of the child process and returns
it in a data structure of the following form:

```text
{
    status  = 0,        ## exit code
    success = $true,    ## $true or $false
    stdout  = $b"...",  ## data printed to stdout
    stderr  = $b"...",  ## data printed to stderr
}
```

Here is an example for Linux:

```text
!ret = unwrap ~ std:process:run "sh" $["-c", "echo \"test\""];

std:assert_eq ret.status  0;
std:assert_eq ret.success $true;
std:assert_eq ret.stdout  $b"test\n";
std:assert_eq ret.stderr  $b"";
```

#### <a name="1132-stdprocessspawn-executable-path-arg-vector-inheritout--inheritall"></a>11.3.2 - std:process:spawn _executable-path_ _arg-vector_ [:inherit\_out | :inherit\_all]

Like `std:process:run` starts a process from the _executable-path_ and _arg-vector_.
But it does not wait until the process finished running, it returns a child process handle
or an error if something went wrong.

The handle can then be used by functions like:

* `std:process:kill_wait` - to kill and wait for the process to exit
* `std:process:try_wait` - to check if the process exited
* `std:process:wait` - to wait until the process exits

The third argument specifies what happens with the standard I/O file handles.
By default the child process gets _null_ handles, so neither output is captured
nor input is passed:

* _default_ - child process gets _null_ handles for stdin, stdout and stderr.
* `:inherit_out` - child process inherits stdout and stderr, but stdin will be _null_.
* `:inherit_all` - child process inherits all (stdout, stderr and stdin) from the
parent and uses them until it exits.

TODO: Implement pipe to/from the child process to be
read/written to via `std:io:read_some` and `std:io:write`.

```wlambda
!hdl = unwrap ~ std:process:spawn "bash" $[
    "-c", "for i in `seq 0 10`; do echo $i; sleep 0.2; done; exit 20"
];

## do something in your program....

!result = unwrap ~ std:process:wait hdl;

std:assert ~ not result.success;
std:assert result.status == 20;
```

#### <a name="1133-stdprocesstrywait-child-handle"></a>11.3.3 - std:process:try\_wait _child-handle_

Checks if the child process behind _child-handle_ exited. Returns `$none` if
it did not exit yet. Returns a map with the structure
`${ status = ..., success = $true / $false }` if the child exited.
Or an error if something failed.

```wlambda
!hdl = unwrap ~ std:process:spawn "bash" $[
    "-c", "for i in `seq 0 10`; do echo $i; sleep 0.2; done; exit 20"
];

!counter = 0;
!ret = $none;
while $true {
    std:thread:sleep :ms => 250;
    .counter += 1;

    .ret = unwrap ~ std:process:try_wait hdl;
    if ret {
        break ret;
    };
};

std:assert counter > 0;
std:assert ~ not ret.success;
std:assert ret.status == 20;
```

#### <a name="1134-stdprocesskillwait-child-handle"></a>11.3.4 - std:process:kill\_wait _child-handle_

Kills the child process behind _child-handle_ and waits for it to return the exit status.
Returns a map with the structure `${ status = ..., success = $true / $false }` if the child exited.
Or an error if something failed.

```wlambda
!hdl = unwrap ~ std:process:spawn "bash" $[
    "-c", "for i in `seq 0 10`; do echo $i; sleep 0.2; done; exit 20"
];

!res = std:process:kill_wait hdl;

std:assert ~ not res.success;
std:assert_eq res.status -1;
```

#### <a name="1135-stdprocesswait-child-handle"></a>11.3.5 - std:process:wait _child-handle_

Waits until the child process behind _child-handle_ exits by itself.
Returns a map with the structure `${ status = ..., success = $true / $false }`
if the child exited. Or an error if something failed.

```wlambda
!hdl = unwrap ~ std:process:spawn "bash" $[
    "-c", "for i in `seq 0 10`; do echo $i; sleep 0.2; done; exit 20"
];

!res = std:process:wait hdl;

std:assert ~ not res.success;
std:assert_eq res.status 20;
```

### <a name="114-file-system"></a>11.4 - File System

#### <a name="1141-stdfspathexists-string"></a>11.4.1 - std:fs:path:exists _string_

Returns `$true` if the path _string_ exists. Otherwise it returns `$false` or an error if there
was an issue with the path or the system.

```wlambda
std:fs:path:exists "C:\\temp\\test.txt";
```

#### <a name="1142-stdfscanonicalize-string"></a>11.4.2 - std:fs:canonicalize _string_

Creates a canonical path from the relative path _string_. If there is some problem with either the path _string_
or the system, an error is returned.

#### <a name="1143-stdfsrename-file-path-new-file-name"></a>11.4.3 - std:fs:rename _file-path_ _new-file-name_

Renames the file at _file-path_ to the new name _new-file-name_. This
usually does only work on a single file system.
Returns `$true` if renaming was successful, and an error object if it was not
successful.

#### <a name="1144-stdfscopy-src-file-path-dst-file-path"></a>11.4.4 - std:fs:copy _src-file-path_ _dst-file-path_

Copies the file _src-file-path_ to the _dst-file-path_.
Returns an error if something went wrong.

#### <a name="1145-stdfsreaddir-path-function"></a>11.4.5 - std:fs:read\_dir _path_ _function_

Calls _function_ with the first argument being the directory entry as map
of this structure:

```wlambda
    ${
        atime     = 1587628635, ## seconds since UNIX Epoch
        ctime     = 1557382099, ## seconds since UNIX Epoch
        mtime     = 1557382099, ## seconds since UNIX Epoch
        len       = 478,        ## bytes
        name      = "test",     ## file name
        path      = "..\\test", ## path name
        read_only = $false,     ## read only flag
        type      = :f          ## possible values:
                                ##   - :f for files
                                ##   - :l for symlinks
                                ##   - :d for directories
    }
```

If the _function_ is called with a directory, you can recurse into that
directory by returning a `$true` value.

You can format the timestamps using `std:chrono:format_utc`.

#### <a name="1146-stdfsremovefile-file-path"></a>11.4.6 - std:fs:remove\_file _file-path_

Removes the file at the given _file-path_.
Returns an error if the file is missing or some other error occured.

#### <a name="1147-stdfsremovedir-dir-path"></a>11.4.7 - std:fs:remove\_dir _dir-path_

Removes the dir at the given _dir-path_.
Returns an error if the dir is missing, is not empty
or some other error occured.

#### <a name="1148-stdfsremovedirall-dir-path"></a>11.4.8 - std:fs:remove\_dir\_all _dir-path_

Removes the given _dir-path_ recursively. Use with care!
Returns an error if the directory does not exist.

### <a name="115-system"></a>11.5 - System

This chapter contains a few system as in _operating system_ related functions.

#### <a name="1151-stdsysenvvar-variable-name"></a>11.5.1 - std:sys:env:var _variable-name_

Returns the variable contents of the environment variable _variable-name_.
If any error occurs, this returns an error.

```wlambda
std:assert (len (std:sys:env:var "HOME")) > 0;
```

#### <a name="1152-stdsysos"></a>11.5.2 - std:sys:os

Returns the name of the operating system. These are some possible
values:

- linux
- macos
- ios
- freebsd
- dragonfly
- netbsd
- openbsd
- solaris
- android
- windows

```wlambda
std:assert (len std:sys:os[]) > 0;
```

### <a name="116-threading"></a>11.6 - Threading

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

The scope of threading in WLambda is primarily to provide a way to do asynchronous
work and not so much for high performance computing. You can of course enhance
performance a bit with threading, but if you want to do heavy computing I recommend
implementing your algorithm in Rust instead. You can of course still manage
thread orchestration in WLambda if you just provide a simple function
API to your algorithms.

#### <a name="1161-stdthreadspawn-string-globals-map"></a>11.6.1 - std:thread:spawn _string_ [_globals-map_]

This evaluates the given _string_ as WLambda code in a new thread.
It returns a thread handle, that can be used to join the thread or
wait for it to have properly started.

The new thread starts out with a completely empty global environment.
If you need any builtin functions, use `!@wlambda`, `!@import std`
and other import statements to load the needed functions.

If a _globals-map_ is given, inside the thread the given global
variables will be set to the given value.

Inside the thread, a global variable called `_READY` is set to
an atomic slot, which can be used to signal the parent thread
that the new thread has successfully spawned.

This is a very basic example how to calculate something in
a worker thread and wait in a blocking manner:

```wlambda
!handle = std:thread:spawn $code {
    ## Attention: Even if this example does not use any
    ##            built in functions, it's a good practice
    ##            to load them into the global environment
    ##            of the thread!
    !@wlambda;
    !@import std;

    !sum = $@i iter i 0 => 10 {
        $+ i;
    };
    _READY.send $[:ok, sum];

    100
};

std:assert_str_eq handle.recv_ready[] $[:ok, 45];

!res = handle.join[];

std:assert_str_eq res 100;
```

Here an example on how to pass values to the thread.
Please be aware, that only a limited set of data types can be
passed to a thread. Functions and iterators can not be passed for
instance.

```wlambda
!globals = ${
    a = 99,
    b = $[1, 3, 4],
};

!handle = std:thread:spawn $code {
    a + b.1
} globals;

!res = handle.join[];

std:assert_str_eq res 102;
```

#### <a name="1162-stdthreadsleep-duration"></a>11.6.2 - std:thread:sleep _duration_

Lets the current thread sleep for the given _duration_.
_duration_ can either be an integer that will be interpreted
as milliseconds to sleep. Or a pair, containing the duration unit as first element
and the integer as second element. Following units are supported:

- `$p(:s, _seconds_)`
- `$p(:ms, _milliseconds_)`
- `$p(:us, _microseconds_)`
- `$p(:ns, _nanoseconds_)`

```wlambda
!before = std:time:now :ms;
!thrd = std:thread:spawn $code {
    !@import std;
    std:thread:sleep :ms => 150;
};

thrd.join[];

!after = std:time:now :ms;

std:assert (after - before) >= 150;
```

#### <a name="1163-thread-handle-api"></a>11.6.3 - Thread Handle API

##### <a name="11631-thdljoin"></a>11.6.3.1 - thdl.join

This method will wait for the thread to finish and return
the return value of the thread.

```wlambda
!thdl = std:thread:spawn "4 + 3";
std:assert_eq thdl.join[] 7;
```

This method will return an error if the thread handle was already joined.

##### <a name="11632-thdlrecvready"></a>11.6.3.2 - thdl.recv\_ready

Waits for the global `_READY` atomic value slot to be sent a value by the
thread. This is useful for waiting until the thread has started without an
error before expecting it to run properly.
If an error happens, you will receive an error object as return value of
`recv_ready`:

```wlambda
!thdl = std:thread:spawn $code {
    !@wlambda; # importing `panic`
    panic "SOME ERR";
    _READY.send :ok;
};

!err_msg =
    match thdl.recv_ready[]
        ($e err) => { $\.err.0 }
        v        => $\.v;

$DEBUG err_msg;
std:assert err_msg &> $r/ *SOME\ ERR* /;

thdl.join[];
```

This method might return an error if the thread provider
made a handle without a ready slot.

#### <a name="1164-atom-api"></a>11.6.4 - Atom API

For threads a VVal (WLambda data value) is transformed into a value
that can be shared between threads safely. For this the data values are cloned
deeply and transformed into a structure of atomic values.

These values are then stored in a so called _Atom_. They can be safely changed
by threads.

##### <a name="11641-stdsyncatomnew-value"></a>11.6.4.1 - std:sync:atom:new _value_

Creates an Atom, containing the given _value_. The data types for _value_
is limited to these:

- Numbers (Integer, Float)
- Numerical Vectors
- Vectors
- Maps
- Strings
- Symbols
- Byte vectors
- Pairs
- Booleans
- Optionals
- Errors

And also these special types:

- Atom
- Atom Value Slot
- Channel

```wlambda
!at = std:sync:atom:new $[1, 2, 3];

!thdl = std:thread:spawn $code {
    at.write ~ $@i iter i at.read[] {
        $+ i;
    };
    _READY.send :ok;
} ${ at = at };

thdl.recv_ready[];

std:assert_eq at.read[] 6;

thdl.join[]
```

##### <a name="11642-atomread"></a>11.6.4.2 - atom.read

Returns the value stored in the atom.

```wlambda
!at = std:sync:atom:new 99;

std:assert_eq at.read[] 99;
```

This method might return an error if the internal mutex was poisoned.

##### <a name="11643-atomwrite-value"></a>11.6.4.3 - atom.write _value_

Overwrites the contents of the atom with the given _value_.

```wlambda
!at = std:sync:atom:new 99;

at.write 100;

std:assert_eq at.read[] 100;
```

This method might return an error if the internal mutex was poisoned.

##### <a name="11644-atomswap-value"></a>11.6.4.4 - atom.swap _value_

Returns the previous value of the atom and writes in
the given _value_.

```wlambda
!at = std:sync:atom:new 99;

std:assert_eq at.swap[100] 99;

std:assert_eq at.read[] 100;
```

This method might return an error if the internal mutex was poisoned.

#### <a name="1165-atom-value-slot-api"></a>11.6.5 - Atom Value Slot API

An Atom value slot offers more synchronization than a normal Atom value.
It allows you to set the value of the slot, wait for it to be collected
and wait for a value becoming available.
It can be thought of a single element queue, where the element will be
overwritten when a new value is sent to the slot.

You can theoretically receive or wait from multiple threads and also write from
multiple threads. But be aware, that the order of which threads get to read or
write is determined by the operating system and might lead to reader or writer
starvation.

Best recommendation here is to use a slot only from a single writer and
a single reader.

##### <a name="11651-stdsyncslotnew"></a>11.6.5.1 - std:sync:slot:new

Constructs a new Atom slot and returns it.
The slot has the initial status of being _empty_.
If a value is sent to it, it will not be _empty_ anymore.
After a value is received from the slot, the status is _empty_ again.

##### <a name="11652-atomslotsend-value"></a>11.6.5.2 - atom\_slot.send _value_

This method sends the value into the slot, overwriting any previous
set values. The slot can also ha

```wlambda
!slot = std:sync:slot:new[];

std:assert slot.check_empty[];

slot.send $[:ok, $i(1,2,3)];

std:assert ~ not slot.check_empty[];

std:assert_eq slot.recv[].1 $i(1,2,3);

std:assert slot.check_empty[];
```

This method might return an error if there was an issue with locking
the internal mutex or the mutex was poisoned.

##### <a name="11653-atomslotrecv"></a>11.6.5.3 - atom\_slot.recv

If the slot is empty, it will wait for a value to become available.
Once a value is available it is returned and the slot is set to _empty_ again.

```wlambda
!slot = std:sync:slot:new[];

!thrd = std:thread:spawn $code {
    slot.send 99;
} ${ slot = slot };

std:assert_eq slot.recv[] 99;

thrd.join[];
```

This method might return an error if there was an issue with locking
the internal mutex or the mutex was poisoned.

##### <a name="11654-atomslottryrecv"></a>11.6.5.4 - atom\_slot.try\_recv

This method returns an optional value. It will provide an empty optional
value if no value is stored in the slot. But if the slot contains
a value, it will return the value (wrapped in an optional) and set the
slot to be _empty_ again.

```wlambda
!slot       = std:sync:slot:new[];
!start_flag = std:sync:slot:new[];

!thrd = std:thread:spawn $code {

    start_flag.recv[]; # sync with parent

    slot.send 99;

    _READY.send :ok;

} ${ slot = slot, start_flag = start_flag };

std:assert_eq slot.try_recv[] $o();

start_flag.send :ok;
thrd.recv_ready[];

std:assert_eq slot.try_recv[] $o(99);

thrd.join[];
```

This method might return an error if there was an issue with locking
the internal mutex or the mutex was poisoned.

##### <a name="11655-atomslotrecvtimeout-duration"></a>11.6.5.5 - atom\_slot.recv\_timeout _duration_

Acts like `atom_slot.recv`, but it will only wait for the given _duration_.  If
no value was received in the given _duration_ (see std:thread:sleep), `$o()` is
returned.  Otherwise the optional value will contain the received value.

```wlambda
!slot = std:sync:slot:new[];

std:assert_eq (slot.recv_timeout :ms => 100) $o();

slot.send 4;

std:assert_eq (slot.recv_timeout :ms => 100) $o(4);
```

This method might return an error if there was an issue with locking
the internal mutex or the mutex was poisoned.

##### <a name="11656-atomslotcheckempty"></a>11.6.5.6 - atom\_slot.check\_empty

Returns `$true` if the slot is empty.

This method might return an error if there was an issue with locking
the internal mutex or the mutex was poisoned.

##### <a name="11657-atomslotwaitempty"></a>11.6.5.7 - atom\_slot.wait\_empty

Waits until the slot is empty and then returns `$true`.

This method might return an error if there was an issue with locking
the internal mutex or the mutex was poisoned.

##### <a name="11658-atomslotwaitemptytimeout-duration"></a>11.6.5.8 - atom\_slot.wait\_empty\_timeout _duration_

Waits a predefined timeout until the slot is empty. If it did become
empty within the given _duration_ (see std:thread:sleep) it will return `$true`.
Otherwise it will return `$false`.

This method might return an error if there was an issue with locking
the interal mutex or the mutex was poisoned.

#### <a name="1166-channel-api"></a>11.6.6 - Channel API

A channel is a multiple sender, single consumer queue. It can be used to
establish a message passing based communication between threads.

It is basically a wrapper around the Rust `std::sync::mpsc::channel`.

```wlambda
!chan = std:sync:mpsc:new[];

!thdl = std:thread:spawn $code {
    _READY.send :ok;
    iter i 0 => 10 {
        chan.send $p(:val, i);
    };
    chan.send $p(:quit, $none);
} ${ chan = chan };

match thdl.recv_ready[]
    ($e ?) => { std:assert $false };

!item = $none;

!sum = $@i
    while { .item = chan.recv[]; item.0 == :val } {
        $+ item.1;
    };

std:assert_eq sum 45;

thdl.join[];
```

##### <a name="11661-stdsyncmpscnew"></a>11.6.6.1 - std:sync:mpsc:new

This creates a new channel. You can safely send from multiple threads
while reading from one thread at a time.

```wlambda
!chan = std:sync:mpsc:new[];

chan.send :a;
chan.send :b;
chan.send :c;

std:assert_eq chan.recv[] :a;
std:assert_eq chan.recv[] :b;
std:assert_eq chan.recv[] :c;
```

##### <a name="11662-channelsend-value"></a>11.6.6.2 - channel.send _value_

Sends the given _value_ to the channel queue.

```wlambda
!chan = std:sync:mpsc:new[];

chan.send :a;
std:assert_eq chan.recv[] :a;
```

This method might return an error if the channel failed, for instance due
to a poisoned internal mutex.

##### <a name="11663-channelrecv"></a>11.6.6.3 - channel.recv

Receives the next element from the channel. If no element is available
this method will block the thread until an element becomes available.

```wlambda
!chan = std:sync:mpsc:new[];

chan.send :a;
std:assert_eq chan.recv[] :a;
```

This method might return an error if the channel failed, for instance due
to a poisoned internal mutex.

##### <a name="11664-channeltryrecv"></a>11.6.6.4 - channel.try\_recv

Tries to receive the next element from the channel and return it wrapped
into an optional. If no element is available an empty optional `$o()` is returned.

```wlambda
!chan = std:sync:mpsc:new[];

std:assert_eq chan.try_recv[] $o();

chan.send :a;

std:assert_eq chan.try_recv[] $o(:a);
```

This method might return an error if the channel failed, for instance due
to a poisoned internal mutex.

##### <a name="11665-channelrecvtimeout-duration"></a>11.6.6.5 - channel.recv\_timeout _duration_

Tries to receive the next element in the given _duration_ (see std:thread:sleep)
and return it wrapped into an optional. If no element could be received
within that time an empty optional is returned `$o()`.

```wlambda
!chan = std:sync:mpsc:new[];

std:assert_eq (chan.recv_timeout $p(:ms, 100)) $o();

chan.send :x;

std:assert_eq (chan.recv_timeout $p(:ms, 100)) $o(:x);
```

This method might return an error if the channel failed, for instance due
to a poisoned internal mutex.

## <a name="12-optional-standard-library"></a>12 - Optional Standard Library

### <a name="121-serialization"></a>12.1 - serialization

#### <a name="1211-stdserwlambda-arg"></a>12.1.1 - std:ser:wlambda _arg_

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

#### <a name="1212-stdserjson-data-nopretty"></a>12.1.2 - std:ser:json _data_ \[_no\_pretty_]

Serializes the _data_ and returns a JSON formatted (and pretty printed) string.
Optionally not pretty printed if _no_pretty_ is a true value.

```wlambda
!str = std:ser:json $[1,2.3,${a=4}] $t;
std:assert_eq str "[1,2.3,{\"a\":4}]";
```

#### <a name="1213-stddeserjson-string"></a>12.1.3 - std:deser:json _string_

Deserializes the JSON formatted _string_ into a data structure.

```wlambda
!data = std:deser:json ~ std:ser:json $[1,2.3,${a=4}];
std:assert_eq data.0 1;
std:assert_eq data.1 2.3;
std:assert_eq data.(2).a 4;
```

#### <a name="1214-stdsertoml-data-nopretty"></a>12.1.4 - std:ser:toml _data_ \[no\_pretty\]

Serializes the _data_ and returns a TOML formatted string. Pretty printing does
not much, as white spaces belongs to the TOML format anyways.

```wlambda
!str = std:ser:toml ${main = ${some_option = 100}};
std:assert_eq str "[main]\nsome_option = 100\n";
```

#### <a name="1215-stddesertoml-string"></a>12.1.5 - std:deser:toml _string_

Deserializes/parses a TOML formatted _string_ and returns a data structure.
This is super useful for parsing/reading configuration files.

```wlambda
## Quick example how to read TOML config files:
!config = std:deser:toml ~ std:io:file:read_text "Cargo.toml";
```

#### <a name="1216-stdsercsv-fielddelim-rowseparator-escapeall-table"></a>12.1.6 - std:ser:csv _field\_delim_ _row\_separator_ _escape\_all_ _table_

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

#### <a name="1217-stddesercsv-fielddelim-rowseparator-data"></a>12.1.7 - std:deser:csv _field\_delim_ _row\_separator_ _data_

Parses the string _data_ as CSV. With the field delimiter _field_delim_
and the _row_separator_ for the data rows.

Note: Some CSV files in the wild might come with an UTF8 BOM. You might want to run it through
`std:str:strip_utf8_bom` before.

```wlambda
!table = std:deser:csv ";" "\r\n" "foo;bar\r\nx;y\r\n";
std:assert_eq table.0.0 "foo";
std:assert_eq table.0.1 "bar";
std:assert_eq table.1.1 "y";
```

#### <a name="1218-stdsermsgpack-data"></a>12.1.8 - std:ser:msgpack _data_

Serializes the _data_ and returns a msgpack bytes value.

```wlambda
std:assert_eq (std:ser:msgpack $b"abc") $b"\xC4\x03abc";
```

#### <a name="1219-stddesermsgpack-bytes"></a>12.1.9 - std:deser:msgpack _bytes_

Deserializes the msgpack bytes value into a data structure.

```wlambda
std:assert_eq (std:deser:msgpack $b"\xC4\x03abc") $b"abc";
```

### <a name="122-regular-expressions-more-classic-syntax"></a>12.2 - Regular Expressions (more classic syntax)

WLambda supports a more common form of regular expression syntax
if the "regex" feature is enabled when compiling WLambda.

It uses the regex syntax as described in the Rust "regex" crate:
[https://docs.rs/regex/newest/regex/](https://docs.rs/regex/newest/regex/#syntax).

#### <a name="1221-stdrematch-regex-string-input-string-function"></a>12.2.1 - std:re:match _regex-string_ _input-string_ _function_

If the given regular expression _regex-string_ matches the given _input-string_
the _function_ will be called with a vector containing the matched string
and the captures as first argument.
The return value is the return value of the function call or `$none`.
Returns an error if the _regex-string_ has a syntax error.

```wlambda
!ret = std:re:match $q|(\d+)-(\d+)-(\d+)| "2020-05-01" {
    std:str:join "." std:reverse <& (1 => 3) <&_;
};

std:assert_str_eq ret "01.05.2020";
```

#### <a name="1222-stdrematchcompile-regex-string"></a>12.2.2 - std:re:match\_compile _regex-string_

This function compiles the given _regex-string_ and returns a function that
will execute the regex matching. If the syntax in _regex-string_ is wrong,
an error is returned.

The returned function takes the string to match against as first parameter
and the function that is called if the regex matches as second parameter.

```wlambda
!match_fun = std:re:match_compile $q|(\d+)-(\d+)-(\d+)|;

!ret = match_fun "2020-05-01" {
    std:str:join "." std:reverse <& (1 => 3) <&_;
};

std:assert_str_eq ret "01.05.2020";
```

#### <a name="1223-stdremap-regex-string-function-input-string"></a>12.2.3 - std:re:map _regex-string_ _function_ _input-string_

Executes _function_ for each match of the regex defined by _regex-string_
on the _input-string_. Returns an error if there is a syntax error in the regex.
Otherwise returns the last return value of _function_.

```wlambda
!res = $@vec std:re:map $q|(\d+)-(\d+)-(\d+)| {!(str, d1, d2, d3) = _;
    $+ $[int d1, int d2, int d3]
} "foo bar 2020-05-01 fofo 2020-06-01 bar 2019-12-31";

std:assert_str_eq res $[$[2020,5,1], $[2020,6,1], $[2019,12,31]];
```

#### <a name="1224-stdrereplaceall-regex-string-replace-function-input-string"></a>12.2.4 - std:re:replace\_all _regex-string_ _replace-function_ _input-string_

This function replaces all matches of the regex _regex-string_ in the _input-string_
by the return value of the _replace-function_. Returns an error if there is
a syntax error in the _regex-string_.

```wlambda
!res = std:re:replace_all $q"(\d+)" {
    str (int _.1) + 1
} "foo 32 fifi 99";

std:assert_eq res "foo 33 fifi 100";
```

### <a name="123-xml"></a>12.3 - xml

#### <a name="1231-stdxmlreadsax-xml-string-event-callback-function-do-not-trim-text"></a>12.3.1 - std:xml:read\_sax _xml-string_ _event-callback-function_ [_do-not-trim-text_]

```wlambda
\:x {
    $@v _? :x ~ std:xml:read_sax
        "x<x a='12'>fooo fweor weio ew <i/> foefoe</i></x>y"
        $+;
}[]
```

#### <a name="1232-stdxmlcreatesaxwriter-indent"></a>12.3.2 - std:xml:create\_sax\_writer \[_indent_\]

Creates an XML SAX event based writer function. The function
should be called with single events as received by `std:xml:read_sax`.
To receive the final output of the writer, call the returned function
without any arguments.

```wlambda
!writer = std:xml:create_sax_writer[];
writer $[:start, "test"];
writer $[:end, "test"];

std:assert_eq writer[] "<test></test>";
```

#### <a name="1233-stdxmlcreatetreebuilder"></a>12.3.3 - std:xml:create\_tree\_builder

This function returns a function that acts as WLambda data structure builder
that accepts SAX events as given to the callback of `std:xml:read_sax`.

The returned data structure is a tree build of the following elements:

- `$[:decl,    ${ version=..., encoding=..., standalone=... }]`
- `$[:elem,    name, ${ <attributes> }, $[ <child elements> ]]`
- `$[:comment, text]`
- `$[:pi,      text]`
- `$[:text,    text]`
- `$[:doctype, text]`
- `$[:cdata,   text]`

Here is a more elaborate example on how to use it:

```wlambda
!tb = std:xml:create_tree_builder[];

!tree = std:xml:read_sax $q$
    <foo>
        <i x="here"/>
        hello
    </foo>
    <foo>
        blop<i x="10"/>
        lol
    </foo>$
    tb;

std:assert_str_eq
    tree
    $[:root,$n,$n,$[
        $[:elem,"foo",$n,$[
            $[:elem,"i",${x="here"},$n],$[:text,"hello"]]],
        $[:elem,"foo",$n,$[
            $[:text,"blop"],$[:elem,"i",${x="10"},$n],$[:text,"lol"]]]
    ]];

## Here we use the structure matchers for finding the values of the x attributes
## of the <i> elements:
std:assert_str_eq
    $S[ 3 / *:{0=elem,1=foo} /
        3 / *:{0=elem,1=i} /
        2 / x
    ] <& tree
    $["here","10"];
```

### <a name="124-chrono"></a>12.4 - chrono

#### <a name="1241-stdchronotimestamp-format"></a>12.4.1 - std:chrono:timestamp \[_format_]

For the documentation of _format_ please consule the
chrono Rust crate documentation: [chrono crate strftime format](https://docs.rs/chrono/latest/chrono/format/strftime/index.html#specifiers).

```wlambda
!year_str = std:chrono:timestamp "%Y";
std:displayln :XXXX ~ (year_str | int) == 2023;
std:assert ~ (year_str | int) == 2023;

!now_str = std:chrono:timestamp[];
```

#### <a name="1242-stdchronoformatutc-utc-timestamp-format---string"></a>12.4.2 - std:chrono:format\_utc _utc-timestamp_ \[_format_\] -> _string_

Formats the given _utc-timestamp_ in seconds according to _format_.

For the documentation of _format_ please consule the
chrono Rust crate documentation: [chrono crate strftime format](https://docs.rs/chrono/latest/chrono/format/strftime/index.html#specifiers).

```wlambda
!year_str = std:chrono:format_utc 1603796989 "%H:%M:%S %Y";

std:assert_str_eq year_str "11:09:49 2020";
```

#### <a name="1243-stdchronoformatlocal-utc-timestamp-format---string"></a>12.4.3 - std:chrono:format\_local _utc-timestamp_ \[_format_\] -> _string_

Formats the given _utc-timestamp_ in seconds according to _format_ in the local timezone.

For the documentation of _format_ please consule the
chrono Rust crate documentation: [chrono crate strftime format](https://docs.rs/chrono/latest/chrono/format/strftime/index.html#specifiers).

```wlambda
!year_str = std:chrono:format_local 1603796989 "%H:%M:%S %Y";

std:assert_str_eq year_str "12:09:49 2020";
```

### <a name="125-color-conversion"></a>12.5 - color conversion

This section highlights the color conversion functions available in WLambda.
Numerical vectors are used in WLambda to represent colors. There are two
representations of a color.

If you use a float vector `$f(r, g, b, a)` the values for RGB are in the range
of 0.0 to 1.0. For HSV `$f(h, s, v, a)` h is within 0.0 to 360.0 while the
others are in the range 0.0 to 1.0.

For integer vectors the values for RGB are in the range 0 to 255.
And the values for HSV are in the range 0 to 360, and the others in the range
0 to 100.

You can also use 3 dimensional vectors without the alpha value: `$i(r, g, b)` / `$i(h, s, v)`
and `$f(r, g, b)` / `$f(h, s, v)`.

#### <a name="1251-stdvrgb2hsv-color-vector"></a>12.5.1 - std:v:rgb2hsv _color-vector_

Converts an RGB color into a HSV color representation.

```wlambda
std:assert_eq std:v:rgb2hsv <& $i(0, 255, 0, 255)   $i(120, 100, 100, 100);
std:assert_eq std:v:rgb2hsv <& $i(0, 255, 0)        $i(120, 100, 100);

std:assert_eq std:v:rgb2hsv <& $f(0, 1.0, 0, 1.0)   $f(120, 1, 1, 1);
std:assert_eq std:v:rgb2hsv <& $f(0, 1.0, 0)        $f(120, 1, 1);

std:assert_eq std:v:rgb2hsv <& $f(0, 0.5, 0, 1.0)     $f(120, 1, 0.5, 1);
std:assert_eq std:v:rgb2hsv <& $f(0.1, 0.5, 0.1, 1.0) $f(120, 0.8, 0.5, 1);
```

#### <a name="1252-stdvhsv2rgb-color-vector"></a>12.5.2 - std:v:hsv2rgb _color-vector_

Converts a color from HSV to RGB representation.

```wlambda
std:assert_eq std:v:hsv2rgb <& $i(120, 80, 50, 100)   $i(25,128,25,255);

!clr = std:v:hsv2rgb <& $f(120, 0.8, 0.5, 1.0);
std:assert_rel_eq clr.r 0.1 0.001;
std:assert_rel_eq clr.g 0.5 0.001;
std:assert_rel_eq clr.b 0.1 0.001;
```

#### <a name="1253-stdvrgba2hex-color-vector"></a>12.5.3 - std:v:rgba2hex _color-vector_

This function converts a color to a string of hex digits (without the common '#'
prefix however).

```wlambda
std:assert_eq std:v:rgba2hex <& $i(255, 128, 64, 32)       "ff804020";
std:assert_eq std:v:rgba2hex <& $f(1.0, 0.5, 0.25, 0.125)  "ff804020";
```

#### <a name="1254-stdvhex2rgbaf-string"></a>12.5.4 - std:v:hex2rgba\_f _string_

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

#### <a name="1255-stdvhex2rgbai-string"></a>12.5.5 - std:v:hex2rgba\_i _string_

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

#### <a name="1256-stdvhex2hsvai-string"></a>12.5.6 - std:v:hex2hsva\_i _string_

Converts the hex represenation of a HSVA color to an integer vector `$i(h, s, v, a)`.
This function is probably not that useful, as the bit distribution along
the 3 bytes is not ideal. If you want to store colors properly, don't use this.
It's mostly useful for testing and quick experiments.

```wlambda
!color = std:v:hex2hsva_i "FF8040FF";
std:assert_eq color $i(360,50,25,100);
```

#### <a name="1257-stdvhex2hsvaf-string"></a>12.5.7 - std:v:hex2hsva\_f _string_

Converts the hex represenation of a HSVA color to a float vector `$i(h, s, v, a)`.
This function is probably not that useful, as the bit distribution along
the 3 bytes is not ideal. If you want to store colors properly, don't use this.
It's mostly useful for testing and quick experiments.

```wlambda
!color = std:v:hex2hsva_f "FF8040FF";

std:assert_rel_eq color.0 360.0 1.0;
std:assert_rel_eq color.1  50.0 1.0;
std:assert_rel_eq color.2  25.0 1.0;
std:assert_rel_eq color.3 100.0 1.0;
```

### <a name="126-hash"></a>12.6 - hash

#### <a name="1261-stdhashfnv1a-arg1-"></a>12.6.1 - std:hash:fnv1a _arg1_ ...

Hashes all the arguments as FNV1a and returns an integer.

### <a name="127-rand"></a>12.7 - rand

#### <a name="1271-stdrandsplitmix64new"></a>12.7.1 - std:rand:split\_mix64\_new

Initializes the _sm_state_ from the current time (seconds) and returns it.
The time is retrieved in seconds, so don't expect different seed states
if you call this multiple times in the same wall clock second.
The returned value is supposed to be passed to `rand:split_mix64_next`
or `rand:split_mix64_next_open01`.

#### <a name="1272-stdrandsplitmix64newfrom-seed"></a>12.7.2 - std:rand:split\_mix64\_new\_from _seed_

Initializes the _sm_state_ from the given _seed_ and returns it.
The returned value is supposed to be passed to `rand:split_mix64_next`
or `rand:split_mix64_next_open01`.

#### <a name="1273-stdrandsplitmix64next-smstate-count"></a>12.7.3 - std:rand:split\_mix64\_next _sm\_state_ \[_count_]

Returns the _count_ next integer values generated from the given
_sm_state_.

#### <a name="1274-stdrandsplitmix64nextopen01-smstate-count"></a>12.7.4 - std:rand:split\_mix64\_next\_open01 _sm\_state_ \[_count_]

Returns the _count_ next float values (in an open [0, 1) interval)
generated from the given _sm_state_.

#### <a name="1275-stdrandsplitmix64nextopenclosed01-smstate-count"></a>12.7.5 - std:rand:split\_mix64\_next\_open\_closed01 _sm\_state_ \[_count_]

Returns the _count_ next float values (in an open (0, 1] interval)
generated from the given _sm_state_.

#### <a name="1276-stdrandsplitmix64nextclosedopen01-smstate-count"></a>12.7.6 - std:rand:split\_mix64\_next\_closed\_open01 _sm\_state_ \[_count_]

Returns the _count_ next float values (in an open [0, 1) interval)
generated from the given _sm_state_.

### <a name="128-utility-functions"></a>12.8 - Utility Functions

#### <a name="1281-stddumpupvals-function"></a>12.8.1 - std:dump\_upvals _function_

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

#### <a name="1282-stdwlambdaversion"></a>12.8.2 - std:wlambda:version

Returns the version number of the WLambda crate when called.

#### <a name="1283-stdwlambdasizes"></a>12.8.3 - std:wlambda:sizes

Writes a table of internal data structure sizes to stdout. Just for development
purposes.

#### <a name="1284-stdwlambdaparse-string"></a>12.8.4 - std:wlambda:parse _string_

Parses _string_ as if it was a piece of WLambda code and returns an abstract syntax tree.

```wlambda
std:assert_str_eq
    (std:wlambda:parse "1 + 2")
    $[$%:Block,$[$%:BinOpAdd,1,2]]
```

### <a name="129-http-client"></a>12.9 - HTTP Client

WLambda offers an optional integrated HTTP client by enabling the `reqwest`
feature at compile time. With this you can create a new client using `std:http:client:new` and make HTTP requests using `std:http:get`, `std:http:post` and `std:http:request`. Also support for basic authentication and token based bearer authentication
is there.

#### <a name="1291-stdhttpclientnew"></a>12.9.1 - std:http:client:new

Creates a new HTTP client instance and returns it. You can use it to make
HTTP requests afterwards.

```wlambda
!client = std:http:client:new[];
!response = std:http:get client "https://duckduckgo.com/";

!body = std:str:from_utf8_lossy response.body;
std:assert ($p(0, "</html>") body) > 0;
std:assert_eq response.status 200;
std:assert_eq response.headers.content-type "text/html; charset=UTF-8";
```

See also `std:http:get` for a more elaborate example with providing headers.

#### <a name="1292-stdhttpget-http-client-url-string-headers-and-options-map"></a>12.9.2 - std:http:get _http-client_ _url-string_ [_headers-and-options-map_]

Performs a HTTP GET request to the given _url-string_ using the _http-client_.
The _headers-and-options-map_ can contain following special keys apart from your
custom HTTP headers themself:

- `@basic_auth` with `$[user, $none]` or `$[user, password]` as value.
- `@bearer_auth` with `token` as value.
- `@timeout` with a timeout duration as value. (See also `std:thread:sleep`).
- `@dump` will dump the HTTP Request information and response information
in an extra data structure. The return value of the `std:http:get` (and others)
will then be a pair of `$p(response, $p(request_dump, response))`.
- `@query` with a map of query parameters and their values to modify the
query string of the _url-string_. This properly encodes the strings.

The client will either return an error or a response map with following keys:

- `status` contains the HTTP response code (usually `200` if everything went fine).
- `reason` contains a human readable canonical reason string for the status.
- `body` contains the byte vector with the body data.
- `headers` contains a map of all headers, where the keys are the lower case
header names.

Here is an example on how to use this while providing HTTP Headers:

```wlambda
!client = std:http:client:new[];
!response =
    std:http:get client "https://crates.io/api/v1/crates?page=1&per_page=10&q=wlambda" ${
        Accept        = "application/json",
        Cache-Control = "no-cache",
        DNT           = 1,
        User-Agent    = "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:97.0) Gecko/20100101 Firefox/97.0",
    };

!body = std:deser:json ~ std:str:from_utf8_lossy response.body;
## std:displayln ~ std:ser:json body $f;
std:assert_eq body.crates.0.name            "wlambda";
std:assert_eq response.headers.content-type "application/json";
std:assert_eq response.status               200;
```

#### <a name="1293-stdhttppost-http-client-url-string-body-bytes-headers-and-options-map"></a>12.9.3 - std:http:post _http-client_ _url-string_ _body-bytes_ [_headers-and-options-map_]

This call is like `std:http:get` but makes a HTTP POST request with the given
payload _body_. For HTTP requests with other methods please look at
`std:http:request`. The rest of the options are the same as `std:http:get`. But
here is an example how to transmit a piece of JSON easily:

```wlambda
!client = std:http:client:new[];
!response =
    std:http:post client "http://httpbin.org/post"
        (std:str:to_bytes ~ std:ser:json $[
            :x => 10,
            ${ y = 20 },
        ]);

!body = std:deser:json ~ std:str:from_utf8_lossy response.body;

std:assert_eq body.url "http://httpbin.org/post";
std:assert_str_eq body.json  $[ $["x", 10], ${ y = 20 } ];
std:assert_eq response.status 200;
```

#### <a name="1294-stdhttprequest-http-client-method-string-url-string-body-bytes-headers-and-options-map"></a>12.9.4 - std:http:request _http-client_ _method-string_ _url-string_ [_body-bytes_ [_headers-and-options-map_]]

This call is like `std:http:post` but makes HTTP requests with an almost
arbitrary method with the optional given payload _body_. The rest of the
options are the same as `std:http:get`.


```wlambda
!client = std:http:client:new[];
!response =
    std:http:request client :GET "http://httpbin.org/get";

!body = std:deser:json ~ std:str:from_utf8_lossy response.body;
std:assert_eq body.url "http://httpbin.org/get";
```

Or a POST request:

```wlambda
!client = std:http:client:new[];
!response =
    std:http:request client :POST "http://httpbin.org/post"
        (std:str:to_bytes ~ std:ser:json $[
            :x => 10,
            ${ y = 20 },
        ]);

!body = std:deser:json ~ std:str:from_utf8_lossy response.body;

std:assert_eq body.url "http://httpbin.org/post";
std:assert_str_eq body.json  $[ $["x", 10], ${ y = 20 } ];
std:assert_eq response.status 200;
```

### <a name="1210-operating-system-utilities"></a>12.10 - Operating System Utilities

Some operating system utility functions. Some might only be available if
WLambda was compiled with the corresponding features.

#### <a name="12101-stdosgetclipboardtext"></a>12.10.1 - std:os:get\_clipboard\_text

Only available if WLambda was compiled with the `clipboard` feature.
Retrieves the text contents of the operating system clipboard.

#### <a name="12102-stdossetclipboardtext-string"></a>12.10.2 - std:os:set\_clipboard\_text _string_

Only available if WLambda was compiled with the `clipboard` feature.
Sets the clipboard text contents.

### <a name="1211-mqtt-messaging"></a>12.11 - MQTT Messaging

WLambda offers an optional support for the MQTT protocol. You can setup a MQTT client
as well as an embedded MQTT broker. The very simple integration offers you a very
easy way to setup inter process communication between WLambda applications.

Support for MQTT has to be explicitly compiled into WLambda by selecting the
`mqtt` feature.

#### <a name="12111-stdmqttbrokernew-config"></a>12.11.1 - std:mqtt:broker:new _config_

This function sets up an embedded MQTT broker. A handle is returned that you can use
to publish messages using the locally connected client link.
You can configure it's endpoints via the _config_.
The _config_ offers following keys:

```text
${
    id             = 0,                 # Broker ID
    listen         = "0.0.0.0:1883",    # Broker server endpoint
    console_listen = "0.0.0.0:18088",   # An extra HTTP console endpoint
    name           = "nameofbroker",    # Name of the Broker server
    version        = "v4",              # MQTT Version: "v4", "v5" and "ws"
    link = ${                           # Configure the local link
        client_id = "some_id",          # Link client ID, default is 'wl_local'
        recv = <std:sync:mpsc channel>, # Channel to receive messages for the
                                        ## subscribed topics.
        ## Topics to subscribe to, if not given, '#' will be used:
        topics = $["test/me", "another/topic", ...]
    },
}
```

The default maximum MQTT payload the broker is setup to support is 10240 bytes
(10 kb).

Here is an example:

```wlambda
!broker = std:mqtt:broker:new ${
    listen         = "0.0.0.0:1883",
    console_listen = "0.0.0.0:18080",
};

## sleep a bit until the broker is initialized:
std:thread:sleep :ms => 500;

!chan = std:sync:mpsc:new[];
!cl = std:mqtt:client:new chan "test1" "localhost" 1883;

## let it connect:
std:thread:sleep :ms => 200;

!_ = cl.subscribe "test/me";
!_ = cl.publish "test/me" $b"test123\xFF";

std:assert_str_eq chan.recv[] $p(:"$WL/connected", $n);
std:assert_str_eq chan.recv[] $p(:"$WL/subscribed", $n);
std:assert_str_eq chan.recv[] $p("test/me", $b"test123\xFF");
```

##### <a name="121111-brokerpublish-topic-string-payload-bytes"></a>12.11.1.1 - broker.publish _topic-string_ _payload-bytes_

Publishes the _payload-bytes_ under the _topic-string_. Returns an error
if something went wrong (client not connected, or some other error). It might
block.

#### <a name="12112-stdmqttclientnew-channel-client-id-broker-host-broker-port"></a>12.11.2 - std:mqtt:client:new _channel_ _client-id_ _broker-host_ _broker-port_

This sets up a MQTT client that connects to the given _broker-host_ and _broker-port_.
It will connect and reconnect upon connection failure in the background automatically
for you. So you don't have to manage the connection yourself.

This function returns a client handle that is describe a little bit further below.

The _client-id_ should be a unique ID to identify your MQTT client.

The _channel_ must be a `std:sync:mpsc` channel that you can create using `std:sync:mpsc:new`.
It's the source of incoming messages and connection control information.
It will send you following possible message data:

- `$p(:"$WL/connected", $n)`    - When the client connection was setup
- `$p(:"$WL/error", "some message here...")` - When an error occurred in the connection handling.
- `$p(topic, payload_bytes)` - An incoming MQTT message that belongs to the _topic_.

Here is an example of a common client setup:

```wlambda
!broker = std:mqtt:broker:new ${
    listen         = "0.0.0.0:1883",
    console_listen = "0.0.0.0:18080",
};

## sleep a bit until the broker is initialized:
std:thread:sleep :ms => 500;

!chan = std:sync:mpsc:new[];
!cl = std:mqtt:client:new chan "test1" "localhost" 1883;

## let it connect:
std:thread:sleep :ms => 200;

!_ = cl.subscribe "test/me";
!_ = cl.publish "test/me" $b"test";
!_ = cl.publish "test/me" $b"quit";

!got_some_stuff = $n;

while $t {
    !msg = chan.recv[];
    match msg
        $p(topic, $b"quit") => { break[]; }
        $p(topic, data)     => { .got_some_stuff = std:copy $\; }; # std:copy because $\ is changing!
};

std:assert_eq got_some_stuff.topic "test/me";
std:assert_eq got_some_stuff.data  $b"test";
```

The returned client handle understands the following methods:

##### <a name="121121-mqttclientpublish-topic-string-payload-bytes"></a>12.11.2.1 - mqtt\_client.publish _topic-string_ _payload-bytes_

Publishes the _payload-bytes_ under the _topic-string_. Returns an error
if something went wrong (client not connected, or some other error). It might
block.

##### <a name="121122-mqttclientsubscribe-topic-string"></a>12.11.2.2 - mqtt\_client.subscribe _topic-string_

Subscribes to the _topic-string_. Returns an error if something went wrong.
It might block.

### <a name="1212-cursive-consoleterminal-text-user-interface"></a>12.12 - Cursive Console/Terminal Text User Interface

WLambda comes with a text based user interface library binding to the `cursive` crate.
You will need to enable the feature `cursive` when compiling WLambda. It's not included
in the default feature set of WLambda.

The `view-def` view definition format is defined in an extra section of this reference at the end.

#### <a name="12121-stdcursivenew---cursive"></a>12.12.1 - std:cursive:new -> $\<Cursive\>

Create a new Cursive instance. You can call the following functions on it:

```text
!c = std:cursive:new[];
c.add_layer :panel => ${} =>
    :button => ${
        label = "Hello World!",
        cb = \_.quit[],
    };
c.run[];
```

#### <a name="12122-stdcursiveinstallcursivestdio"></a>12.12.2 - std:cursive:install\_cursive\_stdio

Sets up WLambda to output things that would go to the standard output, to go to the `stdout`
text view. This allows eg. `std:displayln` to be rerouted to the Cursive UI.

#### <a name="12123-cursive-object"></a>12.12.3 - $\<Cursive\> Object

##### <a name="121231-cursiverun---none--error"></a>12.12.3.1 - $\<Cursive\>.run -> $none | $error

Start the event loop. You can only exit this event loop using `$<Cursive>.quit[]`.

##### <a name="121232-cursiveaddlayer-view-def"></a>12.12.3.2 - $\<Cursive\>.add\_layer _view-def_

Adds a layer on top of the current screen and instanciates a _view-def_.
You can remove the layer using `$<Cursive>.pop_layer[]`.

##### <a name="121233-cursivepoplayer"></a>12.12.3.3 - $\<Cursive\>.pop\_layer

Removes the top most layer of the current screen.

##### <a name="121234-cursiveget-name-of-view---none--namedviewnametype"></a>12.12.3.4 - $\<Cursive\>.get _name-of-view_ -> $none | $\<NamedView:Name:Type\>

Retrieves the handle of a specific view. You must not store this handle anywhere except
in a local variable. This handle is only available inside a context where you have
a `$<Cursive>` handle. This is typically only inside an event callback.

Here is an example:

```text
cursive.add_layer :vbox => $[
    :button => ${
        label = "Toggle Button Label",
        name = "LblBtn",
        on_press = {!(cursive) = @;
            (cursive.get "LblBtn").set_label "Yay Button Label";
        },
    },
];
```

##### <a name="121235-cursivepopup-popup-def-view-def"></a>12.12.3.5 - $\<Cursive\>.popup _popup-def_ _view-def_

This opens a popup dialog that shows the view _view-def_. It has a frame, a title and you can
supply multiple buttons for the user to interact with the popup.

The _popup-def_ can contain the following keys:

```text
_popup-def_ = ${
    _view-default-def_,                 # The default definitions a _view-def_ also has.
    title = "window title",             # The window title of the popup
    focus = _button-index_ or $none,    # Focuses a specific button or the content _view-def_

    ## If no buttons (or a close_label) are defined,
    ## a default "Ok" dismiss button will be added.
    ##
    ## The `focus` property allows to focus a specific button from this list:
    buttons = $[
        $p("button1_name", _click-callback_),
        ...
    ],

    ## If defined, a dismiss/close button
    ## with the corresponding Label is added.
    close_label = _string_ or $none_,
};
```

Here is an example for a message popup:

```text
cursive.popup
    ${ title = "Message for you!", }
    :textview => ${ content = "Attention! You did good!" };
```

See also _view-def_ example for the "list" view futher below.

##### <a name="121236-cursiveaddscreen-view-def---screen-id"></a>12.12.3.6 - $\<Cursive\>.add\_screen _view-def_ -> _screen-id_

Creates a new screen (not visible) and instanciates the _view-def_ on a new
layer there.  The new screen ID is returned. To activate the screen use
`$<Cursive>.set_screen id`.

##### <a name="121237-cursivesetscreen-screen-id"></a>12.12.3.7 - $\<Cursive\>.*set\_screen* _screen-id_

Sets the currently visible screen by it's ID as returned by `$<Cursive>.add_screen`.
The first screen has the ID 0.

##### <a name="121238-cursiveactivescreen---screen-id"></a>12.12.3.8 - $\<Cursive\>.active\_screen -> _screen-id_

Returns the current active/visible screen's ID.

##### <a name="121239-cursivesender---cursivesendmsgcb"></a>12.12.3.9 - $\<Cursive\>.sender -> $\<Cursive:SendMsgCb\>

Creates an event sender callback to communicate with the Cursive UI thread/event loop from
another thread. You can call the `$<Cursive:SendMsgCb>` like a regular function.

###### <a name="1212391-cursivesendmsgcb-event-tag-value---true--error"></a>12.12.3.9.1 - $\<Cursive:SendMsgCb\> _event-tag_ _value_ -> $true | $error

Sends the _value_ to the Cursive UI thread's event loop. The _event-tag_
can be used as event name to dispatch specific callbacks that have been installed
using `$<Cursive>.send_cb _event-tag_ { ... }`.

Returns a true value if sending was successful or an `$error`.

##### <a name="1212310-cursivedefaultcb-widget-name-function"></a>12.12.3.10 - $\<Cursive\>.default\_cb _widget-name_ _function_

Installs a default callback for handling events from the correspondig _widget-name_.
The _widget-name_ `"*"` is a wildcard for all widgets without a name assigned.

```text
!cursive = std:cursive:new[];

cursive.add_layer :vbox => $[
    :checkbox => ${ name = :check1, checked = $true },
];

cursive.default_cb :check1 {
    !event_name = _;
    std:displayln "Checkbox 1 event:" @;
};

cursive.default_cb :* {
    std:displayln "Unnamed widget sent event:" @;
};
```

##### <a name="1212311-cursivesetwindowtitle-string"></a>12.12.3.11 - $\<Cursive\>.set\_window\_title _string_

Sets the window title of the application. That title will usually appear in the window title bar of
the console or terminal the application runs in.

#### <a name="12124-cursiveinitconsolelogging"></a>12.12.4 - $\<Cursive\>.init\_console\_logging

Enables logging of Cursive debugging information to the console. Use `$<Cursive>.toggle_debug_console[]`
to show/hide the debug console.

#### <a name="12125-cursivetoggledebugconsole"></a>12.12.5 - $\<Cursive\>.toggle\_debug\_console

Shows/Hides the Cursive debugging console. See also `$<Cursive>.init_console_logging[]`.

#### <a name="12126-cursiveshowdebugconsole"></a>12.12.6 - $\<Cursive\>.show\_debug\_console

Shows the Cursive debugging console. See also `$<Cursive>.init_console_logging[]`.

##### <a name="121261-cursivequit"></a>12.12.6.1 - $\<Cursive\>.quit

Quit the main event loop.

##### <a name="121262-cursivecounter---cursivecounter"></a>12.12.6.2 - $\<Cursive\>.counter -> $\<Cursive:Counter\>

Creates a counter instance to be used by the `progress` view-def. It's initialized with 0.
It returns a `$<Cursive:Counter>` object. See further below about an API definition of that
object.

```wlambda
!c = std:cursive:new[];
!counter = c.counter[];

## Set the counter value:
counter.set 10;

## Setting the counter from another thread:
std:thread:spawn $code{
    counter.set_update 20;
} ${ counter = counter };

## Increment the counter by 11 ticks:
counter.tick 11;

## Get the counter value:
std:assert counter.get[] > 0;
```

#### <a name="12127-cursivecounter-object"></a>12.12.7 - $\<Cursive:Counter\> Object

This section describes the `$<Cursive:Counter>` object API. It is instanciated
by the `$<Cursive>.counter` method.

##### <a name="121271-cursivecounterget"></a>12.12.7.1 - $\<Cursive:Counter\>.get

Returns the current counter integer value.

##### <a name="121272-cursivecounterset-integer"></a>12.12.7.2 - $\<Cursive:Counter\>.set _integer_

Sets the current counter integer value.

##### <a name="121273-cursivecountersetupdate-integer"></a>12.12.7.3 - $\<Cursive:Counter\>.set\_update _integer_

Sets the current counter integer value and sends a signal to the Cursive UI thread
to update it's views. This can be used to update a `progress` bar from another thread.

It may returns an error if the Cursive UI thread is not available anymore.

##### <a name="121274-cursivecountertick-integer"></a>12.12.7.4 - $\<Cursive:Counter\>.tick _integer_

Increments the counter by _integer_.

##### <a name="121275-cursivecountertickupdate-integer"></a>12.12.7.5 - $\<Cursive:Counter\>.tick\_update _integer_

Increments the counter by _integer_ and updates the Cursive UI thread.

It may returns an error if the Cursive UI thread is not available anymore.

## <a name="13-wlambda-lexical-syntax-and-grammar"></a>13 - WLambda Lexical Syntax and Grammar

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
    ascii_c_name  = (* note: upper and lower case versions are possible *)
                    "NULL" | "SOH" | "STX" | "ETX" | "EOT" | "ENQ" | "ACK"
                  | "BEL" | "BS" | "HT" | "LF" | "VT" | "FF" | "CR" | "SO"
                  | "SI" | "DLE" | "DC1" | "DC2" | "DC3" | "DC4" | "NAK"
                  | "SYN" | "ETB" | "CAN" | "EM" | "SUB" | "ESC" | "FS"
                  | "GS" | "RS" | "US" | "DEL" | "SPACE" | "NBSP"
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
                  | "\\<", ascii_c_name, ">" (* the corresponding ascii value *)
                  ;
    string_lit    = string
                  | character
                  | "$", quote_string
                  | "$", byte_string
                  | "$", code_string
                  ;
    character     = "'", ( "\\", string_escape | ?any character? - "\\" and "'" ), "'"
                  ;
    string        = "\"", { "\\", string_escape | ?any character? - "\\" and "\"" },"\""
                  ;
    byte_char     = "b", character
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
    pattern       = "r", [ "g" ], (* "g" for find all *)
                         ?any character as quote?,
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
    formatter     = "F", string_lit (* compiles the string into a formatter, that
                                       takes as many arguments as there are format
                                       specifiers in the string. See also: String
                                       formatting syntax in the next section. *)
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
    code_string   = ("c" | "code" ), block
                  | ("c" | "code" ), expr
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
    syntax        = "%:", {? any possible Syntax Type Identifier
                             eg. "Block", "Call", ... ?}
                    (* A syntax VVal, the possible identifiers are one of the
                       possible result symbols of std:syn:type. This also
                       saves the current parser position. *)
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
    debug_print   = "DEBUG" (* evaluates to a debug print function that
                               also prints source position and dynamic type.
                               very useful for debugging. *)
                  ;
    import        = "@import", ident, [ ident ]
                  ;
    export        = "@export", ident, [ "=" ], expr
                  ;
    capture_ref   = ":", var
                  ;
    deref         = "*", value
                  ;
    special_value = list
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
                  | debug_print
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
                  | string_lit
                  | "$", special_value
                  | "(", expr, ")"
                  | function
                  | symbol
                  | var
                  ;
    op            = (* here all operators are listed line by line regarding
                       their precedence, top to bottom *)
                    "&>" | "&@>"      (* call rhs with lhs operator *)
                  | "<&" | "<@&"      (* call lhs with rhs operator *)
                  | "//" | "/?" | "/$n" | "/$o" | "/$e" (* default value operators *)
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
                  | "+>"              (* take lhs, wrap it into list if not already
                                         and append the right side.
                                         if lhs is an iterator, append all elements. *)
                  | "<+"              (* take rhs, wrap it into list if not already
                                         and prepend the left side.
                                         if rhs is an iterator, prepend all elements. *)
                  ;
    bin_op        = call_no_ops, { op, bin_op } (* precedence parsing is done
                                                   in a Pratt parser style *)
                  ;
    arg_list      = "[", [ expr, { ",", expr }, [ "," ] ], "]"
                  | "[[", expr, "]]"  (* apply result vector of expr as argument list *)
                  ;
    field         = ".", ( integer | ident | value ), [ field ]
                  ;
    field_access  = field, [ op ], "=", expr
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
                          an identifier as field, it is quoted and
                          interpreted as symbol. *)
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
    simple_assign = qident, [ op ], "=", expr
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

### <a name="131-special-forms"></a>13.1 - Special Forms

There are certain calls that are handled by the compiler differently.

- `if _condition_ _then-block-or-expr_ [_else-block-or-expr_]`
- `while _condition_ _block-or-expr_`
- `iter _var_ _value-expr_ _block-or-expr_`
- `next _x_`
- `break`
- `match _value-expr_ $p(structure_pattern, branch_block) ... \[branch_block\]
- `jump _idx-expr_ _block1_ ...`

### <a name="132-string-formatting-syntax"></a>13.2 - String Formatting Syntax

The `$F` special value takes a string and creates a formatting function.
The syntax for formatting is very similar to Rust's string formatting:

```ebnf
    format_string = <text>, { maybe-format, <text> }
                  ;
    maybe-format  = '{', '{'
                  | '}', '}'
                  | <format>
                  ;
    format        = '{' [ argument ], [ ':' format_spec ] '}'
                  ;
    argument      = integer    (* index into argument vector *)
                  | identifier (* requires a map as parameter *)
                  ;

    format_spec   = [[fill]align][sign]['#']['0'][width]['.' precision]['!' cast_type][type]
                  ;
    fill          = character
                  ;
    align         = '<' | '^' | '>'
                  ;
    sign          = '+' | '-'
                  ;
    width         = count
                  ;
    precision     = count | '*'
                  ;
    cast_type     = 'i'             (* interpret arg as integer *)
                  | 'f'             (* interpret arg as float *)
                  | 's'             (* (default) interpret arg as string *)
                  ;
    type          = 'x'             (* hex lower case *)
                  | 'X'             (* hex upper case *)
                  | 'b'             (* binary *)
                  | 'o'             (* octal *)
                  | 'j', character  (* a string join of a vector,
                                       separated by 'character' *)
                  | 'C', character  (* A CSV row, separated by 'character'.
                                       This also supports proper
                                       escaping using '"' *)
                  | 'J', ['p']      (* JSON, optional pretty printed *)
                  | 'w', ['p']      (* print written WLambda representation,
                                       optional pretty printed *)
                  | ''
                  ;
    count         = parameter | integer
                  ;
    parameter     = argument, '$'
                  ;
```

### <a name="133-format-string-syntax-for-stdbytespack-and-stdbytesunpack"></a>13.3 - Format String Syntax for std:bytes:pack and std:bytes:unpack

This syntax describes the accepted format strigns for the `std:bytes:pack` and
`std:bytes:unpack`. The format is loosely adapted from the Lua syntax for
`string.pack` and `string.unpack`.

| Syntax | Semantics |
|-|-|
| `<` | Sets little endian |
| `>` | Sets big endian |
| `=` | Sets native endian |
| `x` | One zero byte of padding |
| `y` | Byte content with unspecified length. On `unpack` this reads to the end of the input.  |
| `z` | A zero-terminated string of bytes. |
| `c<n>` | An `n` long field of bytes. |
| `b` | A single byte. |
| `s<bits>` | A string of bytes that is prefixed with a `<bits>` wide binary length field. |
| `u<bits>` | A `<bits>` wide unsigned integer field. |
| `i<bits>` | A `<bits>` wide signed integer field. |
| `f` | A float field (32 bits). |
| `d` | A double field (64 bits). |

- `<n>` can be any number.
- `<bits>` can be 8, 16, 32, 64 or 128.

## <a name="14-cursive-view-definition"></a>14 - Cursive View Definition

The `Cursive` TUI view definition describes the data structure layout
for defining `Cursive` views. You can specify the layout, the properties
and the callbacks in this definition.

### <a name="141-size-def-cursive-widthheight-size-definition"></a>14.1 - size-def Cursive Width/Height Size Definition

There are the following size specifications possible:

- `:free`
- `:full` makes the view use the available space
- `:fixed => characters` makes the view the specified amount of characters wide/high
- `:min => characters` makes the view the at least as big as the amount of characters specified
- `:max => characters` makes the view the at most as big as the amount of characters specified

### <a name="142-view-default-def-view-default-definitions"></a>14.2 - view-default-def View Default Definitions

These properties are available for all _view-def_ of any kind of view.

```code
_view-default-def_ = ${
    ## The name allows you to retrieve a temporary view handle
    ## view using the `$<Cursive>.get` method.
    ## You can call methods that depend on the type of view on that handle.
    name = "aViewName",

    ## The horizontal and vertical sizes of this view.
    ## See above about the possible values for _size-def_
    ## (eg. `:free`, `:full`, `:fixed => 30`, `:min => 30`, `:max => 30`).
    size_w = $none | _size-def_,
    size_h = $none | _size-def_,

    ## If not `$none`: Wraps the view in a scrolled view with
    ## the corresponding scroll strategy. You will get a scroll bar if the view
    ## does not fit inside the available space.
    scroll = $none | "top" | "bottom" | "row",

    ## Enables/disables the vertical scroll bar. (Enabled by default)
    scroll_y = $true | $false,

    ## Enables/disables the horizontal scroll bar. (Disabled by default)
    scroll_x = $true | $false,

    ##  Wraps the view into a hideable view, which can be shown
    ##  or hidden. For a listing of the available methods see below.
    hideable_name = "hide_name",
};
```

#### <a name="1421-hideable-views"></a>14.2.1 - Hideable Views

You can specify the `hideable_name` property in the _view-default-def_ as specified above.
In that case you can refer to the hideable part of that view by name and call one of the following
methods on that.

```text
cursive.add_layer :vbox => $[
    :button => ${
        label = "Test",
        hideable_name = "TestHideBtn",
    },
    :button => ${
        label = "Hide Test",
        on_press = {!(cursive) = @;
            (cursive.get "TestHideBtn").hide[];
        },
    },
];
```

##### <a name="14211-hideablesetvisible-bool"></a>14.2.1.1 - hideable.set\_visible _bool_

Sets the visibility of the view.

##### <a name="14212-hideableisvisible---bool"></a>14.2.1.2 - hideable.is\_visible -> _bool_

Returns the visibility of the view.

##### <a name="14213-hideablehide"></a>14.2.1.3 - hideable.hide

Hides the view.

##### <a name="14214-hideableunhide"></a>14.2.1.4 - hideable.unhide

Shows the view.

### <a name="143-view-def-panel-grouping-views-in-a-panel"></a>14.3 - view-def `panel` Grouping Views in a Panel

A panel is usually for visual separation and grouping of other views.
This panel view is wrapping around another _view-def_.

```text
    :panel => ${ config } => _view-def_
```

```text
    :panel => ${
        _view-default-def_, # The default definitions a _view-def_ also has.
        title = "title of the panel", # Can be left out for no title.
    } => _view-def_
```

Here is an example that just makes a border around a text view:

```text
    cursive.add_layer
        :panel => ${} => :textview => ${ content = "Test is framed!" };
```

### <a name="144-view-def-list-a-list-view-with-labels"></a>14.4 - view-def `list` A List View with Labels

This view allows you to make an entry form with labels in front of views that are vertically
arranged.

```text
    :list => ${
        _view-default-def_, # The default definitions a _view-def_ also has.

        ## This callback is called when a certain entry is selected.
        on_select = {!(cursive, item) = @; ... },

        ## A list of pairs that contain the label and the _view-def_.
        ## If you want to add a delimiter, use `$none`.
        childs = $[
            $p("label 1", _view-def1_),     # Add a view
            $none,                          # Delimiter
            $p("label 2", _view-def2_),     # Add a view
            ...                             # Add more views
        ],
    }
```

Here is an example of a simple configuration dialog:

```text
    !CONFIG = ${
        port1 = 12931,
        port2 = 39392,
    };

    cursive.popup ${
        title = "Settings",
        width = :min => 50,
        buttons = $[
            $p("Next", {!(cursive) = @;
                open_some_other_dialog_here cursive;
            }),
            $p("Exit", { _.quit[] })
        ],
    } :list => ${
        childs = $[
            $p("Port1", :edit => ${
                content = CONFIG.port1, on_edit = {!(cursive, text, cursor) = @;
                    CONFIG.port1 = int text;
                },
            }),
            $p("Port2", :edit => ${
                content = CONFIG.port2, on_edit = {!(cursive, text, cursor) = @;
                    CONFIG.port2 = int text;
                },
            }),
        ],
    };
```

### <a name="145-view-def-hbox-horizontal-layout"></a>14.5 - view-def `hbox` Horizontal Layout

This horizontal box is a short hand form to define a horizontal layout of
other views. There are no auto wrap properties definable here.

```text
    :hbox => $[
        view-def1,
        view-def2,
        ...
    ]
```

### <a name="146-view-def-vbox-vertical-layout"></a>14.6 - view-def `vbox` Vertical Layout

This vertical box is a short hand form to define a vertical layout of
other views. There are no auto wrap properties definable here.

```text
    :vbox => $[
        view-def1,
        view-def2,
        ...
    ]
```

### <a name="147-view-def-progress-progress-bar"></a>14.7 - view-def `progress` Progress Bar

## <a name="15-wlambda-interpreter-cli-command-line-interface"></a>15 - WLambda Interpreter (CLI) Command Line Interface

There are currently the following command line parameters available:

```text
-p <script or zip> <output binary file>     # Packing a script or ZIP into an executable
-P <script or zip> <output binary file>     # Packing a script or ZIP into an executable but
                                            ## disabling the copyright & license output.
-x <output script or zip>                   # Unpacking a script or ZIP from a packed executable
-parse <file>                               # Parse the given <file> and check for syntax errors
-e <wlambda code>                           # Execute the <wlambda code> directly.
<file>                                      # Execute the <file>
```

### <a name="151-wlambda-script-to-executable-packing"></a>15.1 - WLambda Script To Executable Packing

You can append WLambda scripts to the WLambda binary using the `-p` command line parameter.
This feature makes your script easily portable as single binary.
You can either pack a single WLambda script file, or a ZIP archive of multiple WLambda
files. In the latter case the `main.wl` file will be executed and the other files
will be made available as modules you can `!@import` into your `main.wl`.

**Attention: You need to enable the `zip` feature for the ZIP files to work!**

Here is an example for Windows with EXE files. On Linux you will need to make
the resulting binary file executable with `chmod a+x <filename>`:

```text
---- test.wl --------------
std:displayln "Hello World!";
---------------------------

> wlambda -p test.wl my_test.exe
Written 'my_test.exe'

> my_test
WLambda Version 0.8.1
Copyright (C) 2020-2022 Weird Constructor <weirdconstructor@gmail.com>
License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.

For documentation visit: <http://wlambda.m8geil.de>.

Hello World!
```

You can use the `-X` command line option to pack an executable and disable the copyright
and license information.

You can unpack the `my_test.exe` file using the `-x` command line parameter.
Here an example for the Windows `cmd.exe` shell:

```text
> my_test -p out_test.wl
Written 'out_test.wl'

> type out_test.wl
std:displayln "Hello World!"
```


[]: ---- REFERENCE DOC END ----

*/

const VERSION: &str = env!("CARGO_PKG_VERSION");

use crate::compiler::*;
use crate::vval::*;
use crate::util;
use std::rc::Rc;
use crate::threads::*;
use crate::selector::*;
use crate::formatter::*;
use crate::io::print_value;

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

macro_rules! add_multi_op_zero {
    ($g: ident, $op: tt, $err: expr) => {
        add_func!($g, $op, env, argc, {
            if argc <= 0 { return Ok(VVal::None); }
            if let VVal::Flt(f) = env.arg(0) {
                let mut accum = f;
                for i in 1..argc { accum = accum $op env.arg(i).f() }
                Ok(VVal::Flt(accum))

            } else {
                let mut accum = env.arg(0).i();
                for i in 1..argc {
                    let v = env.arg(i).i();
                    if v == 0 {
                        return
                            Err(StackAction::panic_str(
                                format!("{}", $err),
                                None,
                                env.argv()));
                    }
                    accum = accum $op v;
                }
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
        func!($g, $op, |env: &mut Env, argc: usize| {
            if argc < 2 { return Ok(VVal::None); }
            let $a = env.arg(0);
            let $b = env.arg(1);
            $e
        }, Some(2), Some(2), false);
    }
}

macro_rules! add_num_fun {
    ($g: ident, $op: literal, $e: tt) => {
        func!($g, $op,
            |env: &mut Env, _argc: usize| {
                Ok(match env.arg(0).deref() {
                    VVal::Int(f) => { VVal::Int(f.$e()) }
                    v            => { VVal::Flt(v.f().$e()) }
                })
            }, Some(1), Some(1), false);
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

macro_rules! process_vec_input {
    ($env: ident, $arg: ident, $v: ident, $x: ident, $y: ident, $z: ident, $w: ident, $three_i: block, $three_f: block, $four_i: block, $four_f: block) => {
        match $arg.nvec_len() {
            3 => match $arg {
                VVal::FVec($v) => {
                    let $x = $v.x_raw();
                    let $y = $v.y_raw();
                    let $z = $v.z_raw().unwrap_or(0.0);
                    $three_f
                },
                VVal::IVec($v) => {
                    let $x = $v.x_raw();
                    let $y = $v.y_raw();
                    let $z = $v.z_raw().unwrap_or(0);
                    $three_i
                },
                _ => {
                    Ok($env.new_err(
                        "expects float or int vectors".to_string()))
                },
            },
            4 => match $arg {
                VVal::FVec($v) => {
                    let $x = $v.x_raw();
                    let $y = $v.y_raw();
                    let $z = $v.z_raw().unwrap_or(0.0);
                    let $w = $v.w_raw().unwrap_or(0.0);
                    $four_f
                },
                VVal::IVec($v) => {
                    let $x = $v.x_raw();
                    let $y = $v.y_raw();
                    let $z = $v.z_raw().unwrap_or(0);
                    let $w = $v.w_raw().unwrap_or(0);
                    $four_i
                },
                _ => {
                    Ok($env.new_err(
                        "v:rgba2hex expects float or int vectors"
                        .to_string()))
                },
            },
            _ => Ok($env.new_err(
                "expects 3 or 4 dimensional vectors".to_string()))
        }
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
    add_multi_op_zero!(st, /, "Division by 0");
    add_multi_op_zero!(st, %, "Remainder with divisor by 0");

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

    func!(st, "//",
        |env: &mut Env, _argc: usize| {
            let a = env.arg(0);
            let b = env.arg(1);

            Ok(match a {
                VVal::Opt(Some(a)) => a.as_ref().clone(),
                VVal::Opt(None)    => b,
                VVal::None         => b,
                _                  => a,
            })
        }, Some(2), Some(2), true);

    func!(st, "/?",
        |env: &mut Env, _argc: usize| {
            let a = env.arg(0);
            let b = env.arg(1);

            Ok(match a {
                VVal::Opt(Some(a)) => a.as_ref().clone(),
                VVal::Opt(None)    => b,
                VVal::Err(_)       => b,
                VVal::None         => b,
                _                  => a,
            })
        }, Some(2), Some(2), true);

    func!(st, "/$n",
        |env: &mut Env, _argc: usize| {
            let a = env.arg(0);
            let b = env.arg(1);

            Ok(match a {
                VVal::None => b,
                _          => a,
            })
        }, Some(2), Some(2), true);

    func!(st, "/$o",
        |env: &mut Env, _argc: usize| {
            let a = env.arg(0);
            let b = env.arg(1);

            Ok(match a {
                VVal::Opt(Some(a)) => a.as_ref().clone(),
                VVal::Opt(None)    => b,
                _                  => a,
            })
        }, Some(2), Some(2), true);

    func!(st, "/$e",
        |env: &mut Env, _argc: usize| {
            let a = env.arg(0);
            let b = env.arg(1);

            Ok(match a {
                VVal::Err(_) => b,
                _            => a,
            })
        }, Some(2), Some(2), true);

    func!(st, "+>",
        |env: &mut Env, argc: usize| {
            use crate::compiler::collection_add;
            collection_add(env, argc, CollectionAdd::Push)
        }, None, None, false);

    func!(st, "<+",
        |env: &mut Env, argc: usize| {
            use crate::compiler::collection_add;
            collection_add(env, argc, CollectionAdd::Unshift)
        }, None, None, false);

    func!(st, "not",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Bol(!env.arg(0).b()))
        }, Some(1), Some(1), false);

    func!(st, "panic",
        |env: &mut Env, _argc: usize| {
            Err(env.new_panic(env.arg(0)))
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
                        Some(err_v.borrow().1.clone()),
                        err_v.borrow().0.clone()))
                },
                VVal::Opt(None) => {
                    Err(StackAction::panic_str(
                        "unwrap empty option!".to_string(), None,
                        VVal::None))
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
                        e.push(VVal::Int(err_v.borrow().1.line() as i64));
                        e.push(VVal::Int(err_v.borrow().1.col() as i64));
                        e.push(VVal::new_str(err_v.borrow().1.filename()));
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
    func!(st, "char",
        |env: &mut Env, _argc: usize| { Ok(VVal::Chr(VValChr::Char(env.arg(0).c()))) },
        Some(1), Some(1), false);
    func!(st, "byte",
        |env: &mut Env, _argc: usize| { Ok(VVal::Chr(VValChr::Byte(env.arg(0).byte()))) },
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
    func!(st, "is_char",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_char())) },
        Some(1), Some(1), true);
    func!(st, "is_byte",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_byte())) },
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

    func!(st, "filter",
        |env: &mut Env, _argc: usize| {
            let f   = env.arg(0);
            let val = env.arg(1);

            val.with_iter(move |iter| {

                let ret = VVal::vec();

                for (v, k) in iter {
                    env.push(v.clone());
                    let n =
                        if let Some(k) = &k { env.push(k.clone()); 2 }
                        else                { 1 };
                    match f.call_internal(env, n) {
                        Ok(test) => {
                            if test.b() {
                                if let Some(k) = k { ret.push(VVal::pair(v, k)); }
                                else               { ret.push(v); }
                            }
                        },
                        Err(StackAction::Break(v)) => { env.popn(n); return Ok(*v); },
                        Err(StackAction::Next)     => { },
                        Err(e)                     => { env.popn(n); return Err(e); }
                    }
                    env.popn(n);
                }

                Ok(ret)
            })
        }, Some(2), Some(2), false);

    func!(st, "map",
        |env: &mut Env, _argc: usize| {
            let f   = env.arg(0);
            let val = env.arg(1);

            val.with_iter(move |iter| {
                let ret = VVal::vec();

                for (v, k) in iter {
                    env.push(v);
                    let n =
                        if let Some(k) = k { env.push(k); 2 }
                        else               { 1 };
                    match f.call_internal(env, n) {
                        Ok(v)                      => { ret.push(v); },
                        Err(StackAction::Break(v)) => { env.popn(n); return Ok(*v); },
                        Err(StackAction::Next)     => { },
                        Err(e)                     => { env.popn(n); return Err(e); }
                    }
                    env.popn(n);
                }

                Ok(ret)
            })
        }, Some(2), Some(2), false);

    func!(st, "for",
        |env: &mut Env, _argc: usize| {
            let val = env.arg(0);
            let f   = env.arg(1);

            val.with_iter(move |iter| {
                let mut ret = VVal::None;

                for (v, k) in iter {
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
            })
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

fn systime_to_unix(syst: &std::time::SystemTime, unit: &str) -> Result<VVal, StackAction> {
    match syst.duration_since(std::time::SystemTime::UNIX_EPOCH) {
        Ok(n) => {
            Ok(duration_to_vval(n, unit))
        },
        Err(_) =>
            Err(StackAction::panic_msg(
                "SystemTime before UNIX EPOCH!".to_string()))
    }
}

fn duration_to_vval(dur: std::time::Duration, unit: &str) -> VVal {
    match unit {
        "s"  => { VVal::Int(i64::try_from(dur.as_secs())  .unwrap_or(0)) },
        "ms" => { VVal::Int(i64::try_from(dur.as_millis()).unwrap_or(0)) },
        "us" => { VVal::Int(i64::try_from(dur.as_micros()).unwrap_or(0)) },
        "ns" => { VVal::Int(i64::try_from(dur.as_nanos()) .unwrap_or(0)) },
        _    => { VVal::Int(i64::try_from(dur.as_millis()).unwrap_or(0)) },
    }
}

fn dir_entry_to_vval(env: &mut Env, path: &str, entry: Result<std::fs::DirEntry, std::io::Error>) -> Result<VVal, StackAction> {
    let entry = match entry {
        Ok(e) => e,
        Err(e) => {
            return Ok(env.new_err(
                format!(
                    "Couldn't directory read entry '{}': {}",
                    path, e)))
        },
    };

    let metadata = match entry.metadata() {
        Ok(m) => m,
        Err(e) => {
            return Ok(env.new_err(
                format!(
                    "Couldn't read entry metadata '{}': {}",
                    entry.path().to_string_lossy(), e)))
        },
    };

    let ve = VVal::map3(
        "name", VVal::new_str_mv(entry.file_name().to_string_lossy().to_string()),
        "path", VVal::new_str_mv(entry.path().to_string_lossy().to_string()),
        "type", if metadata.file_type().is_dir() {
            VVal::sym("d")
        } else if metadata.file_type().is_symlink() {
            VVal::sym("l")
        } else {
            VVal::sym("f")
        });
    ve.set_key_str("len", VVal::Int(metadata.len() as i64))
      .expect("single use");

    if let Ok(atime) = metadata.accessed() {
        ve.set_key_str(
            "atime", systime_to_unix(&atime, "s")?)
          .expect("single use");
    }

    if let Ok(mtime) = metadata.modified() {
        ve.set_key_str(
            "mtime", systime_to_unix(&mtime, "s")?)
          .expect("single use");
    }

    if let Ok(ctime) = metadata.created() {
        ve.set_key_str(
            "ctime", systime_to_unix(&ctime, "s")?)
          .expect("single use");
    }

    ve.set_key_str(
      "read_only",
      VVal::Bol(metadata.permissions().readonly()))
      .expect("single use");

    Ok(ve)
}

/// Returns a SymbolTable with all WLambda standard library language symbols.
#[allow(clippy::question_mark)]
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

    func!(st, "reverse",
        |env: &mut Env, _argc: usize| {
            Ok(env.arg(0).reverse())
        }, Some(1), Some(1), false);

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

    func!(st, "error_to_str",
        |env: &mut Env, _argc: usize| {
            match env.arg(0) {
                VVal::Err(_) => {
                    Ok(VVal::new_str_mv(env.arg(0).s()))
                },
                v => {
                    Err(StackAction::panic_msg(
                        format!("std:error_to_str on non error value: {}", v.s())))
                },
            }
        }, Some(1), Some(1), true);

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
    func!(st, "str:find",
        |env: &mut Env, _argc: usize| {
            Ok(env.arg_ref(1).unwrap()
               .find(env.arg_ref(0).unwrap(),
                     env.arg_ref(2).unwrap_or(&VVal::Int(0)).i() as usize,
                     false))
        }, Some(2), Some(3), false);
    func!(st, "bytes:find",
        |env: &mut Env, _argc: usize| {
            Ok(env.arg_ref(1).unwrap()
               .find(env.arg_ref(0).unwrap(),
                     env.arg_ref(2).unwrap_or(&VVal::Int(0)).i() as usize,
                     true))
        }, Some(2), Some(3), false);
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
    func!(st, "str:from_latin1",
        |env: &mut Env, _argc: usize| {
            let b = env.arg(0);
            if let VVal::Byt(u) = b {
                let new_str =
                    u.iter()
                     .map(|byte| std::char::from_u32(*byte as u32).unwrap_or('?'))
                     .collect();

                Ok(VVal::new_str_mv(new_str))
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
            env.arg(0).with_iter(move |iter| {
                let mut s = String::new();

                for (vc, _) in iter {
                    s.push(std::char::from_u32(vc.i() as u32).unwrap_or('?'));
                }

                Ok(VVal::new_str_mv(s))
            })
        }, Some(1), Some(1), false);

    func!(st, "str:to_bytes_latin1",
        |env: &mut Env, _argc: usize| {
            env.arg(0).with_s_ref(|s| {
                Ok(VVal::new_byt(
                    s.chars()
                     .map(|c| {
                         let c = c as u32;
                         if c > 0xFF { '?' as u32 as u8 } else { c as u8 }
                     })
                     .collect()))
            })
        }, Some(1), Some(1), false);

    func!(st, "str:to_bytes",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::new_byt(env.arg(0).as_bytes()))
        }, Some(1), Some(1), false);

    func!(st, "str:edit_distance",
        |env: &mut Env, _argc: usize| {
            env.arg_ref(0).unwrap().with_s_ref(|a: &str| {
                env.arg_ref(1).unwrap().with_s_ref(|b: &str| {
                    Ok(VVal::Int(util::edit_distance(a, b) as i64))
                })
            })
        }, Some(2), Some(2), false);

    func!(st, "char:to_lowercase",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Chr(VValChr::Char(
                env.arg(0).c().to_lowercase().next().unwrap_or('\0'))))
        }, Some(1), Some(1), false);

    func!(st, "char:to_uppercase",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Chr(VValChr::Char(
                env.arg(0).c().to_uppercase().next().unwrap_or('\0'))))
        }, Some(1), Some(1), false);

    func!(st, "bytes:replace",
        |env: &mut Env, _argc: usize| {
            let bv   = env.arg(0);
            let pat  = env.arg(1);
            let repl = env.arg(2);

            Ok(bv.bytes_replace(&pat, &repl))
        }, Some(3), Some(3), false);

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

    func!(st, "bytes:pack",
        |env: &mut Env, _argc: usize| {
            use crate::packer;
            let data = env.arg(1);
            env.arg_ref(0).unwrap().with_s_ref(|s: &str| {
                match packer::do_pack(s, &data) {
                    Ok(v) => Ok(v),
                    Err(e) => {
                        Ok(env.new_err(
                            format!("Bad pack '{}': {}", s, e)))
                    },
                }
            })
        }, Some(2), Some(2), false);

    func!(st, "bytes:unpack",
        |env: &mut Env, _argc: usize| {
            use crate::packer;
            let data = env.arg(1);
            env.arg_ref(0).unwrap().with_s_ref(|s: &str| {
                match packer::do_unpack(s, &data) {
                    Ok(v) => Ok(v),
                    Err(e) => {
                        Ok(env.new_err(
                            format!("Bad (un)pack '{}': {}", s, e)))
                    },
                }
            })
        }, Some(2), Some(2), false);

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
            let f       = env.arg(1);
            let lst     = env.arg(2);

            lst.with_iter(move |iter| {
                let mut acc = env.arg(0);

                for (i, _) in iter {
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
            })
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

    add_num_fun!(st, "num:signum",     signum);

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

    func!(st, "num:fract",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Flt(env.arg(0).f().fract()))
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

    func!(st, "fs:path:exists",
        |env: &mut Env, _argc: usize| {
            match env.arg(0).with_s_ref(|filename|
                    std::path::Path::new(filename).try_exists()) {
                Ok(bol) => Ok(VVal::Bol(bol)),
                Err(e) =>
                    return Ok(env.new_err(
                        format!(
                            "Couldn't check for file existance '{}': {}",
                            env.arg(0).s_raw(),
                            e)))
            }
        },
        Some(1),
        Some(1),
        false);

    func!(st, "fs:canonicalize",
        |env: &mut Env, _argc: usize| {
            match std::fs::canonicalize(env.arg(0).s_raw()) {
                Ok(path) => {
                    match path.to_str() {
                        Some(path) => {
                            if path.starts_with("\\\\?") {
                                Ok(VVal::new_str(&path[4..]))
                            } else {
                                Ok(VVal::new_str(path))
                            }
                        },
                        None => {
                            return Ok(env.new_err(
                                format!(
                                    "Couldn't canonicalize path '{}': bad filename found!",
                                    env.arg(0).s_raw())))
                        },
                    }
                },
                Err(e) => {
                    return Ok(env.new_err(
                        format!(
                            "Couldn't canonicalize path '{}': {}",
                            env.arg(0).s_raw(), e)))
                },
            }
        }, Some(1), Some(1), false);
    func!(st, "fs:read_dir",
        |env: &mut Env, _argc: usize| {
            let path = env.arg(0);
            let f    = env.arg(1);

            path.with_s_ref(|path| {
                let mut ret = VVal::None;

                let mut stack = vec![path.to_string()];
                while !stack.is_empty() {
                    let path = stack.pop().unwrap();

                    match std::fs::read_dir(&path) {
                        Ok(iter) => {
                            for entry in iter {
                                let ve = dir_entry_to_vval(env, &path, entry)?;
                                let is_dir =
                                    ve.get_key("type")
                                      .unwrap_or(VVal::None)
                                      .with_s_ref(|s| s == "d");
                                let entry_path =
                                    if is_dir {
                                        Some(
                                            ve.get_key("path")
                                              .unwrap_or(VVal::None)
                                              .s_raw())
                                    } else { None };
                                env.push(ve);
                                match f.call_internal(env, 1) {
                                    Ok(v)                      => { ret = v; },
                                    Err(StackAction::Break(v)) => { env.popn(1); return Ok(*v); },
                                    Err(StackAction::Next)     => { },
                                    Err(e)                     => { env.popn(1); return Err(e); }
                                }
                                env.popn(1);

                                if is_dir && ret.b() {
                                    if let Some(ep) = entry_path {
                                        stack.push(ep);
                                    }
                                }
                            }
                        },
                        Err(e) => {
                            return Ok(env.new_err(
                                format!(
                                    "Couldn't read directory '{}': {}",
                                    path, e)))
                        },
                    }
                }

                Ok(ret)
            })
        }, Some(2), Some(2), false);

    func!(st, "fs:copy",
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

    func!(st, "fs:remove_file",
        |env: &mut Env, _argc: usize| {
            let path = env.arg(0);
            path.with_s_ref(|path| {
                if let Err(e) = std::fs::remove_file(&path) {
                    return Ok(env.new_err(
                        format!(
                            "Couldn't remove file '{}': {}",
                            path, e)));
                }

                Ok(VVal::Bol(true))
            })
        }, Some(1), Some(1), false);

    func!(st, "fs:remove_dir",
        |env: &mut Env, _argc: usize| {
            let path = env.arg(0);
            path.with_s_ref(|path| {
                if let Err(e) = std::fs::remove_dir(&path) {
                    return Ok(env.new_err(
                        format!(
                            "Couldn't remove dir '{}': {}",
                            path, e)));
                }

                Ok(VVal::Bol(true))
            })
        }, Some(1), Some(1), false);

    func!(st, "fs:remove_dir_all",
        |env: &mut Env, _argc: usize| {
            let path = env.arg(0);
            path.with_s_ref(|path| {
                if let Err(e) = std::fs::remove_dir_all(&path) {
                    return Ok(env.new_err(
                        format!(
                            "Couldn't remove dir (recursively) '{}': {}",
                            path, e)));
                }

                Ok(VVal::Bol(true))
            })
        }, Some(1), Some(1), false);

    func!(st, "io:line",
        |env: &mut Env, _argc: usize| {
            let mut line = String::new();

            let mut read = env.stdio.read.borrow_mut();
            match read.read_line(&mut line) {
                Ok(n) => {
                    if n == 0 {
                        return Ok(VVal::None);
                    }
                },
                Err(e) => {
                    return Ok(env.new_err(
                        format!("IO-Error on std:io:line: {}", e)))
                },
            }

            Ok(VVal::new_str_mv(line))
        }, Some(0), Some(0), false);

    func!(st, "io:lines",
        |env: &mut Env, argc: usize| {
            let (f, mut ret) =
                if argc == 0 {
                    let vec = VVal::vec();
                    (vec.clone(), vec)
                } else {
                    (env.arg(0), VVal::None)
                };

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

                if f.is_vec() {
                    f.push(VVal::new_str_mv(line));
                } else {
                    env.push(VVal::new_str_mv(line));
                    match f.call_internal(env, 1) {
                        Ok(v)                      => { ret = v; },
                        Err(StackAction::Break(v)) => { env.popn(1); return Ok(*v); },
                        Err(StackAction::Next)     => { },
                        Err(e)                     => { env.popn(1); return Err(e); }
                    }
                    env.popn(1);
                }
            }

            Ok(ret)
        }, Some(0), Some(1), false);

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

    func!(st, "write_str",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::new_str_mv(env.arg_ref(0).unwrap().s()))
        }, Some(1), Some(1), false);

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
            sizeof_writeln!(write, Box<crate::nvec::NVec<f64>>);
            sizeof_writeln!(write, Result<VVal, StackAction>);
            sizeof_writeln!(write, StackAction);
            sizeof_writeln!(write, Box<String>);
            sizeof_writeln!(write, Box<Vec<VVal>>);
            sizeof_writeln!(write, Vec<VVal>);
            sizeof_writeln!(write, Box<dyn VValUserData>);
            sizeof_writeln!(write, std::rc::Weak<std::cell::RefCell<VVal>>);
            Ok(VVal::None)
        }, Some(0), Some(0), false);

////    println!("sizeof OP:{} bytes", std::mem::size_of::<(ResPos, Box<String>, Box<String>, Box<String>, ResPos)>());

    func!(st, "wlambda:version",
        |_env: &mut Env, _argc: usize| {
            Ok(VVal::new_str(VERSION))
        }, Some(0), Some(0), false);

    func!(st, "wlambda:parse",
        |env: &mut Env, argc: usize| {
            let filename =
                if argc == 2 { env.arg(1).s_raw() }
                else { "<wlambda:parse:input>".to_string() };
            let res =
                env.arg(0).with_s_ref(|s| { crate::parser::parse(s, &filename) });
            match res {
                Ok(ast) => Ok(ast),
                Err(e)  => Ok(env.new_err(e))
            }
        }, Some(1), Some(2), false);

    func!(st, "syn:type",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::new_sym(env.arg(0).syntax_type()))
        }, Some(1), Some(1), false);

    func!(st, "syn:pos",
        |env: &mut Env, _argc: usize| {
            if let VVal::Syn(sp) = env.arg(0) {
                if let Some(name) = sp.info.name.as_ref() {
                    Ok(VVal::vec4(
                        VVal::new_str(sp.info.file.s()),
                        VVal::Int(sp.info.line as i64),
                        VVal::Int(sp.info.col as i64),
                        VVal::new_str(name)))
                } else {
                    Ok(VVal::vec3(
                        VVal::new_str(sp.info.file.s()),
                        VVal::Int(sp.info.line as i64),
                        VVal::Int(sp.info.col as i64)))
                }
            } else {
                Ok(VVal::None)
            }
        }, Some(1), Some(1), false);

    func!(st, "measure_time",
        |env: &mut Env, _argc: usize| {
            let t = std::time::Instant::now();
            let unit = env.arg(0).s_raw();
            match env.arg(1).call_no_args(env) {
                Ok(v) => {
                    let ret = VVal::vec();
                    ret.push(duration_to_vval(t.elapsed(), &unit[..]));
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
            let pat_src = env.arg_ref(0).cloned().unwrap_or(VVal::None);
            let res_ref =
                env.global.borrow_mut()
                   .get_var_ref("\\")
                   .unwrap_or(VVal::None);
            let mode = env.arg(1).with_s_ref(RegexMode::from_str);
            pat_src.with_s_ref(|pat_src|
                match create_regex_find_function(pat_src, res_ref, mode) {
                    Ok(fun) => Ok(fun),
                    Err(e) => {
                        Ok(env.new_err(
                            format!("bad pattern: {}, pattern was: /{}/",
                                    e, pat_src)))
                    }
                })
        }, Some(1), Some(2), false);

    func!(st, "selector",
        |env: &mut Env, _argc: usize| {
            let pat_src = env.arg_ref(0).cloned().unwrap_or(VVal::None);
            let res_ref =
                env.global.borrow_mut()
                   .get_var_ref("\\")
                   .unwrap_or(VVal::None);
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

    func!(st, "formatter",
        |env: &mut Env, _argc: usize| {
            let fmt_src = env.arg(0);
            match create_formatter_fun(&fmt_src) {
                Ok(fun) => Ok(fun),
                Err(e) => {
                    Ok(env.new_err(
                        format!("bad formatter: {}, formatter was: /{}/",
                                e, fmt_src.s_raw())))
                }
            }
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

    func!(st, "str:strip_utf8_bom",
        |env: &mut Env, _argc: usize| {
            env.arg(0).with_bv_ref(|s| {
                if s.starts_with(b"\xEF\xBB\xBF") {
                    Ok(VVal::new_str_mv(
                            std::str::from_utf8(&s[3..])
                            .expect("valid utf-8 after removing bom").to_string()))
                } else {
                    Ok(env.arg(0))
                }
            })
        }, Some(1), Some(1), false);

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

            let rx = re.with_s_ref(Regex::new);
            if let Err(e) = rx {
                return Ok(env.new_err(
                    format!("Regex '{}' did not compile: {}", re.s_raw(), e)));
            }
            let rx = rx.unwrap();

            let mut finished = false;
            let mut ret = Ok(VVal::None);
            let ret_str = text.with_s_ref(|text: &str| {
                VVal::new_str_mv(String::from(
                    rx.replace_all(text, |capts: &regex::Captures| {
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

            let rx = re.with_s_ref(Regex::new);
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

            let rx = re.with_s_ref(Regex::new);
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

            let rx = re.with_s_ref(Regex::new);
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

    func!(st, "srand",
        |env: &mut Env, argc: usize| {
            if argc == 0 {
                use std::time::SystemTime;
                match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
                    Ok(n)  => util::srand(n.as_nanos() as i64),
                    Err(_) => util::srand(1_234_567_890),
                }
            } else {
                util::srand(env.arg(0).i());
            }
            Ok(VVal::None)
        }, Some(0), Some(1), false);

    func!(st, "rand",
        |env: &mut Env, argc: usize| {
            if argc == 0 {
                Ok(VVal::Flt(util::rand_closed_open01()))
            } else {
                match env.arg(0).deref() {
                    VVal::Flt(f)
                        => Ok(VVal::Flt(util::rand_closed_open01() * f)),
                    VVal::Int(i)
                        => Ok(VVal::Int(util::rand_i(i as u64))),
                    v   => {
                        if v.with_s_ref(|s| s == "i64") {
                            Ok(VVal::Int(util::rand_full_i()))
                        } else {
                            Ok(VVal::Int(util::rand_i(v.i() as u64)))
                        }
                    }
                }
            }
        }, Some(0), Some(1), false);

    func!(st, "sys:os",
        |_env: &mut Env, _argc: usize| {
            Ok(VVal::new_str(std::env::consts::OS))
        }, Some(0), Some(0), false);

    func!(st, "sys:env:var",
        |env: &mut Env, _argc: usize| {
            env.arg(0).with_s_ref(|s|
                match std::env::var(s) {
                    Ok(val) => Ok(VVal::new_str_mv(val)),
                    Err(e) => {
                        Ok(env.new_err(
                            format!("Couldn't get env var '{}': {}", s, e)))
                    }
                })
        }, Some(1), Some(1), false);

    func!(st, "time:now",
        |env: &mut Env, _argc: usize| {
            use std::time::SystemTime;
            match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
                Ok(n) => {
                    Ok(env.arg(0).with_s_ref(|unit|
                        duration_to_vval(n, unit)))
                },
                Err(_) =>
                    Err(StackAction::panic_msg(
                        "SystemTime before UNIX EPOCH!".to_string()))
            }
        }, Some(0), Some(1), false);

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

    #[cfg(feature="chrono")]
    func!(st, "chrono:parse:rfc_2822",
        |env: &mut Env, _argc: usize| {
            use chrono::prelude::*;
            env.arg(0).with_s_ref(|s| {
                match DateTime::parse_from_rfc2822(s) {
                    Ok(dt) => Ok(VVal::Int(dt.timestamp())),
                    Err(e) =>
                        Ok(env.new_err(
                            format!("std:chrono:parse:rfc_2822 can't parse date {}: {}", s, e))),
                }
            })
        }, Some(1), Some(1), false);

    #[cfg(feature="chrono")]
    func!(st, "chrono:format_utc",
        |env: &mut Env, _argc: usize| {
            use chrono::prelude::*;
            use chrono::offset::*;
            match Utc.timestamp_opt(env.arg(0).i(), 0) {
                LocalResult::Single(dt) | LocalResult::Ambiguous(dt, _) => {
                    let fmt = env.arg(1);
                    if fmt.is_str() {
                        fmt.with_s_ref(|fmt: &str|
                            Ok(VVal::new_str_mv(dt.format(fmt).to_string())))
                    } else {
                        Ok(VVal::new_str_mv(dt.format("%Y-%m-%d %H:%M:%S.%f").to_string()))
                    }
                }
                LocalResult::None => {
                    Err(StackAction::panic_msg(
                        format!("Can't get Timestamp!")))
                }
            }

        }, Some(1), Some(2), false);

    #[cfg(feature="chrono")]
    func!(st, "chrono:format_local",
        |env: &mut Env, _argc: usize| {
            use chrono::prelude::*;
            use chrono::offset::*;
            match Local.timestamp_opt(env.arg(0).i(), 0) {
                LocalResult::Single(dt) | LocalResult::Ambiguous(dt, _) => {
                    let fmt = env.arg(1);
                    if fmt.is_str() {
                        fmt.with_s_ref(|fmt: &str|
                            Ok(VVal::new_str_mv(dt.format(fmt).to_string())))
                    } else {
                        Ok(VVal::new_str_mv(dt.format("%Y-%m-%d %H:%M:%S.%f").to_string()))
                    }
                }
                LocalResult::None => {
                    Err(StackAction::panic_msg(
                        format!("Can't get Timestamp")))
                }
            }
        }, Some(1), Some(2), false);

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

    #[cfg(feature="toml")]
    func!(st, "ser:toml",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            let pp = env.arg(1).b();

            match v.to_toml(pp) {
                Ok(s) => Ok(VVal::new_str_mv(s)),
                Err(e) => Ok(env.new_err(e)),
            }
        }, Some(1), Some(2), false);

    #[cfg(feature="toml")]
    func!(st, "deser:toml",
        |env: &mut Env, _argc: usize| {
            env.arg_ref(0).unwrap().with_s_ref(
                |toml_txt: &str|
                    match VVal::from_toml(toml_txt) {
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

    #[cfg(feature="base64")]
    func!(st, "bytes:to_base64",
        |env: &mut Env, _argc: usize| {
            let mut cfg = base64::STANDARD;
            let config = env.arg(1);
            if config.is_some() {
                cfg = config.with_s_ref(|s| match s {
                    "url"        => base64::URL_SAFE,
                    "url_no_pad" => base64::URL_SAFE_NO_PAD,
                    "std"        => base64::STANDARD,
                    "std_no_pad" => base64::STANDARD_NO_PAD,
                    _            => base64::STANDARD,
                })
            }

            env.arg(0).with_bv_ref(|bytes| {
                Ok(VVal::new_str_mv(base64::encode_config(bytes, cfg)))
            })
        }, Some(1), Some(2), false);

    #[cfg(feature="base64")]
    func!(st, "bytes:from_base64",
        |env: &mut Env, _argc: usize| {
            let mut cfg = base64::STANDARD;
            let config = env.arg(1);
            if config.is_some() {
                cfg = config.with_s_ref(|s| match s {
                    "url"        => base64::URL_SAFE,
                    "url_no_pad" => base64::URL_SAFE_NO_PAD,
                    "std"        => base64::STANDARD,
                    "std_no_pad" => base64::STANDARD_NO_PAD,
                    _            => base64::STANDARD,
                })
            }

            env.arg(0).with_bv_ref(|bytes| {
                match base64::decode_config(bytes, cfg) {
                    Ok(bytes) => Ok(VVal::new_byt(bytes)),
                    Err(e) =>
                        Ok(env.new_err(
                            format!("bytes:from_base64: {}", e)))
                }
            })
        }, Some(1), Some(2), false);

    #[cfg(feature="flate2")]
    func!(st, "bytes:gzip:decode",
        |env: &mut Env, _argc: usize| {
            use std::io::Read;
            use flate2::read::GzDecoder;

            env.arg(0).with_bv_ref(|bytes| {
                let mut defl = GzDecoder::new(bytes);
                let mut buf = vec![];

                match defl.read_to_end(&mut buf) {
                    Ok(_) => Ok(VVal::new_byt(buf)),
                    Err(e) =>
                        Ok(env.new_err(
                            format!("bytes:gzip:decode: {}", e)))
                }
            })
        }, Some(1), Some(1), false);

    #[cfg(feature="flate2")]
    func!(st, "bytes:gzip:encode",
        |env: &mut Env, _argc: usize| {
            use std::io::Read;
            use flate2::read::GzEncoder;
            use flate2::Compression;

            env.arg(0).with_bv_ref(|bytes| {
                let mut defl = GzEncoder::new(bytes, Compression::best());
                let mut buf = vec![];

                match defl.read_to_end(&mut buf) {
                    Ok(_) => Ok(VVal::new_byt(buf)),
                    Err(e) =>
                        Ok(env.new_err(
                            format!("bytes:gzip:encode: {}", e)))
                }
            })
        }, Some(1), Some(1), false);

    #[cfg(feature="flate2")]
    func!(st, "bytes:zlib:decode",
        |env: &mut Env, _argc: usize| {
            use std::io::Read;
            use flate2::read::ZlibDecoder;

            env.arg(0).with_bv_ref(|bytes| {
                let mut defl = ZlibDecoder::new(bytes);
                let mut buf = vec![];

                match defl.read_to_end(&mut buf) {
                    Ok(_) => Ok(VVal::new_byt(buf)),
                    Err(e) =>
                        Ok(env.new_err(
                            format!("bytes:zlib:decode: {}", e)))
                }
            })
        }, Some(1), Some(1), false);

    #[cfg(feature="flate2")]
    func!(st, "bytes:zlib:encode",
        |env: &mut Env, _argc: usize| {
            use std::io::Read;
            use flate2::read::ZlibEncoder;
            use flate2::Compression;

            env.arg(0).with_bv_ref(|bytes| {
                let mut defl = ZlibEncoder::new(bytes, Compression::best());
                let mut buf = vec![];

                match defl.read_to_end(&mut buf) {
                    Ok(_) => Ok(VVal::new_byt(buf)),
                    Err(e) =>
                        Ok(env.new_err(
                            format!("bytes:zlib:encode: {}", e)))
                }
            })
        }, Some(1), Some(1), false);

    #[cfg(feature="flate2")]
    func!(st, "bytes:deflate:decode",
        |env: &mut Env, _argc: usize| {
            use std::io::Read;
            use flate2::read::DeflateDecoder;

            env.arg(0).with_bv_ref(|bytes| {
                let mut defl = DeflateDecoder::new(bytes);
                let mut buf = vec![];

                match defl.read_to_end(&mut buf) {
                    Ok(_) => Ok(VVal::new_byt(buf)),
                    Err(e) =>
                        Ok(env.new_err(
                            format!("bytes:deflate:decode: {}", e)))
                }
            })
        }, Some(1), Some(1), false);

    #[cfg(feature="flate2")]
    func!(st, "bytes:deflate:encode",
        |env: &mut Env, _argc: usize| {
            use std::io::Read;
            use flate2::read::DeflateEncoder;
            use flate2::Compression;

            env.arg(0).with_bv_ref(|bytes| {
                let mut defl = DeflateEncoder::new(bytes, Compression::best());
                let mut buf = vec![];

                match defl.read_to_end(&mut buf) {
                    Ok(_) => Ok(VVal::new_byt(buf)),
                    Err(e) =>
                        Ok(env.new_err(
                            format!("bytes:deflate:encode: {}", e)))
                }
            })
        }, Some(1), Some(1), false);

    func!(st, "bytes:lzw:encode",
        |env: &mut Env, _argc: usize| {
            use weezl::{BitOrder, encode::Encoder};

            let bitsize = env.arg(1);
            let bitsize = if bitsize.is_none() {
                9
            } else {
                bitsize.i() as u8
            };

            if bitsize < 2 || bitsize > 12 {
                return Ok(env.new_err(format!("bytes:lzw:encode: invalid bitsize: {}", bitsize)));
            }

            let bitorder = if env.arg(2).with_s_ref(|s| s == "lsb") {
                BitOrder::Lsb
            } else {
                BitOrder::Msb
            };

            env.arg(0).with_bv_ref(|bytes| {
                match Encoder::new(bitorder, bitsize).encode(bytes) {
                    Ok(data) => Ok(VVal::new_byt(data)),
                    Err(e) => 
                        Ok(env.new_err(
                            format!("bytes:lzw:encode: {}", e)))
                }
            })
        }, Some(1), Some(3), false);

    func!(st, "bytes:lzw:decode",
        |env: &mut Env, _argc: usize| {
            use weezl::{BitOrder, decode::Decoder};

            let bitsize = env.arg(1);
            let bitsize = if bitsize.is_none() {
                9
            } else {
                bitsize.i() as u8
            };

            if bitsize < 2 || bitsize > 12 {
                return Ok(env.new_err(format!("bytes:lzw:decode: invalid bitsize: {}", bitsize)));
            }

            let bitorder = if env.arg(2).with_s_ref(|s| s == "lsb") {
                BitOrder::Lsb
            } else {
                BitOrder::Msb
            };

            env.arg(0).with_bv_ref(|bytes| {
                match Decoder::new(bitorder, bitsize).decode(bytes) {
                    Ok(data) => Ok(VVal::new_byt(data)),
                    Err(e) => 
                        Ok(env.new_err(
                            format!("bytes:lzw:decode: {}", e)))
                }
            })
        }, Some(1), Some(3), false);

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
                   .with_s_ref(util::hex2hsvaf);
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
                   .with_s_ref(util::hex2hsvaf);
            Ok(VVal::fvec4(
                hsvaf.0,
                hsvaf.1,
                hsvaf.2,
                hsvaf.3))
        }, Some(1), Some(1), false);

    func!(st, "v:hex2rgba_i",
        |env: &mut Env, _argc: usize| {
            let (r, g, b, a) =
                env.arg(0).with_s_ref(util::hex2rgba);
            Ok(VVal::ivec_from_tpl4((r as i64, g as i64, b as i64, a as i64)))
        }, Some(1), Some(1), false);

    func!(st, "v:hex2rgba_f",
        |env: &mut Env, _argc: usize| {
            let (r, g, b, a) =
                env.arg(0).with_s_ref(util::hex2rgbaf);
            Ok(VVal::fvec_from_tpl4((r, g, b, a)))
        }, Some(1), Some(1), false);

    func!(st, "v:rgba2hex",
        |env: &mut Env, _argc: usize| {
            let arg = env.arg_ref(0).unwrap().deref();
            process_vec_input!(env, arg, v, x, y, z, w, {
                Ok(VVal::new_str_mv(util::rgba2hex( (x as u8, y as u8, z as u8, 255))))
            }, {
                Ok(VVal::new_str_mv(util::rgba2hexf((x, y, z, 1.0))))
            }, {
                Ok(VVal::new_str_mv(util::rgba2hex( (x as u8, y as u8, z as u8, w as u8))))
            }, {
                Ok(VVal::new_str_mv(util::rgba2hexf((x, y, z, w))))
            })
        }, Some(1), Some(1), false);

    func!(st, "v:hsv2rgb",
        |env: &mut Env, _argc: usize| {
            let arg = env.arg_ref(0).unwrap().deref();
            process_vec_input!(env, arg, v, x, y, z, w, {
                let c =
                    util::hsv2rgb(
                        x as f64,
                        y as f64 / 100.0,
                        z as f64 / 100.0);
                Ok(VVal::ivec_from_tpl3((
                    (c.0 * 255.0).round() as i64,
                    (c.1 * 255.0).round() as i64,
                    (c.2 * 255.0).round() as i64)))
            }, {
                let c = util::hsv2rgb(x, y, z);
                Ok(VVal::fvec_from_tpl3(c))
            }, {
                let c =
                    util::hsva2rgba((
                        x as f64,
                        (y as f64 / 100.0),
                        (z as f64 / 100.0),
                        (w as f64 / 100.0)));
                Ok(VVal::ivec_from_tpl4((
                    (c.0 * 255.0).round() as i64,
                    (c.1 * 255.0).round() as i64,
                    (c.2 * 255.0).round() as i64,
                    (c.3 * 255.0).round() as i64)))
            }, {
                let c = util::hsva2rgba((x, y, z, w));
                Ok(VVal::fvec_from_tpl4(c))
            })
        }, Some(1), Some(1), false);

    func!(st, "v:rgb2hsv",
        |env: &mut Env, _argc: usize| {
            let arg = env.arg_ref(0).unwrap().deref();
            process_vec_input!(env, arg, v, x, y, z, w, {
                let c =
                    util::rgb2hsv(
                        (x as f64) / 255.0,
                        (y as f64) / 255.0,
                        (z as f64) / 255.0);
                Ok(VVal::ivec_from_tpl3((
                    c.0.round() as i64,
                    (c.1 * 100.0).round() as i64,
                    (c.2 * 100.0).round() as i64)))
            }, {
                let c = util::rgb2hsv(x, y, z);
                Ok(VVal::fvec_from_tpl3(c))
            }, {
                let c =
                    util::rgb2hsv(
                        (x as f64) / 255.0,
                        (y as f64) / 255.0,
                        (z as f64) / 255.0);
                Ok(VVal::ivec_from_tpl4((
                    c.0.round() as i64,
                    (c.1 * 100.0).round() as i64,
                    (c.2 * 100.0).round() as i64,
                    ((w as f64 / 255.0) * 100.0).round() as i64)))
            }, {
                let c = util::rgba2hsvaf((x, y, z, w));
                Ok(VVal::fvec_from_tpl4(c))
            })
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
                            hash.write(s.as_bytes()));
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
                        let mod_resolver = env.global.borrow().get_resolver();
                        env.arg(0).with_s_ref(|code: &str|
                            Ok(tcg.spawn(ntc, mod_resolver, code.to_string(), avs)))
                    },
                    Err(e) => {
                        Err(StackAction::panic_str(
                            format!("Couldn't create thread: {}", e),
                            None,
                            env.argv()))
                    },
                }

            } else {
                Err(StackAction::panic_str(
                    "This global environment does not provide threads.".to_string(),
                    None,
                    env.argv()))
            }
        }, Some(1), Some(2), false);

    func!(st, "chem:data",
        |_env: &mut Env, _argc: usize| {
            Ok(crate::chemistry::get_periodic_table_data())
        }, Some(0), Some(0), false);

    func!(st, "chem:parse",
        |env: &mut Env, _argc: usize| {
            env.arg(0).with_s_ref(|s| {
                match crate::chemistry::parse_chemical_sum_formula(s) {
                    Ok(form) => Ok(form),
                    Err(e) => {
                        Ok(env.new_err(
                            format!("bad chemical sum formula ({}): {}",
                                    s, e)))
                    }
                }
            })
        }, Some(1), Some(1), false);

    func!(st, "min",
        |env: &mut Env, argc: usize| {
            if env.arg(0).is_float() {
                let mut r = env.arg(0).f();

                for i in 1..argc {
                    r = r.min(env.arg(i).f());
                }

                Ok(VVal::Flt(r))
            } else if env.arg(0).is_int() {
                let mut r = env.arg(0).i();

                for i in 1..argc {
                    r = r.min(env.arg(i).i());
                }

                Ok(VVal::Int(r))
            } else {
                let mut r = env.arg(0).s_raw();

                for i in 1..argc {
                    r = r.min(env.arg(i).s_raw());
                }

                Ok(VVal::new_str_mv(r))
            }
        }, None, None, false);

    func!(st, "max",
        |env: &mut Env, argc: usize| {
            if env.arg(0).is_float() {
                let mut r = env.arg(0).f();

                for i in 1..argc {
                    r = r.max(env.arg(i).f());
                }

                Ok(VVal::Flt(r))
            } else if env.arg(0).is_int() {
                let mut r = env.arg(0).i();

                for i in 1..argc {
                    r = r.max(env.arg(i).i());
                }

                Ok(VVal::Int(r))
            } else {
                let mut r = env.arg(0).s_raw();

                for i in 1..argc {
                    r = r.max(env.arg(i).s_raw());
                }

                Ok(VVal::new_str_mv(r))
            }
        }, None, None, false);

    crate::stdlib::add_to_symtable(&mut st);

    st
}
