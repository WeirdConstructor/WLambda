0.8.2 (unreleased)
==================

* **Bugfix:** If you defined multiple variables in a local scope, variables from the outer
scope were cleared.
* **Bugfix:** Assignment ops did not work in deeper object structures eg. `o.x.y += 1`.
* **Change:** `{ } .FIELD = ...` is no longer allowed syntax. This is helpful if you forget a
`;` after an `if`, `while` or `!fun = { } .XYT = 10` for instance.
* **Maintenance:** Updated the rumqttd version.
* **Feature:** `std:min` and `std:max` work on symbols too now.
* **Feature:** `$iter` and `iter` are now accepting functions as arguments
and can create an iterator over a WLambda generator function from that.
* **Feature:** Extended `std:http:*` requests with `@dump` option to dump
the resulting request and response information.
* **Feature:** Added `std:ser:toml` and `std:deser:toml` for reading and writing
TOML configuration files.
* **Feature:** The `LocalFileModuleResolver` can now preload files. This is useful
for adding WLambda sources at Rust compile time, for easier deploying of the
Rust binary later.
* **Feature:** Access to environment variables is now possible using `std:sys:env:var`.
* **Feature:** Added `std:str:strip_utf8_bom`.
* **Feature:** Added default compression algorithm LZW to WLambda,
available via `std:bytes:lzw:encode` and `std:bytes:lzw:decode`.
* **Feature:** Added `std:merge`, `std:shift`
* **Feature:** Added `std:app:simple_cli` for quickly implementing CLI applications.
* **Feature:** Added `std:num:statistics` for common statistical data over a vector of floats.
* **Feature:** Added `std:str:markdown_to_html` for a simple markdown to HTML converter.
* **Feature:** Added some vector functions, such as `std:insert`, `std:remove` and `std:shift`.
* **Feature:** Added a lot of math constants, such as `std:const:PI`, `std:const:TAU`, ...
* **Feature:** Added `std:add_uniq` to add elements to a collection, if they are not already present.
* **Feature:** Added `std:fs:path:cwd`,`std:fs:path:parent`, `std:fs:path:temp_dir`, `std:fs:path:current_exe` and `std:fs:path:current_exe_dir`.

0.8.1 (2022-03-05)
==================

* **Feature:** (Optional) Embedded MQTT broker and client API for inter process
communication across WLambda applications. See that you compile in the 
`mqtt` feature and check out `std:mqtt:broker:new` and `std:mqtt:client:new`.
* **Feature:** (Optional) HTTP Client API that supports GET for now. See
`std:http:client:new` and `std:http:get`.
And also `std:http:post` and `std:http:request`.
* **Feature:** The LocalFileModuleResolver now does keep around the
loaded symbol table and does not re-evaluate the module source on every
import. This makes it possible to define global stuff inside the modules
and keep that around.
* **Feature:** Default value operators `//`, `/?`, `/$n`, `/$o` and `/$e`
added. Which can be conveniently used to provide default values in many useful
circumstances.
* **Feature:** Stack traces now also contain the function arguments.
* **Feature:** Added `v_b` and `v_bk` to the wlambda::VVal API.
* **Change:** Error messages and stack traces more readable and with line feeds.
* **Change:** Improved parse and compile error output readability.
* **Change:** Vim syntax file does not define ':' as keyword character anymore.
* **Change:** Vim syntax file extended by more function combinators and
special operators like `//`, `/?`, ...

0.8.0 (2021-07-28)
==================

* **Bugfix:** Code generation for direct blocks `if $true { !x = 3; x } $n`
was broken. It unwound the locals while the return value was still referencing
those locals. No crash, just wrong results. Code became slightly faster too.
* **Bugfix:** Catch `x % 0` and make a WLambda panic of that.

0.7.1 (2021-05-25)
==================

* **Feature:** Added `std:process:spawn` and `std:process:kill_wait`.
* **Feature:** Added `std:process:wait` and `std:process:try_wait`
for waiting it to exit.
* **Feature:** Calling into EvalContext::eval\*() can now be done
recursively.
* **Feature:** Added simple UDP networking via `std:net:udp:new`,
`std:net:udp:send` and `std:net:udp:recv`.
* **Feature:** Added `map` and `filter` to the core language keywords.
* **Feature:** The command line REPL can now search and display contents
of the reference manual with the `?` command.
* **Feature:** Implemented `std:str:edit_distance` utility function.
* **Feature:** Added more filesystem related functions: `std:fs:remove_file`,
`std:fs:remove_dir` and `std:fs:remove_dir_all`.
* **Change:** VVal::new\_fun() added.
* **Change:** Refactored the code base a bit.
* **Bugfix:** Panic on pair mutation (like on integer/float vector mutation).
* **Bugfix:** `$iter` iterators did not work with functions like
`for`, `filter`, `map` and `std:fold`.
* **Bugfix:** Fixed a crash in `std:fs:read_dir`.
* **Bugfix:** panic in parser when a ';' was missed at the wrong place.
* **Bugfix:** `$F` formatters did eat whitespace and wlambda comments, which
was and is not intended.
* **Change:** Improved vim syntax file.

0.7.0 (2021-01-18)
==================

* **Feature:** Implemented `std:sys:os` and `std:process:run`.
* **Change:** Regexes/Patterns allow more characters like `/` and `!` to be
used directly now. But only in the context of a single regex
pattern like `$r(foo/bar)`. This makes it easier to process text.
* **Change:** Decided it's more idiomatic to use `if` instead of `?`
after all. `?` will of course remain.
* **Optimization:** Boxed debugging information, which made some benchmarks
faster by 15%.
* **Feature:** Character and Byte literals `'c'` and `$b'c'` were added.
With functions: `is_byte`, `is_char`.
* **Feature:** Global regex matches with `$rg/.../`.
* **Feature:** Global regex substitutions with `$rs/.../`.
* **Feature:** Implemented `&or` and `$and` for structure patterns (`match` and `$M`).
* **Feature:** Commandline scripts with -e and file based evaluation get
command line arguments via global variable `@@` now.
* **Feature:** Added `std:bytes:pack` and `std:bytes:unpack` for handling
binary data formats.
* **Feature:** Added `std:fs:read_dir`, `std:chrono:format_utc`
and `std:chrono:format_local`.
* **Feature:** Added XML parsing and writing functionality `std:xml:read_sax`
and `std:xml:create_sax_writer`.
* **Feature:** You can customize the `DefaultThreadCreator` easily with a
`DefaultGlobalEnvCreator` trait implementation which is conveniently provided
by `FunctionGlobalEnvCreator`. This makes embedding WLambda with threading
easier.
* **Feature:** Implemented `std:bytes:find`.
* **Feature:** Extended syntax for string escapes by ASCII character names
like `$b"\<STX>"` or `$b"\<DEL>"`.
* **Feature:** Added `std:str:from_latin1` and `std:str:to_bytes_latin1` to
be able to convert bytes to/from ISO-8859-1 encoding in a more or less
clean way. It allows better parsing of text protocols by converting
them to strings directly.
* **Feature:** Added `std:num:fract`.
* **Feature:** Added basic TCP networking and sockets: `std:net:tcp:connect`,
`std:net:tcp:listen`, `std:io:write`, `std:io:write_some` and `std:io:read_some`.
* **Bugfix:** `v.0 => v.1` did not parse correctly.
* **Bugfix:** Calling `$i(...)` and `$p()` without any arguments did not yield the
called value itself.

0.6.3 (2020-08-06)
==================

* Incompatible Change: Renamed `std:io:file:copy` to `std:fs:copy`.
* Incompatible Change: Renamed `error_to_str` to `std:error_to_str`.
* Documentation: Completed standard library function reference documentation.
* Feature: `std:rand :i64` now returns an integer in the full i64 range.
* Feature: Implemented slicing operations for calling pairse and integer vectors
with vectors and iterators.

0.6.2 (2020-07-20)
==================

* Bugfix: Selectors for recursive `$S(**/..)` did not process node conditions
right.
* Bugfix: `!@import wlambda` did not work as expected, it should have the same
meaning as `!@wlambda`.
* Bugfix: `$i(...)` and `$f(...)` constructions were done in the wrong order
if arguments were on the stack.
* Bugfix: If a call in RPCHandle paniced, there was absolutely no error message.
* Change: Give proper error if a numeric vector is mutated.
* Change: Numerical vectors serialize to lists of numbers now instead some
custom Rust data structure serialization.
* Change: `$code` now parses blocks directly. This means `$code { ... }` will
only contain the text inside the curly bracktes but not the brackets themself.
This allows for more idiomatic `std:thread:spawn $code { ... }` syntax.
* Feature: Added negative key match to recursive selectors `$S(**!key=childs)`
to prohibit it to recurse into certain keys.
* Feature: Added positive value condition to recursive selectors `$S(**=:{x=10})`
to select only specific values to recurse into.
* Feature: Added std:bytes:replace function.
* Documentation: Color conversion functions `std:v:rgb2hsv`, `std:v:hsv2rgb`
and others.
* Documentation: Added documentation for `std:values`, `std:thread:spawn`,
* Documentation: Completely documented the currently implemented threading support.
`fvec`, `ivec`, `is_ivec`, `is_fvec`, `is_nvec`, `std:str:find`, `std:ref_id`
and many more.
* Feature: Added debug print function special value, that includes source position and
dynamic type name: `$DEBUG ...`.
* Feature: Added `std:time:now`, `std:srand` and `std:rand`.
* Feature: Introduced variable and field assignment
operator syntax `.x += 10` and `o.x += 10`.
* Feature: Added apply operators `&@>` and `<@&`.
* Feature: Added collection addition operators `+>` and `<+`.
* Feature: `std:values` also returns the individual elements of a numerical vector.
* Feature: `std:keys` also returns the keys of a numerical vector.
* Feature: Implemented `std:str:find`.
* Feature: Implemented `std:write_str`.
* Feature: Implemented custom WLambda string formatting, that even supports
writing numerical vectors, vectors or maps in a nicer formatted way.
See also `$F` and `std:formatter`.

0.6.1 (2020-06-04)
==================

* Incompatible Change: Reverted the weakable reference semantics with regard
to capturing. It's too unintuitive. You will have to invest the overhead
about which closures capture what and why. The other semantics are too
unpredictable.
* Incompatible Change: Removed the extra value of drop function. They can
just as easily passed as upvalue.
* Change: Previous _weakable_ references are now called _hidden_ references,
because they are automatically dereferenced on variable access.
* Incompatible Change: Strong references are _not_ implicitly dereferenced
on variable access anymore.
* Incompatible Change: `iter $&&i`, which was a bad idea, was reverted.

0.6.0 (2020-05-31)
==================

* Incompatible Change: The nested parenthesis matching for quoted strings
was a bad design because you can't properly quote strings
with non matching parenthesis anymore - which is sometimes desired if a
regex pattern or selector pattern is to be quoted.
* Incompatible Change: `a + b ~ c` is no longer `a + (b ~ c)` but `(a + b) ~ c`
to let `iter i 0 => 10 ~ std:displayln i` work properly.
* Incompatible Change: Removed old `match` function and replaced it with a
structure pattern match function, which is even faster than the match
before.
* Feature: Allowing to use `if` instead of `?`.
* Feature: `iter $&&i ...` syntax implemented, for creating new references
for each iteration for easier closure generation.
* Feature: Implemented pair operator `a => b` which does the same as `$p(a, b)`.
* Feature: `$iter $p(0, 10)` does now the same as `$iter $i(0, 10)`, allowing
nice composition with the pair operator: `$iter 0 => 10`.
* Feature: Implemented argument function call operators `a &> f` and `f <& a`,
so you can write regular expressions like `? "foo" &> $r/a*b/ { ... }`
without parenthesis.
* Feature: Implementes structure patterns as complement to the selector patterns
and regex patterns: `$M _expr_`.
* Feature: Added a jump table operation `jump idx branch-a branch-b branch-c`
as supporting operation for `match`.
* Feature: Updates VIM syntax file `vim/wlambda.vim`.
* Optimization: VVal size is down from 48 bytes to 32 bytes.
* Optimization: VM Op size is down from 40 bytes to 24 bytes.
* Bugfix: The stack size is no longer fixed and is dynamically grown if
required.

0.5.0 (2020-05-21)
==================

* Incompatible Change: VVal::Nul was renamed to VVal::None because it
was called `$none` in WLambda anyways.
* Incompatible Change: Renamed `padl` and `padr` to `pad_start` and `pad_end`
to make it more consistent naming to `trim_start` and `trim_end`.
* Incompatible Change: Renamed `neg` to `neg_i64`, `uneg` to `neg_u32`.
* Incompatible Change: Rewrote the complete threading implementation
of WLambda.
* Incompatible Change: Replaced while statement in prelude with `while <cond> <block>`.
* Incompatible Change: `zip` and `enumerate` push their arguments
to the end of the argument list now.
* Incompatible Change: `VVal::set_map_key()` has been removed and replaced by
`VVal::set_key_mv()`.
* Incompatible Change: Strings and byte vectors are immutable data types now.
* Incompatible Change: VValUserData::call takes an Env now instead of the
argument vector directly.
* Incompatible Change: Replaced MsgHandle by wlambda::rpc\_helper::RPCHandle.
* Potentially Incompatible Change: The compiler and evaluator was completely replaced
by a VM and code generator for it.
* Change: Function arguments are stored in right order (function, arg1, arg2, ...) on the
stack now. This changes some of the argument handling and semantics of functions
that did depend on the other order. (`zip`, `enumerate`, ...).
* Change: Added proper error location to errors generated from
prelude or other pure Rust functions.
* Change: Added recently defined variable name or key to syntax positions
for functions. This might help finding the actual function where
the error occurred easier.
* Change: $q and $Q recognizes nested parenthesis now:
`$q{parses { nested } stuff now! Also [}] is fine}`.
* Optimization: Removed many unnecessary String allocations.
* Feature: Numerical integer and float vectors for 2D, 3D and 4D by Cedric Hutchings.
* Feature: VValUserData can now define a `call_method(...)` trait function,
that will be called when a method call is invoked on the object: `obj.method[...]`.
* Feature: Added `not_i64` and `not_u32`.
* Feature: Added compile time constant values: `!:const X = ...`.
* Feature: Added if statement `? <cond> <then> <else>`.
* Feature: Added `iter <var> <list-expr> <block>` statement.
* Feature: Added `std:delete` added for removing elements from maps or vectors.
* Feature: Added `std:num:lerp` and `std:num:smoothstep` added.
* Feature: Added pair values `$p(a, b)`.
* Feature: Added `$code <expr>` string literals for creating literal strings
from (syntax checked) code snippets in WLambda.
* Feature: Implemented optional values `$o(...)` and `$o()` for representing
function return values that need a better distinction
between `$none` and really no value.
* Feature: Implemented iterator values `$iter ...` that can either be called
directly of passed to `iter`.
* Feature: Symbols are now fully interned strings (per thread).
* Feature: Added color functions `std:v:hex2rgba_*`.
* Feature: Implemented built in regex pattern matching using the `$r(...)` syntax.
* Feature: Implemented built in data structure selector using the `$S(...)` syntax.
* Feature: The result vector of a regex pattern or data selector can be
accessed conveniently with the variable `$\`.
* Documentation: More!

0.4.4 (2020-03-06)
==================

* Incompatible Change: Removed implicit collection of values when
iterating over a list. Implemented `$@vec`/`$+` collection/accumulation
operations for building new data structures.
* Incompatible Change: `VVal::iter` returns now a tuple for key/value pairs
instead of a newly allocated vector.
* Feature: Added _Left Hand Function Chaining_ syntax `|>` and documentation
for it.
* Feature: Added backtick quoting syntax for identifiers to make it possible
to get the prefix form of some operators like `/` or `*`.
* Feature: std:io:stdout:print, :newline and :write added.
* Feature: Calling `$true` or `$false` with a vector will return one
of the first elements. Contributed by Cedric Hutchings (cedric-h).
* Feature: Added `pick` function for picking values based on a boolean
value. Contributed by Cedric Hutchings (cedric-h).
* Feature: Added `std:zip` and `std:enumerate`.
Contributed by Cedric Hutchings (cedric-h).
* Feature: Made it possible to capture stdio output/input for
operations like `std:displayln` and alike.
* Feature: Added `std:io:lines` for reading lines of UTF-8 text.
* Feature: Added `std:str:trim`, `:trim_start` and `:trim_end`.
* Bugfix: Cargo.toml default features are now really optional.
* Documentation: Wrote wlambda\_functions.rs test for checking
that all std: and core functions are documented.
* Documentation: Fixed grammar errors in README.md and lib.rs
contributed by Cedric Hutchings (cedric-h).
* Documentation: Reference has a TOC now.
* Documentation: Many more functions in reference.

0.4.3 (2020-02-29)
==================

* Change: Actually update README.md with new guide.

0.4.2 (2020-02-29)
==================

* Change: Completely changed how weak and weakable references work `$&`.
Closures implicitly convert captured variables to weakable references which
are then caught weakly. This makes common code work less surprising and
more complicated code possible.
* Documentation: Improved quick language guide in README / lib.rs.

0.4.1 (2020-02-28)
==================

* Feature: Support for method calls on vectors.
* Feature: Added `$data` shorthand for `$self._data` or `$self.1` to access
object member data and encourage saving member data separately from the
class methods.

0.4.0 (2020-02-27)
==================

* Feature: Added `std:to_no_arity` for disabling arity checks for functions.
* Feature: Added `std:strengthen` for weak refs and weakening refs.
* Feature: Added `std:num:int_to_open01`, `std:num:int_to_closed_open01`
and `std:num:int_to_open_closed01`.
* Feature: Added `std:rand:split_mix64_next_closed_open01` and
`std:rand:split_mix64_next_open_closed01`.
* Feature: Added basic prototyped object orientation.
* Optimization: Unrolled argument processing for function calls.
* Optimization: Unrolled GetKey operations.
* Change: `std:to_drop` disables arity checks for drop functions.
* Change: Optimized a few hot spots.
* Change: While optimizing I made arithmetic operations compiler syntax and not
functions anymore.
* Change: Symbols are handled equally to strings internally now, also improves
performance a bit when accessing maps.
* Bugfix: Basic arithmetics are now wrapping and don't panic on overflow anymore.
* Bugfix: Parser used unwrap() in some places where an EOF was not expected.
          These errors are now handled properly.
* Bugfix: Protect against division by 0 and make a wlambda panic from it.

0.3.6 (2020-01-24)
==================

* Feature: Added `std:shuffle` implementing fisher yates shuffle.
* Feature: Added `std:sort`, `std:cmp:num` and `std:cmp:str`.
* Feature: Added `std:copy` for shallow copies.
* Feature: Added `std:unshift`.
* Feature: The `LocalFileModuleResolver` now also searches relative
to the file where the `!@import` statement is evaluated. And custom
resolvers are passed the file path.
* Change: Made calling `$none` an invalid action that causes a panic.
* Bugfix: Correctly handle modification of lists or maps that are being
iterated over. They cause a WLambda panic now with a backtrace instead of
panicing on borrow\_mut() in the host application.
* Bugfix: Fixed recursive module loading, as borrow\_mut() prevented it.

0.3.5 (2019-12-06)
==================

* Feature: Added `std:measure_time _unit_ _function_` for simple benchmarking.
* Feature: Made optional '=' in `!@export` and `!@import` statements.
* Incompatible Change: Error values are not directly assignable to data
structure fields anymore. So that your data structures don't end up with
errors in it, without knowing where they were actually assigned.
* Change: `VVal::s_raw()` returns an empty string now for VVal::Nul instead of `$n`.
* Bugfix: Assigning to lists and maps inside a std:to\_drop marked value
works now too.
* Bugfix: The parser didn't handle quoted symbols (`:"foo"`) not correct and
just returned them as string.
* Doc: Documented closures briefly, reference data types and how to do
object oriented programming.
* Doc: Documented more reference handling.
* Feature: Added `std:write\_str` and added std:to\_drop string representation.
* Feature: Added `std:deser:csv` simple but working CSV parser that copes
also with multiline Excel-CSV exports. It deserves it's own crate probably.
* Feature: Added `std:ser:csv` which is a simple but working VVal table
to CSV serializer that also handles quoting `"` in CSV files correctly.
* Feature: Added splice syntax for splicing vectors and maps into literals:
`$[1,2,*$[3,4]]` and `${a = 1, b = 2, *${c = 3}, d = 4}`.

0.3.4 (2019-11-20)
==================

* Feature: Version of WLambda is printed to stderr in the executable
and is available with the `std:wlambda:version` function at runtime.
* Feature: EvalContext::eval\_string() implemented.
* Feature: Added set\_vval\_method! macro for providing an OO API more easily.
* Feature: Expanded the VVal API with quick access methods, for
example `self.v_i(idx)` or `self.v_ik(key)`.
* Feature: Added `for` function, to iterate over lists, maps, bytes and strings.
* Feature: Added VVal::iter() for nicer iteration over VVal lists/maps from Rust.
* Feature: Added `std:str:to_char_vec` to convert a string to/from a list of it's unicode code points.
* Bugfix: The arguments passed on the stack weren't cleared properly
after the call. This affected std:to\_drop functions, which weren't
called when expected.
* Bugfix: Calling references (including VVal::DropVVal) now resolves
to the inner values of them.

0.3.3 (2019-11-06)
==================

* Feature: If a string or byte vector is called with a function as first
argument, the function is called for each character or byte.
* Feature: You can iterate and map over a map now by calling it with a
function as first argument.
* Change: If a string is called with index/len, and it's outside the range,
an empty string is returned and not `$none`.
* Bugfix: Fixed std:displayln space placement and emitting empty lines with
no parameters.

0.3.2 (2019-11-05)
==================

* Incompatible Change: Error values are no longer allowed as function arguments.
This greatly improves the situation where error values linger around in
data structures or get written out unintentionally. With this the risk of
unintentionally ignoring an error is mitigated.
* Feature: Calling numbers with strings and byte vectors returns the
character or byte substring at that index (based on 0). This is symmetrical,
you can also call a string or a byte vector with an integer to get the
same result.
* Feature: Added std:io:file:read\_text, std:io:file:read, std:io:file:write\_safe
and std:io:file:append.
* Bugfix: VValUserData get\_key() was never called.
* Feature: Added VValUserData::call() to make VValUserData callable.
* Bugfix: LocalFileModuleResolver now properly inherits the loaded modules from the
parent GlobalEnv.
* Feature: Added SymbolTable::set() for setting variables that are provided by a module
from Rust.
* Feature: Added many std:num:\* functions (still undocumented)
* Feature: Added std:hash:fnv1a, and std:rand:split\_mix64\_\* functions.

0.3.1 (2019-09-16)
==================

* Incompatible Change: Removed create\_wlamba\_prelude() and added proper
module support instead. Splitted up the prelude into a core part and the
standard library utility functions. With `!@wlambda` you can import the core
language globals. With `!@import std std` you import the standard library.
* Feature: re:match added.
* Bugfix: Errors weren't properly propagated by the threads::MsgHandle.
* Change: Error reporting for arity mismatch contains the position of the
function definition and the position of the call site.
* Feature: Added std:append and std:prepend.
* Change: Enhanced stringified version of a function.
* Feature: Added apply `[[@]]` syntax for applying a function to a vector of arguments directly.
* Change: Defined equality with == for everything.
* Doc: Documented bool data type.
* Bugfix: Fixed evaluation order of function arguments.
* Feature: Implemented VVal::ref\_id and `std:ref_id` which returns a
unique 64bit signed integer dependent on the memory ID of the value.
This is only useful for reference types, where the storage location of a
value doesn't depend on the stack.
* Feature: Made stringifying of WLambda data cope properly with cyclic data
structures, this makes printing objects with closures work without
locking up your script.
* Change: Greatly enhanced error message file position reporting and
panics have better backtraces now.

0.3.0 (2019-09-13)
==================

* Incompatible Change: Removed :wref and :ref definition labels in favor of
proper $&&, $& and $\* reference handling. Added `wl:weaken` and `wl:set_ref` and
`.*x = ...` syntax for even more reference handling.
* Incompatible Change: Replaced [] with () parenthesis. This means function calls
are now having the [] brackets as argument list delimiters and
regular arithmetics and expression grouping/delimiting is done with the more
traditional () parenthesis. This is hopefully the last big incompatible syntax change
before the 1.0 release.
* Feature: ser:json got a non-pretty print argument.
* Doc: Documented ser:json and deser:json, with additional examples.
* Feature: ser:msgpack and deser:msgpack added.
* Feature: Implemented `wl:eval` for evaluating wlambda code from inside
wlambda code, so you can evaluate code while you are evaluating code.
* Feature: Thread communication helpers implemented for connecting
WLambda EvalContext instances accross threads and providing easy
cross thread messaging and RPC.
* Feature: Custom userdata can be implemented using the VValUserData trait.
* Feature: Added more bytes related functions and call properties.
`bytes:to_vec`, `bytes:from_vec`, `bytes:to_hex`, `bytes:from_hex`,
`str:to_utf8`, `str:from_utf8`, `str:from_utf8_lossy`.
* Feature: Added `len` function to get the length of vectors and other
sequential data structures. `str:len` returns the length in unicode characters
now.
* Bugfix: Weak upvalue references were not properly handled and directly returned
by variable accesses.

0.2.2 (2019-09-01)
==================

* `-` and `+` are not parsed as operator if a digit follows.
  `fun :bar -2 2` was parsed as operator call for `-`.
  Sometime in the future we should change to a proper tokenizer.
* Bugfix: Assigning to a local variable cleared it's :ref state.
* Feature: Added optional serde/serde_json implementation and prelude
function `ser:json` and `deser:json`.

0.2.1 (2019-07-18)
==================

* Removed unprefixed destructuring syntax for a more consistent syntax.
* Assignments and definitions no longer return the stored value because
it mixes badly with $error values.

0.2.0 (2019-07-18)
==================

* Completed string support in compiler.
* Started documentation for wlambda prelude.
* Incompatible change: Introduced function arity checks and arity definition syntax.
* Added support for arbitrarily quoted string with $q/fooobar/.
* Added string calling semantics for substring and concatenation.
* Added 'match'.
* Incompatible change: Changed map syntax, keys are delimited by '=' now, not ':'
to make parsing easier with multiple symbols as arguments for calls.
* Added ability to call VVal::Fun in the EvalContext and an available Env.
Useful for storing callbacks.
* Added global variables to syntax, compiler and API.
* EvalContext got set_global_var and get_global_var for easy access to the
global environment.
* Added user field to Env, so the user of wlambda can provide his own registry for
storing data from globally added functions.
* Added error values for error reporting, and \_? and on\_error functions for
handling error values. \_? returns error values directly upwards, like ? in
Rust. And on\_error handles an error value in the function passed as first
argument.
* Renamed `$nul` to `$none` which makes more sense in the `$error` context.
* Added `panic` and `assert` and also make the compiler aware of
the debugging positions that the parser augmented the AST with for
error reporting.
* Added unwrap routine to prelude.
* Added match with ?e for errors.
* Added block and return for non local jumps upwards the stack.
* Added syntax: \:func_label { ... } for easier returns.
* Added syntax: \ \<statement\>  for one statement functions.
* Added panic for fatal error reporting.
* Added bool, int, float, str and sym routines to prelude for converting values.
* Changed symbol text representation.
* Added checking functions like is_nul, is_sym, is_str, ...
* Added regex and chrono as optional features.
* Added wl:assert.
* Added || syntax for prepending a function call.
* Added 'not'.
* Added re:map, str:len, str:join, fold and made lists do a map operation on calling.
* Added take, drop, str:cat and re:replace_all.
* Created an integration test for testing the WLambda code blocks in
the prelude.rs reference documentation.

0.1.1 (2019-05-23)
==================

* Implemented EvalContext to keep a global and top-level local
  environment around for future eval() operations in it.
* Implemented a basic REPL in main.rs.

0.1.0 (2019-05-17)
==================
Initial release on GitHub.
Initial release on crates.io.

* Documented most things.
* Finished most important functionality in WLambda.
* This code is still proof of concept. And not even ready
  to be used as library.
