0.2.3 (unreleased)
=================

* Feature: ser:json got a non-pretty print argument.
* Doc: Documented ser:json and deser:json, with additional examples.
* Feature: Implemented `wl:eval` for evaluating wlambda code from inside
wlambda code, so you can evaluate code while you are evaluating code.
* Feature: Thread communication helpers implemented for connecting
WLambda EvalContext instances accross threads and providing easy
cross thread messaging and RPC. (TODO: async send).
* Feature: Custom userdata can be implemented using the VValUserData trait.

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
