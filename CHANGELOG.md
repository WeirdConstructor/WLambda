0.1.2 (unreleased)
==================

* Completed string support in compiler.
* Added support for arbitrarily quoted string with $q/fooobar/.
* Added string calling semantics for substring and concatenation.
* Added 'match' 
* Changed map syntax incompatible. Keys are delimited by '=' now, not ':' because
to make parsing easier with multiple symbols as arguments for calls.
* Added ability to call VVal::Fun in the EvalContext and an available Env.
Useful for storing callbacks.
* Added user field to Env, so the user of wlambda can provide his own registry for
storing data from globally added functions.
* Added error values for error reporting, and \_? and on\_error functions for
handling error values. \_? returns error values directly upwards, like ? in
Rust. And on\_error handles an error value in the function passed as first
argument.
* Added unwrap routine to prelude.
* Added match with ?e for errors.
* Added block and return for non local jumps upwards the stack.
* Added syntax: \:func_label { ... } for easier returns.
* Added syntax: \ \<statement\>  for one statement functions.
* Added panic for fatal error reporting.
* Added bool, int, float, str and sym routines to prelude for converting values.
* Changed symbol text representation.
* Added checking functions like is_nul, is_sym, is_str, ...

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
