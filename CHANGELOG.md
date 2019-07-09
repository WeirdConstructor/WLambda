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
