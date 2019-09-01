/// Threading API:
///
/// ```
/// # Get some random user thread:
/// let global  = wlambda::prelude::create_wlamba_prelude();
/// let mut ctx = wlambda::compiler::EvalContext::new(global);
///
/// let rpc_handle = threads::RPCHandle::new();
/// # You may register on multiple threads, but only one thread can use it at a time.
/// let sender = rpc_handle.sender();
/// sender.register_on_as(ctx, "worker");
///
/// // Calls the global `displayln` in the Worker thread with the supplied arguments.
/// ctx.eval("worker :displayln \"hello world from worker thread!\";");
///
/// ctx.eval("wl:assert_eq [worker :wl:eval \"X\"] 123;");
///
/// std::thread::spawn(move || {
///     let global_t = wlambda::prelude::create_wlamba_prelude();
///     let mut ctx  = wlambda::compiler::EvalContext::new(global_t);
///
///     ctx.eval("!:global X = 123");
///
///     rpc_handle.run(ctx);
///
///     # or alternatively:
///
///     loop {
///         // Tries to handle one RPC call within 10ms.
///         if let None = rpc_handle.step(ctx, std::time::Duration::from_millis(10)) {
///             break;
///         }
///
///         // do some other work here, that is not blocking the thread indefinitely.
///     }
/// });
///
/// ```
///
/// The alternative async messaging API, that does not provide any return values
/// from the Thread. However, you could theoretically generate two message handles
/// for a two way communication.
///
/// ```
/// # Get some random user thread:
/// let global = wlambda::prelude::create_wlamba_prelude();
/// let ctx    = wlambda::compiler::EvalContext::new(global);
///
/// let msg_handle = threads::MsgHandle::new();
///
/// let sender = msg_handle.sender();
/// # You may register on multiple threads, but only one thread can use it at a time.
/// sender.register_on_as(ctx, "worker");
///
/// std::thread::spawn(move || {
///     let global_t = wlambda::prelude::create_wlamba_prelude();
///     let ctx      = wlambda::compiler::EvalContext::new(global_t);
///
///     msg_handle.run(ctx);
///
///     # or alternatively:
///
///     loop {
///         // Tries to handle one message within 10ms.
///         if let None = msg_handle.step(ctx, std::time::Duration::from_millis(10)) {
///             break;
///         }
///
///         // do some other work here, that is not blocking the thread indefinitely.
///     }
/// });
///
/// ```
