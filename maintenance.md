# corofork: notes for maintainers

Written for a maintainer who knows C++ well and has not worked with C++20
coroutines yet. README.md describes the library from the user side, this file
describes the inside: how coroutines work in terms of ordinary C++, how
`corofork.h` and `coro_expected.h` are built, which decisions were taken and why,
the known sharp edges, and the plans.

- [Files](#files)
- [Coroutines in terms of ordinary C++](#coroutines-in-terms-of-ordinary-c)
- [Map of corofork.h](#map-of-coroforkh)
- [Who owns what](#who-owns-what)
- [Design decisions](#design-decisions)
- [Exceptions](#exceptions)
- [Known sharp edges](#known-sharp-edges)
- [Tests](#tests)
- [Plans](#plans)

## Files

| File | Content |
| --- | --- |
| `corofork.h` | the whole library: `corofork`, `corosync`, `invert_function`, `invert_callback`, `invert_subscription`, `callback_from_lambda`, `scoped_callback` |
| `coro_expected.h` | `coro_expected<Value, Error>`, independent from `corofork.h` |
| `Tests/` | one executable per file, `Tests/test_framework.h` is the dependency free harness |
| `Demo/` | example programs, built by `CMakeLists.txt` |
| `CMakeLists.txt` | demos, tests, CTest registration |

## Coroutines in terms of ordinary C++

### What the compiler generates

A function becomes a coroutine as soon as the body contains `co_await`,
`co_return` or `co_yield`. The return type of the function selects a class
`ReturnType::promise_type`, and the compiler rewrites the function roughly into:

```cpp
ReturnType function(Parameters... parameters){
    // 1. one heap block ("coroutine frame") for: copies of the parameters,
    //    the promise object, locals that live across a suspension, and
    //    temporaries of a co_await expression that is in progress
    Frame* frame = new Frame(parameters...);   // promise_type::operator new, if declared

    ReturnType returned = frame->promise.get_return_object();

    co_await frame->promise.initial_suspend(); // suspend_always: the call returns here,
                                               // the body has not run yet
    try{
        /* the body */
    }
    catch(...){
        frame->promise.unhandled_exception();
    }
final_suspend:
    co_await frame->promise.final_suspend();   // suspend_never: the frame is deleted after this point
}
// the call returns `returned` at the first suspension (or at the end, if nothing suspends)
```

`co_return value;` calls `promise.return_value(value)`, `co_return;` calls
`promise.return_void()`, and both jump to `final_suspend`.

### What `co_await operand` does

```cpp
auto&& awaiter = /* operand, after promise.await_transform(operand) if the promise declares
                    await_transform, then after operator co_await if the type declares one */;
if( !awaiter.await_ready() ){
    // the coroutine counts as suspended from here on
    awaiter.await_suspend(handle);  // returns void: go back to the code that resumed the coroutine
                                    // returns false: continue right away, no suspension
    /* return to the caller of resume() (or to the original caller of the function) */
    /* ... later some code calls handle.resume(), execution continues at the next line */
}
auto result = awaiter.await_resume();  // the value of the co_await expression
```

`std::coroutine_handle<>` is a pointer to the frame: `resume()` continues the body
at the suspension point, `destroy()` runs the destructors of the locals that are
alive at that point and frees the frame, `address()` gives the frame address.

### Facts the library relies on

- **One shot.** A suspension point is resumed at most once. A coroutine cannot be
  copied.
- **Temporaries of a `co_await` full expression live in the frame** until the end of
  that full expression. The awaitable returned by `invert_function` (the awaitable owns
  the setup lambda) stays alive while the coroutine waits.
- **A lambda used as a coroutine is not copied into the frame.** The frame keeps only
  a pointer to the closure object (the implicit `this` parameter). The captures live in
  the closure object, outside the frame. This is the reason for
  `CoroFork::LambdaStorage`: `CoroFork::Start` moves the closure object to the heap and
  the promise owns the closure object.
- **After `await_suspend` gives the handle to another thread, the coroutine may run,
  and even finish, in that other thread before `await_suspend` returns.** The awaiter
  object lives in the frame, so `await_suspend` must not touch members of the awaiter
  after giving the handle away.
- **An exception leaving `await_suspend`** is rethrown at the `co_await` inside the
  coroutine and can be caught there.
- **An exception leaving `unhandled_exception`** leaves the coroutine "suspended at the
  final suspend point": `final_suspend` does not run, nobody frees the frame. Bug A
  (fixed) was exactly this leak in `corofork`.
- **Order of `return_void` and local destructors** (measured with GCC 16.2 and
  Clang 22.1.8): reaching the end of the body destroys the locals first and calls
  `return_void` afterwards, an explicit `co_return;` calls `return_void` first and
  destroys the locals afterwards. Completion must therefore be reported from
  `final_suspend`, never from `return_void` (bug B, fixed in `corosync`).

## Map of corofork.h

In file order:

1. **Macros `corofork` and `corosync`.** `corofork(captures){ body };` expands to
   `CoroFork_detail::tagTieWithOverloadedOperator ->* [captures]() -> CoroFork_detail::CoroFork { body };`.
   `operator->*` with a tag object receives the closure object without calling the
   lambda, and `CoroFork::Start` decides how to call the lambda. `corosync` adds a
   lambda parameter `back_to_thread`.
2. **`AwaitAsTemporaryOnly`.** Wraps every awaitable returned by `invert_function` and
   `invert_callback`. The constructor taking `std::in_place` constructs the real
   awaitable inside `AwaitAsTemporaryOnly` from the arguments of `invert_*`.
   `operator co_await() &&` hands out the real awaitable, `operator co_await() &` is
   deleted, so `auto pending = invert_function(...); co_await pending;` does not compile.
3. **Declarations of the utility API**: `callback_from_lambda`, `callback_extend_lifetime`,
   `scoped_callback_holder`, `scoped_callback`.
4. **Concepts** `CoroForkLambdaToSetupFunction`, `CoroForkLambdaToSetupCallback`.
5. **Public functions**: three overloads of `invert_function` (no value, value, function
   computing the value), the same three of `invert_callback<N>`, two of `invert_subscription`.
6. **`ResumeScope`, `ResumeAndPropagateException`**: see [Exceptions](#exceptions).
7. **`CoroFork`**: `promise_type`, `LambdaStorageBase`, `LambdaStorage<Lambda>`, `Start`.
8. **`CoroSync`**: `promise_type::SharedState` (mutex, condition variable, state,
   exception), `BackToThreadAwaitable`, `ReportCompletionAwaiter`, the loop in `Start`.
9. **`GenerateForSetupMethod<decltype(&SetupLambda::operator())>`**: signature extraction
   by partial specialization over the pointer to member function of the setup lambda.
   Twelve specializations: {0, 1, N callback arguments} × {callback returns `void`,
   callback returns a value} × {setup lambda returns `void`, setup lambda returns a
   result for immediate completion}. Members:
   - `Signature`, `FunctionType` (`std::function<Signature>`),
   - `CoResultType`: what `co_await` gives (decayed; `std::tuple` for N arguments),
   - `FunctionResultType`: what the callback returns to the API,
   - static functions `SetupCBNoResult`, `SetupCBReturnsValue`, `SetupCBReturnsResultOfCall`,
     `SubscribeCBNoResult`, `SubscribeCBReturnsValue`, each builds the generated callback
     and calls the setup lambda.

   Further specializations forward `operator() const`, `const std::function&` parameters
   and raw function pointer parameters (adding `RawPointerCallbackType`) to the twelve.
10. **Awaitables** `AwaitableGeneratedCBNoResult`, `AwaitableGeneratedCBReturnsValue`,
    `AwaitableGeneratedCBReturnsResultOfCall` (plus specializations for `void` results):
    the member `setupLambda` owns the setup lambda (moved in by `invert_*`),
    `await_ready()` returns `false`, `await_suspend` calls the matching
    `GenerateForSetupMethod` function, `await_resume` moves the result out of a
    `std::optional`. The base class (`Decorator`) turns the generated lambda into what
    the API receives:
    - `TheSame`: the lambda as is (becomes the `std::function`),
    - `ToPlainC<N, Tag>`: the lambda goes into a slot of the callback pool, the API
      receives the function pointer of that slot, `keepAlive` frees the slot when the
      awaitable is destroyed.
11. **Callback pool** (behind `invert_callback`, `callback_from_lambda`, `scoped_callback`):
    - `LambdaListNode`: one slot, a union of the stored lambda and the "next free" link,
      plus the function pointer of the slot,
    - `ObtainAutoFreePlainFunction` / `ObtainNoAutoFreePlainFunction`:
      `template<Node* self> struct For{ static Res apply(Args...); }`. Templating on the
      node address produces one distinct static function per slot, so the function
      pointer itself identifies the slot and no `void* context` is needed,
    - `LambdaListNodeGenerator<..., reservedCount>`: compile time recursion
      `reservedCount → reservedCount-1 → ... → 1` that defines `reservedCount` static
      nodes chained into a free list,
    - `CallbackAllocator`: free list under a `std::mutex`, a specialization for one slot
      uses a `bool` flag instead.

    A pool exists per (reservedCount, tag, lambda type). Every lambda expression has its
    own type, so in practice every place in the source code gets its own pool.
    `AdditionalOptionalTag` separates pools when the same type is used in several places.
12. **`InvertSubscriptionHolder`** (plus the `void` specialization): event queue,
    `whoAwaitsNow`, the cleanup function, the awaitable.
13. **Out-of-class definitions** of `callback_from_lambda` and `scoped_callback`.

## Who owns what

| Object | Owner | Destroyed |
| --- | --- | --- |
| `corofork` frame | the frame itself | `final_suspend` is `suspend_never`: at the end of the body |
| `corofork` closure object (captures) | `CoroFork::promise_type::capturingFunction` | together with the frame |
| `corosync` frame | `CoroSync::Start` | `Start` calls `handle.destroy()` after `ReportCompletionAwaiter` reported completion |
| `corosync` closure object | the full expression of the `corosync` statement | after `Start` returns, `Start` waits for the body |
| `CoroSync::promise_type::SharedState` | stack of `CoroSync::Start` | when `Start` returns |
| setup lambda | the awaitable (member `setupLambda`, moved in by `invert_*`) | together with the awaitable |
| `AwaitableGeneratedCB*` awaitable | temporary of the `co_await` full expression | end of that full expression |
| generated `std::function` | the API that received the `std::function` | decided by the API |
| callback pool slot of `invert_callback` | `ToPlainC::keepAlive` of the awaitable | end of the `co_await` full expression |
| callback pool slot of `callback_from_lambda` | nobody | after the first call of the function pointer (or `callback_extend_lifetime::dispose()`) |
| callback pool slot of `scoped_callback` | `scoped_callback_holder` | destructor of the holder |
| `coro_expected` frame | the `coro_expected` object | `~coro_expected` |

## Design decisions

1. **The setup lambda runs inside `await_suspend`.** When the API receives the
   callback, the coroutine is already suspended, so the API may call the callback at
   once from any thread without a race. The price: `await_ready()` is always `false`.
   Immediate completion goes through the setup lambda returning `std::optional` (or
   `bool` for `void()` callbacks): `ProcessSetupResult` stores the value and
   `await_suspend` returns `false`, so the coroutine continues without suspending.
2. **`AwaitAsTemporaryOnly::operator co_await() &&` returns the awaitable by value.**
   According to the comment in the code, returning a reference made GCC copy the
   awaiter into the frame, which fails for the non copyable awaitables.
3. **Generated callbacks copy what the callback returns before resuming.** Resuming
   may destroy the generated lambda itself: the `invert_callback` slot is freed at the
   end of the `co_await` full expression, the coroutine may replace the
   `std::function`, or the coroutine may unsubscribe. Regression tests:
   `invert_callback_returned_value_outlives_freed_trampoline`,
   `invert_callback_result_of_call_outlives_freed_trampoline`,
   `function_returned_value_survives_callback_replaced_while_resumed`.
4. **Callback arguments are forwarded** with `std::forward<CallbackArg>`, so move only
   arguments (`std::unique_ptr`) pass through. Tests: `*_move_only_type`.
5. **`CoResultType` is decayed.** A callback `void(const std::string&)` gives the
   coroutine an owned `std::string`: the queue of `invert_subscription` may keep the
   value long after the call.
6. **`callback_from_lambda` trampolines free the slot before calling the lambda**: the
   trampoline moves the lambda out, frees the slot, then calls the moved lambda. The
   lambda may allocate a new callback from the same pool during the call.
7. **`scoped_callback_holder` is neither copyable nor movable on purpose**, see the long
   comment above the class: moving a holder would keep a function pointer valid while
   silently changing the lambda behind the function pointer.
8. **Lock handoff in `invert_subscription`.** `operator co_await` constructs the
   awaitable, and the constructor of the awaitable locks `protectEventsQueue`.
   `await_ready` either takes an event from the queue and unlocks, or returns `false`
   with the mutex still locked. `await_suspend` registers `whoAwaitsNow` and only then
   unlocks. No event can slip in between "the queue is empty" and "the awaitable is
   registered". `OnArgumentsArrived` resumes the coroutine outside the lock.
9. **`corosync` reports completion from `final_suspend`** (`ReportCompletionAwaiter`),
   because of the `return_void` order described above. Test:
   `corosync_returns_only_after_body_locals_are_destroyed`. The test contains a bounded
   200 ms wait on purpose: the wrong order can only be observed by letting a destructor
   wait for the caller of `corosync`.
10. **`coro_expected` keeps the frame until `~coro_expected`** (`final_suspend` is
    `suspend_always`, the outcome stays in the promise). The standard leaves open
    when the compiler converts the result of `get_return_object` to the return type
    (core issue CWG2563), and `coro_expected` stores no address of the returned object,
    so that moment does not matter. An error makes the coroutine stay suspended forever,
    and `~coro_expected` destroys the frame, which runs the destructors of the locals in
    reverse order. `await_transform` accepts only `coro_expected`, `std::expected` and
    `coro_unexpected`, everything else hits a `static_assert` with an explanation.
    `value() &&` and `error() &&` return by value: returning `Value&&` dangled for
    `auto&& number = parseDigit('7').value();` (GCC reported `-Wdangling-reference`).
11. **The awaitables own the setup lambda.** `invert_function` and `invert_callback`
    take the setup lambda as a forwarding reference, and the awaitable keeps the setup
    lambda in the member `setupLambda`: a temporary lambda is moved in, a named lambda
    is copied (a named lambda stays usable after the call). Before, the awaitables kept
    `const SetupLambdaType&`, and a helper function returning `invert_function(...)`
    left a reference to a destroyed lambda (GCC: crash, Clang `-O2`: silently
    "worked"). Measured cost per `co_await` with GCC 16.2, Clang 22.1.8 (`-O0`, `-O2`)
    and MSVC 2019 (`/O2`), for `invert_function` and `invert_callback`:
    0 heap allocations, 0 copies, 2 moves of the setup lambda. The 2 moves:
    - into `setupLambda`, while `AwaitAsTemporaryOnly` constructs the awaitable in place
      (`std::in_place` constructor, no move of a ready awaitable),
    - out of `AwaitAsTemporaryOnly::operator co_await() &&`, which returns the awaitable
      by value (item 2) into the coroutine frame.

    A move of a lambda moves the captures: no allocation for `std::string`,
    `std::unique_ptr`, `std::vector`. A setup lambda may capture move only values.
    Tests: `function_helper_may_return_invert_function`,
    `invert_callback_helper_may_return_invert_callback`,
    `function_setup_lambda_is_moved_not_copied`,
    `invert_callback_setup_lambda_is_moved_not_copied`,
    `function_named_setup_lambda_is_copied_once`,
    `function_setup_lambda_may_capture_move_only_value`.

## Exceptions

| Coroutine | Mechanism |
| --- | --- |
| `corofork` | `ResumeScope` + `ResumeAndPropagateException` |
| `corosync` | `SharedState::exception`, rethrown by `CoroSync::Start` |
| `coro_expected` | `std::exception_ptr` in the outcome, rethrown on access |

**`corofork`.** Each resumption done by corofork code goes through
`ResumeAndPropagateException(handle)`: `CoroFork::Start`, every generated callback,
`InvertSubscriptionHolder`. The function pushes a `ResumeScope` (frame address,
exception slot) onto a `thread_local` list, calls `resume()`, pops the scope, and
rethrows the exception found in the slot. `CoroFork::promise_type::unhandled_exception`
compares the innermost scope with the own frame address:

- equal: the exception goes into the slot, the body continues to `final_suspend`
  (`suspend_never`), the frame and the captures are freed, then
  `ResumeAndPropagateException` rethrows,
- different (an awaitable of another library called `resume()` directly): `throw;`.
  The exception leaves `resume()` immediately, and the frame is never freed, because
  the coroutine counts as suspended at the final suspend point. This is a known
  limitation, documented in README.md.

**`corosync`.** `unhandled_exception` stores the exception in `SharedState` and never
throws, whoever resumed the coroutine. `ReportCompletionAwaiter` sets
`FinishedWithException`, `CoroSync::Start` destroys the frame and rethrows in the
thread of the caller. A nested `corofork` inside a `corosync` body is independent: an
exception thrown by the nested `corofork` after its first suspension goes to the code
that resumed the nested `corofork`. A scope that makes `corosync` wait for nested
`corofork` blocks was considered and deferred, see [Plans](#considered-and-deferred).

## Known sharp edges

Verified by running test programs:

| Sharp edge | Where | Root cause |
| --- | --- | --- |
| The callback called inside the setup lambda, then the setup lambda reads its captures: reads destroyed objects | `SetupCB*` | the nested resume finishes the `co_await` full expression and destroys the setup lambda while the setup lambda runs |
| `corosync` in the thread that delivers the awaited callback: deadlock | `CoroSync::Start` | `Start` blocks the thread |
| A `corofork` body without `co_await` and `co_return`: GCC and Clang warn and the program crashes, MSVC 2019 reports error C4716 | macros | the lambda is not a coroutine, the lambda returns no `CoroFork` |
| Exception in a `corofork` resumed by an awaitable of another library: the frame is not freed | `CoroFork::promise_type::unhandled_exception` | no `ResumeScope` for the frame |
| `coro_expected` allocates the frame on the heap per call: GCC 16.2 and MSVC 2019 1000 allocations per 1000 calls, Clang `-O2` none | `coro_expected` | heap allocation elision only in Clang |

From reading the code, not verified by a run:

- `CallbackAllocator<1, ...>::isAllocated` is a plain `bool`: `invert_callback<1>` or
  `callback_from_lambda<1>` from the same place in several threads at once is a data race.
- `InvertSubscriptionHolder`: a callback already running in another thread while the
  holder is destroyed reaches a destroyed holder. README.md states the contract: no
  calls after the cleanup function returns.
- `InvertSubscriptionHolder::awaitable` declares `awaitable(const awaitable&&)` and
  `operator=(const awaitable&&)` without a definition.
- `invert_function(setup, function)`: the generated callback calls the copy of
  `function` after the coroutine reaches its next suspension. When the coroutine has
  ended by then, references captured by `function` into the block dangle.
- Callback called inside the setup lambda: after the nested resume finished the
  coroutine, `await_suspend` returns `true` for a destroyed frame. The code relies on
  the compilers not touching the frame after `await_suspend` returns.

## Tests

```sh
cmake -S . -B build
cmake --build build
ctest --test-dir build --output-on-failure
```

- Every `Tests/test_*.cpp` is one executable, the exit code is the number of failed
  cases. `test_coro_expected.cpp` is built twice: `test_coro_expected` (C++20) and
  `test_coro_expected_cpp23` (C++23, adds the `std::expected` cases where `<expected>`
  exists; MSVC 2019 has no `<expected>`).
- Rules of `Tests/test_framework.h`: no sleeps, no timing assumptions, every test fails
  loudly instead of hanging. The single exception is the bounded wait described in
  [Design decisions](#design-decisions), item 9.
- `corofork_test::LifetimeTrackedValue` detects copies made from a destroyed object
  (the destructor overwrites a `volatile` marker). Use the type for every "is the
  capture still alive" question.
- `corofork_test::CopyMoveCounter` counts copies and moves of itself. Captured by a
  setup lambda, the counters show what `invert_*` costs (decision 11).
- To prove a new test catches a bug, build the test against the header without the fix:
  `git show HEAD:corofork.h > <directory>/corofork.h` and put `<directory>` first in the
  include path. Every test added for bugs A–E failed that way before the fix. Of the
  tests for decision 11, the helper tests crashed and
  `function_named_setup_lambda_is_copied_once` failed with the header from before;
  the `*_moved_not_copied` tests pass with both headers and guard the cost from now on.
- Checked toolchains: GCC 16.2 and Clang 22.1.8 from MSYS2 UCRT64, MSVC 2019 (14.29)
  through CMake with `/std:c++20`.
- Every C++ sample of README.md compiles with all three compilers (each sample as a
  separate translation unit, with declarations of the fictional APIs added). The
  single exception is intended: MSVC rejects the sample of the pitfall "A block without
  co_await" with error C4716, as README.md states. Recheck the samples after changing
  README.md.

## Plans

### Maintenance

1. **One move of the setup lambda per `co_await` instead of two**: return
   `RealAwaitable&&` from `AwaitAsTemporaryOnly::operator co_await() &&` (see
   [Design decisions](#design-decisions), item 11). Measured to work with GCC 16.2,
   Clang 22.1.8 and MSVC 2019. Blocked by the note in the code that GCC copied the
   awaiter for a returned reference: check the oldest supported GCC first.
2. **Safe synchronous completion through the callback.** When the API calls the
   callback before the setup lambda returns, the generated callback sets an atomic flag
   instead of resuming nested, and `await_suspend` returns `false`. A callback arriving
   from another thread at the same moment needs an atomic state exchange (waiting,
   completed early, resumed). The rule "the API call is the last statement of the setup
   lambda" then disappears.
3. **Collapse the twelve `GenerateForSetupMethod` specializations** with `if constexpr`
   over `std::is_void_v<OperatorResult>` and over the argument count. Bugs C and E
   touched 6 to 20 copies of the same code because of the duplication.
4. **`= delete`** the undefined `awaitable(const awaitable&&)` and
   `operator=(const awaitable&&)` of `InvertSubscriptionHolder::awaitable`.
5. **Thread safe single slot pool**: `std::atomic<bool>` instead of the plain `bool` in
   `CallbackAllocator<1, ...>`.
6. **Fill the placeholders** "RETURN sample" and "Windows API sample" in the comment at
   the top of `corofork.h`.
7. **Frame allocation pool for `coro_expected`** (`promise_type::operator new` taking
   blocks from a free list) for GCC and MSVC, which allocate per call.

### More uses of callback inversion

All of the following keep the principle "no task type, no scheduler: the thread that
calls the callback continues":

- **`first_of(setupA, setupB)`**: start two operations, continue with whichever callback
  arrives first, for example "the result or a timeout timer". The callback arriving
  second must find an atomic "already done" flag and do nothing, and the setup may
  return a cancel function for the losing operation. Gives timeouts and cancellation.
- **`all_of(setupA, setupB)`**: start several operations at once, continue when all
  callbacks arrived: `auto [user, settings] = co_await all_of(fetchingUser(id), fetchingSettings(id));`.
- **Filters on subscriptions**: `co_await messages.next_matching(predicate)` skips
  events, `co_await messages.next_for(timeout)` combines a subscription with a timer.
- **`corosync` returning a value**: `int answer = corosync_value(&) -> int { ... co_return 42; };`.
- **User interface flows as linear code**: a wizard with several dialogs, each
  `co_await` is the next button click through `invert_subscription`.
- **Protocol state machines as linear code**: a handshake reads like the protocol
  description (README.md, recipe 4).
- **Push to pull, with a limit**: an API that calls a visitor synchronously for every
  item (an enumeration function) cannot become a pull style iteration with C++20
  coroutines alone: the stack of the enumeration function must stay alive between
  items, and C++20 coroutines keep only their own frame. A producer thread plus
  `invert_subscription` works.

### Coroutines as "monads" (synchronous, like coro_expected)

The recipe of `coro_expected` generalizes: `initial_suspend` is `suspend_never`,
`final_suspend` is `suspend_always`, `await_transform` accepts a short list of operand
types, "stop here" means "store into the promise and never resume", the owner object
destroys the frame. Candidates:

- **`coro_optional<T>`**: `co_await` on `std::optional` gives the value or makes the
  whole function return `std::nullopt`.
- **Log collection**: `co_await log_line("...")` appends to a log kept in the promise,
  the caller receives the value together with the log.
- **State**: `co_await get_state` and `co_await set_state(newState)` read and write
  state kept in the promise and handed to the caller at the end.
- **Parsers**: `coro_parse<T>`, where `co_await` on a sub-parser consumes input or
  makes the whole parser fail with the error position.
- **Asynchronous code with an error channel**: a `corofork` block has no place to return
  an error to. A variant with an error handler, for example
  `corofork_with_error_handler(handler, captures){ ... }`, could accept
  `co_await check(errorCode)` and call `handler` instead of continuing.

Limits of the recipe:

- One shot: a suspension point resumes at most once, so "try every alternative"
  (resuming the same point several times) is impossible without calling the
  function again.
- Short-circuit only: collecting every error of a validation needs a different design,
  `co_await` stops at the first error.
- Each call allocates a frame, see the `coro_expected` measurements above.

### Considered and deferred

- **A scope that makes `corosync` wait for `corofork` blocks started inside the body**
  (to route their exceptions to the caller of `corosync`). Deferred on 2026-10-16:
  the scope changes the meaning of `corosync`, risks deadlocks (a nested block may wait
  for an event that happens only after `corosync` returns), and needs an explicit API,
  because the body runs in arbitrary threads and awaitables of other libraries resume
  the body without corofork code. Instead the documentation of `corosync` and README.md
  recommend `co_await invert_*` directly inside `corosync`.
