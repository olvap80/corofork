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
- [Compilers](#compilers)
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
2. **`AwaitAsTemporaryOnly::operator co_await() &&` returns by rvalue reference on
   the measured compilers, by value elsewhere.** The macro
   `COROFORK_AWAITER_BY_REFERENCE` selects the form: `1` returns `RealAwaitable&&`
   (the awaiter is the member of the `AwaitAsTemporaryOnly` temporary, alive until the
   end of the `co_await` full expression), `0` returns `RealAwaitable` (the awaitable is
   moved into the coroutine frame). Default `1` for GCC 16 and later, Clang 22 and
   later, MSVC 2019 (`_MSC_VER` 1929) and later, `0` for every other compiler.
   `-DCOROFORK_AWAITER_BY_REFERENCE=0` or `=1` overrides the default. Measured for the
   three forms of the return type (moves of the setup lambda per `co_await`):

   | Return type | GCC 12.2 | GCC 16.2 | Clang 22.1.8 | MSVC 2019 |
   | --- | --- | --- | --- | --- |
   | `RealAwaitable` (macro `0`) | 2, all tests pass | 2, all tests pass | 2, all tests pass | 2 |
   | `RealAwaitable&&` (macro `1`) | 2, the two `*_setup_lambda_is_moved_not_copied` tests fail (they expect 1) | 1, all tests pass | 1, all tests pass | 1, all tests pass |
   | `RealAwaitable&` (before commit `a636459`, not used) | does not compile: GCC copies the awaiter, the copy constructor is deleted | compiles | compiles | not checked |

   To add a compiler to the default list (GCC 13, 14, 15 are not measured yet): build
   the tests with `-DCOROFORK_AWAITER_BY_REFERENCE=1`; when
   `function_setup_lambda_is_moved_not_copied` and
   `invert_callback_setup_lambda_is_moved_not_copied` pass, the compiler makes 1 move.
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
    0 heap allocations, 0 copies, 1 move of the setup lambda with
    `COROFORK_AWAITER_BY_REFERENCE` `1` (the default on these three compilers), 2 moves
    with `0` (the default elsewhere, measured with GCC 12.2 too). The moves:
    - into `setupLambda`, while `AwaitAsTemporaryOnly` constructs the awaitable in place
      (`std::in_place` constructor, no move of a ready awaitable),
    - only with `0`: out of `AwaitAsTemporaryOnly::operator co_await() &&`, which then
      returns the awaitable by value into the coroutine frame (item 2).

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
| GCC 12 (12.1 to 12.5): a setup lambda written inside `co_await` and capturing by value an object that frees memory in its destructor crashes with `double free` | the compiler | GCC 12 destroys a bitwise duplicate of the lambda temporary too; the caller avoids the crash with `[&]`, a named lambda moved in, or a helper function, see [Compilers](#gcc-12-destroys-co_await-temporaries-twice) |

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

## Compilers

| Compiler | Where | Result |
| --- | --- | --- |
| GCC 16.2 | MSYS2 UCRT64 | all tests pass, `-O0` to `-O2` |
| Clang 22.1.8 | MSYS2 UCRT64 | all tests pass, `-O0` to `-O2` |
| MSVC 2019 (14.29) | Visual Studio 2019, CMake, `/std:c++20` | all tests pass, Debug and Release |
| GCC 13.1, 13.2, 13.3, 13.4, 14.1, 14.2, 14.3, 14.4, 15.1, 15.2, 15.3 | Compiler Explorer (godbolt.org), x86-64 | the reproduction below passes at `-O0` and `-O2`; the test suite was not run |
| GCC 12.2 | WSL Debian 12 (`g++ (Debian 12.2.0-14+deb12u1) 12.2.0`) | wrong code: `test_invert_function` aborts with `free(): double free detected` in `function_setup_lambda_may_capture_move_only_value` (a by value capture written inside `co_await`, kept on purpose to show the compiler bug); `test_corofork`, `test_callback_pool`, `test_invert_subscription`, `test_coro_expected` pass |
| GCC 12.1, 12.2, 12.3, 12.4, 12.5 | Compiler Explorer (godbolt.org), x86-64 | wrong code: the reproduction below fails at `-O0` and `-O2` |

### GCC 12 destroys co_await temporaries twice

GCC 12 runs the destructor twice for a temporary object of a `co_await` full
expression, when the temporary is created by a lambda expression or by aggregate
initialization and holds a member with a destructor. Every GCC 12 release is
affected (12.1 to 12.5), GCC 13.1 and every later release up to 15.3 are not
(the reproduction below, run on Compiler Explorer on 2026-10-16). Measured with
GCC 12.2 at `-O0` and `-O2` (constructions / destructions of the counted member):

| Shape | GCC 12.2 | GCC 16.2, Clang 22.1.8, MSVC 2019 |
| --- | --- | --- |
| `co_await referencing([member = CountedMember()]{})` (lambda temporary) | 1 / 2 | 1 / 1 |
| `co_await AggregateAwaiter{}` (struct without constructors, counted member) | 1 / 2 | 1 / 1 |
| `co_await ConstructedAwaiter{}` (user constructor, counted member) | 1 / 1 | 1 / 1 |
| `co_await SelfCountedAwaiter{}` (the awaiter itself counted, user constructors) | 1 / 1 | 1 / 1 |

Minimal reproduction, no corofork involved (exit code 0: the compiler is fine,
exit code 1: the lambda was destroyed twice):

```cpp
#include <coroutine>
#include <cstdio>

int constructions = 0;
int destructions = 0;

struct CountedMember{
    CountedMember(){ ++constructions; }
    CountedMember(const CountedMember&){ ++constructions; }
    CountedMember(CountedMember&&) noexcept { ++constructions; }
    ~CountedMember(){ ++destructions; }
};

/// Awaiter without counted members, keeps only a reference to the lambda (shape of the old invert_function)
template<class Lambda>
struct LambdaReferencingAwaiter{
    const Lambda& lambda;
    bool await_ready() const noexcept { return false; }
    bool await_suspend(std::coroutine_handle<>) const { lambda(); return false; }
    int await_resume() const noexcept { return 7; }
};

template<class Lambda>
LambdaReferencingAwaiter<Lambda> referencing(const Lambda& lambda){ return {lambda}; }

struct FireAndForget{
    struct promise_type{
        FireAndForget get_return_object(){ return {}; }
        std::suspend_never initial_suspend() noexcept { return {}; }
        std::suspend_never final_suspend() noexcept { return {}; }
        void return_void(){}
        void unhandled_exception(){}
    };
};

int received = 0;
FireAndForget awaitWithLambdaTemporary(){
    received = co_await referencing([member = CountedMember()]{});
}

int main(){
    awaitWithLambdaTemporary();
    std::printf("lambda temporary with a counted capture: constructions=%d destructions=%d%s\n",
                constructions, destructions, constructions == destructions ? "" : "   <-- destroyed twice");
    return constructions == destructions ? 0 : 1;
}
```

```sh
g++ -std=c++20 -O2 reproduction.cpp -o reproduction && ./reproduction; echo "exit=$?"
# GCC 12.x: lambda temporary with a counted capture: constructions=1 destructions=2   <-- destroyed twice
#           exit=1
# GCC 13.1 and later: lambda temporary with a counted capture: constructions=1 destructions=1
#           exit=0
```

Consequences for corofork:

- Every `invert_*` call passes a lambda temporary inside a `co_await` expression,
  so with GCC 12 a setup lambda capturing `std::string`, `std::unique_ptr` or
  `std::vector` by value frees the same memory twice. The header of commit `a636459`
  crashes the same way (a setup lambda capturing a 64 character `std::string`
  aborts with exit code 134), so the problem is older than the setup lambda
  ownership of decision 11.
- The tests of the repository saw nothing before `function_setup_lambda_may_capture_move_only_value`
  existed: the other setup lambdas capture with `[&]`, and values moved out through
  `co_await` are empty afterwards, a second destructor call on an empty
  `std::string` or a null `std::unique_ptr` frees nothing.
- Which object is destroyed twice (traced with GCC 12.2, every construction and
  destruction printed with an id): for `co_await invert_function([traced = TracedValue()](...){...})`
  the values are constructed as id 1 (the capture of the lambda temporary), id 2
  (moved into the awaitable), id 3 (moved into the frame), and destroyed as id 3,
  id 2, then an object at an address where no constructor ever ran, carrying id 1,
  then id 1. GCC 12 makes a bitwise duplicate of the lambda temporary before
  `invert_function` receives the lambda, and destroys the duplicate as well. A
  `std::string` or `std::unique_ptr` capture of the duplicate still points to the
  memory owned by id 2 and id 3: double free.
- No workaround inside the library is possible: the duplicate is made in the code of
  the caller, before any corofork code runs. Making `AwaitAsTemporaryOnly` itself the
  awaiter (forwarding `await_*`, with only `operator co_await() &` deleted) fails with
  GCC 12.2 and GCC 16.2 alike anyway: GCC picks the deleted `&` overload for a prvalue
  operand.
- The workaround is on the side of the caller, and costs nothing with the other
  compilers (the same code). Checked with a 64 character `std::string` and a
  `std::unique_ptr` capture, GCC 12.2 at `-O0` and `-O2`, GCC 16.2, Clang 22.1.8:

  | Setup lambda | GCC 12.2 | GCC 16.2, Clang 22.1.8 |
  | --- | --- | --- |
  | written inside `co_await`, captures by value | double free, exit code 134 | works |
  | written inside `co_await`, captures by reference (`[&]`) | works | works |
  | named lambda with captures by value, `co_await invert_function(std::move(setup))` | works | works |
  | helper function returning `invert_function([url = std::move(url)](...){...})` | works | works |
  | a value for the callback to return: `invert_function(setup, TracedValue())` | works (traced: no double destruction) | works |

  README.md ("GCC 12: three rules for setup lambdas") and the text of the GCC 12
  warning in `corofork.h` state the rules. Tests of the recommended forms:
  `function_named_setup_lambda_with_move_only_capture_is_moved_in` (rule 2),
  `function_helper_may_return_invert_function` (rule 3).
- The GCC bug tracker has reports of the same kind: PR 99576
  (<https://gcc.gnu.org/pipermail/gcc-bugs/2021-June/744474.html>), PR 107288
  (<https://gcc.gnu.org/pipermail/gcc-bugs/2024-May/864034.html>), and a patch
  addressing PR 99576, 100611, 101976, 101367 and 107288
  (<https://patchwork.sourceware.org/comment/133834/>). Which of the reported bugs
  matches the measured one was not confirmed; the measurement shows the first good
  release: GCC 13.1.

`corofork.h` emits `#pragma GCC warning` when `__GNUC__ == 12` (Clang excluded),
right after the includes; `-DCOROFORK_SUPPRESS_GCC12_WARNING` removes the warning.
Checked with GCC 12.2: one warning per translation unit, with `-Werror` the build
stops; GCC 16.2, Clang 22.1.8 and MSVC 2019 show no warning. The pragma is used
instead of `#warning`: with `-std=c++20 -Wpedantic`, `#warning` adds a warning of its
own ("#warning is a GCC extension" from GCC 12.2, "'#warning' before C++23 is a GCC
extension" from GCC 16.2), `#pragma GCC warning` adds nothing.

To check another GCC version: compile and run the reproduction above (exit code 0
expected), then run the test suite. The single test that exposes the bug is
`function_setup_lambda_may_capture_move_only_value` in `test_invert_function`.
The test suite of corofork has not been run with GCC 13, 14 or 15 yet: Compiler
Explorer received only the reproduction above, never code of corofork.

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
- Checked toolchains and their results: see [Compilers](#compilers).
- Every C++ sample of README.md compiles with all three compilers (each sample as a
  separate translation unit, with declarations of the fictional APIs added). The
  single exception is intended: MSVC rejects the sample of the pitfall "A block without
  co_await" with error C4716, as README.md states. Recheck the samples after changing
  README.md.

## Plans

### Maintenance

1. **Extend the default list of `COROFORK_AWAITER_BY_REFERENCE`** with GCC 13, 14, 15
   (and older Clang, MSVC 2022) after the check described in
   [Design decisions](#design-decisions), item 2. Needs a local compiler: Compiler
   Explorer got only the reproduction of the GCC 12 bug, never code of corofork.
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
