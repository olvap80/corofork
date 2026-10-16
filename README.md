# corofork

Write asynchronous code as plain top-to-bottom code, on top of the callback based
APIs you already have.

```cpp
// Before: every next step lives inside the callback of the previous step
void showAvatar(int userId){
    fetchUser(userId, [](User user){
        download(user.avatarUrl, [](int status, std::string body){
            if( status != 200 ){
                showError(status);
                return;
            }
            decodeImage(body, [](Image image){
                showImage(image);
            });
        });
    });
}

// After: the same steps, one after another
void showAvatar(int userId){
    corofork(=){
        User user = co_await invert_function([&](std::function<void(User)> onDone){
            fetchUser(userId, onDone);
        });
        auto [status, body] = co_await invert_function(
            [&](std::function<void(int, std::string)> onDone){
                download(user.avatarUrl, onDone);
            });
        if( status != 200 ){
            showError(status);
            co_return;
        }
        Image image = co_await invert_function([&](std::function<void(Image)> onDone){
            decodeImage(body, onDone);
        });
        showImage(image);
    };
}
```

`fetchUser`, `download` and `decodeImage` need no changes. corofork is one header
(`corofork.h`, C++20). corofork has no task type to pass around, no scheduler and
no event loop of its own: the thread that calls your callback runs the rest of
your code.

- [Getting started](#getting-started)
- [How invert_function works](#how-invert_function-works)
- [corofork: start a sequence of steps and move on](#corofork-start-a-sequence-of-steps-and-move-on)
- [Recipes](#recipes)
- [Where exceptions go](#where-exceptions-go)
- [Pitfalls](#pitfalls)
- [API at a glance](#api-at-a-glance)
- [Building the tests](#building-the-tests)

## Getting started

Copy `corofork.h` into your project (plus `coro_expected.h` for
[error codes without exceptions](#9-error-codes-without-exceptions-coro_expected)),
include `corofork.h` and compile as C++20:

| Compiler | Flag | Checked with |
| --- | --- | --- |
| g++ | `-std=c++20` | GCC 16.2 (MSYS2) |
| clang++ | `-std=c++20` | Clang 22.1.8 (MSYS2) |
| MSVC | `/std:c++20` | Visual Studio 2019 (MSVC 14.29) |

The test suite of the repository passes with all three compilers.

## How invert_function works

An existing API wants a callback: `download(url, onDone)`. With corofork you write a
small **setup lambda**. The setup lambda receives a ready-made callback and passes
the callback to the API:

```cpp
auto [status, body] = co_await invert_function(
    [&](std::function<void(int, std::string)> onDone){ // 1. the signature download expects
        download(url, onDone);                          // 2. hand the callback to download
    });
// 3. the code continues here once download calls onDone(status, body)
```

Step by step:

1. `co_await` pauses the corofork block. `invert_function` generates `onDone` and
   calls the setup lambda with `onDone`.
2. The setup lambda starts the download and returns. The thread that executed the
   corofork statement goes on with the code after the corofork statement, nobody
   waits.
3. Later `download` calls `onDone(200, "...")`, in whatever thread `download` uses.
   The generated `onDone` continues the block right after `co_await`, in that same
   thread. The arguments of `onDone` become the result of `co_await`.

Rule of thumb: **repeat the callback signature the API expects, receive the callback
arguments as the result of `co_await`.**

| Callback signature | `co_await` gives |
| --- | --- |
| `void()` | nothing |
| `void(Image)` | `Image` |
| `void(const std::string&)` | `std::string` (your own copy) |
| `void(int, std::string)` | `std::tuple<int, std::string>`, unpack with `auto [status, body] = ...` |

## corofork: start a sequence of steps and move on

```cpp
void onLoginClicked(int userId){
    corofork(=){
        showSpinner(); // runs right away, in this thread
        User user = co_await invert_function([&](std::function<void(User)> onDone){
            fetchUser(userId, onDone);
        });
        showUser(user); // runs in the thread that called onDone
    };
    logLine("login started"); // runs as soon as the block waits for onDone
}
```

- The block starts immediately in the current thread and runs up to the first
  `co_await` that has to wait. Then the corofork statement ends and the caller
  continues.
- Captures work as in a lambda: `corofork(=)` copies the variables the block uses,
  `corofork(&)` refers to them, `corofork(url, &log)` names them,
  `corofork(data = std::move(data))` moves them in. The copies live until the block
  ends. Read the [pitfall about `&`](#corofork-refers-to-variables-of-a-function-that-already-returned)
  before using `corofork(&)`.
- A block without any `co_await` must end with `co_return;`
  ([why](#a-block-without-co_await)).

## Recipes

### 1. A callback that returns a value to the API

Some APIs expect an answer from the callback, for example "return `true` to accept".
Pass that answer as the second argument of `invert_function`:

```cpp
// existing API: calls onReady once, onReady returns true to accept the connection
void listenOnce(int port, std::function<bool(Connection)> onReady);

Connection connection = co_await invert_function(
    [&](std::function<bool(Connection)> onReady){ listenOnce(port, onReady); },
    true // what onReady returns to listenOnce
);
serve(connection);
```

### 2. The result may be ready right away

When the answer is sometimes known without asking the API (a cache, a validation
error), return the answer from the setup lambda:

```cpp
std::string text = co_await invert_function(
    [&](std::function<void(std::string)> onDone) -> std::optional<std::string> {
        if( auto cached = cache.find(url); cached != cache.end() ){
            return cached->second; // ready now: loadText is not called, onDone is never used
        }
        loadText(url, onDone);
        return std::nullopt;       // not ready: wait for onDone
    });
```

| Callback signature | Setup lambda returns |
| --- | --- |
| `void()` | `bool`, `true` means "already done, the callback will not be called" |
| `void(T)` | `std::optional<T>` |
| `void(A, B)` | `std::optional<std::tuple<A, B>>` |

Do not call `onDone` yourself inside the setup lambda for that purpose, return the
value instead ([why](#calling-the-callback-inside-the-setup-lambda)).

### 3. Plain C callbacks (function pointers)

```cpp
// existing C style API: calls on_done once, from a worker thread of the SDK
void sensor_read(int sensorId, void (*on_done)(void* context, double value), void* context);

void showTemperature(int sensorId){
    corofork(=){
        auto [context, value] = co_await invert_callback<1>([&](void (*on_done)(void*, double)){
            sensor_read(sensorId, on_done, nullptr); // no context object needed
        });
        showValue(value);
    };
}
```

- `invert_callback` produces a real C function pointer, so you need no `void* context`
  juggling and no global variables.
- `<1>` is the maximum number of callbacks generated by this line of code that wait
  at the same time. One corofork waiting at the line needs `1`. Ten coroforks started
  from a loop and waiting at the same line at once need `<10>`. With more waiting
  callbacks than reserved, `co_await` throws `std::runtime_error`.
- The reserved callbacks are prepared at compile time in static storage:
  no heap allocation per callback.
- Variants with a return value work as in [recipe 1](#1-a-callback-that-returns-a-value-to-the-api):
  `invert_callback<1>(setup, valueForTheApi)`.

### 4. Events that repeat

```cpp
// existing API: calls onMessage for every incoming message until unsubscribe(id)
int subscribeToMessages(std::function<void(std::string)> onMessage);
void unsubscribe(int subscriptionId);

void serveClient(){
    corofork(=){
        auto messages = invert_subscription([](std::function<void(std::string)> onMessage){
            int subscriptionId = subscribeToMessages(onMessage);
            return [subscriptionId]{ unsubscribe(subscriptionId); }; // cleanup
        });

        std::string userName = co_await messages; // first message
        std::string password = co_await messages; // second message
        if( checkLogin(userName, password) ){
            for( ;; ){
                std::string command = co_await messages;
                if( command == "quit" ){
                    break;
                }
                execute(command);
            }
        }
    }; // the destructor of messages calls the cleanup: unsubscribe(subscriptionId)
}
```

- Repeat `co_await messages` as often as you need, each `co_await` gives the next
  event. A login handshake or any other protocol reads as the protocol description.
- Events that arrive while the block is busy elsewhere are queued in arrival order,
  `messages.count()` gives the number of queued events.
- The setup lambda may return a cleanup function, for example the unsubscribe call.
  The destructor of `messages` calls the cleanup function. Return nothing when no
  cleanup is needed.

### 5. Switch threads, post to the UI thread, wait for a timer

Each of the three is "an API that calls a callback later":

```cpp
// continue in a new thread
co_await invert_function([](std::function<void()> resume){
    std::thread(resume).detach();
});

// continue in the UI thread (the "post" / "invoke later" function of your toolkit)
co_await invert_function([](std::function<void()> resume){
    postToUiThread(resume);
});
label.setText(text); // runs in the UI thread

// pause the block for one second, no thread is blocked meanwhile
co_await invert_function([](std::function<void()> wake){
    startTimer(std::chrono::seconds(1), wake);
});
```

### 6. One helper per API call

The setup lambdas repeat. Put each one into a helper function that **returns the
setup lambda**, and keep `invert_function` at the `co_await`:

```cpp
auto downloading(std::string url){
    return [url](std::function<void(int, std::string)> onDone){
        download(url, onDone);
    };
}

auto waiting(std::chrono::milliseconds delay){
    return [delay](std::function<void()> wake){
        startTimer(delay, wake);
    };
}
```

```cpp
auto [status, body] = co_await invert_function(downloading(url));
co_await invert_function(waiting(std::chrono::seconds(1)));
```

Do not return `invert_function(...)` itself from a helper
([why](#returning-invert_function-from-a-helper)).

With helpers, a retry with a growing pause is a plain loop:

```cpp
corofork(url){
    for( int attempt = 1; attempt <= 3; ++attempt ){
        auto [status, body] = co_await invert_function(downloading(url));
        if( status == 200 ){
            save(body);
            co_return;
        }
        co_await invert_function(waiting(std::chrono::seconds(attempt)));
    }
    showError(503);
};
```

### 7. Call asynchronous code from ordinary blocking code: corosync

```cpp
int main(){
    corosync(&){
        auto [status, body] = co_await invert_function(downloading("https://example.com"));
        std::printf("%d: %s\n", status, body.c_str());
    };
    // the download has finished here
}
```

- `corosync` blocks the calling thread until the block ends. Use `corosync` in
  `main`, in command line tools, in unit tests, or behind an old synchronous
  interface.
- `corosync(&)` is safe: the caller waits, so the referenced variables stay alive.
- Every exception thrown in the block reaches the caller of `corosync`, in the
  thread of the caller, whichever thread ran the code that threw.
- `co_await back_to_thread;` continues the block in the thread that called
  `corosync`. Useful when the callback runs in a thread that must not be held up
  (network, UI) and the next step is heavy:

```cpp
corosync(&){
    auto [status, body] = co_await invert_function(downloading(url)); // in the network thread now
    co_await back_to_thread;                                           // in the calling thread again
    Report report = parseHugeDocument(body);                           // the network thread is free
    printReport(report);
};
```

- Inside `corosync`, `co_await invert_*` directly. Do not start a nested `corofork`
  for the same purpose: `corosync` does not wait for the nested `corofork`, and an
  exception thrown by the nested `corofork` after the nested `corofork` waited once
  goes to whatever thread continued the nested `corofork`, never to the caller of
  `corosync`.
- Do not call `corosync` in the thread that has to deliver the callback
  ([why](#corosync-in-the-thread-that-delivers-the-callback)).

### 8. C function pointers from lambdas with captures (no coroutine involved)

A C API wants a function pointer, and you want to capture state:

```cpp
// C style API: calls on_finished once
void job_start(const char* jobName, void (*on_finished)(int exitCode));

void startBackup(std::string jobName){
    job_start(jobName.c_str(), callback_from_lambda<4>([jobName](int exitCode){
        std::printf("%s finished with %d\n", jobName.c_str(), exitCode);
    }));
}
```

- `callback_from_lambda<4>` reserves 4 function pointers for this line of code.
  Each function pointer frees its slot after its first call. A function pointer
  that is never called keeps its slot forever.
- For a callback that is called many times and later removed, use `scoped_callback`.
  The slot stays reserved as long as the holder object lives:

```cpp
// C style API: calls handler for every log line, nullptr removes the handler
void set_log_handler(void (*handler)(const char* line));

void runWithLogFile(std::ofstream& logFile){
    auto handler = scoped_callback<1>([&logFile](const char* line){
        logFile << line << '\n';
    });
    set_log_handler(handler.Callback());
    runEverything();
    set_log_handler(nullptr);
} // the destructor of handler frees the slot
```

- A lambda without captures converts to a function pointer by itself, pass such a
  lambda to the API directly. `callback_from_lambda` and `scoped_callback` reject a
  lambda without captures at compile time.

### 9. Error codes without exceptions: coro_expected

`coro_expected.h` uses the same `co_await` keyword for a different job: "give me the
value, or return the error to my caller right now".

```cpp
#include "coro_expected.h"

coro_expected<int, std::errc> parseDigit(char text){
    if( text < '0' || text > '9' ){
        co_return coro_unexpected{std::errc::invalid_argument};
    }
    co_return text - '0';
}

coro_expected<int, std::errc> parseTwoDigits(const char* text){
    int tens = co_await parseDigit(text[0]); // on error: parseTwoDigits returns that error here
    int ones = co_await parseDigit(text[1]);
    co_return tens * 10 + ones;
}

void useParsed(){
    auto parsed = parseTwoDigits("4x");
    if( !parsed ){
        std::printf("error %d\n", int(parsed.error())); // std::errc::invalid_argument
    }
    int number = parseTwoDigits("42").value(); // 42
    std::printf("%d\n", number);
}
```

- Works without exceptions, also with `-fno-exceptions`.
- `co_await` accepts `coro_expected`, `std::expected` (C++23) and
  `coro_unexpected{error}`. A function returning `coro_expected<void, Error>` returns
  an error with `co_await coro_unexpected{error};`.
- A `coro_expected` function runs to its end inside the call. `co_await` on
  `invert_function` inside such a function does not compile. Call `coro_expected`
  functions from a `corofork` or `corosync` block like any other function.
- Each call allocates memory on the heap: measured with GCC 16.2 and MSVC 2019,
  1000 calls cost 1000 allocations. Clang with `-O2` removed all of them.

## Where exceptions go

| Exception thrown | Who receives the exception |
| --- | --- |
| in a `corofork` block, before the first `co_await` that waits | the code that executed the `corofork` statement |
| in a `corofork` block, after a `co_await` | the code that called the callback, for example a thread of the network library. Catch inside the block if that code cannot handle exceptions |
| in a `corosync` block, anywhere | the caller of `corosync` |
| in a `coro_expected` function | rethrown by the first `has_value()`, `value()` or `error()` |
| `co_await invert_callback<N>` while all `N` callbacks wait | `std::runtime_error` at that `co_await`, catchable inside the block |

Before the exception travels further, the locals and the captured variables of the
block are destroyed. For a `corofork` block, that holds when the block continues
through the callbacks of `invert_function`, `invert_callback` or
`invert_subscription`. When an awaitable of another library continues a `corofork`
block, the exception leaves without destroying the block (details in
[maintenance.md](maintenance.md#exceptions)).

## Pitfalls

### corofork(&) refers to variables of a function that already returned

```cpp
void onRefreshClicked(){
    std::string url = currentUrl();
    corofork(&){ // WRONG
        auto [status, body] = co_await invert_function(downloading(url));
        logLine(url); // onRefreshClicked has returned long ago, url is destroyed
    };
}
```

The corofork statement ends at the first `co_await` that waits.
`onRefreshClicked` returns and destroys `url`, while the block continues later.

Use `corofork(=)` or name the copies: `corofork(url)`. Move big objects in:
`corofork(url = std::move(url))`. Use `corofork(&)` only for variables that outlive
the block, and in `corosync`.

The same holds for `this` in a member function: the object must outlive the block.
Capture a `std::shared_ptr` to the object, for example
`corofork(self = shared_from_this())`.

### Holding a mutex across co_await

```cpp
std::lock_guard lock(cacheMutex); // WRONG
auto [status, body] = co_await invert_function(downloading(url));
cache[url] = body;
```

- The code after `co_await` may run in another thread, the thread that called the
  callback. A `std::mutex` must be unlocked by the thread that locked the mutex,
  an unlock from another thread is undefined behavior.
- While the block waits, the mutex stays locked for an unknown time, and every
  other thread that needs `cacheMutex` waits as well. When the thread that is
  supposed to call the callback needs `cacheMutex` first, nothing moves any more
  (deadlock).

Lock only around code without `co_await`:

```cpp
auto [status, body] = co_await invert_function(downloading(url));
{
    std::lock_guard lock(cacheMutex);
    cache[url] = body;
}
```

The same applies to everything bound to a thread: `thread_local` variables, thread
ids, objects that only the UI thread may touch.

### Code after co_await runs in the thread of the callback

After `co_await`, the block runs in the thread that called the callback: a network
thread, a timer thread, a thread of an SDK. Before touching the UI from there, go
to the UI thread with [recipe 5](#5-switch-threads-post-to-the-ui-thread-wait-for-a-timer).
In `corosync`, `co_await back_to_thread;` returns to the thread that called
`corosync`.

### The generated callback must be called exactly once

- A second call of the callback from `invert_function` or `invert_callback` is
  undefined behavior: the block has already moved past that `co_await`, or has
  ended. For callbacks that repeat, use [`invert_subscription`](#4-events-that-repeat).
- A callback that is never called means the block never continues: a `corofork`
  keeps the captured copies forever, a `corosync` blocks the caller forever. Make sure
  the API calls the callback on errors and on cancellation too.

### Returning invert_function from a helper

```cpp
auto downloadAsync(std::string url){ // WRONG
    return invert_function([url](std::function<void(int, std::string)> onDone){
        download(url, onDone);
    });
}
```

`invert_function` keeps a reference to the setup lambda, and the setup lambda is
destroyed when `downloadAsync` returns. The later `co_await downloadAsync(url)`
uses the destroyed lambda: the test program crashed with GCC and silently
"worked" with Clang `-O2`, both are undefined behavior.

Return the setup lambda instead ([recipe 6](#6-one-helper-per-api-call)). For the same
reason `auto pending = invert_function(...); co_await pending;` is rejected at compile
time.

### Calling the callback inside the setup lambda

```cpp
co_await invert_function([&, key](std::function<void(std::string)> onDone){ // WRONG
    if( auto cached = cache.find(key); cached != cache.end() ){
        onDone(cached->second); // the block continues right here, inside the setup lambda
    }
    else{
        loadText(key, onDone);
    }
    logLine(key); // the block may have moved on: the setup lambda and key are destroyed
});
```

Calling `onDone` continues the block immediately, inside the running setup lambda.
When the block moves past the `co_await`, the setup lambda is destroyed while the
setup lambda still runs, and the code after the call reads destroyed captures.

Return the ready value instead ([recipe 2](#2-the-result-may-be-ready-right-away)), and
make the API call the last statement of the setup lambda: some APIs call the callback
before returning.

### corosync in the thread that delivers the callback

```cpp
// in the UI thread
corosync(&){ // WRONG: deadlock
    co_await invert_function([](std::function<void()> resume){ postToUiThread(resume); });
};
```

`corosync` blocks the UI thread until the block ends, and the block waits for the UI
thread to run `resume`. Neither thread moves.

In the UI thread, use `corofork`. Use `corosync` only in threads that may block and
that do not deliver the awaited callbacks.

### A block without co_await

```cpp
corofork(&){ // WRONG: GCC and Clang only warn, the program crashes at run time
    counter = 1;
};
```

Only a block that contains `co_await` or `co_return` becomes a coroutine. Without
either keyword, the block is an ordinary function that has to return a value and
returns nothing.

End such a block with `co_return;`. Enable `-Werror=return-type` (GCC, Clang) to turn
the warning into an error. MSVC 2019 rejects such a block with error C4716.

### invert_subscription: no calls after the cleanup

After the cleanup function returned by the setup lambda has finished, the API must
not call the callback any more: the destructor of the subscription object has run,
and a late call reaches a destroyed object.

## API at a glance

| Write | To |
| --- | --- |
| `corofork(captures){ ... };` | start a block, the caller continues at the first `co_await` that waits |
| `corosync(captures){ ... };` | run a block, the caller waits until the block ends, exceptions reach the caller |
| `co_await back_to_thread;` | inside `corosync`: continue in the thread that called `corosync` |
| `co_await invert_function(setup)` | wait for a `std::function` callback, receive its arguments |
| `co_await invert_function(setup, value)` | same, the callback returns `value` to the API |
| `co_await invert_function(setup, function)` | same, the callback returns `function()`, called once the block reached its next `co_await` or its end |
| `co_await invert_callback<N>(setup)` | same as `invert_function` for a C function pointer, up to `N` waiting at once |
| `auto events = invert_subscription(setup);` then `co_await events` | receive repeated events one by one, `events.count()` queued |
| `callback_from_lambda<N>(lambda)` | C function pointer from a capturing lambda, freed after the first call |
| `scoped_callback<N>(lambda)` then `.Callback()` | C function pointer valid while the holder lives |
| `coro_expected<Value, Error>`, `co_await`, `coro_unexpected{error}` | error codes without exceptions (`coro_expected.h`) |

## Building the tests

```sh
cmake -S . -B build
cmake --build build
ctest --test-dir build --output-on-failure
```

`Demo/` contains small programs with more examples.

## More

[maintenance.md](maintenance.md): how corofork works inside, design decisions,
known limitations and plans, written for maintainers.
