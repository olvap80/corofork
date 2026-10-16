/** @file coro_expected.h
    @brief Return error codes without exceptions: co_await on a result
           either gives the value or returns the error to the caller
           right away (like the ? operator in Rust).

 @code
    coro_expected<int, std::errc> parseDigit(char text){
        if( text < '0' || text > '9' ){
            co_return coro_unexpected{std::errc::invalid_argument};
        }
        co_return text - '0';
    }

    coro_expected<int, std::errc> parseTwoDigits(const char* text){
        int tens = co_await parseDigit(text[0]); //error: parseTwoDigits returns it here
        int ones = co_await parseDigit(text[1]);
        co_return tens * 10 + ones;
    }

    auto parsed = parseTwoDigits("4x");
    if( !parsed ){
        report(parsed.error()); //std::errc::invalid_argument
    }
    int number = parseTwoDigits("42").value();

    std::expected<int, std::errc> converted = parseTwoDigits("42"); //C++23 only
 @endcode

Operands of co_await inside a coro_expected body:
    co_await coro_expected<OtherValue, OtherError>  rvalue, OtherError converts to Error
    co_await std::expected<OtherValue, OtherError>  C++23 (when <expected> is available)
    co_await coro_unexpected{error}                 returns the error, for any Value
Any other operand (invert_function, invert_subscription, ...) is a compile error:
the body runs synchronously, and a real suspension would give the caller an
unfinished result. Use corofork / corosync for asynchronous code instead,
a coro_expected function can be called from there as any other function.

Ways to finish the body:
    co_return value;                   Value is not void
    co_return coro_unexpected{error};  Value is not void
    co_await coro_unexpected{error};   any Value (in a coroutine with void Value
                                       co_return of an error is impossible:
                                       return_void and return_value cannot coexist)
    reaching the end of the body       Value is void

Inside:
  - initial_suspend is suspend_never, the body runs inside the call,
  - co_await of an operand holding an error: await_suspend stores the error
    into the promise and the coroutine stays suspended forever,
    the call returns coro_expected right away,
  - final_suspend is suspend_always, the outcome stays in the promise
    (no address of the returned object is stored anywhere, so the outcome does
     not depend on when the compiler converts get_return_object),
  - ~coro_expected destroys the frame: destructors of the body locals that were
    alive at the failed co_await run at that moment, in reverse order.

Exception escaping the body is stored and rethrown by the first has_value(),
value(), error() or conversion to std::expected. Builds with -fno-exceptions
work as well (nothing is ever stored then).

Cost: every call allocates the coroutine frame on the heap.
Measured for 1000 calls of a one line function with -O2:
g++ 16.2 and MSVC 2019 allocate 1000 times,
clang++ allocates 0 times (heap allocation elision).


Copyright (c) 2023-2026, Pavlo M, https://github.com/olvap80
All rights reserved.
Distributed under the same BSD 3-Clause license as corofork.h (see LICENSE).
*/


#ifndef CORO_EXPECTED_H_INCLUDED
#define CORO_EXPECTED_H_INCLUDED

//based on standard C++20 coroutines
#include <coroutine>
//interfaces are declared with concepts
#include <concepts>
//exception escaping the body is kept as std::exception_ptr
#include <exception>
//std::move, std::forward, std::exchange
#include <utility>
//std::remove_cvref_t and friends
#include <type_traits>
//the outcome kept in the promise
#include <variant>
//for std::size_t
#include <cstddef>
//__cpp_lib_expected tells whether std::expected interop is available
#include <version>
#if defined(__cpp_lib_expected)
#   include <expected>
#endif


/**Error to return from a coro_expected coroutine (std::unexpected for C++20).
    co_return coro_unexpected{std::errc::invalid_argument};
    co_await coro_unexpected{std::errc::invalid_argument}; */
template<class Error>
struct coro_unexpected{
    /// The error being returned
    Error error;
};

template<class Error>
coro_unexpected(Error) -> coro_unexpected<Error>;


/// Result of a coroutine: either Value or Error, see the samples above
template<class Value, class Error>
class [[nodiscard("coro_expected carries an error, that error is lost once discarded")]]
coro_expected;


namespace CoroExpected_detail{

    /// Index of the alternatives inside OutcomeStorage
    enum OutcomeIndex : std::size_t{
        NotFinished = 0,
        HasValue = 1,
        HasError = 2,
        HasException = 3
    };

    /// Outcome kept in the promise (void Value is kept as std::monostate)
    template<class Value, class Error>
    using OutcomeStorage = std::variant<
        std::monostate,
        std::conditional_t<std::is_void_v<Value>, std::monostate, Value>,
        Error,
        std::exception_ptr
    >;

    /**Awaiter for an operand holding either a value or an error.
    Operand is coro_expected or std::expected, both have
    has_value(), value() && and error() && */
    template<class Operand, class PromiseType>
    class UnwrapOrReturnError{
    public:
        /// What co_await gives (by value, the operand dies with this awaiter)
        using AwaitedValue = std::remove_cvref_t<decltype(std::declval<Operand&&>().value())>;

        explicit UnwrapOrReturnError(Operand&& operandToAwait)
            : operand(std::move(operandToAwait)) {}

        /// Value present: no suspension at all
        bool await_ready() const { return operand.has_value(); }

        /**Error present: store the error into the promise of the awaiting coroutine.
        The coroutine is never resumed, ~coro_expected destroys the frame */
        void await_suspend(std::coroutine_handle<PromiseType> handle){
            handle.promise().outcome.template emplace<HasError>(std::move(operand).error());
        }

        AwaitedValue await_resume(){
            if constexpr( std::is_void_v<AwaitedValue> ){
                return;
            }
            else{
                return std::move(operand).value();
            }
        }

    private:
        /// Moved in, destroyed together with this awaiter
        Operand operand;
    };

    /// Awaiter for co_await coro_unexpected{error}: always returns the error
    template<class OtherError, class PromiseType>
    class ReturnError{
    public:
        explicit ReturnError(coro_unexpected<OtherError>&& errorToReturn)
            : returnedError(std::move(errorToReturn)) {}

        bool await_ready() const noexcept { return false; }

        /// Store the error, the coroutine is never resumed
        void await_suspend(std::coroutine_handle<PromiseType> handle){
            handle.promise().outcome.template emplace<HasError>(std::move(returnedError.error));
        }

        /// Never called (the coroutine is never resumed after await_suspend)
        void await_resume() const noexcept {}

    private:
        coro_unexpected<OtherError> returnedError;
    };

    /// return_value overloads of the promise (Value is not void)
    template<class Value, class Error, class PromiseType>
    struct ReturnValueOrError{
        /// co_return value;
        template<class ReturnedValue>
        requires std::convertible_to<ReturnedValue, Value>
        void return_value(ReturnedValue&& returnedValue){
            static_cast<PromiseType*>(this)->outcome.template emplace<HasValue>(
                std::forward<ReturnedValue>(returnedValue)
            );
        }

        /// co_return coro_unexpected{error};
        template<class ReturnedError>
        requires std::convertible_to<ReturnedError, Error>
        void return_value(coro_unexpected<ReturnedError> returnedError){
            static_cast<PromiseType*>(this)->outcome.template emplace<HasError>(
                std::move(returnedError.error)
            );
        }
    };

    /// return_void of the promise (Value is void, errors go through co_await)
    template<class Error, class PromiseType>
    struct ReturnValueOrError<void, Error, PromiseType>{
        /// co_return; or reaching the end of the body
        void return_void(){
            static_cast<PromiseType*>(this)->outcome.template emplace<HasValue>();
        }
    };

} //namespace CoroExpected_detail


template<class Value, class Error>
class coro_expected{
public:
    /// Define coroutine behavior (used by compiler to generate coroutine frame)
    struct promise_type
        : CoroExpected_detail::ReturnValueOrError<Value, Error, promise_type>
    {
        /// Value, error or exception, read by coro_expected
        CoroExpected_detail::OutcomeStorage<Value, Error> outcome;

        coro_expected get_return_object(){
            return coro_expected(std::coroutine_handle<promise_type>::from_promise(*this));
        }

        /// The body runs inside the call
        std::suspend_never initial_suspend() const noexcept { return {}; }

        /// Keep the outcome until ~coro_expected destroys the frame
        std::suspend_always final_suspend() const noexcept { return {}; }

        /// Rethrown by the first access to the outcome
        void unhandled_exception() noexcept {
            outcome.template emplace<CoroExpected_detail::HasException>(
                std::current_exception()
            );
        }

        /// co_await coro_expected<OtherValue, OtherError>
        template<class OtherValue, class OtherError>
        requires std::convertible_to<OtherError, Error>
        auto await_transform(coro_expected<OtherValue, OtherError>&& operand){
            return CoroExpected_detail::UnwrapOrReturnError<
                coro_expected<OtherValue, OtherError>, promise_type
            >(std::move(operand));
        }

#   if defined(__cpp_lib_expected)
        /// co_await std::expected<OtherValue, OtherError> (C++23)
        template<class OtherValue, class OtherError>
        requires std::convertible_to<OtherError, Error>
        auto await_transform(std::expected<OtherValue, OtherError> operand){
            return CoroExpected_detail::UnwrapOrReturnError<
                std::expected<OtherValue, OtherError>, promise_type
            >(std::move(operand));
        }
#   endif

        /// co_await coro_unexpected{error}: return the error right here
        template<class OtherError>
        requires std::convertible_to<OtherError, Error>
        auto await_transform(coro_unexpected<OtherError> operand){
            return CoroExpected_detail::ReturnError<OtherError, promise_type>(
                std::move(operand)
            );
        }

        /// Any other operand is rejected at compile time
        template<class Other>
        std::suspend_never await_transform(Other&&){
            static_assert(
                !std::is_same_v<Other, Other>,
                "coro_expected body runs synchronously and accepts co_await only on\n"
                "  coro_expected<OtherValue, OtherError> as rvalue,\n"
                "  std::expected<OtherValue, OtherError> (C++23),\n"
                "  coro_unexpected<OtherError>,\n"
                "where OtherError converts to Error.\n"
                "A real suspension (invert_function, invert_subscription, ...)\n"
                "would give the caller an unfinished result:\n"
                "use corofork / corosync for asynchronous code instead."
            );
            return {};
        }
    };

    /// Moved from coro_expected owns nothing, only destruction and assignment are allowed
    coro_expected(coro_expected&& other) noexcept
        : handle(std::exchange(other.handle, nullptr)) {}

    coro_expected& operator=(coro_expected&& other) noexcept{
        if( this != &other ){
            DestroyFrame();
            handle = std::exchange(other.handle, nullptr);
        }
        return *this;
    }

    //one owner of the frame
    coro_expected(const coro_expected&) = delete;
    coro_expected& operator=(const coro_expected&) = delete;

    /// Destroys the frame (destructors of the body locals still alive run here)
    ~coro_expected(){
        DestroyFrame();
    }

    /// True once the body finished with a value
    bool has_value() const{
        RethrowIfException();
        return Outcome().index() == CoroExpected_detail::HasValue;
    }

    /// Same as has_value()
    explicit operator bool() const{
        return has_value();
    }

    /**Value moved out (precondition: has_value()).
    Returned by value, not as Value&&: the outcome lives in the frame owned
    by *this, so for a temporary coro_expected a reference would dangle
    right after the full expression, as in auto&& number = parseDigit('7').value(); */
    Value value() &&{
        RethrowIfException();
        if constexpr( !std::is_void_v<Value> ){
            return std::move(std::get<CoroExpected_detail::HasValue>(Outcome()));
        }
    }

    /// Value (precondition: has_value())
    decltype(auto) value() const &{
        RethrowIfException();
        if constexpr( !std::is_void_v<Value> ){
            return static_cast<const Value&>(std::get<CoroExpected_detail::HasValue>(Outcome()));
        }
    }

    /// Error moved out (precondition: !has_value()), by value for the reason given at value() &&
    Error error() &&{
        RethrowIfException();
        return std::move(std::get<CoroExpected_detail::HasError>(Outcome()));
    }

    /// Error (precondition: !has_value())
    const Error& error() const &{
        RethrowIfException();
        return std::get<CoroExpected_detail::HasError>(Outcome());
    }

#   if defined(__cpp_lib_expected)
    /// Move the outcome into std::expected (C++23)
    operator std::expected<Value, Error>() &&{
        if( has_value() ){
            if constexpr( std::is_void_v<Value> ){
                return {};
            }
            else{
                return std::move(*this).value();
            }
        }
        return std::unexpected(std::move(*this).error());
    }
#   endif

private:
    /// Created by promise_type::get_return_object only
    explicit coro_expected(std::coroutine_handle<promise_type> createdFrom)
        : handle(createdFrom) {}

    /// Outcome kept in the promise
    CoroExpected_detail::OutcomeStorage<Value, Error>& Outcome() const{
        return handle.promise().outcome;
    }

    /// Exception escaped from the body is rethrown on every access
    void RethrowIfException() const{
        if( Outcome().index() == CoroExpected_detail::HasException ){
            std::rethrow_exception(std::get<CoroExpected_detail::HasException>(Outcome()));
        }
    }

    void DestroyFrame(){
        if( handle ){
            handle.destroy();
        }
    }

    /// Frame owned by this coro_expected (nullptr once moved from)
    std::coroutine_handle<promise_type> handle;
};

#endif
