/** @file corofork.h
    @brief Use coroutines inline, integrate existing asynchronous API easy.
           Simple way to turn callback oriented API into awaitable API.

Each "callback based" API (including those, that require "C style" callbacks)
can be used inside C++20 coroutines with that simple trick.
Easy solution to avoid "callback hell" making code linear and easy to read.
Sample usage (see also more concrete samples below):

 @code
    // Some code
    corofork(=){ //here = means "new coroutine captures by value"
        // Some code that still executes in the same context

        await AnyAwaitableThing; //some await operation

        // Some code that is executed in the context of resumer
        // optionally more awaits can happen here,
        // one can use captured variables, etc here
    };

    //Code that executes independently once we started awaiting
 @endcode

There is a way to make any "asynchronous" API to operate in
synchronous style with the help of introduced helpers.

There are invert_function and invert_callback API to generate
either std::function or "C style" callbacks respectively, and turn
any asynchronous API into awaitable.
NOTE: all the "code generation" happens at compile time.

Those "awaitables" are suitable for waiting from any coroutine.

For example one can turn asynchronous communication to linear in this way:

 @code
    // inside some coroutine, imagine we need a sequence of async calls

    // First we need to do AsyncRequestCustomerNames
    auto customerNames = co_await invert_function(
        [](
            std::function<
                void(std::vector<std::string>) //expected signature to receive customers
            > onReady
        ){
            //Pass generated callback to your async API
            AsyncRequestCustomerNames(onReady);
        }
    );
    //coroutine executes here once onReady is called
    //the customerNames variable is what was received by onReady callback,
    //that is how onReady function was "inverted" (turns into awaitable)

    ...
    auto [age, address] = co_await invert_function(
        [&](
            std::function<
                void(unsigned age, std::string address)
            > onReady
        ){
            //Pass generated callback to your async API
            AsyncRequestAgeAndAddressFor(customerNames, onReady);
        }
    );
    //here age is unsigned and address is std::string
    //(for multiple callback parameters std::tuple is returned)

    ... //etc
 @endcode
The main rule of such "inversion" is to repeat the signature,
as it is expected by the asynchronous API and then those arguments will be
received as a result of the await operation.

The "inverting" lambda can capture both by value[=] and by reference[&]
inverting lambda always executes together with co_await expression.

NOTE: remember invert_function and invert_callback will work with
      any coroutine that is able to do co_await operation,
      you can use them without corofork macro.

      also please remember to await result of invert_* immediately
      while the "full expression" being inverted still exists
      (do not store result of invert_* for later awaiting,
       because that may lead to dangling references inside).

You can pass your generated callback to any API, assuming signature fits.
For example here is artificial sample doing the same thing as
    co_await winrt::resume_background();
but fully C++ standard compliant

 @code
    corofork(){
        co_await invert_function([](std::function<void()> resumer){
            // Note: resumer will continue execution of the coroutine in thread
            std::thread(resumer).detach();
        });
        //from now coroutine executes in own thread

        ... //doing some in parallel with main thread
    };

    ... //continue execution independently of corofork content above))
 @endcode

RETURN sample (what if operation completes synchronously while setup?)

Windows API sample

 @code
 @endcode

All the design was made per
https://en.cppreference.com/cpp/language/coroutines


Copyright (c) 2023-2026, Pavlo M, https://github.com/olvap80
All rights reserved.


Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

Neither the name of corofork nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#ifndef COROFORK_H_INCLUDED
#define COROFORK_H_INCLUDED

//based on standard C++20 coroutines
#include <coroutine>
//can use std::function as callbacks
#include <functional>
//interfaces are declared with concepts
#include <concepts>
//for case when callback accepts multiple arguments
#include <tuple>
//allow immediate return and handle short path when result is known
#include <optional>
//internally tracks lifetime automatically
#include <memory>
//std::move, std::forward and friends))
#include <utility>
//std::decay_t to turn what callback receives into an owned value
#include <type_traits>
//allow allocation for pregenerated trampolines from different threads
#include <mutex>
//for Sync to be used in corosync
#include <condition_variable>
//for rethrowing exceptions from "other threads" in corosync
#include <exception>
//runtime_error throws when trampoline allocation fails
#include <stdexcept>
//placement new is used for placing lambda to trampoline
#include <new>
//for std::size_t
#include <cstddef>
//Used by invert_subscription to accumulate events (calls) happened so far
#include <queue>



//______________________________________________________________________________
// The macro to start coroutine inline (see samples above)


/**Start a coroutine "inline" (body follows in {}).
Macro creates a new coroutine optionally capturing parameters
The way how variables are captured is determined by the macro parameters
one can provide =, &, or named captures as well.
"Forked" coroutine starts execution immediately in current thread,
but can be suspended and resumed in other thread later.
The "fork" suffix means coroutine lifetime and execution
is independent from the creating thread (caller thread is not blocked).
Exception escaping the body propagates to the code that resumed the coroutine:
before the first suspension that is the corofork statement itself,
afterwards that is the call of the generated callback (invert_function,
invert_callback, invert_subscription). In both cases the coroutine frame
and the captures are freed before the exception propagates.
Be aware: mutexes shall not be crossed by co_await, because
one can resume coroutine from different thread, but mutex
is still owned by previous thread! */
#define corofork(...) \
    CoroFork_detail::tagTieWithOverloadedOperator ->* \
    /* Note: lambda is not called below, operator->* does the stuff */ \
    [__VA_ARGS__]()-> \
        CoroFork_detail::CoroFork /* Lambda body follows here in {} */

/**Run a coroutine "inline here" (synchronously), blocks until coroutine completes.
The same as above, but now caller thread will not continue independently,
and will block until coroutine completes
(while coroutine's code is executing probably in other threads).
This means "our" coroutine can execute in "their" threads,
but finally control returns to this one when coroutine is completed.
The "sync" suffix means coroutine lifetime and execution
is tied to the caller thread (initial caller thread is blocked).

One can do
     co_await back_to_thread;
to force this coroutine execution flow to continue in the initial thread
that called the corosync marco.
This is especially useful for the case if long computation
is needed "here" but "their" callback must return immediately,
in this way we can reuse existing thread for running the computation
in the coroutine without blocking other threads/message queues, etc,
see samples above.

Every exception escaping the body is rethrown by corosync in the caller thread
(after the coroutine frame is destroyed), no matter which code resumed the
coroutine: the caller thread, a generated callback (invert_function,
invert_callback, invert_subscription) in another thread, or an awaitable of
another library calling plain handle.resume(). In the thread that resumed the
coroutine, the generated callback or resume() returns normally.

Inside corosync await invert_* directly:
 @code
    corosync(&){
        int answer = co_await invert_function([&](std::function<void(int)> cb){
            AsyncRequest(cb);
        });
        Use(answer); //exception thrown here reaches the corosync caller
    };
 @endcode
Do not start a nested corofork inside the corosync body for the same purpose:
the nested corofork is an independent coroutine, corosync does not wait for
the nested corofork, and an exception thrown by the nested corofork after its
first suspension propagates to the code that resumed the nested corofork
(for example a callback in another thread), never to the corosync caller.

Be aware: mutexes shall not be crossed by co_await, because
one can resume our coroutine from different thread, but mutex
is sill owned by previous thread! */
#define corosync(...) \
    CoroFork_detail::tagTieWithSyncOverloadedOperator ->* \
    /* Note: lambda is not called below, operator->* does the stuff */ \
    [__VA_ARGS__]( \
        [[maybe_unused]] \
            CoroFork_detail::CoroSync::promise_type:: \
                SharedState::BackToThreadAwaitable \
                    back_to_thread \
    )-> \
        CoroFork_detail::CoroSync /* Lambda body follows here in {} */

//______________________________________________________________________________
// Some "must have" internal stuff before declaring API (just skip that!!!))

namespace CoroFork_detail{

    /**Meta function to extract types and code from method pointer.
    This is "pre declaration", implementation is in details below.
    Template parameter is a type/signature of something like
    member *::operator(SomeCallbackFunction cb) of the "setup lambda"
    that uses f to setup asynchronous operation with callback.
    GenerateForSetupMethod extracts expected types from cb
    and generates code for "inverting" callback so that
    it will resume coroutine once issued */
    template<class MethodSignature>
    struct GenerateForSetupMethod;


    //__________________________________________________________________________
    // The GenerateForSetupLambda meta function LambdaType -> related data

    /// General meta function to extract signatures for generated API
    template<class LambdaType>
    using GenerateForSetupLambda =
        GenerateForSetupMethod<decltype(&LambdaType::operator())>;


    //__________________________________________________________________________
    // Forward declare awaitables and holders

    template<class Decorator, class CoResult, class SetupLambdaType>
    class AwaitableGeneratedCBNoResult;

    template<class Decorator, class CoResult,
        class SetupLambdaType, class GeneratedCallbackResultType>
    class AwaitableGeneratedCBReturnsValue;

    template<class Decorator, class CoResult,
        class SetupLambdaType, class CallbackToCallFromGeneratedCallbackType>
    class AwaitableGeneratedCBReturnsResultOfCall;

    /// Internal class to hold subscription as long as instance exists
    template<class Decorator, class AwaitedType>
    class InvertSubscriptionHolder;

    //__________________________________________________________________________
    // Helpers for callback_from_lambda

    /// Meta function to extract both signatures from LambdaType::operator()
    template<class MethodSignature>
    struct ConvertSignature;

    /**Signature the target API receives (what the generated trampoline is).
    This is the "plain function pointer" signature, callback_extend_lifetime&
    is never a part of it, that parameter never reaches the target API */
    template<class LambdaType>
    using PlainFunctionSignatureFromLambda = typename
        ConvertSignature<decltype(&LambdaType::operator())>::PlainFunctionSignature;

    /**Signature as the lambda declares it (callback_extend_lifetime& included).
    Target API always receives PlainFunctionSignatureFromLambda, this one only
    tells apart "asks for lifetime control" from "does not" */
    template<class LambdaType>
    using LifetimeAwareSignatureFromLambda = typename
        ConvertSignature<decltype(&LambdaType::operator())>::LifetimeAwareSignature;

    /* The two concepts below detect a lambda that has NO captures.
       Such lambda already is a "plain C" callback by the standard conversion,
       so it has no business entering the trampoline API at all
       (see the static_assert in callback_from_lambda and scoped_callback) */

    /// Lambda has no captures and converts to the callback the target API wants
    template<class LambdaType>
    concept ConvertsToPlainFunctionCallback = std::convertible_to<
        LambdaType, PlainFunctionSignatureFromLambda<LambdaType>*
    >;

    /**Lambda has no captures, but asks for callback_extend_lifetime as well.
    Such lambda does not convert to the plain function signature (it has that extra
    parameter), but converts to the lifetime aware one, and that combination
    means the lifetime control was requested where nothing needs it */
    template<class LambdaType>
    concept ConvertsToLifetimeAwareCallback = std::convertible_to<
        LambdaType, LifetimeAwareSignatureFromLambda<LambdaType>*
    >;

    //__________________________________________________________________________
    // Utility stuff

    /// Default customization for generated callback (no custom wrapping)
    struct TheSame;

    /// Customization that turns std::function to "plain C" callback
    template<
        std::size_t maxCallbacksCount,
        class AdditionalOptionalTag
    >
    class ToPlainC;

    /// Prevent co_await from "saved value" (forces await on full expression only!)
    template<class RealAwaitable>
    class AwaitAsTemporaryOnly{
    public:
        //non copyable (takes advantage of RVO)
        AwaitAsTemporaryOnly(const AwaitAsTemporaryOnly&) = delete;
        AwaitAsTemporaryOnly& operator=(const AwaitAsTemporaryOnly&) = delete;

        /// Moves awaitable only from the temporary
        AwaitAsTemporaryOnly(RealAwaitable&& realAwaitable)
            : awaitable(std::move(realAwaitable)){}

        /**It is possible to issue co_await only on "full expression" in coroutine.
        Thus "saving/moving to some other location and awaiting later" is explicitly banned,
        to prevent referencing those components of the "full expression" that were destroyed.
        NOTE: the awaiter is handed over BY VALUE (moved into the coroutine
              frame) and not by reference on purpose: returning a reference
              here makes GCC copy the awaiter into the frame, which fails
              for the non copyable awaitables below */
        auto operator co_await() && -> RealAwaitable {
            return std::move(awaitable);
        }

        /// Intentionally prevent co_await on stored values
        std::suspend_always operator co_await() & = delete;

    private:
        /// The item being really awaited
        RealAwaitable awaitable;
    };

} //namespace CoroFork_detail


//______________________________________________________________________________
//Utility API useful on it's own (but one can also skip that)

/**Turn lambda to function pointer by storing lambda in a block pool.
Create "single shot" trampoline by default
(trampoline deallocates automatically after first call,
 thus freeing one item in reservedCount,
 NOTE: never issued callback will never free!
    use scoped_callback to be able to release without call)
For usage sample see explanations at beginning of the file.
NOTE: AdditionalOptionalTag can be used for "nonunique" types,
        when "something else" than lambda is passed to callback_from_lambda
        in several different places.
In your usual case you pass only reservedCount to callback_from_lambda
NOTE: a lambda WITHOUT captures is rejected here on purpose, it already is a
        "plain C" callback by the standard conversion, so pass such lambda to
        the target API directly instead (no trampoline, no block pool slot,
        no reservedCount to guess). The same holds for scoped_callback.
See also https://en.cppreference.com/w/cpp/language/function_template
            section Template argument deduction */
template<
    unsigned reservedCount, ///< how many callbacks coexist simultaneously for
                            ///< that (AdditionalOptionalTag, UniqueLambdaType)
                            ///< unique combination
    class AdditionalOptionalTag = void, ///< tweak for case UniqueLambdaType
                                        ///< is not unique
    class UniqueLambdaType ///< Type for actual callable to be wrapped
>
[[nodiscard(
    "One shall use and call the result of callback_from_lambda,"
    " or else the reserved block pool slot is lost forever"
)]]
auto callback_from_lambda(UniqueLambdaType&& lambda) ->
    CoroFork_detail::PlainFunctionSignatureFromLambda<
        UniqueLambdaType
    >*;

/**Extra parameter to lambda change default allocation behavior in callback_from_lambda
By default trampolines are "single shot", they "deallocate" as soon
as are called, so it is possible to subscribe only to those API that call lambda
For usage sample see explanations at beginning of the file.
See callback_from_lambda */
class callback_extend_lifetime{
public:
    /// No way to copy (is passed by reference to lambda)
    callback_extend_lifetime(const callback_extend_lifetime&) = delete;
    /// No way to copy (is passed by reference to lambda)
    callback_extend_lifetime& operator=(const callback_extend_lifetime&) = delete;

    /**Cause trampoline API to be freed
    Call from inside of lambda to cause "this lambda" to untie from callback,
    (this means resource limited by reservedCount can be reused again,
     and this also means calling lambda must make sure that the old caller
     will not issue the same trampoline any more) */
    void dispose();

    ///Test corresponding trampoline is disposed (freed)
    bool is_disposed() const;

protected:
    /// Created by corresponding apply
    callback_extend_lifetime(){}

private:
    /// True if lambda wands to dispose (free trampoline)
    bool isDisposed = false;
};


/** RAII to hold callback allocation as long as needed.
Use scoped_callback below as convenience API.

DELIBERATELY NARROW SCOPE, please do not "generalize" this back:
a holder is created in the scope that subscribes, hands its Callback() to
"them", and dies in that same scope once the subscription is over.
That is the only supported scenario. Holders are therefore
    - never moved,
    - never reassigned another lambda,
    - never collected (no vector/map/optional of holders).
All four special operations are deleted below on purpose, this is a design
decision and not an omission waiting to be fixed:

1. The single reason the holder exists is to hand out one STABLE "plain C"
    function pointer. Reseating a holder (moving it, or assigning another
    lambda into it) keeps that very pointer valid while silently swapping
    what stands behind it, and whoever already received the pointer cannot
    notice. With reservedCount == 1 the released slot is even handed straight
    back, so the pointer is literally the same one.
2. Such a swap cannot be synchronized against a callback being issued right
    now from another thread, the trampoline has no place to hold a lock.
3. Nothing is actually gained: "one more callback" means one more holder,
    and scoped_callback returns one by value through guaranteed copy elision,
    so no move operation is needed to obtain it. For several callbacks raise
    reservedCount and create several holders in their own scopes.

 Invariant relied upon by the implementation: a live holder always owns a
 slot, so owningBlock is never nullptr (there is no "empty holder" state). */
template<
    unsigned reservedCount, ///< how many callbacks coexist simultaneously for
                            ///< that (AdditionalOptionalTag, UniqueLambdaType)
                            ///< unique combination
    class AdditionalOptionalTag, ///< tweak for case UniqueLambdaType
                                 ///< is not unique
    class UniqueLambdaType ///< Type for actual callable to be wrapped
>
class scoped_callback_holder{
public:
    /// Corresponding "plain C" callback signature
    using Signature = CoroFork_detail::PlainFunctionSignatureFromLambda<UniqueLambdaType>;

    /// Create instance from lambda
    scoped_callback_holder(UniqueLambdaType&& lambda);

    /// Cleanup the stuff once instance goes out of scope
    ~scoped_callback_holder();

    /* Exists only in one place (but takes advantage of RVO!),
       see the "deliberately narrow scope" note above before touching those */
    scoped_callback_holder(scoped_callback_holder&&) = delete;
    scoped_callback_holder& operator=(scoped_callback_holder&&) = delete;
    scoped_callback_holder(const scoped_callback_holder&) = delete;
    scoped_callback_holder& operator=(const scoped_callback_holder&) = delete;


    /// Obtain pointer. Do not use that after scoped_callback_holder destructs.
    Signature* Callback() const;

private:
    /// "Type erased" info for deallocation (never nullptr for a live holder)
    void* owningBlock;
};

/// Generate callback around lambda, that exists as long as scope
template<
    unsigned reservedCount, ///< how many callbacks coexist simultaneously for
                            ///< that (AdditionalOptionalTag, UniqueLambdaType)
                            ///< unique combination
    class AdditionalOptionalTag = void, ///< tweak for case UniqueLambdaType
                                        ///< is not unique
    class UniqueLambdaType ///< Type for actual callable to be wrapped
>
[[nodiscard(
    "The holder returned by scoped_callback owns the callback,"
    " dropping it right away frees the slot and unsubscribes immediately"
)]]
auto scoped_callback(UniqueLambdaType&& lambda) ->
    scoped_callback_holder<
        reservedCount, AdditionalOptionalTag, UniqueLambdaType
    >;


//______________________________________________________________________________
// Describe kinds of lambda used for setup (it is better to look into samples))


/// Define kind of lambda to be used for function setup
template<class SetupLambdaType>
concept CoroForkLambdaToSetupFunction = requires(
    SetupLambdaType setupLambda,
    typename CoroFork_detail::GenerateForSetupLambda<
        SetupLambdaType
    >::FunctionType generatedFunction
){
    //shall have operator(), or else there is no way to extract parameter
    &SetupLambdaType::operator();

    //shall be callable with that function as parameter
    setupLambda(generatedFunction);
};

/// Define kind of lambda to be used for callback setup
template<class SetupLambdaType>
concept CoroForkLambdaToSetupCallback = requires(
    SetupLambdaType setupLambda,
    typename CoroFork_detail::GenerateForSetupLambda<
        SetupLambdaType
    >::RawPointerCallbackType generatedRawPointerCallback
){
    /* Shall have operator(),
      this is a naive test for argument to be anonymous lambda,
      trick with maxCallbacksCount below will not work if we pass
      "something else" than unique lambda type.
      Unique lambda guaranties we have maxCallbacksCount for each
      call site (place of call) is being used individually,
      and will not reuse other (independent) call sites
      (see also https://en.cppreference.com/w/cpp/language/lambda) */
    &SetupLambdaType::operator();

    /* shall be a lambda accepting a raw "plain C" stype pointer!
       use invert_function instead of invert_callback when
       some std::function has to be passed,
       this will avoid the need to guess on maxCallbacksCount */
    setupLambda(generatedRawPointerCallback);
};



//______________________________________________________________________________
// invert_function API to turn std::function based API into awaitable


/**Turn std::function callback based API into coroutine based.
 Provided lambda shall receive desired callback with corresponding signature
as a parameter. Such callback can be passed to asynchronous API
to be called later by "something else" single time.
Once that generated function callback is called it will
resume the coroutine awaiting for the result of invert_function,
and callback arguments will go to caller.
Remember to await result of invert_function immediately.
See samples above for illustration */
template<CoroForkLambdaToSetupFunction LambdaToSetupFunction>
[[nodiscard(
    "The result of invert_function must be co_awaited right away,"
    " dropping it means the setup lambda is never even invoked"
)]]
auto invert_function(
    const LambdaToSetupFunction& setupFunction ///< receives generated callback
){
    using Awaitable = CoroFork_detail::AwaitableGeneratedCBNoResult<
        CoroFork_detail::TheSame,
        typename CoroFork_detail::GenerateForSetupLambda<LambdaToSetupFunction>::CoResultType,
        LambdaToSetupFunction
    >;
    return CoroFork_detail::AwaitAsTemporaryOnly<Awaitable>(
        Awaitable(setupFunction)
    );
}


/**Same as above, but generated function callback has a return value.
In addition to the above logic, here the return value for
the generated function callback can be provided.
Remember to await result of invert_function immediately.
See samples above for illustration. */
template<
    CoroForkLambdaToSetupFunction LambdaToSetupFunction,
    class ResultForCallbackFunctionAsValue
>
requires
    std::convertible_to<
        std::remove_cvref_t<ResultForCallbackFunctionAsValue>,
        std::remove_cvref_t<
            typename CoroFork_detail::GenerateForSetupLambda<LambdaToSetupFunction>::FunctionResultType
        >
    >
[[nodiscard(
    "The result of invert_function must be co_awaited right away,"
    " dropping it means the setup lambda is never even invoked"
)]]
auto invert_function(
    const LambdaToSetupFunction& setupFunction, ///< Your setup
    ResultForCallbackFunctionAsValue&& resultForCallbackFunction
){
    using Awaitable = CoroFork_detail::AwaitableGeneratedCBReturnsValue<
        CoroFork_detail::TheSame,
        typename CoroFork_detail::GenerateForSetupLambda<LambdaToSetupFunction>::CoResultType,
        LambdaToSetupFunction,
        typename CoroFork_detail::GenerateForSetupLambda<LambdaToSetupFunction>::FunctionResultType
    >;

    return CoroFork_detail::AwaitAsTemporaryOnly<Awaitable>(
        Awaitable(
            setupFunction,
            std::forward<ResultForCallbackFunctionAsValue>(resultForCallbackFunction)
        )
    );
}

/**Same as above, but generated callback returns value returned by functionToObtainResult.
Remember here coroutine is resumed directly from the callback, thus
functionToObtainResultForCallback is issued once we await for "something else" */
template<
    CoroForkLambdaToSetupFunction LambdaToSetupFunction, ///< Your setup
    std::invocable FunctionToObtainResult
>
requires
    std::same_as<
        std::remove_cvref_t<
            typename CoroFork_detail::GenerateForSetupLambda<LambdaToSetupFunction>::FunctionResultType
        >,
        std::remove_cvref_t<
            std::invoke_result_t< std::remove_cvref_t<FunctionToObtainResult> >
        >
    >
[[nodiscard(
    "The result of invert_function must be co_awaited right away,"
    " dropping it means the setup lambda is never even invoked"
)]]
auto invert_function(
    const LambdaToSetupFunction& setupFunction,
    FunctionToObtainResult&& functionToObtainResultForCallbackFunction
){
    using Awaitable = CoroFork_detail::AwaitableGeneratedCBReturnsResultOfCall<
        CoroFork_detail::TheSame,
        typename CoroFork_detail::GenerateForSetupLambda<LambdaToSetupFunction>::CoResultType,
        LambdaToSetupFunction,
        FunctionToObtainResult
    >;

    return CoroFork_detail::AwaitAsTemporaryOnly<Awaitable>(
        Awaitable(
            setupFunction, functionToObtainResultForCallbackFunction
        )
    );
}


//______________________________________________________________________________
// invert_callback API for generating "pain C" style callbacks


/**Turn "plain C style" callback based API into coroutine based.
co_await throws std::runtime_error when maxCallbacksCount callbacks
generated at this place are still in use (the exception can be caught
around the co_await expression inside the coroutine) */
template<
    std::size_t maxCallbacksCount, ///< Specify callback count per lambda
    CoroForkLambdaToSetupCallback LambdaToSetupCallback ///< Your setup
>
[[nodiscard(
    "The result of invert_callback must be co_awaited right away,"
    " dropping it means the setup lambda is never even invoked"
)]]
auto invert_callback(
    const LambdaToSetupCallback& setupCallback
){
    using Awaitable = CoroFork_detail::AwaitableGeneratedCBNoResult<
        CoroFork_detail::ToPlainC<
            maxCallbacksCount, LambdaToSetupCallback
        >,
        typename CoroFork_detail::GenerateForSetupLambda<
            LambdaToSetupCallback
        >::CoResultType,
        LambdaToSetupCallback
    >;

    return CoroFork_detail::AwaitAsTemporaryOnly<Awaitable>(
        Awaitable(setupCallback)
    );
}


/**Same as above, but generated callback has a return value.
In addition to the above logic Here the return value for
the generated callback can be provided.
See samples above for illustration.
Remember here coroutine is resumed directly from the callback */
template<
    std::size_t maxCallbacksCount, ///< Specify callback count per lambda
    CoroForkLambdaToSetupCallback LambdaToSetupCallback, ///< Your setup
    class ResultForCallbackFunctionAsValue
>
requires
    std::convertible_to<
        std::remove_cvref_t<ResultForCallbackFunctionAsValue>,
        std::remove_cvref_t<
            typename CoroFork_detail::GenerateForSetupLambda<LambdaToSetupCallback>::FunctionResultType
        >
    >
[[nodiscard(
    "The result of invert_callback must be co_awaited right away,"
    " dropping it means the setup lambda is never even invoked"
)]]
auto invert_callback(
    const LambdaToSetupCallback& setupCallback,
    ResultForCallbackFunctionAsValue&& resultForCallback
){
    using Awaitable = CoroFork_detail::AwaitableGeneratedCBReturnsValue<
        CoroFork_detail::ToPlainC<
            maxCallbacksCount, LambdaToSetupCallback
        >,
        typename CoroFork_detail::GenerateForSetupLambda<LambdaToSetupCallback>::CoResultType,
        LambdaToSetupCallback,
        typename CoroFork_detail::GenerateForSetupLambda<LambdaToSetupCallback>::FunctionResultType
    >;

    return CoroFork_detail::AwaitAsTemporaryOnly<Awaitable>(
        Awaitable(
            setupCallback,
            std::forward<ResultForCallbackFunctionAsValue>(resultForCallback)
        )
    );
}

/**Same as above, but generated callback returns value returned by functionToObtainResultForCallback.
Remember here coroutine is resumed directly from the callback, thus
functionToObtainResultForCallback is issued once we await for "something else" */
template<
    std::size_t maxCallbacksCount, ///< Specify callback count per lambda
    CoroForkLambdaToSetupCallback LambdaToSetupCallback, ///< Your setup
    std::invocable FunctionToObtainResult
>
requires
    std::same_as<
        std::remove_cvref_t<
            typename CoroFork_detail::GenerateForSetupLambda<LambdaToSetupCallback>::FunctionResultType
        >,
        std::remove_cvref_t<
            std::invoke_result_t< std::remove_cvref_t<FunctionToObtainResult> >
        >
    >
[[nodiscard(
    "The result of invert_callback must be co_awaited right away,"
    " dropping it means the setup lambda is never even invoked"
)]]
auto invert_callback(
    const LambdaToSetupCallback& setupCallback,
    FunctionToObtainResult&& functionToObtainResultForCallback
){
    using Awaitable = CoroFork_detail::AwaitableGeneratedCBReturnsResultOfCall<
        CoroFork_detail::ToPlainC<
            maxCallbacksCount, LambdaToSetupCallback
        >,
        typename CoroFork_detail::GenerateForSetupLambda<LambdaToSetupCallback>::CoResultType,
        LambdaToSetupCallback,
        FunctionToObtainResult
    >;

    return CoroFork_detail::AwaitAsTemporaryOnly<Awaitable>(
        Awaitable(
            setupCallback, functionToObtainResultForCallback
        )
    );
}


//______________________________________________________________________________
// Turning subscription to awaitable for function

/**Turn std::function subscription based API into coroutine based.
Provided lambda shall receive desired callback with corresponding signature
as a parameter. Such callback can be passed to asynchronous subscription API
to be called later (likely multiple times) by "something else".
Each time once called such generated function callback
will resume the coroutine awaiting for invert_subscription_holder
being created by the invert_subscription call,
and callback arguments will go to the awaiter.
The setupFunction can optionally return "the unsubscription API",
to be automatically invoked once corresponding invert_subscription_holder
goes out of scope.
See samples above for illustration.
(Remember here "inverted" subscription callback returns result immediately once issued
 and not when subscription is actually awaited) */
template<CoroForkLambdaToSetupFunction LambdaToSetupFunction>
[[nodiscard(
    "The holder returned by invert_subscription keeps the subscription alive,"
    " dropping it right away unsubscribes before any event can arrive"
)]]
auto invert_subscription(
    const LambdaToSetupFunction& setupFunction ///< receives generated callback
){
    //takes advantage of RVO here
    return CoroFork_detail::InvertSubscriptionHolder<
        typename CoroFork_detail::TheSame,
        typename CoroFork_detail::GenerateForSetupLambda<LambdaToSetupFunction>::CoResultType
    >(setupFunction);
}

/// Same as above but now there is a return value of generated callback
template<
    CoroForkLambdaToSetupFunction LambdaToSetupFunction,
    class ResultForCallbackFunctionAsValue
>
requires
    std::convertible_to<
        std::remove_cvref_t<
            ResultForCallbackFunctionAsValue
        >,
        std::remove_cvref_t<
            typename CoroFork_detail::GenerateForSetupLambda<LambdaToSetupFunction>::FunctionResultType
        >
    >
[[nodiscard(
    "The holder returned by invert_subscription keeps the subscription alive,"
    " dropping it right away unsubscribes before any event can arrive"
)]]
auto invert_subscription(
    const LambdaToSetupFunction& setupFunction, ///< Your setup
    ResultForCallbackFunctionAsValue&& resultForCallbackFunction
){
    //takes advantage of RVO here
    return CoroFork_detail::InvertSubscriptionHolder<
        typename CoroFork_detail::TheSame,
        typename CoroFork_detail::GenerateForSetupLambda<LambdaToSetupFunction>::CoResultType
    >(
        setupFunction,
        std::forward<ResultForCallbackFunctionAsValue>(resultForCallbackFunction)
    );
}




//______________________________________________________________________________
//##############################################################################
/*==============================================================================
 More implementation details follow                                          *
*=============================================================================*/
//##############################################################################



namespace CoroFork_detail{

    //__________________________________________________________________________
    // Exceptions escaping the body of a corofork coroutine

    /**One resumption of a coroutine done by corofork itself.
    CoroFork::Start and every generated callback resume the coroutine through
    ResumeAndPropagateException, which keeps a ResumeScope on the stack.
    Nested resumptions on one thread form a list of scopes,
    ResumeScope::innermostScope points to the most recent one */
    struct ResumeScope{
        /// Frame resumed in this scope (std::coroutine_handle<>::address)
        void* resumedFrameAddress;

        /// Scope of the enclosing resumption on the same thread (nullptr if none)
        ResumeScope* enclosingScope;

        /**Exception escaped from the body of the resumed frame.
        CoroFork::promise_type::unhandled_exception writes the exception here,
        final_suspend destroys the frame afterwards, and only then
        ResumeAndPropagateException rethrows the exception */
        std::exception_ptr exceptionFromBody;

        /// Innermost resumption done by corofork on the current thread
        static inline thread_local ResumeScope* innermostScope = nullptr;
    };

    /**Resume the coroutine, then rethrow the exception escaped from the coroutine body.
    The exception is rethrown after the coroutine frame is destroyed,
    so captures and locals of a corofork coroutine are freed first */
    inline void ResumeAndPropagateException(std::coroutine_handle<> handle){
        ResumeScope scope{handle.address(), ResumeScope::innermostScope, nullptr};

        /// Puts the enclosing scope back on every exit (also once resume() throws)
        struct InnermostScopeRestorer{
            ResumeScope& restoredScope;
            ~InnermostScopeRestorer(){
                ResumeScope::innermostScope = restoredScope.enclosingScope;
            }
        } innermostScopeRestorer{scope};

        ResumeScope::innermostScope = &scope;
        handle.resume();

        if( scope.exceptionFromBody ){
            std::rethrow_exception(scope.exceptionFromBody);
        }
    }


    //__________________________________________________________________________
    // General corofork macro support

    /// Actual implementation of corofork/corosync macro
    class CoroFork{
    public:
        //Uncopyable, unmovable (takes advantage of RVO!)
        //NOTE: CoroFork disappears once full expression starting the coroutine is exited
        //      this means promise_type can outlive CoroFork instance
        CoroFork(const CoroFork&) = delete;
        CoroFork& operator=(const CoroFork&) = delete;

        /** Define coroutine behavior
        (used by compiler to generate coroutine frame) */
        struct promise_type;

        /**Start with lambda that creates coroutine instance
        Ensures lambda is alive as long as coroutine is running */
        template<class Lambda>
        static void Start(Lambda&& lambda);

    private:
        /// Tie with promise once created
        CoroFork(promise_type* createdFrom): myPromise(createdFrom) {}

        /// Corresponding promise (may not be alive as coroutine exits)
        promise_type* myPromise;

        /// Common base for stored lambda (to be stored in promise)
        struct LambdaStorageBase;

        /// Store that lambda (with captures) as long as coroutine is running
        template<class Lambda>
        class LambdaStorage;
    };

    struct CoroFork::promise_type{
        /// Default constructor (just do nothing)
        promise_type() = default;

        /* uncopyable, unmovable (ped standard is never copied
        and better to ban it explicitly per
        https://devblogs.microsoft.com/oldnewthing/20210504-00/?p=105176
        to prevent accidental promise copying from arguments */
        promise_type(const promise_type&) = delete;
        promise_type& operator=(const promise_type&) = delete;

        /// How actually CoroFork is obtained from the coroutine
        CoroFork get_return_object() { return CoroFork(this); }

        /// Shall be suspended (prevent deallocation until the lambda is safe)
        std::suspend_always initial_suspend() const noexcept { return {}; }

        /// Just do nothing
        void return_void() const noexcept {}

        /**Once exception happens, let it propagate to the code that resumed the coroutine.
        Resumed by corofork itself (CoroFork::Start or a generated callback):
            the exception goes to the matching ResumeScope, final_suspend destroys
            the frame, then ResumeAndPropagateException rethrows the exception.
        Resumed by an awaitable of another library (no ResumeScope for this frame):
            the exception propagates from resume() right away, and the frame
            is never freed (the coroutine is considered suspended at the
            final suspend point, final_suspend does not run) */
        void unhandled_exception(){
            ResumeScope* innermostScope = ResumeScope::innermostScope;
            void* thisFrameAddress =
                std::coroutine_handle<promise_type>::from_promise(*this).address();

            if( innermostScope && innermostScope->resumedFrameAddress == thisFrameAddress ){
                innermostScope->exceptionFromBody = std::current_exception();
                return; //goes on to final_suspend, which destroys the frame
            }
            throw;
        }

        /** Shall let coroutine to be destroyed by reaching end.
        let it cleanup self in natural way, as coroutine execution reaches end
        https://stackoverflow.com/questions/68352718/is-it-necessary-to-call-destroy-on-a-stdcoroutine-handle
        */
        std::suspend_never final_suspend() const noexcept {
            // do nothing (lambda will fade away together with the promise)
            return {}; //let coroutine destruct
        }

        /// The way to make sure all lambda captures are alive as long coroutine is running
        std::unique_ptr<LambdaStorageBase> capturingFunction;
    };

    struct CoroFork::LambdaStorageBase{
    public:
        /// Allow cleanup via base class (correct destruction of derived)
        virtual ~LambdaStorageBase(){}

        /// Actually starts coroutine and obtains coroutine
        virtual CoroFork RunStoredLambda() = 0;
    };

    ///Store lambda (with captures!) as long as coroutine is running
    template<class Lambda>
    class CoroFork::LambdaStorage: public CoroFork::LambdaStorageBase{
    public:
        /**Create RAII wrapper around Lambda
        Using Lambda directly, optimizer takes case due to [&]() in front.
        Forwarding (instead of taking Lambda by value) keeps the number of
        copies at one and allows move only captures,
        like corofork(owned = std::move(owned)){...} */
        template<class InitialLambda>
        constexpr LambdaStorage(InitialLambda&& initialLambda)
            : lambda(std::forward<InitialLambda>(initialLambda)) {}

        //ensure copy changes go only through move constructor
        LambdaStorage(const LambdaStorage& other) = delete;
        LambdaStorage& operator=(const LambdaStorage& other) = delete;

        /// Actually starts coroutine (and created promise)
        virtual CoroFork RunStoredLambda() override{
            return lambda();
        }

    private:
        Lambda lambda; ///< Hold lambda, avoid slow std::function here
    };


    template<class Lambda>
    void CoroFork::Start(Lambda&& lambda){
        /* captured lambda here will have fixed memory location,
        While std::unique_ptr can be later paced into the promise,
        and live as long as coroutine is running for sure */
        auto capture = std::unique_ptr<LambdaStorageBase>(
            new LambdaStorage< std::remove_reference_t<Lambda> >(
                    std::forward<Lambda>(lambda)
            )
        );

        //this will create corresponding promise (in suspended state)
        CoroFork started = capture->RunStoredLambda();

        /*capture will exist as long as the coroutine is running
        (it is stored in the promise) */
        started.myPromise->capturingFunction = std::move(capture);

        /* Here coroutine is resumed from the current thread (runs first time)
        all actions will happen immediately in this thread,
        exception thrown before the first suspension is rethrown here
        (after the frame and the captures are freed) */
        ResumeAndPropagateException(
            std::coroutine_handle<promise_type>::from_promise(*started.myPromise)
        );

        /*once we are here this means coroutine eiter suspended or executes
        in some other thread, or it is already finished */

        /* Here we intentionally do not destroy coroutine here,
        (final_suspend returs std::suspend_never, so coroutine will be destroyed
        once it reaches the end, and lambda will be destroyed together with the promise)
        https://stackoverflow.com/questions/68352718/is-it-necessary-to-call-destroy-on-a-stdcoroutine-handle
        */
    }


    /// Helper type to trigger operator ->* for corofork macro
    struct TagTieWithOverloadedOperator{
        constexpr TagTieWithOverloadedOperator() = default;
    };
    /** Use this "instance" to trigger overloaded operator ->* for corofork macro.
    The trick with TagTieWithOverloadedOperator is needed
    to infer type of the lambda */
    constexpr TagTieWithOverloadedOperator tagTieWithOverloadedOperator;


    /**Helper operator to easy catch lambda for corofork macro.
    Use template to avoid slow std::function
    (raw lambda is copied/stored here) */
    template<class Lambda>
    constexpr void operator ->* (TagTieWithOverloadedOperator, Lambda&& lambda){
        CoroFork::Start(std::forward<Lambda>(lambda));
    }


    //__________________________________________________________________________
    // General corosync macro support

    /// Actual implementation of corofork/corosync macro
    class CoroSync{
    public:
        //Uncopyable, unmovable (take advantage of RVO)
        //NOTE: CoroSync dissapears once full expression starting the coroutine is exited
        //      this means promise_type can outlive CoroSync instance
        CoroSync(const CoroSync&) = delete;
        CoroSync& operator=(const CoroSync&) = delete;

        /// Define coroutine behavior
        struct promise_type;

        /**Start with lambda that creates coroutine instance
        Ensures lambda is alive as long as coroutine is running */
        template<class Lambda>
        static void Start(Lambda&& lambda);

    private:
        /// Tie with promise once created
        CoroSync(promise_type* createdFrom): myPromise(createdFrom) {}

        /// Corresponding promise (may not be alive as coroutine exits)
        promise_type* myPromise;
    };

    struct CoroSync::promise_type{
        /// Default constructor (just do nothing)
        promise_type() = default;

        /* uncopyable, unmovable (ped standard is never copied
        and better to ban it explicitly per
        https://devblogs.microsoft.com/oldnewthing/20210504-00/?p=105176
        to prevent accidental promise copying from arguments */
        promise_type(const promise_type&) = delete;
        promise_type& operator=(const promise_type&) = delete;

        /// How actually Coro is obtained from the coroutine
        CoroSync get_return_object() { return CoroSync(this); }

        /// Shall be suspended (prevent deallocation until the lambda is safe)
        std::suspend_always initial_suspend() const noexcept { return {}; }

        /// Part to outlive the coroutine lifetime (owned by CoroSync::Start)
        struct SharedState{
            /// Protect promise from multiple access
            std::mutex protectPromise;
            /// Used for waiting for coroutine to finish
            std::condition_variable waitForPromise;

            enum class State{
                Running,
                FinishedNormally,
                FinishedWithException,
                GoBackToThread
            } state = State::Running;

            class BackToThreadAwaitable{
            public:
                BackToThreadAwaitable(SharedState* sharedStateToUse)
                    : sharedState(sharedStateToUse) {}

                /// Always cause coroutine to suspend
                bool await_ready() const noexcept{ return false; }

                /// Cause execution to continue in the initial thread
                void await_suspend(std::coroutine_handle<>){
                    std::lock_guard lock(sharedState->protectPromise);
                    sharedState->state = State::GoBackToThread;
                    sharedState->waitForPromise.notify_one();
                }

                void await_resume() const noexcept{}
            private:
                SharedState* const sharedState;
            };

            /// Exception to be propagated from corosync
            std::exception_ptr exception;

            /**Keep exception to be rethrown by corosync in the initial thread.
            state is not changed here, ReportCompletion changes state
            once the coroutine reaches the final suspend point */
            void unhandled_exception() noexcept {
                std::lock_guard lock(protectPromise);
                exception = std::current_exception();
            }

            /**Mark coroutine as finished (will not resume), wake up CoroSync::Start.
            Called at the final suspend point, all locals of the body are destroyed
            already. CoroSync::Start may destroy the frame and this SharedState
            right after the lock below is released */
            void ReportCompletion() noexcept {
                std::lock_guard lock(protectPromise);
                state = exception ? State::FinishedWithException : State::FinishedNormally;
                waitForPromise.notify_one();
            }
        };
        /** Refers to the SharedState owned by the CoroSync::Start frame.
        Raw pointer is intentional (nothing to clean up here):
        Start does not return until the coroutine reaches a terminal state,
        so SharedState is guaranteed to outlive this promise.
        NOTE: BackToThreadAwaitable above relies on the same guarantee */
        SharedState* sharedState = nullptr;

        /**Do nothing here, ReportCompletionAwaiter reports completion instead.
        After an explicit co_return the locals of the body are destroyed
        AFTER return_void (g++ and clang++ both do so), reporting completion here
        lets corosync return while destructors of those locals still run
        in another thread */
        void return_void() const noexcept {}

        /// Once exception happens, let it propagate to the initial thread
        void unhandled_exception() noexcept {
            sharedState->unhandled_exception();
        }

        /// Awaiter for the final suspend point: reports completion, coroutine stays suspended
        class ReportCompletionAwaiter{
        public:
            /// Always suspend, CoroSync::Start destroys the frame
            bool await_ready() const noexcept { return false; }

            /**All locals of the body are destroyed by now.
            After ReportCompletion returns, CoroSync::Start may destroy the frame
            (this awaiter included), so nothing is touched afterwards */
            void await_suspend(std::coroutine_handle<promise_type> handle) const noexcept {
                handle.promise().sharedState->ReportCompletion();
            }

            void await_resume() const noexcept {}
        };

        /**Report completion once the body is left completely.
        The frame is not destroyed by reaching the end,
        CoroSync::Start calls destroy() once it sees the terminal state */
        ReportCompletionAwaiter final_suspend() const noexcept { return {}; }
    };


    template<class Lambda>
    void CoroSync::Start(Lambda&& lambda){
        /* Storing lambda in capture is not needed here
          as lambda will exist as long as coroutine is running
          due to synchronization */

        /* Owned by this frame:
        the loop below does not exit until the coroutine reached
        a terminal state (and the loop destroyed the coroutine frame),
        this is why "just plain pointers" can be kept by the promise and by
        BackToThreadAwaitable as they stay valid for the entire coroutine lifetime */
        CoroSync::promise_type::SharedState sharedState;

        //this will create corresponding promise and coroutine (in suspended state)
        CoroSync started = lambda(
            //coroutine will be able to go back to the initial thread
            CoroSync::promise_type::SharedState::BackToThreadAwaitable(&sharedState)
        );

        //let the promise reach the sharedState owned by this frame
        started.myPromise->sharedState = &sharedState;

        //handle to the coroutine
        auto handle = std::coroutine_handle<promise_type>::from_promise(*started.myPromise);

        for( ;; ){
            /* Here coroutine is resumed from the current thread (runs first time),
            all actions will happen immediately in this thread */
            handle.resume();

            /*once we are here this means coroutine either suspended or
            executes in some other thread, or it is already finished
            (suspended at the final suspend point, destroyed only below),
            in any case sharedState is still alive, it is owned by this frame */

            std::unique_lock lock(sharedState.protectPromise);
            while(
                    sharedState.state
                !=  CoroSync::promise_type::SharedState::State::GoBackToThread
            ){
                switch( sharedState.state ){
                    case CoroSync::promise_type::SharedState::State::FinishedNormally:
                        /* coroutine is suspended at the final suspend point,
                        locals of the body are destroyed, no other thread touches the frame */
                        lock.unlock();
                        handle.destroy();
                        //sharedState dies with this frame
                        return;
                    case CoroSync::promise_type::SharedState::State::FinishedWithException:
                        lock.unlock();
                        handle.destroy();
                        /* exception_ptr is copied into rethrow_exception,
                        so the exception being thrown outlives sharedState */
                        std::rethrow_exception(sharedState.exception);
                    case CoroSync::promise_type::SharedState::State::Running:
                        //coroutine is still driven by somebody, keep waiting below
                        break;
                    case CoroSync::promise_type::SharedState::State::GoBackToThread:
                        //unreachable, that is exactly what the while above tests
                        break;
                }
                //wait for coroutine to "change something"
                sharedState.waitForPromise.wait(lock);
            }
            // Prevent GoBackToThread until coroutine explicitly awaits back_to_thread
            sharedState.state = CoroSync::promise_type::SharedState::State::Running;
        } // loop for resuming coroutine
    }

    ///Helper type to trigger operator ->* for corosync macro
    struct TagTieWithSyncOverloadedOperator{
        constexpr TagTieWithSyncOverloadedOperator() = default;
    };
    /** Use this "instance" to trigger overloaded operator ->* for corosync macro.
    The trick with TagTieWithSyncOverloadedOperator is needed
    to infer type of the lambda */
    constexpr TagTieWithSyncOverloadedOperator tagTieWithSyncOverloadedOperator;

    /**Helper operator to easy catch lambda for corosync macro.
    Use template to avoid slow std::function
    (raw lambda is copied/stored here) */
    template<class Lambda>
    constexpr void operator ->* (TagTieWithSyncOverloadedOperator, Lambda&& lambda){
        CoroSync::Start(std::forward<Lambda>(lambda));
    }

    //__________________________________________________________________________
    //##########################################################################
    //Extracting types and code from lambda's operator() signature


    //__________________________________________________________________________
    // Helper to decide on result from setup lambda

    /**Handle case then Setup is able to obtain result before async operation.
    Just check result is already there
    and place it to expected destination (or stay suspended) */
    template<
        class CoResultType,
        class AwaitableToPlaceResult
    >
    bool ProcessSetupResult(
        std::optional<CoResultType>&& setupResult,
        AwaitableToPlaceResult* self
    ){
        if( setupResult ){
            //There is a result already present
            self->EmplaceCoResult(std::move(*setupResult));
            // means result is ready (do not suspend)
            return false;
        }

        // means stay suspended (this is ok even if already resumed)
        return true;
    }

    //__________________________________________________________________________
    // case of generating the std::function callback without result

    //version without arguments at all, but lambda can complete synchronously
    template<class OperatorResult, class LambdaType>
    struct GenerateForSetupMethod<
        OperatorResult(LambdaType::*)( std::function<void()> )
    >{
        using Signature = void();
        using FunctionType = std::function<Signature>;
        using CoResultType = void;
        using FunctionResultType = void;


        ///Invoke setup with resuming function
        template<class AwaitableToPlaceResult>
        static bool SetupCBNoResult(
            AwaitableToPlaceResult* self,
            std::coroutine_handle<> handle,
            const LambdaType& setupLambda
        ){
            static_assert(
                std::same_as<OperatorResult, bool>,
                "Such setup lambda shall return void or bool\n"
                "true means operation completed synchronously immediately (and callback was not scheduled),\n"
                "false means one will await for callback (callback was scheduled)"
            );
            // inverts result to resume the coroutine
            return !setupLambda(
                self->Decorate([handle]{ ResumeAndPropagateException(handle); })
            );
        }

        ///Invoke setup with subscribing function
        template<class AwaitableToPlaceResult>
        static auto SubscribeCBNoResult(
            AwaitableToPlaceResult* self,
            const LambdaType& setupLambda
        ){
            static_assert(
                std::invocable<OperatorResult>,
                "Such setup lambda shall return void or invocable\n"
                "void means no cleanup is needed\n"
                "invocable is a simple cleanup function to be called on scope exit"
            );
            // returns cleanup API
            return setupLambda(
                self->Decorate([=]{
                    self->OnEventHappened();
                })
            );
        }
    };
    //version without arguments at all, when lambda always starts async operation!
    template<class LambdaType>
    struct GenerateForSetupMethod<
        void(LambdaType::*)( std::function<void()> )
    >{
        using Signature = void();
        using FunctionType = std::function<Signature>;
        using CoResultType = void;
        using FunctionResultType = void;


        ///Invoke setup with resuming function
        template<class AwaitableToPlaceResult>
        static bool SetupCBNoResult(
            AwaitableToPlaceResult* self,
            std::coroutine_handle<> handle,
            const LambdaType& setupLambda
        ){
            setupLambda(
                self->Decorate([handle]{ ResumeAndPropagateException(handle); })
            );
            return true; //means stay suspended (this is ok even if already resumed)
        }

        ///Invoke setup with subscribing function
        template<class AwaitableToPlaceResult>
        static auto SubscribeCBNoResult(
            AwaitableToPlaceResult* self,
            const LambdaType& setupLambda
        ){
            setupLambda(
                self->Decorate([=]{
                    self->OnEventHappened();
                })
            );
            // returns cleanup API that does nothing
            return [](){};
        }
    };

    //single argument version, lambda optionally can complete synchronously
    template<class OperatorResult, class LambdaType, class CallbackArg>
    struct GenerateForSetupMethod<
        OperatorResult(LambdaType::*)( std::function<void(CallbackArg)> )
    >{
        using Signature = void(CallbackArg);
        using FunctionType = std::function<Signature>;
        /* decayed: what is awaited is an owned value, so a callback taking
           void(const T&) hands a T to the coroutine (and to the queue of
           invert_subscription, which may hold it long after the call) */
        using CoResultType = std::decay_t<CallbackArg>;
        using FunctionResultType = void;


        ///Invoke setup with resuming function that remembers argument
        template<class AwaitableToPlaceResult>
        static bool SetupCBNoResult(
            AwaitableToPlaceResult* self,
            std::coroutine_handle<> handle,
            const LambdaType& setupLambda
        ){
            static_assert(
                std::same_as<OperatorResult, std::optional<CoResultType>>,
                "Call to lambda shall return void or std::optional<std::decay_t<CallbackArg>>\n"
                "(present value means result is ready immediately)"
            );

            return ProcessSetupResult<CoResultType, AwaitableToPlaceResult>(
                setupLambda(
                    self->Decorate(
                        [=](CallbackArg arg){
                            self->EmplaceCoResult(std::forward<CallbackArg>(arg)); //what will be returned to coroutine
                            ResumeAndPropagateException(handle);
                        }
                    )
                ),
                self
            );
        }

        ///Invoke setup with subscribing function
        template<class AwaitableToPlaceResult>
        static auto SubscribeCBNoResult(
            AwaitableToPlaceResult* self,
            const LambdaType& setupLambda
        ){
            static_assert(
                std::invocable<OperatorResult>,
                "Such setup lambda shall return void or invocable\n"
                "void means no cleanup is needed\n"
                "invocable is a simple cleanup function to be called on scope exit"
            );
            // returns cleanup API
            return setupLambda(
                self->Decorate([=](CallbackArg arg){
                    self->OnArgumentsArrived(std::forward<CallbackArg>(arg)); //what will be returned to coroutine
                })
            );
        }
    };
    //single argument version, lambda always starts asynchronous operation
    template<class LambdaType, class CallbackArg>
    struct GenerateForSetupMethod<
        void(LambdaType::*)( std::function<void(CallbackArg)> )
    >{
        using Signature = void(CallbackArg);
        using FunctionType = std::function<Signature>;
        /* decayed: what is awaited is an owned value, so a callback taking
           void(const T&) hands a T to the coroutine (and to the queue of
           invert_subscription, which may hold it long after the call) */
        using CoResultType = std::decay_t<CallbackArg>;
        using FunctionResultType = void;


        ///Invoke setup with resuming function that remembers argument
        template<class AwaitableToPlaceResult>
        static bool SetupCBNoResult(
            AwaitableToPlaceResult* self,
            std::coroutine_handle<> handle,
            const LambdaType& setupLambda
        ){
            setupLambda(
                self->Decorate(
                    [=](CallbackArg arg){
                        self->EmplaceCoResult(std::forward<CallbackArg>(arg)); //what will be returned to coroutine
                        ResumeAndPropagateException(handle);
                    }
                )
            );
            return true; //means stay suspended (this is ok even if already resumed)
        }

        ///Invoke setup with subscribing function
        template<class AwaitableToPlaceResult>
        static auto SubscribeCBNoResult(
            AwaitableToPlaceResult* self,
            const LambdaType& setupLambda
        ){
            setupLambda(
                self->Decorate([=](CallbackArg arg){
                    self->OnArgumentsArrived(std::forward<CallbackArg>(arg)); //what will be returned to coroutine
                })
            );
            // returns cleanup API that does nothing
            return [](){};
        }
    };

    //multiple argument version, lambda optionally can complete synchronously
    template<class OperatorResult, class LambdaType, class... CallbackArgs>
    struct GenerateForSetupMethod<
        OperatorResult(LambdaType::*)( std::function<void(CallbackArgs...)> )
    >{
        using Signature = void(CallbackArgs...);
        using FunctionType = std::function<Signature>;
        /* decayed, see the single argument case above */
        using CoResultType = std::tuple<std::decay_t<CallbackArgs>...>;
        using FunctionResultType = void;


        ///Invoke setup with resuming function that remembers arguments
        template<class AwaitableToPlaceResult>
        static bool SetupCBNoResult(
            AwaitableToPlaceResult* self,
            std::coroutine_handle<> handle,
            const LambdaType& setupLambda
        ){
            static_assert(
                std::same_as<OperatorResult, std::optional<CoResultType>>,
                "Call to lambda shall return void or std::optional<std::tuple<std::decay_t<CallbackArgs>...>>\n"
                "(present value means result is ready immediately)"
            );

            return ProcessSetupResult<CoResultType, AwaitableToPlaceResult>(
                setupLambda(
                    self->Decorate(
                        [=](CallbackArgs... args){
                            self->EmplaceCoResult(std::forward<CallbackArgs>(args)...); //what will be returned to coroutine
                            ResumeAndPropagateException(handle);
                        }
                    )
                ),
                self
            );
        }

        ///Invoke setup with subscribing function
        template<class AwaitableToPlaceResult>
        static auto SubscribeCBNoResult(
            AwaitableToPlaceResult* self,
            const LambdaType& setupLambda
        ){
            static_assert(
                std::invocable<OperatorResult>,
                "Such setup lambda shall return void or invocable\n"
                "void means no cleanup is needed\n"
                "invocable is a simple cleanup function to be called on scope exit"
            );
            // returns cleanup API
            return setupLambda(
                self->Decorate([=](CallbackArgs... args){
                    self->OnArgumentsArrived(std::forward<CallbackArgs>(args)...); //what will be returned to coroutine
                })
            );
        }
    };
    //multiple argument version, lambda always starts asynchronous operation
    template<class LambdaType, class... CallbackArgs>
    struct GenerateForSetupMethod<
        void(LambdaType::*)( std::function<void(CallbackArgs...)> )
    >{
        using Signature = void(CallbackArgs...);
        using FunctionType = std::function<Signature>;
        /* decayed, see the single argument case above */
        using CoResultType = std::tuple<std::decay_t<CallbackArgs>...>;
        using FunctionResultType = void;


        //Invoke setup with resuming function that remembers arguments
        template<class AwaitableToPlaceResult>
        static bool SetupCBNoResult(
            AwaitableToPlaceResult* self,
            std::coroutine_handle<> handle,
            const LambdaType& setupLambda
        ){
            setupLambda(
                self->Decorate(
                    [=](CallbackArgs... args){
                        self->EmplaceCoResult(std::forward<CallbackArgs>(args)...); //what will be returned to coroutine
                        ResumeAndPropagateException(handle);
                    }
                )
            );
            return true; //means stay suspended (this is ok even if already resumed)
        }

        ///Invoke setup with subscribing function
        template<class AwaitableToPlaceResult>
        static auto SubscribeCBNoResult(
            AwaitableToPlaceResult* self,
            const LambdaType& setupLambda
        ){
            setupLambda(
                self->Decorate([=](CallbackArgs... args){
                    self->OnArgumentsArrived(std::forward<CallbackArgs>(args)...); //what will be returned to coroutine
                })
            );
            // returns cleanup API that does nothing
            return [](){};
        }
    };

    //__________________________________________________________________________
    // case of generating the std::function callback with result

    //version without arguments at all, but lambda can complete synchronously
    template<class OperatorResult, class LambdaType, class CallbackRes>
    struct GenerateForSetupMethod<
        OperatorResult(LambdaType::*)( std::function<CallbackRes()> )
    >{
        using Signature = CallbackRes();
        using FunctionType = std::function<Signature>;
        using CoResultType = void;
        using FunctionResultType = CallbackRes;


        ///Invoke setup with resuming function that returns value
        template<class AwaitableToPlaceResult, class Value>
        static bool SetupCBReturnsValue(
            AwaitableToPlaceResult* self,
            std::coroutine_handle<> handle,
            const LambdaType& setupLambda,
            Value&& result
        ){
            static_assert(
                std::same_as<OperatorResult, bool>,
                "Such setup lambda shall return void or bool\n"
                "true means operation completed synchronously immediately (and callback was not scheduled),\n"
                "false means one will await for callback (callback was scheduled)"
            );
            // inverts result
            return !setupLambda(
                self->Decorate(
                    [   =,
                        //captured copy shall be able to exist a for long time
                        storedResult=std::remove_reference_t<Value>(
                            std::forward<Value>(result)
                        )
                    ](){
                        /* copied before the coroutine is resumed: code resumed in the coroutine
                           may destroy this very lambda (frees the trampoline node, unsubscribes
                           or replaces the std::function holding the lambda) */
                        auto resultForCaller = storedResult;
                        ResumeAndPropagateException(handle); //resume the coroutine
                        return resultForCaller;
                    }
                )
            );
        }
        ///Invoke setup with subscribing function that returns value
        template<class AwaitableToPlaceResult, class Value>
        static auto SubscribeCBReturnsValue(
            AwaitableToPlaceResult* self,
            const LambdaType& setupLambda,
            Value&& result
        ){
            static_assert(
                std::invocable<OperatorResult>,
                "Such setup lambda shall return void or invocable\n"
                "void means no cleanup is needed\n"
                "invocable is a simple cleanup function to be called on scope exit"
            );
            // returns cleanup API
            return setupLambda(
                self->Decorate(
                    [   =,
                        //captured copy shall be able to exist a for long time
                        storedResult=std::remove_reference_t<Value>(
                            std::forward<Value>(result)
                        )
                    ](){
                        /* copied before the coroutine is resumed: code resumed in the coroutine
                           may destroy this very lambda (frees the trampoline node, unsubscribes
                           or replaces the std::function holding the lambda) */
                        auto resultForCaller = storedResult;
                        self->OnEventHappened();
                        return resultForCaller;
                    }
                )
            );
        }

        //Invoke setup with resuming function that calculates result
        template<class AwaitableToPlaceResult, class Function>
        static bool SetupCBReturnsResultOfCall(
            AwaitableToPlaceResult* self,
            std::coroutine_handle<> handle,
            const LambdaType& setupLambda,
            Function&& resultGetter
        ){
            static_assert(
                std::same_as<OperatorResult, bool>,
                "Such setup lambda shall return void or bool\n"
                "true means operation completed synchronously immediately (and callback was not scheduled),\n"
                "false means one will await for callback (callback was scheduled)"
            );
            // inverts result
            return !setupLambda(
                self->Decorate(
                    [   =,
                        //captured copy shall be able to exist a for long time
                        calculateResult=std::remove_reference_t<Function>(
                            std::forward<Function>(resultGetter)
                        )
                    ](){
                        /* copied before the coroutine is resumed: code resumed in the coroutine
                           may destroy this very lambda (frees the trampoline node, unsubscribes
                           or replaces the std::function holding the lambda) */
                        auto calculateResultForCaller = calculateResult;
                        ResumeAndPropagateException(handle); //resume the coroutine
                        return calculateResultForCaller();
                    }
                )
            );
        }
    };
    //version without arguments at all, when lambda always starts async operation!
    template<class LambdaType, class CallbackRes>
    struct GenerateForSetupMethod<void(LambdaType::*)( std::function<CallbackRes()> )>{
        using Signature = CallbackRes();
        using FunctionType = std::function<Signature>;
        using CoResultType = void;
        using FunctionResultType = CallbackRes;


        ///Invoke setup with resuming function that returns value
        template<class AwaitableToPlaceResult, class Value>
        static bool SetupCBReturnsValue(
            AwaitableToPlaceResult* self,
            std::coroutine_handle<> handle,
            const LambdaType& setupLambda,
            Value&& result
        ){
            setupLambda(
                self->Decorate(
                    [   =,
                        //captured copy shall be able to exist a for long time
                        storedResult=std::remove_reference_t<Value>(
                            std::forward<Value>(result)
                        )
                    ](){
                        /* copied before the coroutine is resumed: code resumed in the coroutine
                           may destroy this very lambda (frees the trampoline node, unsubscribes
                           or replaces the std::function holding the lambda) */
                        auto resultForCaller = storedResult;
                        ResumeAndPropagateException(handle); //resume the coroutine
                        return resultForCaller;
                    }
                )
            );
            return true; //means stay suspended (this is ok even if already resumed)
        }

        ///Invoke setup with subscribing function
        template<class AwaitableToPlaceResult, class Value>
        static auto SubscribeCBReturnsValue(
            AwaitableToPlaceResult* self,
            const LambdaType& setupLambda,
            Value&& result
        ){
            setupLambda(
                self->Decorate(
                    [   =,
                        //captured copy shall be able to exist a for long time
                        storedResult=std::remove_reference_t<Value>(
                            std::forward<Value>(result)
                        )
                    ](){
                        /* copied before the coroutine is resumed: code resumed in the coroutine
                           may destroy this very lambda (frees the trampoline node, unsubscribes
                           or replaces the std::function holding the lambda) */
                        auto resultForCaller = storedResult;
                        self->OnEventHappened();
                        return resultForCaller;
                    }
                )
            );
            // returns cleanup API that does nothing
            return [](){};
        }

        //Invoke setup with resuming function that calculates result
        template<class AwaitableToPlaceResult, class Function>
        static bool SetupCBReturnsResultOfCall(
            AwaitableToPlaceResult* self,
            std::coroutine_handle<> handle,
            const LambdaType& setupLambda,
            Function&& resultGetter
        ){
            setupLambda(
                self->Decorate(
                    [   =,
                        //captured copy shall be able to exist a for long time
                        calculateResult=std::remove_reference_t<Function>(
                            std::forward<Function>(resultGetter)
                        )
                    ](){
                        /* copied before the coroutine is resumed: code resumed in the coroutine
                           may destroy this very lambda (frees the trampoline node, unsubscribes
                           or replaces the std::function holding the lambda) */
                        auto calculateResultForCaller = calculateResult;
                        ResumeAndPropagateException(handle); //resume the coroutine
                        return calculateResultForCaller();
                    }
                )
            );
            return true; //means stay suspended (this is ok even if already resumed)
        }
    };

    //single argument version, lambda optionally can complete synchronously
    template<class OperatorResult, class LambdaType, class CallbackRes, class CallbackArg>
    struct GenerateForSetupMethod<
        OperatorResult(LambdaType::*)( std::function<CallbackRes(CallbackArg)> )
    >{
        using Signature = CallbackRes(CallbackArg);
        using FunctionType = std::function<Signature>;
        /* decayed: what is awaited is an owned value, so a callback taking
           void(const T&) hands a T to the coroutine (and to the queue of
           invert_subscription, which may hold it long after the call) */
        using CoResultType = std::decay_t<CallbackArg>;
        using FunctionResultType = CallbackRes;


        ///Invoke setup with resuming function that remembers argument and returns value
        template<class AwaitableToPlaceResult, class Value>
        static bool SetupCBReturnsValue(
            AwaitableToPlaceResult* self,
            std::coroutine_handle<> handle,
            const LambdaType& setupLambda,
            Value&& result
        ){
            static_assert(
                std::same_as< OperatorResult, std::optional<CoResultType> >,
                "Call to lambda shall return void or std::optional<std::decay_t<CallbackArg>>\n"
                "(present value means result is ready immediately)"
            );

            return ProcessSetupResult<CoResultType, AwaitableToPlaceResult>(
                setupLambda(
                    self->Decorate(
                        [   =,
                            //captured copy shall be able to exist a for long time
                            storedResult=std::remove_reference_t<Value>(
                                std::forward<Value>(result)
                            )
                        ](CallbackArg arg){
                            /* copied before the coroutine is resumed: code resumed in the coroutine
                               may destroy this very lambda (frees the trampoline node, unsubscribes
                               or replaces the std::function holding the lambda) */
                            auto resultForCaller = storedResult;
                            self->EmplaceCoResult(std::forward<CallbackArg>(arg)); //what will be returned to coroutine
                            ResumeAndPropagateException(handle);
                            return resultForCaller;
                        }
                    )
                ),
                self
            );
        }

        ///Invoke setup with subscribing function
        template<class AwaitableToPlaceResult, class Value>
        static auto SubscribeCBReturnsValue(
            AwaitableToPlaceResult* self,
            const LambdaType& setupLambda,
            Value&& result
        ){
            static_assert(
                std::invocable<OperatorResult>,
                "Such setup lambda shall return void or invocable\n"
                "void means no cleanup is needed\n"
                "invocable is a simple cleanup function to be called on scope exit"
            );
            // returns cleanup API
            return setupLambda(
                self->Decorate(
                    [   =,
                        //captured copy shall be able to exist a for long time
                        storedResult=std::remove_reference_t<Value>(
                            std::forward<Value>(result)
                        )
                    ](CallbackArg arg){
                        /* copied before the coroutine is resumed: code resumed in the coroutine
                           may destroy this very lambda (frees the trampoline node, unsubscribes
                           or replaces the std::function holding the lambda) */
                        auto resultForCaller = storedResult;
                        self->OnArgumentsArrived(std::forward<CallbackArg>(arg));
                        return resultForCaller;
                    }
                )
            );
        }

        //Invoke setup with resuming function that remembers argument and calculates result
        template<class AwaitableToPlaceResult, class Function>
        static bool SetupCBReturnsResultOfCall(
            AwaitableToPlaceResult* self,
            std::coroutine_handle<> handle,
            const LambdaType& setupLambda,
            Function&& resultGetter
        ){
            static_assert(
                std::same_as< OperatorResult, std::optional<CoResultType> >,
                "Call to lambda shall return void or std::optional<std::decay_t<CallbackArg>>\n"
                "(present value means result is ready immediately)"
            );

            return ProcessSetupResult<CoResultType, AwaitableToPlaceResult>(
                setupLambda(
                    self->Decorate(
                        [   =,
                            //captured copy shall be able to exist a for long time
                            calculateResult=std::remove_reference_t<Function>(
                                std::forward<Function>(resultGetter)
                            )
                        ](CallbackArg arg){
                            /* copied before the coroutine is resumed: code resumed in the coroutine
                               may destroy this very lambda (frees the trampoline node, unsubscribes
                               or replaces the std::function holding the lambda) */
                            auto calculateResultForCaller = calculateResult;
                            self->EmplaceCoResult(std::forward<CallbackArg>(arg)); //what will be returned to coroutine
                            ResumeAndPropagateException(handle);
                            return calculateResultForCaller();
                        }
                    )
                ),
                self
            );
        }
    };
    //single argument version, lambda always starts asynchronous operation
    template<class LambdaType, class CallbackRes, class CallbackArg>
    struct GenerateForSetupMethod<
        void(LambdaType::*)( std::function<CallbackRes(CallbackArg)> )
    >{
        using Signature = CallbackRes(CallbackArg);
        using FunctionType = std::function<Signature>;
        /* decayed: what is awaited is an owned value, so a callback taking
           void(const T&) hands a T to the coroutine (and to the queue of
           invert_subscription, which may hold it long after the call) */
        using CoResultType = std::decay_t<CallbackArg>;
        using FunctionResultType = CallbackRes;


        ///Invoke setup with resuming function that remembers argument and returns value
        template<class AwaitableToPlaceResult, class Value>
        static bool SetupCBReturnsValue(
            AwaitableToPlaceResult* self,
            std::coroutine_handle<> handle,
            const LambdaType& setupLambda,
            Value&& result
        ){
            setupLambda(
                self->Decorate(
                    [   =,
                        //captured copy shall be able to exist a for long time
                        storedResult=std::remove_reference_t<Value>(
                            std::forward<Value>(result)
                        )
                    ](CallbackArg arg){
                        /* copied before the coroutine is resumed: code resumed in the coroutine
                           may destroy this very lambda (frees the trampoline node, unsubscribes
                           or replaces the std::function holding the lambda) */
                        auto resultForCaller = storedResult;
                        self->EmplaceCoResult(std::forward<CallbackArg>(arg)); //what will be returned to coroutine
                        ResumeAndPropagateException(handle);
                        return resultForCaller;
                    }
                )
            );
            return true; //means stay suspended (this is ok even if already resumed)
        }

        ///Invoke setup with subscribing function
        template<class AwaitableToPlaceResult, class Value>
        static auto SubscribeCBReturnsValue(
            AwaitableToPlaceResult* self,
            const LambdaType& setupLambda,
            Value&& result
        ){
            setupLambda(
                self->Decorate(
                    [   =,
                        //captured copy shall be able to exist a for long time
                        storedResult=std::remove_reference_t<Value>(
                            std::forward<Value>(result)
                        )
                    ](CallbackArg arg){
                        /* copied before the coroutine is resumed: code resumed in the coroutine
                           may destroy this very lambda (frees the trampoline node, unsubscribes
                           or replaces the std::function holding the lambda) */
                        auto resultForCaller = storedResult;
                        self->OnArgumentsArrived(std::forward<CallbackArg>(arg));
                        return resultForCaller;
                    }
                )
            );
            // returns cleanup API that does nothing
            return [](){};
        }

        //Invoke setup with resuming function that remembers argument and calculates result
        template<class AwaitableToPlaceResult, class Function>
        static bool SetupCBReturnsResultOfCall(
            AwaitableToPlaceResult* self,
            std::coroutine_handle<> handle,
            const LambdaType& setupLambda,
            Function&& resultGetter
        ){
            setupLambda(
                self->Decorate(
                    [   =,
                        //captured copy shall be able to exist a for long time
                        calculateResult=std::remove_reference_t<Function>(
                            std::forward<Function>(resultGetter)
                        )
                    ](CallbackArg arg){
                        /* copied before the coroutine is resumed: code resumed in the coroutine
                           may destroy this very lambda (frees the trampoline node, unsubscribes
                           or replaces the std::function holding the lambda) */
                        auto calculateResultForCaller = calculateResult;
                        self->EmplaceCoResult(std::forward<CallbackArg>(arg)); //what will be returned to coroutine
                        ResumeAndPropagateException(handle);
                        return calculateResultForCaller();
                    }
                )
            );
            return true; //means stay suspended (this is ok even if already resumed)
        }
    };

    //multiple argument version, lambda optionally can complete synchronously
    template<class OperatorResult, class LambdaType, class CallbackRes, class... CallbackArgs>
    struct GenerateForSetupMethod<
        OperatorResult(LambdaType::*)( std::function<CallbackRes(CallbackArgs...)> )
    >{
        using Signature = CallbackRes(CallbackArgs...);
        using FunctionType = std::function<Signature>;
        /* decayed, see the single argument case above */
        using CoResultType = std::tuple<std::decay_t<CallbackArgs>...>;
        using FunctionResultType = CallbackRes;


        ///Invoke setup with resuming function that remembers arguments and returns value
        template<class AwaitableToPlaceResult, class Value>
        static bool SetupCBReturnsValue(
            AwaitableToPlaceResult* self,
            std::coroutine_handle<> handle,
            const LambdaType& setupLambda,
            Value&& result
        ){
            static_assert(
                std::same_as< OperatorResult, std::optional<CoResultType> >,
                "Call to lambda shall return void or std::optional<std::tuple<std::decay_t<CallbackArgs>...>>\n"
                "(present value means result is ready immediately)"
            );

            return ProcessSetupResult<CoResultType, AwaitableToPlaceResult>(
                setupLambda(
                    self->Decorate(
                        [   =,
                            //captured copy shall be able to exist a for long time
                            storedResult=std::remove_reference_t<Value>(
                                std::forward<Value>(result)
                            )
                        ](CallbackArgs... args){
                            /* copied before the coroutine is resumed: code resumed in the coroutine
                               may destroy this very lambda (frees the trampoline node, unsubscribes
                               or replaces the std::function holding the lambda) */
                            auto resultForCaller = storedResult;
                            self->EmplaceCoResult(std::forward<CallbackArgs>(args)...); //what will be returned to coroutine
                            ResumeAndPropagateException(handle);
                            return resultForCaller;
                        }
                    )
                ),
                self
            );
        }

        ///Invoke setup with subscribing function
        template<class AwaitableToPlaceResult, class Value>
        static auto SubscribeCBReturnsValue(
            AwaitableToPlaceResult* self,
            const LambdaType& setupLambda,
            Value&& result
        ){
            static_assert(
                std::invocable<OperatorResult>,
                "Such setup lambda shall return void or invocable\n"
                "void means no cleanup is needed\n"
                "invocable is a simple cleanup function to be called on scope exit"
            );

            // returns cleanup API
            return setupLambda(
                self->Decorate(
                    [   =,
                        //captured copy shall be able to exist a for long time
                        storedResult=std::remove_reference_t<Value>(
                            std::forward<Value>(result)
                        )
                    ](CallbackArgs... args){
                        /* copied before the coroutine is resumed: code resumed in the coroutine
                           may destroy this very lambda (frees the trampoline node, unsubscribes
                           or replaces the std::function holding the lambda) */
                        auto resultForCaller = storedResult;
                        self->OnArgumentsArrived(std::forward<CallbackArgs>(args)...); //what will be returned to coroutine
                        return resultForCaller;
                    }
                )
            );
        }

        //Invoke setup with resuming function that remembers argument and calculates result
        template<class AwaitableToPlaceResult, class Function>
        static bool SetupCBReturnsResultOfCall(
            AwaitableToPlaceResult* self,
            std::coroutine_handle<> handle,
            const LambdaType& setupLambda,
            Function&& resultGetter
        ){
            static_assert(
                std::same_as< OperatorResult, std::optional<CoResultType> >,
                "Call to lambda shall return void or std::optional<std::tuple<std::decay_t<CallbackArgs>...>>\n"
                "(present value means result is ready immediately)"
            );

            return ProcessSetupResult<CoResultType, AwaitableToPlaceResult>(
                setupLambda(
                    self->Decorate(
                        [   =,
                            //captured copy shall be able to exist a for long time
                            calculateResult=std::remove_reference_t<Function>(
                                std::forward<Function>(resultGetter)
                            )
                        ](CallbackArgs... args){
                            /* copied before the coroutine is resumed: code resumed in the coroutine
                               may destroy this very lambda (frees the trampoline node, unsubscribes
                               or replaces the std::function holding the lambda) */
                            auto calculateResultForCaller = calculateResult;
                            self->EmplaceCoResult(std::forward<CallbackArgs>(args)...); //what will be returned to coroutine
                            ResumeAndPropagateException(handle);
                            return calculateResultForCaller();
                        }
                    )
                ),
                self
            );
        }
    };
    //multiple argument version, lambda always starts asynchronous operation
    template<class LambdaType, class CallbackRes, class... CallbackArgs>
    struct GenerateForSetupMethod<
        void(LambdaType::*)( std::function<CallbackRes(CallbackArgs...)> )
    >{
        using Signature = CallbackRes(CallbackArgs...);
        using FunctionType = std::function<Signature>;
        /* decayed, see the single argument case above */
        using CoResultType = std::tuple<std::decay_t<CallbackArgs>...>;
        using FunctionResultType = CallbackRes;


        ///Invoke setup with resuming function that remembers arguments and returns value
        template<class AwaitableToPlaceResult, class Value>
        static bool SetupCBReturnsValue(
            AwaitableToPlaceResult* self,
            std::coroutine_handle<> handle,
            const LambdaType& setupLambda,
            Value&& result
        ){
            setupLambda(
                self->Decorate(
                    [   =,
                        //captured copy shall be able to exist a for long time
                        storedResult=std::remove_reference_t<Value>(
                            std::forward<Value>(result)
                        )
                    ](CallbackArgs... args){
                        /* copied before the coroutine is resumed: code resumed in the coroutine
                           may destroy this very lambda (frees the trampoline node, unsubscribes
                           or replaces the std::function holding the lambda) */
                        auto resultForCaller = storedResult;
                        self->EmplaceCoResult(std::forward<CallbackArgs>(args)...); //what will be returned to coroutine
                        ResumeAndPropagateException(handle);
                        return resultForCaller;
                    }
                )
            );
            return true; //means stay suspended (this is ok even if already resumed)
        }

        ///Invoke setup with subscribing function
        template<class AwaitableToPlaceResult, class Value>
        static auto SubscribeCBReturnsValue(
            AwaitableToPlaceResult* self,
            const LambdaType& setupLambda,
            Value&& result
        ){
            setupLambda(
                self->Decorate(
                    [   =,
                        //captured copy shall be able to exist a for long time
                        storedResult=std::remove_reference_t<Value>(
                            std::forward<Value>(result)
                        )
                    ](CallbackArgs... args){
                        /* copied before the coroutine is resumed: code resumed in the coroutine
                           may destroy this very lambda (frees the trampoline node, unsubscribes
                           or replaces the std::function holding the lambda) */
                        auto resultForCaller = storedResult;
                        self->OnArgumentsArrived(std::forward<CallbackArgs>(args)...); //what will be returned to coroutine
                        return resultForCaller;
                    }
                )
            );
            // returns cleanup API that does nothing
            return [](){};
        }

        //Invoke setup with resuming function that remembers argument and calculates result
        template<class AwaitableToPlaceResult, class Function>
        static bool SetupCBReturnsResultOfCall(
            AwaitableToPlaceResult* self,
            std::coroutine_handle<> handle,
            const LambdaType& setupLambda,
            Function&& resultGetter
        ){
            setupLambda(
                self->Decorate(
                    [   =,
                        //captured copy shall be able to exist a for long time
                        calculateResult=std::remove_reference_t<Function>(
                            std::forward<Function>(resultGetter)
                        )
                    ](CallbackArgs... args){
                        /* copied before the coroutine is resumed: code resumed in the coroutine
                           may destroy this very lambda (frees the trampoline node, unsubscribes
                           or replaces the std::function holding the lambda) */
                        auto calculateResultForCaller = calculateResult;
                        self->EmplaceCoResult(std::forward<CallbackArgs>(args)...); //what will be returned to coroutine
                        ResumeAndPropagateException(handle);
                        return calculateResultForCaller();
                    }
                )
            );
            return true; //means stay suspended (this is ok even if already resumed)
        }
    };


    //__________________________________________________________________________
    // redirect other possible signatures for std::function

    template<class OperatorResult, class LambdaType, class FunctionSignature>
    struct GenerateForSetupMethod<
        OperatorResult(LambdaType::*)( std::function<FunctionSignature> ) const
    >
        : public GenerateForSetupMethod<
            OperatorResult(LambdaType::*)( std::function<FunctionSignature> )
        > {};

    template<class OperatorResult, class LambdaType, class FunctionSignature>
    struct GenerateForSetupMethod<
        OperatorResult(LambdaType::*)( const std::function<FunctionSignature>& )
    >
        : public GenerateForSetupMethod<
            OperatorResult(LambdaType::*)( std::function<FunctionSignature> )
        > {};

    template<class OperatorResult, class LambdaType, class FunctionSignature>
    struct GenerateForSetupMethod<
        OperatorResult(LambdaType::*)( const std::function<FunctionSignature>& ) const
    >
        : public GenerateForSetupMethod<
            OperatorResult(LambdaType::*)( std::function<FunctionSignature> )
        > {};


    //__________________________________________________________________________
    // Raw callback reuses logic applicable to std::function

    template<class OperatorResult, class LambdaType, class CallbackRes, class... CallbackArgs>
    struct GenerateForSetupMethod<
        OperatorResult(LambdaType::*)( CallbackRes(*)(CallbackArgs...) )
    >
        : public GenerateForSetupMethod<
            OperatorResult(LambdaType::*)( std::function<CallbackRes(CallbackArgs...)> )
        >
    {
        using RawPointerCallbackType = CallbackRes(*)(CallbackArgs...);
    };
    template<class OperatorResult, class LambdaType, class CallbackRes, class... CallbackArgs>
    struct GenerateForSetupMethod<
        OperatorResult(LambdaType::*)( CallbackRes(*)(CallbackArgs...) ) const
    >
        : public GenerateForSetupMethod<
            OperatorResult(LambdaType::*)( std::function<CallbackRes(CallbackArgs...)> )
        >
    {
        using RawPointerCallbackType = CallbackRes(*)(CallbackArgs...);
    };


    //__________________________________________________________________________
    //##########################################################################
    // Awaitables to be returned by invert_* functions

    /* Note: there are two possible approaches"
    1.  Awaitable as "thenable", here "setup" can happen before coroutine suspends
        and so Awaitable shall store possible concurrent execution of callback
        Pros: callbacks can execute before suspend
        Cons: complexity due to concurrent execution
    2. Awaitable does setup only once ready to resume
        Pros: simplicity, no concurrent execution
        Cons: coroutine must always suspend

    Decision: use option 2 as a more simple one
    */

    //__________________________________________________________________________
    // Awaitable for generating callback that does not return a value

    /** Awaitable being used by invert_function (generated callback returns nothing).
    Setup lambda is alive as long as full-expression exists,
    thus both setup lambda and Awaitable instance coexist simultaneously,
    see https://stackoverflow.com/questions/62620147/c-coroutines-temporaries-in-co-await-expressions
    for more details (temporaries are destroyed at the end of the full-expression) */
    template<
        class Decorator,
        class CoResult,
        class SetupLambdaType
    >
    class AwaitableGeneratedCBNoResult: public Decorator{
    public:
        /// Tie with setup lambda
        AwaitableGeneratedCBNoResult(const SetupLambdaType& setupLambdaToUse)
            : setupLambda(setupLambdaToUse) {}

        /// Suspends (await_suspend must see handle before setup lambda is invoked)
        constexpr bool await_ready() const noexcept { return false;  }

        /// Pass awaiting handle to be used once callback issued
        bool await_suspend(
            std::coroutine_handle<> handle ///< represent the current coroutine
        ) {
            return GenerateForSetupLambda<SetupLambdaType>
                ::SetupCBNoResult(
                    this, handle, setupLambda
                );
        }

        /// The value, that goes to coroutine as a result of co_await
        CoResult await_resume() noexcept {
            return std::move(*result);
        }


        /// To be called from "somewhere else" once result is ready
        template<class... CallbackArgs>
        void EmplaceCoResult(CallbackArgs&&... args){
            result.emplace( std::forward<CallbackArgs>(args)... );
        }

    private:
        /// Link to corresponding setup
        const SetupLambdaType& setupLambda;

        static_assert(!std::is_same_v<CoResult, void>);

        /// The result being calculated "somewhere else"
        std::optional<CoResult> result;
    };

    /// Specialization for awaitable being used by invert_function (generated callback returns nothing)
    template<class Decorator, class SetupLambdaType>
    class AwaitableGeneratedCBNoResult<Decorator, void, SetupLambdaType>: public Decorator{
    public:
        /// Tie with setup lambda
        AwaitableGeneratedCBNoResult(const SetupLambdaType& setupLambdaToUse)
            : setupLambda(setupLambdaToUse) {}

        /// Always suspends (shall not skip await_suspend)
        constexpr bool await_ready() const noexcept { return false;  }

        /// Pass awaiting handle to be used once callback issued
        bool await_suspend(
            std::coroutine_handle<> handle ///< represent the current coroutine
        ) {
            return GenerateForSetupLambda<SetupLambdaType>
                ::SetupCBNoResult(
                    this, handle, setupLambda
                );
        }

        /// Returns nothing to coroutine
        constexpr void await_resume() const noexcept {}

    private:
        /// Link to corresponding setup
        const SetupLambdaType& setupLambda;
    };


    //__________________________________________________________________________
    // Awaitable for generating callback that returns predefined value

    /** Awaitable being used by invert_function (case of value).
    Setup lambda is alive as long as full-expression exists,
    thus both setup lambda and Awaitable instance coexist simultaneously,
    see https://stackoverflow.com/questions/62620147/c-coroutines-temporaries-in-co-await-expressions
    for more details (temporaries are destroyed at the end of the full-expression) */
    template<
        class Decorator,
        class CoResult,
        class SetupLambdaType,
        class GeneratedCallbackResultType
    >
    class AwaitableGeneratedCBReturnsValue: public Decorator{
    public:
        /// Tie with setup lambda
        template<class ValueType>
        AwaitableGeneratedCBReturnsValue(
            const SetupLambdaType& setupLambdaToUse,
            ValueType&& whatGeneratedCallbackReturns
        )
            : setupLambda(setupLambdaToUse)
            , valueGeneratedCallbackReturns(std::forward<ValueType>(whatGeneratedCallbackReturns))
        {}

        //non copyable but movable
        AwaitableGeneratedCBReturnsValue(const AwaitableGeneratedCBReturnsValue&) = delete;
        AwaitableGeneratedCBReturnsValue& operator=(const AwaitableGeneratedCBReturnsValue&) = delete;
        AwaitableGeneratedCBReturnsValue(AwaitableGeneratedCBReturnsValue&&) = default;
        AwaitableGeneratedCBReturnsValue& operator=(AwaitableGeneratedCBReturnsValue&&) = default;

        /// Suspends (await_suspend must see handle before setup lambda is invoked)
        constexpr bool await_ready() const noexcept { return false;  }

        /// Pass awaiting handle to be used once callback issued
        bool await_suspend(
            std::coroutine_handle<> handle ///< represent the current coroutine
        ) {
            return GenerateForSetupLambda<SetupLambdaType>
                ::SetupCBReturnsValue(
                    this, handle, setupLambda,
                    std::move(valueGeneratedCallbackReturns) //definitely we do not need that value any more
                );
        }

        /// The value, that goes to coroutine as a result of co_await
        CoResult await_resume() noexcept {
            return std::move(*result);
        }


        /// To be called from "somewhere else" once result is ready
        template<class... CallbackArgs>
        void EmplaceCoResult(CallbackArgs&&... args){
            result.emplace( std::forward<CallbackArgs>(args)... );
        }

    private:
        /// Link to corresponding setup
        const SetupLambdaType& setupLambda;
        /** Exactly the same thing as is was received by invert_function.
        Reference is valid since awaitable exists as long as awaitable
        (for the time of full expression) */
        std::remove_reference_t<GeneratedCallbackResultType> valueGeneratedCallbackReturns;

        static_assert(!std::is_same_v<CoResult, void>);

        /// The result being calculated "somewhere else"
        std::optional<CoResult> result;
    };

    /// Specialization for waitable being used by invert_function (case of value)
    template<class Decorator, class SetupLambdaType, class GeneratedCallbackResultType>
    class AwaitableGeneratedCBReturnsValue<
        Decorator, void, SetupLambdaType, GeneratedCallbackResultType
    >: public Decorator{
    public:
        /// Tie with setup lambda
        template<class ValueType>
        AwaitableGeneratedCBReturnsValue(
            const SetupLambdaType& setupLambdaToUse,
            ValueType&& whatGeneratedCallbackReturns
        )
            : setupLambda(setupLambdaToUse)
            , valueGeneratedCallbackReturns(std::forward<ValueType>(whatGeneratedCallbackReturns))
        {}

        //non copyable but movable
        AwaitableGeneratedCBReturnsValue(const AwaitableGeneratedCBReturnsValue&) = delete;
        AwaitableGeneratedCBReturnsValue& operator=(const AwaitableGeneratedCBReturnsValue&) = delete;
        AwaitableGeneratedCBReturnsValue(AwaitableGeneratedCBReturnsValue&&) = default;
        AwaitableGeneratedCBReturnsValue& operator=(AwaitableGeneratedCBReturnsValue&&) = default;

        /// Always suspends (shall not skip await_suspend)
        constexpr bool await_ready() const noexcept { return false;  }

        /// Pass awaiting handle to be used once callback issued
        bool await_suspend(
            std::coroutine_handle<> handle ///< represent the current coroutine
        ) {
            return GenerateForSetupLambda<SetupLambdaType>
                ::SetupCBReturnsValue(
                    this, handle, setupLambda,
                    std::move(valueGeneratedCallbackReturns) //definitely we do not need that value any more
                );
        }

        /// Returns nothing to coroutine
        constexpr void await_resume() const noexcept {}

    private:
        /// Link to corresponding setup
        const SetupLambdaType& setupLambda;
        /** Exactly the same thing as is was received by invert_function.
        Reference is valid since awaitable exists as long as awaitable
            (for the time of full expression) */
        std::remove_reference_t<GeneratedCallbackResultType> valueGeneratedCallbackReturns;
    };


    //__________________________________________________________________________
    // Awaitable for generating callback that calls callback for result

    /** Awaitable being used by invert_function (case of callback returning result).
    Setup lambda is alive as long as full-expression exists,
    thus both setup lambda and Awaitable instance coexist simultaneously,
    see https://stackoverflow.com/questions/62620147/c-coroutines-temporaries-in-co-await-expressions
    for more details (temporaries are destroyed at the end of the full-expression) */
    template<
        class Decorator,
        class CoResult,
        class SetupLambdaType,
        class CallbackToCallFromGeneratedCallbackType
    >
    class AwaitableGeneratedCBReturnsResultOfCall: public Decorator{
    public:
        /// Tie with setup lambda
        template<class CalculateResultFunctionType>
        AwaitableGeneratedCBReturnsResultOfCall(
            const SetupLambdaType& setupLambdaToUse,
            CalculateResultFunctionType&& whatGeneratedCallbackReturns
        )
            : setupLambda(setupLambdaToUse)
            , callbackToCallFromGeneratedCallback(
                std::forward<CalculateResultFunctionType>(whatGeneratedCallbackReturns)
              )
        {}

        //non copyable but movable
        AwaitableGeneratedCBReturnsResultOfCall(const AwaitableGeneratedCBReturnsResultOfCall&) = delete;
        AwaitableGeneratedCBReturnsResultOfCall& operator=(const AwaitableGeneratedCBReturnsResultOfCall&) = delete;
        AwaitableGeneratedCBReturnsResultOfCall(AwaitableGeneratedCBReturnsResultOfCall&&) = default;
        AwaitableGeneratedCBReturnsResultOfCall& operator=(AwaitableGeneratedCBReturnsResultOfCall&&) = default;

        /// Suspends (await_suspend must see handle before setup lambda is invoked)
        constexpr bool await_ready() const noexcept { return false;  }

        /// Pass awaiting handle to be used once callback issued
        bool await_suspend(
            std::coroutine_handle<> handle ///< represent the current coroutine
        ) {
            return GenerateForSetupLambda<SetupLambdaType>
                ::SetupCBReturnsResultOfCall(
                    this, handle, setupLambda,
                    std::move(callbackToCallFromGeneratedCallback)
                );
        }

        /// The value, that goes to coroutine as a result of co_await
        CoResult await_resume() noexcept {
            return std::move(*result);
        }


        /// To be called from "somewhere else" once result is ready
        template<class... CallbackArgs>
        void EmplaceCoResult(CallbackArgs&&... args){
            result.emplace( std::forward<CallbackArgs>(args)... );
        }

    private:
        /// Link to corresponding setup
        const SetupLambdaType& setupLambda;
        /** Exactly the same thing as is was received by invert_function.
        Reference is valid since awaitable exists as long as awaitable
        (for the time of full expression) */
        std::remove_reference_t<CallbackToCallFromGeneratedCallbackType> callbackToCallFromGeneratedCallback;

        static_assert(!std::is_same_v<CoResult, void>);

        /// The result being calculated "somewhere else"
        std::optional<CoResult> result;
    };

    /// Specialization for waitable being used by invert_function (case of callback returning result)
    template<class Decorator, class SetupLambdaType, class CallbackToCallFromGeneratedCallbackType>
    class AwaitableGeneratedCBReturnsResultOfCall<
        Decorator, void, SetupLambdaType, CallbackToCallFromGeneratedCallbackType
    >: public Decorator{
    public:
        /// Tie with setup lambda
        template<class CalculateResultFunctionType>
        AwaitableGeneratedCBReturnsResultOfCall(
            const SetupLambdaType& setupLambdaToUse,
            CalculateResultFunctionType&& whatGeneratedCallbackReturns
        )
            : setupLambda(setupLambdaToUse)
            , callbackToCallFromGeneratedCallback(
                std::forward<CalculateResultFunctionType>(whatGeneratedCallbackReturns)
              )
        {}

        //non copyable but movable
        AwaitableGeneratedCBReturnsResultOfCall(const AwaitableGeneratedCBReturnsResultOfCall&) = delete;
        AwaitableGeneratedCBReturnsResultOfCall& operator=(const AwaitableGeneratedCBReturnsResultOfCall&) = delete;
        AwaitableGeneratedCBReturnsResultOfCall(AwaitableGeneratedCBReturnsResultOfCall&&) = default;
        AwaitableGeneratedCBReturnsResultOfCall& operator=(AwaitableGeneratedCBReturnsResultOfCall&&) = default;

        /// Always suspends (shall not skip await_suspend)
        constexpr bool await_ready() const noexcept { return false;  }

        /// Pass awaiting handle to be used once callback issued
        bool await_suspend(
            std::coroutine_handle<> handle ///< represent the current coroutine
        ) {
            return GenerateForSetupLambda<SetupLambdaType>
                ::SetupCBReturnsResultOfCall(
                    this, handle, setupLambda,
                    std::move(callbackToCallFromGeneratedCallback)
                );
        }

        /// Returns nothing to coroutine
        constexpr void await_resume() const noexcept {}

    private:
        /// Link to corresponding setup
        const SetupLambdaType& setupLambda;
        /** Exactly the same thing as is was received by invert_function.
        Reference is valid since awaitable exists as long as awaitable
        (for the time of full expression) */
        std::remove_reference_t<CallbackToCallFromGeneratedCallbackType> callbackToCallFromGeneratedCallback;
    };


    //__________________________________________________________________________
    //##########################################################################
    // Support for callback_from_lambda


    template<class Res, class LambdaType, class... Args>
    struct ConvertSignature<Res(LambdaType::*)(Args...)>{
        /// Signature as being passed to target API
        using PlainFunctionSignature = Res(Args...);
        /// Signature as being declared (to control lifetime)
        using LifetimeAwareSignature = Res(Args...);
    };
    template<class Res, class LambdaType, class... Args>
    struct ConvertSignature<Res(LambdaType::*)(Args...) const>{
        /// Signature as being passed to target API
        using PlainFunctionSignature = Res(Args...);
        /// Signature as being declared (to control lifetime)
        using LifetimeAwareSignature = Res(Args...);
    };

    template<class Res, class LambdaType, class... Args>
    struct ConvertSignature<Res(LambdaType::*)(callback_extend_lifetime&, Args...)>{
        /// Signature as being passed to target API
        using PlainFunctionSignature = Res(Args...);
        /// Signature as being declared (to control lifetime)
        using LifetimeAwareSignature = Res(callback_extend_lifetime&, Args...);
    };
    template<class Res, class LambdaType, class... Args>
    struct ConvertSignature<Res(LambdaType::*)(callback_extend_lifetime&, Args...) const>{
        /// Signature as being passed to target API
        using PlainFunctionSignature = Res(Args...);
        /// Signature as being declared (to control lifetime)
        using LifetimeAwareSignature = Res(callback_extend_lifetime&, Args...);
    };


    /// Access for apply for creating callback_extend_lifetime
    class CallbackExtendLifetimeImpl: public callback_extend_lifetime{
    public:
        /// Make constructor visible for apply
        CallbackExtendLifetimeImpl(){}
    };


    //__________________________________________________________________________
    // callback_from_lambda: nodes and generated API

    /// General node for lambda allocation (chain of nodes is generated in LambdaListNodeGenerator)
    template<class LambdaType, class AdditionalOptionalTag, void (*freeMe)(void*)>
    struct LambdaListNode{
        /// Signature for corresponding "plain function pointer"
        using PlainFunctionSignature = PlainFunctionSignatureFromLambda<LambdaType>;

        /// Create general allocation node
        LambdaListNode(
            PlainFunctionSignature* individualTrampolineGenerated,
            LambdaListNode* nextNode
        )
            : individualTrampoline(individualTrampolineGenerated), next(nextNode) {}

        /// Destructor to do nothing (assume there is no lambda)
        ~LambdaListNode(){}

        /// Hold corresponding "plain function pointer"/callback when is free
        PlainFunctionSignature* const individualTrampoline;


        union{
            /// Lambda being wrapped when is allocated
            LambdaType lambda;

            /// Link to next node for lambda
            LambdaListNode* next;
        };

        /// Only the one who instantiated or is aware how to free that node
        void Free(){
            freeMe(this);
        }
    };


    /// Meta function to obtain code for LambdaType::operator()
    template<class LambdaListNodeType, class MethodSignature>
    struct ObtainAutoFreePlainFunction;

    /// Specialization to obtain code for LambdaType::operator()
    template<class LambdaListNodeType, class Res, class LambdaType, class... Args>
    struct ObtainAutoFreePlainFunction<LambdaListNodeType, Res(LambdaType::*)(Args...)>{
        template<LambdaListNodeType* self>
        struct For{
            static Res apply(Args... args){
                // moved copy is needed to allow recursive allocation
                auto copyOfLambda = static_cast<LambdaType&&>(
                    self->lambda
                );

                // free corresponding node
                self->Free();

                //call the mover copy
                return copyOfLambda(static_cast<Args&&>(args)...);
            }
        };
    };
    /// Specialization to obtain code for LambdaType::operator() const
    template<class LambdaListNodeType, class Res, class LambdaType, class... Args>
    struct ObtainAutoFreePlainFunction<LambdaListNodeType, Res(LambdaType::*)(Args...) const>:
        public ObtainAutoFreePlainFunction<LambdaListNodeType, Res(LambdaType::*)(Args...)> //reuse existing
    {};

    /// Specialization to obtain code for lambda that controls own lifetime
    template<class LambdaListNodeType, class Res, class LambdaType, class... Args>
    struct ObtainAutoFreePlainFunction<LambdaListNodeType, Res(LambdaType::*)(callback_extend_lifetime&, Args...)>{
        template<LambdaListNodeType* self>
        struct For{
            static Res apply(Args... args){
                CallbackExtendLifetimeImpl extendLifetime;
                Res res = self->lambda(extendLifetime, static_cast<Args&&>(args)...);

                if( extendLifetime.is_disposed() ){
                    // free corresponding node
                    self->Free();
                }

                return res;
            }
        };
    };
    /// Specialization to obtain code for void lambda that controls own lifetime
    template<class LambdaListNodeType, class LambdaType, class... Args>
    struct ObtainAutoFreePlainFunction<LambdaListNodeType, void(LambdaType::*)(callback_extend_lifetime&, Args...)>{
        template<LambdaListNodeType* self>
        struct For{
            static void apply(Args... args){
                CallbackExtendLifetimeImpl extendLifetime;
                self->lambda(extendLifetime, static_cast<Args&&>(args)...);

                if( extendLifetime.is_disposed() ){
                    // free corresponding node
                    self->Free();
                }
            }
        };
    };
    /// Specialization to obtain code for lambda that controls own lifetime
    template<class LambdaListNodeType, class Res, class LambdaType, class... Args>
    struct ObtainAutoFreePlainFunction<LambdaListNodeType, Res(LambdaType::*)(callback_extend_lifetime&, Args...) const>:
        public ObtainAutoFreePlainFunction<LambdaListNodeType, Res(LambdaType::*)(callback_extend_lifetime&, Args...)>
    {};

    /// Meta function to obtain code for LambdaType
    template<class LambdaListNodeType, class LambdaType, LambdaListNodeType* obj>
    using ObtainAutoFreePlainFunctionFor = typename
        ObtainAutoFreePlainFunction<LambdaListNodeType, decltype(&LambdaType::operator())>::template For<obj>;


    // special case of long leaving nodes

    /// Meta function to obtain code for LambdaType::operator()
    template<class LambdaListNodeType, class MethodSignature>
    struct ObtainNoAutoFreePlainFunction;

    /// Specialization to obtain code for LambdaType::operator()
    template<class LambdaListNodeType, class Res, class LambdaType, class... Args>
    struct ObtainNoAutoFreePlainFunction<LambdaListNodeType, Res(LambdaType::*)(Args...)>{
        template<LambdaListNodeType* self>
        struct For{
            static Res apply(Args... args){
                /* Call in place, the way the lifetime aware case above does.
                   No "moved copy" here on purpose: nothing is destructed by
                   this call, so the node keeps owning its lambda, and moving
                   that lambda out would leave whatever it captured by value
                   in a "moved from" state for every next call */
                return self->lambda(
                    static_cast<Args&&>(args)...
                );
            }
        };
    };
    /// Specialization to obtain code for LambdaType::operator() const
    template<class LambdaListNodeType, class Res, class LambdaType, class... Args>
    struct ObtainNoAutoFreePlainFunction<LambdaListNodeType, Res(LambdaType::*)(Args...) const>:
        public ObtainNoAutoFreePlainFunction<LambdaListNodeType, Res(LambdaType::*)(Args...)> //reuse existing
    {};

    /// Meta function to obtain code for LambdaType
    template<class LambdaListNodeType, class LambdaType, LambdaListNodeType* obj>
    using ObtainNoAutoFreePlainFunctionFor = typename
        ObtainNoAutoFreePlainFunction<LambdaListNodeType, decltype(&LambdaType::operator())>::template For<obj>;


    //__________________________________________________________________________
    // callback_from_lambda: generate linked list of nodes


    /// Meta function to generate linked list of LambdaListNode and corresponding trampolines
    template<
        class LambdaType, class AdditionalOptionalTag, void (*freeMe)(void*),
        template<class ObtainNode, class ObtainLambda, ObtainNode* obj> class ObtainPlainFunction, //Plain function is obtained here
        unsigned reservedCount
    >
    struct LambdaListNodeGenerator;

    /// Generate source for individual caller (all except first)
    template<
        class LambdaType, class AdditionalOptionalTag, void (*freeMe)(void*),
        template<class ObtainNode, class ObtainLambda, ObtainNode* obj> class ObtainPlainFunction,
        unsigned reservedCount
    >
    struct LambdaListNodeGenerator{
        /* Backstop for the recursion below:\
        reservedCount == 1 has its own specialization (and goes there without recursion),
        so only 2 and above may reach this primary template
        (and 0 must never get here, it would wrap around in reservedCount-1) */
        static_assert(
            reservedCount > 1,
            "reservedCount shall be at least 1\n"
            "(a pool of 0 callbacks is unable to allocate anything)"
        );

        /// Node to store corresponding lambda
        static LambdaListNode<LambdaType, AdditionalOptionalTag, freeMe> node;
    };
    /// Generate source for individual caller (the first one)
    template<
        class LambdaType, class AdditionalOptionalTag, void (*freeMe)(void*),
        template<class ObtainNode, class ObtainLambda, ObtainNode* obj> class ObtainPlainFunction
    >
    struct LambdaListNodeGenerator<LambdaType, AdditionalOptionalTag, freeMe, ObtainPlainFunction, 1>{
        /// Node to store corresponding lambda
        static LambdaListNode<LambdaType, AdditionalOptionalTag, freeMe> node;
    };

    //The last node storing corresponding lambda is nas no "next" node
    template<
        class LambdaType, class AdditionalOptionalTag, void (*freeMe)(void*),
        template<class ObtainNode, class ObtainLambda, ObtainNode* obj> class ObtainPlainFunction
    >
    LambdaListNode<LambdaType, AdditionalOptionalTag, freeMe>
    LambdaListNodeGenerator<LambdaType, AdditionalOptionalTag, freeMe, ObtainPlainFunction, 1>::node{
        &ObtainPlainFunction<
            LambdaListNode<LambdaType, AdditionalOptionalTag, freeMe>,
            LambdaType,
            &LambdaListNodeGenerator<LambdaType, AdditionalOptionalTag, freeMe, ObtainPlainFunction, 1>::node
        >::apply,
        nullptr // no "next" node
    };
    //All the rest of the nodes reference
    template<
        class LambdaType, class AdditionalOptionalTag, void (*freeMe)(void*),
        template<class ObtainNode, class ObtainLambda, ObtainNode* obj> class ObtainPlainFunction,
        unsigned reservedCount
    >
    LambdaListNode<LambdaType, AdditionalOptionalTag, freeMe>
    LambdaListNodeGenerator<LambdaType, AdditionalOptionalTag, freeMe, ObtainPlainFunction, reservedCount>::node{
        /* Each node needs a trampoline of its own, tied to that very node
           (pointing all of them to node 1 would make every generated callback
            operate on the same storage) */
        &ObtainPlainFunction<
            LambdaListNode<LambdaType, AdditionalOptionalTag, freeMe>,
            LambdaType,
            &LambdaListNodeGenerator<
                LambdaType, AdditionalOptionalTag, freeMe, ObtainPlainFunction, reservedCount
            >::node
        >::apply,
        // chain to the rest of the free list: reservedCount -> ... -> 1 -> nullptr
        &LambdaListNodeGenerator<
            LambdaType, AdditionalOptionalTag, freeMe, ObtainPlainFunction, reservedCount-1
        >::node
    };


    //__________________________________________________________________________
    // callback_from_lambda: managing nodes that are able to free self

    /// CallbackAllocator encapsulating linked list operations
    template<
        unsigned reservedCount, class AdditionalOptionalTag, class LambdaType,
        template<class ObtainNode, class ObtainLambda, ObtainNode* obj> class ObtainPlainFunction
    >
    class CallbackAllocator{
        /* Guard the single choke point every path goes through, otherwise
           reservedCount == 0 wraps around in reservedCount-1 below and the
           node generator recurses until the compiler gives up */
        static_assert(
            reservedCount > 0,
            "reservedCount shall be at least 1\n"
            "(a pool of 0 callbacks is unable to allocate anything)"
        );

        /// Freeing node (to be passed as template parameter)
        static void FreeNode(void* rawNode);

        /// Cut off references to ger original lambda type
        using OriginalLambda = std::remove_reference_t<LambdaType>;
    public:
        /// Type of nodes used for allocation
        using Node = LambdaListNode<LambdaType, AdditionalOptionalTag, FreeNode>;

        /// Allocate item that forwards from plain function to lambda
        template<class UsedLambda>
        static Node* Allocate(UsedLambda&& lambda){
            Node* allocatedNode;

            {std::lock_guard<std::mutex> lock(protectAllocation);
                if( !first ){
                    throw std::runtime_error("callback_from_lambda cannot allocate trampoline");
                }
                allocatedNode = first;
                // step over allocated item
                first = first->next;
            }
            // lambda is active instead of next
            new( &allocatedNode->lambda ) OriginalLambda(
                std::forward<UsedLambda>(lambda)
            );

            return allocatedNode;
        }

    private:
        /// Generator to use for obtaining linked list of nodes
        using Generator = LambdaListNodeGenerator<
            LambdaType, AdditionalOptionalTag, FreeNode,
            ObtainPlainFunction,
            reservedCount
        >;

        /// Head of the list
        static Node* first;

        /// Ensure it possible to allocate from different threads
        static std::mutex protectAllocation;
    };

    template<
        unsigned reservedCount, class AdditionalOptionalTag, class LambdaType,
        template<class ObtainNode, class ObtainLambda, ObtainNode* obj> class ObtainPlainFunction
    >
    typename CallbackAllocator<reservedCount, AdditionalOptionalTag, LambdaType, ObtainPlainFunction>::Node*
        CallbackAllocator<reservedCount, AdditionalOptionalTag, LambdaType, ObtainPlainFunction>::first =
            &CallbackAllocator<reservedCount, AdditionalOptionalTag, LambdaType, ObtainPlainFunction>::Generator::node;

    template<
        unsigned reservedCount, class AdditionalOptionalTag, class LambdaType,
        template<class ObtainNode, class ObtainLambda, ObtainNode* obj> class ObtainPlainFunction
    >
    inline void CallbackAllocator<reservedCount, AdditionalOptionalTag, LambdaType, ObtainPlainFunction>::FreeNode(void* rawNode){
        auto node = static_cast<Node*>(rawNode);

        //Destruct existing lambda before using next field
        node->lambda.~LambdaType();

        {std::lock_guard<std::mutex> lock(protectAllocation);
            node->next = first;
            first = node;
        }
    }

    // additional static member is needed for EnterCritical/LeaveCritical
    template<
        unsigned reservedCount, class AdditionalOptionalTag, class LambdaType,
        template<class ObtainNode, class ObtainLambda, ObtainNode* obj> class ObtainPlainFunction
    >
    std::mutex
    CallbackAllocator<reservedCount, AdditionalOptionalTag, LambdaType, ObtainPlainFunction>::protectAllocation;


    // now specialization for the case of one node
    // for debugging purposes #define COROFORK_SUPPRESS_SPECIALIZATION
#   ifndef COROFORK_SUPPRESS_SPECIALIZATION

    /// CallbackAllocator with single item
    template<
        class AdditionalOptionalTag, class LambdaType,
        template<class ObtainNode, class ObtainLambda, ObtainNode* obj> class ObtainPlainFunction
    >
    class CallbackAllocator<1, AdditionalOptionalTag, LambdaType, ObtainPlainFunction>{
        /// Freeing node (to be passed as template parameter)
        static void FreeSingleNode(void* rawNode);

        /// Cut off references to ger original lambda type
        using OriginalLambda = std::remove_reference_t<LambdaType>;
    public:
        /// Type of nodes used for allocation
        using Node = LambdaListNode<LambdaType, AdditionalOptionalTag, FreeSingleNode>;

        /// Allocate item that forwards from plain function to lambda
        template<class UsedLambda>
        static Node* Allocate(UsedLambda&& lambda){
            if( isAllocated ){
                throw std::runtime_error("callback_from_lambda cannot allocate trampoline");
            }
            isAllocated = true;

            // lambda is active instead of next
            new( &theOnlyNode.lambda ) OriginalLambda(
                std::forward<UsedLambda>(lambda)
            );

            // Returns well known generated node
            return &theOnlyNode;
        }

    private:
        /// Single item owned by "one item capacity allocator"
        static Node theOnlyNode;

        /** Hack for asserting existing allocation.
        ignore thread safety here))
        Used only for assertion purposes with single item allocation.
        We assume one will not use single item allocation in case
        of multiple threads */
        static bool isAllocated;
    public:
        using ObtainPlainFunctionShortcut = ObtainPlainFunction<
            Node,
            LambdaType,
            &CallbackAllocator<1, AdditionalOptionalTag, LambdaType, ObtainPlainFunction>::theOnlyNode
        >;
    };

    template<
        class AdditionalOptionalTag, class LambdaType,
        template<class ObtainNode, class ObtainLambda, ObtainNode* obj> class ObtainPlainFunction
    >
    typename CallbackAllocator<1, AdditionalOptionalTag, LambdaType, ObtainPlainFunction>::Node
        CallbackAllocator<1, AdditionalOptionalTag, LambdaType, ObtainPlainFunction>::theOnlyNode(
            &CallbackAllocator<1, AdditionalOptionalTag, LambdaType, ObtainPlainFunction>::ObtainPlainFunctionShortcut::apply,
            nullptr
        );

    template<
        class AdditionalOptionalTag, class LambdaType,
        template<class ObtainNode, class ObtainLambda, ObtainNode* obj> class ObtainPlainFunction
    >
    bool CallbackAllocator<1, AdditionalOptionalTag, LambdaType, ObtainPlainFunction>::isAllocated = false;

    template<
        class AdditionalOptionalTag, class LambdaType,
        template<class ObtainNode, class ObtainLambda, ObtainNode* obj> class ObtainPlainFunction
    >
    inline void CallbackAllocator<1, AdditionalOptionalTag, LambdaType, ObtainPlainFunction>::FreeSingleNode(void* /*rawNode*/){
        //Destruct existing lambda before using next field
        theOnlyNode.lambda.~LambdaType();

        //Mark existing only node as free
        isAllocated = false;
    }
#   endif

    //__________________________________________________________________________
    // Implement utility stuff

    /// Default customization for generated callback (no custom wrapping)
    struct TheSame{
        /// Returns argument as is
        template<class WrappedCallback>
        constexpr std::remove_reference_t<WrappedCallback>&&
        Decorate(WrappedCallback&& cb) noexcept{
            return std::move(cb);
        }
    };

    /// Customization that turns std::function to "plain C" callback
    template<
        std::size_t maxCallbacksCount,
        class AdditionalOptionalTag
    >
    class ToPlainC{
        using KeepAlive = std::unique_ptr<void, void(*)(void*)>;
    public:
        /**Return argument wrapped into "plain C" callback.
        Not noexcept on purpose: CallbackAllocator::Allocate throws
        std::runtime_error once all maxCallbacksCount trampolines are in use,
        the exception leaves await_suspend and is rethrown by the co_await
        expression inside the coroutine */
        template<class WrappedCallback>
        auto Decorate(WrappedCallback&& cb){
            using Allocator = CallbackAllocator<
                maxCallbacksCount,
                AdditionalOptionalTag,
                std::remove_reference_t<WrappedCallback>,
                ObtainNoAutoFreePlainFunctionFor
            >;
            auto node = Allocator::Allocate(
                std::forward<WrappedCallback>(cb)
            );
            keepAlive = KeepAlive(
                node,
                [](void* allocation){
                    static_cast<Allocator::Node*>(allocation)->Free();
                }
            );

            return node->individualTrampoline;
        }
    private:
        /// Ensures callback will be valid as long as ToPlainC exists
        KeepAlive keepAlive{nullptr, [](void*){}};
    };


    //__________________________________________________________________________
    //##########################################################################
    // Support for invert_subscription

    /// Internal class to hold subscription as long as instance exists
    template<class Decorator, class AwaitedType>
    class InvertSubscriptionHolder: public Decorator{
    public:
        /** Setup generated subscription whose callback returns nothing.
        See corresponding invert_subscription */
        template<CoroForkLambdaToSetupFunction LambdaToSetupFunction>
        InvertSubscriptionHolder(
            const LambdaToSetupFunction& setupFunction ///< receives generated callback
        ):  finalCleanup( CoroFork_detail::GenerateForSetupLambda<LambdaToSetupFunction>
                            ::SubscribeCBNoResult(this, setupFunction)
            ) {}

        /** Setup generated subscription whose callback returns a value.
        See corresponding invert_subscription,
        (Remember here callback returns result immediately once issued
         and not when value is actually awaited) */
        template<
            CoroForkLambdaToSetupFunction LambdaToSetupFunction,
            class ResultForCallbackFunctionAsValue
        >
        requires
            std::convertible_to<
                std::remove_cvref_t<ResultForCallbackFunctionAsValue>,
                std::remove_cvref_t<
                    typename CoroFork_detail::GenerateForSetupLambda<LambdaToSetupFunction>::FunctionResultType
                >
            >
        InvertSubscriptionHolder(
            const LambdaToSetupFunction& setupFunction, ///< Your setup
            ResultForCallbackFunctionAsValue&& resultForCallbackFunction
        ):  finalCleanup( CoroFork_detail::GenerateForSetupLambda<LambdaToSetupFunction>
                ::SubscribeCBReturnsValue(
                    this,
                    setupFunction,
                    std::forward<ResultForCallbackFunctionAsValue>(
                        resultForCallbackFunction
                    )
                )
            ) {}

        /// Issue cleanup procedure if it was provided
        ~InvertSubscriptionHolder(){
            finalCleanup(); //safe, "do nothing" is placed here if there is no cleanup
        }

        /// Number of items accumulated by this subscription so far
        std::size_t count() const{
            //this is not called simultaneously with await))
            std::lock_guard lockEventsQueue(protectEventsQueue);
            return eventsQueue.size();
        }

        // exists only in one place (but takes advantage of RVO!)
        InvertSubscriptionHolder(const InvertSubscriptionHolder&) = delete;
        InvertSubscriptionHolder& operator=(const InvertSubscriptionHolder&) = delete;


        /// Awaitable being used for obtaining subscription data
        class awaitable{
        public:
            // can move, but cannot copy
            awaitable(const awaitable&&);
            awaitable& operator=(const awaitable&&);
            awaitable(const awaitable&) = delete;
            awaitable& operator=(const awaitable&) = delete;

            /** Awaitable is ready once result is available from the queue.
            \returns true if result is already there
            false if we need to suspend (callback was not issued yet) */
            bool await_ready() noexcept{
                // we still own the lock over holder->protectEventsQueue
                if( holder->eventsQueue.empty() ){
                    /* no items so far, need to suspend, lock is still here
                       we will enter into await_suspend owning the lock */
                    return false;
                }
                //extract one item from the queue to result
                result.emplace( std::move(holder->eventsQueue.front()) );
                holder->eventsQueue.pop();

                //no need to lock anymore (this awaitable is "already used")
                lockEventsQueue.unlock();

                /* coroutine will now pick up the result via await_resume
                  await_suspend is not called in this case
                  (we do only await_resume that returns ready to use result) */
                return true;
            }

            /// Suspend until result is available
            void await_suspend(std::coroutine_handle<> handleToResume){
                /* we still own the lock over holder->protectEventsQueue
                   so result was not placed here for sure! */
                handle = handleToResume; //will be used by OnArgumentsArrived
                holder->whoAwaitsNow = this; //sign of "write result directly to me"

                //now it is allowed for other threads to push arguments to the queue
                lockEventsQueue.unlock();
            }

            /// Get the result once it is available
            AwaitedType await_resume() noexcept{
                /* No need to lock here we own the result
                   we are sure this API is called once coroutine continues */
                return std::move(*result);
            }

        private:
            // Instances are created only by InvertSubscriptionHolder
            friend class InvertSubscriptionHolder;
            /// Constructor for awaitable tied to InvertSubscriptionHolder
            awaitable(InvertSubscriptionHolder* correspondingHolder)
                : lockEventsQueue(correspondingHolder->protectEventsQueue)
                , holder(correspondingHolder)
                {}

            /// Ensure lock os obatined from the moment we are created
            std::unique_lock<std::mutex> lockEventsQueue;

            /// Subscription holder we use in awaitable
            InvertSubscriptionHolder* holder;

            /** What to resume once value arrives (used by InvertSubscriptionHolder).
            For scenario once there were no items */
            std::coroutine_handle<> handle;

            /** Result to be returned once it is available.
            Storing value here prevents extra locking */
            std::optional<AwaitedType> result;

            /// Called once result is available, but not under lock!
            void OnResultReady(){
                ResumeAndPropagateException(handle);
            }
        };

        /// Allow the same InvertSubscriptionHolder to be awaited multiple times
        awaitable operator co_await(){
            return awaitable(this);
        }

        /// To be called from "somewhere else" once result is ready
        template<class... CallbackArgs>
        void OnArgumentsArrived(CallbackArgs&&... args){
            awaitable* whoHasToBeResumed = nullptr;
            {
                std::lock_guard lockEventsQueue(protectEventsQueue);
                if( whoAwaitsNow ){
                    //will resume the one who waits right now
                    whoHasToBeResumed = whoAwaitsNow;
                    /* no need to store the result in the queue
                       place the result directly to the one who waits */
                    whoHasToBeResumed->result.emplace( std::forward<CallbackArgs>(args)... );

                    //that awaitable is "already used"
                    whoAwaitsNow = nullptr;
                }
                else{
                    //else means no one is waiting right now, save "for later"
                    eventsQueue.emplace( std::forward<CallbackArgs>(args)... );
                }
            }
            /* No need to lock here, we are sure this API is called once
               result is ready and threre is only one consumer (the coroutine).
               Coroutine co_alwait happen sequentially, so we can resume
               even if other thread is also pushing arguments */
            if( whoHasToBeResumed ){
                //doing this outside of lock to avoid deadlocks
                whoHasToBeResumed->OnResultReady();
            }
        }
    private:
        /// Called once InvertSubscriptionHolder goes out of scope
        std::function< void() > finalCleanup;

        /// Awaitable being used "right now" for obtaining subscription data
        awaitable* whoAwaitsNow = nullptr;

        /// Events that are available so far
        std::queue<AwaitedType> eventsQueue;

        /** Protect operations with eventsQueue.
        Subscription data may arrive from different threads */
        mutable std::mutex protectEventsQueue;
    };


    /// Hold subscription as long as instance exists (void specialization)
    template<class Decorator>
    class InvertSubscriptionHolder<Decorator, void>: public Decorator{
    public:
        /** Setup generated subscription whose callback returns nothing.
        See corresponding invert_subscription */
        template<CoroForkLambdaToSetupFunction LambdaToSetupFunction>
        InvertSubscriptionHolder(
            const LambdaToSetupFunction& setupFunction ///< receives generated callback
        ):  finalCleanup( CoroFork_detail::GenerateForSetupLambda<LambdaToSetupFunction>
                            ::SubscribeCBNoResult(this, setupFunction)
            ) {}

        /** Setup generated subscription whose callback returns a value.
        ee corresponding invert_subscription,
        (Remember here callback returns result immediately once issued
         and not when value is actually awaited) */
        template<
            CoroForkLambdaToSetupFunction LambdaToSetupFunction,
            class ResultForCallbackFunctionAsValue
        >
        requires
            std::convertible_to<
                std::remove_cvref_t<ResultForCallbackFunctionAsValue>,
                std::remove_cvref_t<
                    typename CoroFork_detail::GenerateForSetupLambda<LambdaToSetupFunction>::FunctionResultType
                >
            >
        InvertSubscriptionHolder(
            const LambdaToSetupFunction& setupFunction, ///< Your setup
            ResultForCallbackFunctionAsValue&& resultForCallbackFunction
        ):  finalCleanup( CoroFork_detail::GenerateForSetupLambda<LambdaToSetupFunction>
                ::SubscribeCBReturnsValue(
                    this,
                    setupFunction,
                    std::forward<ResultForCallbackFunctionAsValue>(
                        resultForCallbackFunction
                    )
                )
            ) {}

        /// Issue cleanup procedure if it was provided
        ~InvertSubscriptionHolder(){
            finalCleanup(); //safe, "do nothing" is placed here if there is no cleanup
        }

        /// Number of events accumulated by this subscription so far
        std::size_t count() const{
            //this is not called simultaneously with await))
            std::lock_guard lockEvents(protectEvents);
            return eventsCount;
        }

        // exists only in one place (but takes advantage of RVO!)
        InvertSubscriptionHolder(const InvertSubscriptionHolder&) = delete;
        InvertSubscriptionHolder& operator=(const InvertSubscriptionHolder&) = delete;


        /// Awaitable being used for obtaining subscription data
        class awaitable{
        public:
            // can move, but cannot copy
            awaitable(const awaitable&&);
            awaitable& operator=(const awaitable&&);
            awaitable(const awaitable&) = delete;
            awaitable& operator=(const awaitable&) = delete;

            /** Awaitable is ready once callback was already issued.
            \returns true if callback was already issued
            false if we need to suspend (callback was not issued yet) */
            bool await_ready() noexcept{
                // we still own the lock over holder->protectEventsQueue
                if( !holder->eventsCount ){
                    /* no events so far, need to suspend, lock is still here
                       we will enter into await_suspend owning the lock */
                    return false;
                }
                //one more event is consumed
                --holder->eventsCount;

                //no need to lock anymore (this awaitable is "already used")
                lockEvents.unlock();

                /* coroutine can continue execution right now
                  await_suspend is not called in this case */
                return true;
            }

            /// Suspend until callback is issued
            void await_suspend(std::coroutine_handle<> handleToResume){
                /* we still own the lock over holder->protectEventsQueue
                   so callback did not increment eventsCount here for sure! */
                handle = handleToResume; //will be used by OnEventHappened
                holder->whoAwaitsNow = this; //sign of "resume me directly"

                //now it is allowed for other threads to push arguments
                lockEvents.unlock();
            }

            /// Does nothing but must be formally present, to make C++20 happy
            constexpr void await_resume() const noexcept{}

        private:
            // Instaces are created only by InvertSubscriptionHolder
            friend class InvertSubscriptionHolder;
            /// Constructor for awaitable tied to InvertSubscriptionHolder
            awaitable(InvertSubscriptionHolder* correspondingHolder)
                : lockEvents(correspondingHolder->protectEvents)
                , holder(correspondingHolder)
                {}

            /// Ensure lock os obatined from the moment we are created
            std::unique_lock<std::mutex> lockEvents;

            /// Subscription holder we use in awaitable
            InvertSubscriptionHolder* holder;

            /** What to resume once value arrives (used by InvertSubscriptionHolder).
            For scenario once there were no items */
            std::coroutine_handle<> handle;

            /// Called once callback was issued, but not under lock!
            void OnEventHappened(){
                ResumeAndPropagateException(handle);
            }
        };

        /// Allow the same InvertSubscriptionHolder to be awaited multiple times
        awaitable operator co_await(){
            return awaitable(this);
        }

        /// To be called from "somewhere else" once callback is issued
        template<class... CallbackArgs>
        void OnEventHappened(CallbackArgs&&...){
            awaitable* whoHasToBeResumed = nullptr;
            {
                std::lock_guard lockEventsQueue(protectEvents);
                if( whoAwaitsNow ){
                    //will resume the one who waits right now
                    whoHasToBeResumed = whoAwaitsNow;
                    //that awaitable is "already used"
                    whoAwaitsNow = nullptr;
                }
                else{
                    //else means no one is waiting right now, save "for later"
                    ++eventsCount;
                }
            }
            /* No need to lock here, we are sure this API is called once
               callbach happened and threre is only one consumer (the coroutine).
               Coroutine co_alwait happen sequentially, so we can resume
               even if other thread is also pushing arguments */
            if( whoHasToBeResumed ){
                //doing this outside of lock to avoid deadlocks
                whoHasToBeResumed->OnEventHappened();
            }
        }
    private:
        /// Called once InvertSubscriptionHolder goes out of scope
        std::function< void() > finalCleanup;

        /// Awaitable being used "right now" for obtaining subscription data
        awaitable* whoAwaitsNow = nullptr;

        /// Events that are available so far
        unsigned eventsCount = 0;

        /**Protect operations with eventsQueue.
            Subscription data may arrive from different threads */
        mutable std::mutex protectEvents;
    };

} //namespace CoroFork_detail


template<unsigned reservedCount, class AdditionalOptionalTag, class UniqueLambdaType>
/* repeated on the definition intentionally: the attribute on the declaration
   alone is not honored for such split templates by all compilers */
[[nodiscard(
    "One shall use and call the result of callback_from_lambda,"
    " or else the reserved block pool slot is lost forever"
)]]
auto callback_from_lambda(UniqueLambdaType&& lambda) ->
    CoroFork_detail::PlainFunctionSignatureFromLambda<UniqueLambdaType>*
{
    using OriginalLambda = std::remove_reference_t<UniqueLambdaType>;

    static_assert(
            !CoroFork_detail::ConvertsToPlainFunctionCallback<OriginalLambda>
        &&  !CoroFork_detail::ConvertsToLifetimeAwareCallback<OriginalLambda>,
        "This lambda has no captures, so there is nothing to keep alive and\n"
        "callback_from_lambda has nothing to do here.\n"
        "Pass the lambda to the target API directly, as is: a capture less\n"
        "lambda already converts to a \"plain C\" callback that stays valid\n"
        "forever, with no trampoline, no block pool slot and no reservedCount\n"
        "to guess.\n"
        "If the lambda declares a callback_extend_lifetime& parameter, drop\n"
        "that parameter too: without captures there is no lifetime to extend."
    );

    /* lambda will be moved to internal block pool
       and "auto free" strategy is applied,
       one shall not allocate nodes manually  */
    return CoroFork_detail::CallbackAllocator<
        reservedCount,
        AdditionalOptionalTag,
        OriginalLambda,
        CoroFork_detail::ObtainAutoFreePlainFunctionFor
    >::Allocate(std::forward<UniqueLambdaType>(lambda))->individualTrampoline;
}


inline void callback_extend_lifetime::dispose(){
    isDisposed = true; //called by "the same thread", no protection needed
}

inline bool callback_extend_lifetime::is_disposed() const{
    return isDisposed; //called by "the same thread", no protection needed
}


template<
    unsigned reservedCount, class AdditionalOptionalTag, class UniqueLambdaType
> scoped_callback_holder<reservedCount, AdditionalOptionalTag, UniqueLambdaType>
::scoped_callback_holder(UniqueLambdaType&& lambda)
    : owningBlock(
        /* lambda will be moved to internal block pool
        and "auto free" strategy is applied,
        one shall not allocate nodes manually  */
        CoroFork_detail::CallbackAllocator<
            reservedCount,
            AdditionalOptionalTag,
            std::remove_reference_t<UniqueLambdaType>,
            CoroFork_detail::ObtainNoAutoFreePlainFunctionFor
        >::Allocate(std::forward<UniqueLambdaType>(lambda))
    )
{}


template<
    unsigned reservedCount, class AdditionalOptionalTag, class UniqueLambdaType
> scoped_callback_holder<reservedCount, AdditionalOptionalTag, UniqueLambdaType>
::~scoped_callback_holder(){
    using Node = typename CoroFork_detail::CallbackAllocator<
        reservedCount,
        AdditionalOptionalTag,
        std::remove_reference_t<UniqueLambdaType>,
        CoroFork_detail::ObtainNoAutoFreePlainFunctionFor
    >::Node;
    /* No "is it still owned" test here: the holder is neither movable nor
       reassignable, so a live instance always owns its slot */
    // free the block (lambda is destructed inside Free)
    static_cast<Node*>(owningBlock)->Free();
}

template<
    unsigned reservedCount, class AdditionalOptionalTag, class UniqueLambdaType
>
scoped_callback_holder<reservedCount, AdditionalOptionalTag, UniqueLambdaType>::Signature*
scoped_callback_holder<reservedCount, AdditionalOptionalTag, UniqueLambdaType>
::Callback() const{
    using Node = typename CoroFork_detail::CallbackAllocator<
        reservedCount,
        AdditionalOptionalTag,
        std::remove_reference_t<UniqueLambdaType>,
        CoroFork_detail::ObtainNoAutoFreePlainFunctionFor
    >::Node;
    //Note: it is expected lambda was attached
    return static_cast<Node*>(owningBlock)->individualTrampoline;
}

template<
    unsigned reservedCount, class AdditionalOptionalTag, class UniqueLambdaType
>
/* repeated on the definition intentionally, see callback_from_lambda above */
[[nodiscard(
    "The holder returned by scoped_callback owns the callback,"
    " dropping it right away frees the slot and unsubscribes immediately"
)]]
auto scoped_callback(UniqueLambdaType&& lambda) ->
    scoped_callback_holder<
        reservedCount, AdditionalOptionalTag, UniqueLambdaType
    >
{
    using OriginalLambda = std::remove_reference_t<UniqueLambdaType>;

    static_assert(
            !CoroFork_detail::ConvertsToPlainFunctionCallback<OriginalLambda>
        &&  !CoroFork_detail::ConvertsToLifetimeAwareCallback<OriginalLambda>,
        "This lambda has no captures, so there is nothing for the holder to own\n"
        "and nothing that could dangle once the scope is left.\n"
        "Pass the lambda to the target API directly, without scoped_callback\n"
        "(a capture less lambda already converts to a \"plain C\" callback that\n"
        " stays valid forever, no trampoline and no block pool slot are needed)."
    );

    return scoped_callback_holder<
        reservedCount, AdditionalOptionalTag, UniqueLambdaType
    >(std::forward<UniqueLambdaType>(lambda));
}


#endif
