/** @file corofork.h
    @brief Use coroutines inline, integrate existing asynchronous API easy.
           Simple way to turn callback oriented API into awaitable API.

Each "callback based" API (including those, that require "C style" callbacks)
can be used inside C++20 coroutines with that simple trick.
Easy solution to "callback hell", to make code linear and easy to read.
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
std::function of "C style" callbacks respectively, and turn
any asynchronous API into awaitable.

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
        //from now coroutine executes in thread

        ... //doing some in parallel with main
    };

    ... //continue execution independently of corofork content above))
 @endcode

RETURN sample

Windows API sample

 @code
 @endcode

Copyright (c) 2023-2024, Pavlo M, https://github.com/olvap80
All rights reserved.


Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

* Neither the name of corofork nor the names of its
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
//std::move, std::swap and friends))
#include <utility>
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
//for std::nullptr_t
#include <cstddef>
//Used by invert_subscription to accumulate events (calls) happened so far
#include <queue>



//______________________________________________________________________________
// The macro to start coroutine inline (see samples above)


/// Start a coroutine "inline" (body follows in {})
/** Macro creates a new coroutine optionally capturing parameters  
 * The way how variables are captured is determined by the macro parameters
 * one can provide =, &, or named captures as well.
 * "Forked" coroutine starts execution immediately in current thread,
 * but can be suspended and resumed in other threqad later.
 * The "fork" suffix means coroutine lifetime and execution
 * is independent from the caller thread (caller thread is not blocked).
 * Be aware: mutexes shall not be crossed by co_await, because
 * one can resune corourine from different thread, but mutex
 * is sill owned by previous thread! */
#define corofork(...) \
    CoroFork_detail::tagTieWithOverloadedOperator ->* \
    /* Note: lambda is not called below, operator->* does the stuff */ \
    [__VA_ARGS__]()-> \
        CoroFork_detail::CoroFork /* Lambda body follows here in {} */

/// Run a coroutine "inline" (synchronously), blocks until coroutine completes
/** The same as above, but now caller thread will not continue independently,
 * and will block until coroutine completes (probablu in other threads),
 * this means coroutine executes in "their" threads, but finally control
 * return to this one onve coroutine completed.
 * The "sync" suffix means coroutine lifetime and execution
 * is tied to the caller thread (initial caller thread is blocked).
 * 
 * One can do 
 *      co_await back_to_thread;
 * to force this coroutine execution flow to continue in the initial thread
 * that called the corosync marco.
 * This is especially useful for the case if long computation
 * is needed "here" but "their" callback must return immediately,
 * in this way we can reuse existing thread for running the computation
 * in the coroutine, see samples above.
 * 
 * Exceptions are forwarded from "other thread" to this one
 * (so corosync will rethrow all exceptions being trown in
 * any "other thread" driving the coroutine to the caller thread)
 * Be aware: mutexes shall not be crossed by co_await, because
 * one can resune corourine from different thread, but mutex
 * is sill owned by previous thread! */
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

    /// Meta function to extract types from LambdaType::operator()
    /** This is "pre declaration", implementation is in details below */
    template<class MethodSignature>
    struct FromOperator;


    //__________________________________________________________________________
    // The ExtractFromLambda meta function LambdaType -> related data

    /// General meta function to extract signatures for generated API
    template<class LambdaType>
    using ExtractFromLambda = FromOperator<decltype(&LambdaType::operator())>;


    //__________________________________________________________________________
    // Forward declare awaitables and holders

    template<class Decorator, class CoResult, class SetupLambdaType>
    class AwaitableSimpleCB;

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

    /// Meta function to extract types and code from LambdaType::operator()
    template<class MethodSignature>
    struct ConvertSignature;

    /// Signature for trampoline obtained directly from lambda
    template<class LambdaType>
    using SimpleSignatureFromLambda = typename
        ConvertSignature<decltype(&LambdaType::operator())>::SimpleSignature;

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

        /// It is possible to issue co_await only on "full expression" in coroutine
        /** Thus "saving/moving to some other location and awaiting later" is explicitly banned,
            to prevent referencing those components of the "full expression" that were destroyed */
        auto operator co_await() && -> RealAwaitable& {
            return awaitable;
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

///Turn lambda to function pointer by storing lambda in a block pool
/** Create "single shot" trampoline by default
(trampoline deallocates automatically after first call, 
 thus freeing one item in reservedCount,
 NOTE: never issued callback will never free!
       use scoped_callback to be able to release without call)
For usage sample see explanations at beginning of the file.
NOTE: AdditionalOptionalTag can be used for "nonunique" types, 
      when "something else" than lambda is passed to CallbackFrom
      in several different places.
In your usual case you pass only reservedCount to CallbackFrom  
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
auto callback_from_lambda(UniqueLambdaType&& lambda) ->
    CoroFork_detail::SimpleSignatureFromLambda<
        UniqueLambdaType
    >*;

/// Extra parameter to lambda change default allocation behavior in callback_from_lambda
/** By default trampolines are "single shot", they "deallocate" as soon 
as are called, so it is possible to subscribe only to those API that call lambda
For usage sample see explanations at beginning of the file.
See callback_from_lambda */
class callback_extend_lifetime{
public:
    /// No way to copy (is passed by reference to lambda)
    callback_extend_lifetime(const callback_extend_lifetime&) = delete;
    /// No way to copy (is passed by reference to lambda)
    callback_extend_lifetime& operator=(const callback_extend_lifetime&) = delete;

    /// Cause trampoline API to be freed
    /** Call from inside of lambda to cause "this lambda" to untie from callback,
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


/// RAII to hold callback allocation as long as needed
/** Use scoped_callback below as convenience API */
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
    /// Corresponding simple API signature
    using Signature = CoroFork_detail::SimpleSignatureFromLambda<UniqueLambdaType>;

    /// Create instance from lambda
    scoped_callback_holder(UniqueLambdaType&& lambda);

    /// Cleanup the stuff once instance goes out of scope
    ~scoped_callback_holder();

    /// Assign callback after scoped_callback_holder was created
    scoped_callback_holder& operator=(UniqueLambdaType&& lambda);

    // can move, but cannot copy
    scoped_callback_holder(const scoped_callback_holder&&);
    scoped_callback_holder& operator=(const scoped_callback_holder&&);
    scoped_callback_holder(const scoped_callback_holder&) = delete;
    scoped_callback_holder& operator=(const scoped_callback_holder&) = delete;


    /// Obtain pointer. Do not use that after scoped_callback_holder destructs.
    Signature* Callback() const;

private:
    /// "Type erased" info for deallocation
    void* owningBlock = nullptr;
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
    typename CoroFork_detail::ExtractFromLambda<
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
    typename CoroFork_detail::ExtractFromLambda<
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


/// Turn std::function callback based API into coroutine based
/** Provided lambda shall receive desired callback with corresponding signature
 * as a parameter. Such callback can be passed to asynchronous API
 * to be called later by "something else" single time. 
 * Once that generated function callback is called it will 
 * resume the coroutine awaiting for the result of invert_function,
 * and callback arguments will go to caller.
 * Remember to await result of invert_function immediately.
 * See samples above for illustration */
template<CoroForkLambdaToSetupFunction LambdaToSetupFunction>
auto invert_function(
    const LambdaToSetupFunction& setupFunction ///< receives generated callback
){
    using Awaitable = CoroFork_detail::AwaitableSimpleCB<
        CoroFork_detail::TheSame,
        typename CoroFork_detail::ExtractFromLambda<LambdaToSetupFunction>::CoResultType,
        LambdaToSetupFunction
    >;
    return CoroFork_detail::AwaitAsTemporaryOnly<Awaitable>(
        Awaitable(setupFunction)
    );
}


/// Same as above, but generated function callback has a return value
/** In addition to the above logic, here the return value for
 * the generated function callback can be provided.
 * Remember to await result of invert_function immediately.
 * See samples above for illustration. */
template<
    CoroForkLambdaToSetupFunction LambdaToSetupFunction,
    class ResultForCallbackFunctionAsValue
>
requires    
    std::convertible_to<
        std::remove_cvref_t<ResultForCallbackFunctionAsValue>,
        std::remove_cvref_t<
            typename CoroFork_detail::ExtractFromLambda<LambdaToSetupFunction>::FunctionResultType
        >
    >
auto invert_function(
    const LambdaToSetupFunction& setupFunction, ///< Your setup
    ResultForCallbackFunctionAsValue&& resultForCallbackFunction
){
    using Awaitable = CoroFork_detail::AwaitableGeneratedCBReturnsValue<
        CoroFork_detail::TheSame,
        typename CoroFork_detail::ExtractFromLambda<LambdaToSetupFunction>::CoResultType,
        LambdaToSetupFunction,
        typename CoroFork_detail::ExtractFromLambda<LambdaToSetupFunction>::FunctionResultType
    >;
    
    return CoroFork_detail::AwaitAsTemporaryOnly<Awaitable>(
        Awaitable(
            setupFunction, 
            std::forward<ResultForCallbackFunctionAsValue>(resultForCallbackFunction)
        )
    );
}

/// Same as above, but generated callback returns value returned by functionToObtainResult
/** Remember here coroutine is resumed directly from the callback, thus
 * functionToObtainResultForCallback is issued once we await for "somenting else" */
template<
    CoroForkLambdaToSetupFunction LambdaToSetupFunction, ///< Your setup
    std::invocable FunctionToObtainResult
>
requires    
    std::same_as<
        std::remove_cvref_t<
            typename CoroFork_detail::ExtractFromLambda<LambdaToSetupFunction>::FunctionResultType
        >,
        std::remove_cvref_t<
            std::invoke_result_t< std::remove_cvref_t<FunctionToObtainResult> >
        >
    >
auto invert_function(
    const LambdaToSetupFunction& setupFunction,
    FunctionToObtainResult&& functionToObtainResultForCallbackFunction
){
    using Awaitable = CoroFork_detail::AwaitableGeneratedCBReturnsResultOfCall<
        CoroFork_detail::TheSame,
        typename CoroFork_detail::ExtractFromLambda<LambdaToSetupFunction>::CoResultType,
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


/// Turn "plain C style" callback based API into coroutine based
template<
    std::size_t maxCallbacksCount, ///< Specify callback count per lambda
    CoroForkLambdaToSetupCallback LambdaToSetupCallback ///< Your setup
>
auto invert_callback(
    const LambdaToSetupCallback& setupCallback
){
    using Awaitable = CoroFork_detail::AwaitableSimpleCB<
        CoroFork_detail::ToPlainC<
            maxCallbacksCount, LambdaToSetupCallback
        >,
        typename CoroFork_detail::ExtractFromLambda<
            LambdaToSetupCallback
        >::CoResultType,
        LambdaToSetupCallback
    >;
    
    return CoroFork_detail::AwaitAsTemporaryOnly<Awaitable>(
        Awaitable(setupCallback)
    );
}


/// Same as above, but generated callback has a return value
/** In addition to the above logic Here the return value for
 * the generated callback can be provided.
 * See samples above for illustration.
 * Remember here coroutine is resumed directly from the callback */
template<
    std::size_t maxCallbacksCount, ///< Specify callback count per lambda
    CoroForkLambdaToSetupCallback LambdaToSetupCallback, ///< Your setup
    class ResultForCallbackFunctionAsValue
>
requires    
    std::convertible_to<
        std::remove_cvref_t<ResultForCallbackFunctionAsValue>,
        std::remove_cvref_t<
            typename CoroFork_detail::ExtractFromLambda<LambdaToSetupCallback>::FunctionResultType
        >
    >
auto invert_callback(
    const LambdaToSetupCallback& setupCallback,
    ResultForCallbackFunctionAsValue&& resultForCallback
){
    using Awaitable = CoroFork_detail::AwaitableGeneratedCBReturnsValue<
        CoroFork_detail::ToPlainC<
            maxCallbacksCount, LambdaToSetupCallback
        >,
        typename CoroFork_detail::ExtractFromLambda<LambdaToSetupCallback>::CoResultType,
        LambdaToSetupCallback,
        typename CoroFork_detail::ExtractFromLambda<LambdaToSetupCallback>::FunctionResultType
    >;
    
    return CoroFork_detail::AwaitAsTemporaryOnly<Awaitable>(
        Awaitable(
            setupCallback, 
            std::forward<ResultForCallbackFunctionAsValue>(resultForCallback)
        )
    );
}

/// Same as above, but generated callback returns value returned by functionToObtainResultForCallback
/** Remember here coroutine is resumed directly from the callback, thus
 * functionToObtainResultForCallback is issued once we await for "somenting else" */
template<
    std::size_t maxCallbacksCount, ///< Specify callback count per lambda
    CoroForkLambdaToSetupCallback LambdaToSetupCallback, ///< Your setup
    std::invocable FunctionToObtainResult
>
requires    
    std::same_as<
        std::remove_cvref_t<
            typename CoroFork_detail::ExtractFromLambda<LambdaToSetupCallback>::FunctionResultType
        >,
        std::remove_cvref_t<
            std::invoke_result_t< std::remove_cvref_t<FunctionToObtainResult> >
        >
    >
auto invert_callback(
    const LambdaToSetupCallback& setupCallback,
    FunctionToObtainResult&& functionToObtainResultForCallback
){
    using Awaitable = CoroFork_detail::AwaitableGeneratedCBReturnsResultOfCall<
        CoroFork_detail::ToPlainC<
            maxCallbacksCount, LambdaToSetupCallback
        >,
        typename CoroFork_detail::ExtractFromLambda<LambdaToSetupCallback>::CoResultType,
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

namespace CoroFork_detail{

} //namespace CoroFork_detail

/// Turn std::function subscription based API into coroutine based
/** Provided lambda shall receive desired callback with corresponding signature
 * as a parameter. Such callback can be passed to asynchronous subscription API
 * to be called later (likely multiple times) by "something else".
 * Each time once called such generated function callback 
 * will resume the coroutine awaiting for invert_subscription_holder
 * being created by the invert_subscription call,
 * and callback arguments will go to the awaiter.
 * The setupFunction can optionally return "the unsubscription API",
 * to be automatically invoked once corresponding invert_subscription_holder 
 * goes out of scope. 
 * See samples above for illustration.
 * (Remember here "inverted" subscription callback returns result immediately once issued
 *  and not when subscription is actually awaited) */
template<CoroForkLambdaToSetupFunction LambdaToSetupFunction>
auto invert_subscription(
    const LambdaToSetupFunction& setupFunction ///< receives generated callback
){
    //takes advantage of RVO here
    return CoroFork_detail::InvertSubscriptionHolder<
        typename CoroFork_detail::TheSame,
        typename CoroFork_detail::ExtractFromLambda<LambdaToSetupFunction>::CoResultType
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
            typename CoroFork_detail::ExtractFromLambda<LambdaToSetupFunction>::FunctionResultType
        >
    >
auto invert_subscription(
    const LambdaToSetupFunction& setupFunction, ///< Your setup
    ResultForCallbackFunctionAsValue&& resultForCallbackFunction
){
    //takes advantage of RVO here
    return CoroFork_detail::InvertSubscriptionHolder<
        typename CoroFork_detail::ExtractFromLambda<LambdaToSetupFunction>::CoResultType
    >(
        setupFunction, 
        std::forward<ResultForCallbackFunctionAsValue>(resultForCallbackFunction)
    );
}




//______________________________________________________________________________
//##############################################################################
/*==============================================================================
*  More implementation details follow                                          *
*=============================================================================*/
//##############################################################################



namespace CoroFork_detail{

    //__________________________________________________________________________
    // General corofork macro support

    /// Actual implementation of corofork/corosync macro
    class CoroFork{
    public:
        //Uncopyable, unmovable (take advantage of RVO)
        //NOTE: CoroFork dissapears once full expression starting the coroutine is exited
        //      this means promise_type can outlive CoroFork instance
        CoroFork(const CoroFork&) = delete;
        CoroFork& operator=(const CoroFork&) = delete;

        /// Define coroutine behavior
        struct promise_type;

        ///Start with lambda that creates coroutine instance
        /** Ensures lambda is alive as long as coroutine is running */
        template<class Lambda>
        static void Start(Lambda&& lambda);
        
    private:
        /// Tie with promise once created
        CoroFork(promise_type* createdFrom): myPromise(createdFrom) {}

        /// Corresponding promise (may not be alive as coroutine exits)
        promise_type* myPromise;

        /// Common base for stored lambda (to be stored in promise)
        class LambdaStorageBase;

        /// Store that lambda (with captures) as long as coroutine is running
        template<class Lambda>
        class LambdaStorage;
    };

    struct CoroFork::promise_type{
        /// Default constructor (just do nothing)
        promise_type() = default;
        
        //uncopyable, unmovable (take advantage of RVO)
        promise_type(const promise_type&) = delete;
        promise_type& operator=(const promise_type&) = delete;

        /// How actually CoroFork is obtained from the coroutine
        CoroFork get_return_object() { return CoroFork(this); }

        /// Shall be suspended (prevent deallocation until the lambda is safe)
        std::suspend_always initial_suspend() const noexcept { return {}; }

        /// Just do nothing
        void return_void() const noexcept {}

        /// Once exception happens, let it propagate
        void unhandled_exception() const { throw; }

        /// Shall let coroutine to be destroyed by reaching end
        /** let it cleanup self in natural way, as coroutine execution reaches end
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
        ///Create RAII wrapper around Lambda
        /** Using Lambda directly, optimizer takes case due to [&]() in front */
        constexpr LambdaStorage(Lambda initialLambda)
            : lambda(initialLambda) {}

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

        /* Here coroutine is resumed from the current thread,
        all actions will happen immediately in this thread */
        std::coroutine_handle<promise_type>::from_promise(*started.myPromise).resume();

        /*once we are here this means coroutine eiter suspended or executes
        in some other thread, or it is already finished */

        /* Here we intentionally do not destroy coroutine here,
        (final_suspend returs std::suspend_never, so coroutine will be destroyed
        once it reaches the end, and lambda will be destroyed together with the promise)
        https://stackoverflow.com/questions/68352718/is-it-necessary-to-call-destroy-on-a-stdcoroutine-handle
        */        
    }


    ///Helper type to trigger operator ->* for corofork macro
    struct TagTieWithOverloadedOperator{
        constexpr TagTieWithOverloadedOperator() = default;
    };
    ///Use this "instance" to trigger overloaded operator ->* for corofork macro
    /** The trick with TagTieWithOverloadedOperator is needed
        to infer type of the lambda */
    constexpr TagTieWithOverloadedOperator tagTieWithOverloadedOperator;


    ///Helper operator to easy catch lambda for corofork macro
    /** Use template to avoid slow std::function
        (raw lambda is copied/stored here) */
    template<class Lambda>
    constexpr void operator ->* (TagTieWithOverloadedOperator, Lambda&& lambda){
        CoroFork::Start(std::move(lambda));
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

        ///Start with lambda that creates coroutine instance
        /** Ensures lambda is alive as long as coroutine is running */
        template<class Lambda>
        static void Start(Lambda&& lambda);
        
    private:
        /// Tie with promise once created
        CoroSync(promise_type* createdFrom): myPromise(createdFrom) {}

        /// Corresponding promise (may not be alive as coroutine exits)
        promise_type* myPromise;

        /// Common base for stored lambda (to be stored in promise)
        class LambdaStorageBase;

        /// Store that lambda (with captures) as long as coroutine is running
        template<class Lambda>
        class LambdaStorage;
    };

    struct CoroSync::promise_type{
        /// Default constructor (just do nothing)
        promise_type() = default;
        
        //uncopyable, unmovable (take advantage of RVO)
        promise_type(const promise_type&) = delete;
        promise_type& operator=(const promise_type&) = delete;

        /// How actually Coro is obtained from the coroutine
        CoroSync get_return_object() { return CoroSync(this); }

        /// Shall be suspended (prevent deallocation until the lambda is safe)
        std::suspend_always initial_suspend() const noexcept { return {}; }

        /// Part to outlive the coroutine lifetime 
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
                void await_suspend(std::coroutine_handle<> handle){
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

            /// Mark coroutine as finished (will not resume)
            void return_void() noexcept {
                std::lock_guard lock(protectPromise);
                state = State::FinishedNormally;
                waitForPromise.notify_one();
            }

            /// Once exception happens, let it propagate to the initial thread
            void unhandled_exception() noexcept {
                //propagate exception to the future (to be propagated from corosync)
                std::lock_guard lock(protectPromise);
                state = State::FinishedWithException;
                exception = std::current_exception();
                waitForPromise.notify_one();
            }
        };
        std::shared_ptr<SharedState> sharedState;

        /// Mark coroutine as finished
        void return_void() noexcept {
            sharedState->return_void();
        }

        /// Once exception happens, let it propagate to the initial thread
        void unhandled_exception() noexcept {
            sharedState->unhandled_exception();
        }

        /// Shall let coroutine to be destroyed by reaching end
        /** let it cleanup self in natural way, as coroutine execution reaches end
        https://stackoverflow.com/questions/68352718/is-it-necessary-to-call-destroy-on-a-stdcoroutine-handle
        */
        std::suspend_never final_suspend() const noexcept {
            // do nothing (lambda will fade away together with the promise)
            return {}; //let coroutine destruct
        }
    };

    
    template<class Lambda>
    void CoroSync::Start(Lambda&& lambda){
        /* Storing lambda in capture is not needed here 
          as lambda will exist as long as coroutine is running
          due to synchronization */

        //Eiher promise or Start API can use sharedState longer
        auto sharedState = std::make_shared<CoroSync::promise_type::SharedState>();

        //this will create corresponding promise and coroutine (in suspended state) 
        CoroSync started = lambda(
            //coroutine will be able to go back to the initial thread
            CoroSync::promise_type::SharedState::BackToThreadAwaitable(sharedState.get()) 
        );

        //ensure sharedState is alive as long as coroutine is running
        started.myPromise->sharedState = sharedState;

        //handle to the coroutine
        auto handle = std::coroutine_handle<promise_type>::from_promise(*started.myPromise);

        for( ;; ){
            /* Here coroutine is resumed from the current thread,
            all actions will happen immediately in this thread */
            handle.resume();

            /*once we are here this means coroutine eiter suspended or
            executes in some other thread, or it is already finished and meybe destroyed,
            (but is any case sharedState is still alive,
            eiter other thread is finished it or this one marks for end) */

            std::unique_lock lock(sharedState->protectPromise);
            while(
                    sharedState->state
                !=  CoroSync::promise_type::SharedState::State::GoBackToThread
            ){
                switch( sharedState->state ){
                    case CoroSync::promise_type::SharedState::State::FinishedNormally:
                        //normal exit, will cleanup sharedState automatically
                        return;
                    case CoroSync::promise_type::SharedState::State::FinishedWithException:
                        //normal exit, also will cleanup sharedState automatically
                        std::rethrow_exception(sharedState->exception);
                }
                //wait for coroutine to "change something"
                sharedState->waitForPromise.wait(lock);
            }
            // Prevent GoBackToThread until coroutine explicitly awaits back_to_thread
            sharedState->state = CoroSync::promise_type::SharedState::State::Running;
        } // loop for resuming coroutine   
    }

    ///Helper type to trigger operator ->* for corosync macro
    struct TagTieWithSyncOverloadedOperator{
        constexpr TagTieWithSyncOverloadedOperator() = default;
    };
    ///Use this "instance" to trigger overloaded operator ->* for corosync macro
    /** The trick with TagTieWithSyncOverloadedOperator is needed
        to infer type of the lambda */
    constexpr TagTieWithSyncOverloadedOperator tagTieWithSyncOverloadedOperator;

    ///Helper operator to easy catch lambda for corosync macro
    /** Use template to avoid slow std::function
        (raw lambda is copied/stored here) */
    template<class Lambda>
    constexpr void operator ->* (TagTieWithSyncOverloadedOperator, Lambda&& lambda){
        CoroSync::Start(std::move(lambda));
    }

    //__________________________________________________________________________
    //##########################################################################
    //Extracting types and code from lambda's operator() signature


    //__________________________________________________________________________
    // Helper to decide on result from setup lambda

    /// Handle case then Setup is able to obtain result before async operation
    /** Just check result is already there 
     * and place it to expected destination (or stay suspended) */
    template<
        class CoResultType,
        class AwaitableToPlaceResult
    >
    bool ProcessResult(
        std::optional<CoResultType>&& operatorResult,
        AwaitableToPlaceResult* self
    ){
        if( operatorResult ){
            //There is a result already present 
            self->EmplaceCoResult(std::move(*operatorResult));
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
    struct FromOperator<
        OperatorResult(LambdaType::*)( std::function<void()> )
    >{
        using Signature = void();
        using FunctionType = std::function<Signature>;
        using CoResultType = void;
        using FunctionResultType = void;


        ///Invoke setup with resuming function
        template<class AwaitableToPlaceResult>
        static bool SetupSimpleCB(
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
                self->Decorate(handle)
            );
        }

        ///Invoke setup with subscribing function
        template<class AwaitableToPlaceResult>
        static auto SubscribeSimpleCB(
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
    struct FromOperator<
        void(LambdaType::*)( std::function<void()> )
    >{
        using Signature = void();
        using FunctionType = std::function<Signature>;
        using CoResultType = void;
        using FunctionResultType = void;


        ///Invoke setup with resuming function
        template<class AwaitableToPlaceResult>
        static bool SetupSimpleCB(
            AwaitableToPlaceResult* self,
            std::coroutine_handle<> handle,
            const LambdaType& setupLambda
        ){
            setupLambda(
                self->Decorate(handle)
            );
            return true; //means stay suspended (this is ok even if already resumed)
        }

        ///Invoke setup with subscribing function
        template<class AwaitableToPlaceResult>
        static auto SubscribeSimpleCB(
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
    struct FromOperator<
        OperatorResult(LambdaType::*)( std::function<void(CallbackArg)> )
    >{
        using Signature = void(CallbackArg);
        using FunctionType = std::function<Signature>;
        using CoResultType = CallbackArg;
        using FunctionResultType = void;


        ///Invoke setup with resuming function that remembers argument
        template<class AwaitableToPlaceResult>
        static bool SetupSimpleCB(
            AwaitableToPlaceResult* self,
            std::coroutine_handle<> handle,
            const LambdaType& setupLambda
        ){
            static_assert(
                std::same_as<OperatorResult, std::optional<CallbackArg>>,
                "Call to lambda shall return void of std::optional<CallbackArg>\n"
                "(present value means result is ready immediately)"
            );

            return ProcessResult<CoResultType, AwaitableToPlaceResult>(
                setupLambda(
                    self->Decorate(
                        [=](CallbackArg arg){
                            self->EmplaceCoResult(arg); //what will be returned to coroutine
                            handle();
                        }
                    )
                ),
                self
            );
        }

        ///Invoke setup with subscribing function
        template<class AwaitableToPlaceResult>
        static auto SubscribeSimpleCB(
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
                    self->OnArgumentsArrived(arg); //what will be returned to coroutine
                })
            );
        }
    };
    //single argument version, lambda always starts asynchronous operation
    template<class LambdaType, class CallbackArg>
    struct FromOperator<
        void(LambdaType::*)( std::function<void(CallbackArg)> )
    >{
        using Signature = void(CallbackArg);
        using FunctionType = std::function<Signature>;
        using CoResultType = CallbackArg;
        using FunctionResultType = void;


        ///Invoke setup with resuming function that remembers argument
        template<class AwaitableToPlaceResult>
        static bool SetupSimpleCB(
            AwaitableToPlaceResult* self,
            std::coroutine_handle<> handle,
            const LambdaType& setupLambda
        ){
            setupLambda(
                self->Decorate(
                    [=](CallbackArg arg){
                        self->EmplaceCoResult(arg); //what will be returned to coroutine
                        handle();
                    }
                )
            );
            return true; //means stay suspended (this is ok even if already resumed)
        }

        ///Invoke setup with subscribing function
        template<class AwaitableToPlaceResult>
        static auto SubscribeSimpleCB(
            AwaitableToPlaceResult* self,
            const LambdaType& setupLambda
        ){
            setupLambda(
                self->Decorate([=](CallbackArg arg){
                    self->OnArgumentsArrived(arg); //what will be returned to coroutine
                })
            );
            // returns cleanup API that does nothing
            return [](){};
        }
    };

    //multiple argument version, lambda optionally can complete synchronously
    template<class OperatorResult, class LambdaType, class... CallbackArgs>
    struct FromOperator<
        OperatorResult(LambdaType::*)( std::function<void(CallbackArgs...)> )
    >{
        using Signature = void(CallbackArgs...);
        using FunctionType = std::function<Signature>;
        using CoResultType = std::tuple<CallbackArgs...>;
        using FunctionResultType = void;


        ///Invoke setup with resuming function that remembers arguments
        template<class AwaitableToPlaceResult>
        static bool SetupSimpleCB(
            AwaitableToPlaceResult* self,
            std::coroutine_handle<> handle,
            const LambdaType& setupLambda
        ){
            static_assert(
                std::same_as<OperatorResult, std::optional<CoResultType>>,
                "Call to lambda shall return void of std::optional<std::tuple<CallbackArgs...>>\n"
                "(present value means result is ready immediately)"
            );

            return ProcessResult<CoResultType, AwaitableToPlaceResult>(
                setupLambda(
                    self->Decorate(
                        [=](CallbackArgs... args){
                            self->EmplaceCoResult(args...); //what will be returned to coroutine
                            handle();
                        }
                    )
                ),
                self
            );
        }

        ///Invoke setup with subscribing function
        template<class AwaitableToPlaceResult>
        static auto SubscribeSimpleCB(
            AwaitableToPlaceResult* self,
            const LambdaType& setupLambda
        ){
            static_assert(
                std::same_as<OperatorResult, std::optional<CoResultType>>,
                "Call to lambda shall return void of std::optional<std::tuple<CallbackArgs...>>\n"
                "(present value means result is ready immediately)"
            );

            // returns cleanup API
            return setupLambda(
                self->Decorate([=](CallbackArgs... args){
                    self->OnArgumentsArrived(args...); //what will be returned to coroutine
                })
            );
        }
    };
    //multiple argument version, lambda always starts asynchronous operation
    template<class LambdaType, class... CallbackArgs>
    struct FromOperator<
        void(LambdaType::*)( std::function<void(CallbackArgs...)> )
    >{
        using Signature = void(CallbackArgs...);
        using FunctionType = std::function<Signature>;
        using CoResultType = std::tuple<CallbackArgs...>;
        using FunctionResultType = void;


        //Invoke setup with resuming function that remembers arguments
        template<class AwaitableToPlaceResult>
        static bool SetupSimpleCB(
            AwaitableToPlaceResult* self,
            std::coroutine_handle<> handle,
            const LambdaType& setupLambda
        ){
            setupLambda(
                self->Decorate(
                    [=](CallbackArgs... args){
                        self->EmplaceCoResult(args...); //what will be returned to coroutine
                        handle();
                    }
                )
            );
            return true; //means stay suspended (this is ok even if already resumed)
        }

        ///Invoke setup with subscribing function
        template<class AwaitableToPlaceResult>
        static auto SubscribeSimpleCB(
            AwaitableToPlaceResult* self,
            const LambdaType& setupLambda
        ){
            setupLambda(
                self->Decorate([=](CallbackArgs... args){
                    self->OnArgumentsArrived(args...); //what will be returned to coroutine
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
    struct FromOperator<
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
                        handle(); //resume the coroutine
                        // storedResult is alive as long as capture exists
                        return storedResult;
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
                        self->OnEventHappened();
                        // storedResult is alive as long as capture exists
                        return storedResult;
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
                        handle(); //resume the coroutine
                        // calculateResult is alive as long as capture exists
                        return calculateResult();
                    }
                )
            );
        }
    };
    //version without arguments at all, when lambda always starts async operation!
    template<class LambdaType, class CallbackRes>
    struct FromOperator<void(LambdaType::*)( std::function<CallbackRes()> )>{
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
                        handle(); //resume the coroutine
                        // storedResult is alive as long as capture exists
                        return storedResult;
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
                        self->OnEventHappened();
                        // storedResult is alive as long as capture exists
                        return storedResult;
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
                        handle(); //resume the coroutine
                        // calculateResult is alive as long as capture exists
                        return calculateResult();
                    }
                )
            );
            return true; //means stay suspended (this is ok even if already resumed)
        }
    };

    //single argument version, lambda optionally can complete synchronously
    template<class OperatorResult, class LambdaType, class CallbackRes, class CallbackArg>
    struct FromOperator<
        OperatorResult(LambdaType::*)( std::function<CallbackRes(CallbackArg)> )
    >{
        using Signature = CallbackRes(CallbackArg);
        using FunctionType = std::function<Signature>;
        using CoResultType = CallbackArg;
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
                std::same_as< OperatorResult, std::optional<CallbackArg> >,
                "Call to lambda shall return void or std::optional<CallbackArg>\n"
                "(present value means result is ready immediately)"
            );

            return ProcessResult<CoResultType, AwaitableToPlaceResult>(
                setupLambda(
                    self->Decorate(
                        [   =,
                            //captured copy shall be able to exist a for long time
                            storedResult=std::remove_reference_t<Value>(
                                std::forward<Value>(result)
                            )
                        ](CallbackArg arg){
                            self->EmplaceCoResult(arg); //what will be returned to coroutine
                            handle(); 
                            // storedResult is alive as long as capture exists
                            return storedResult;
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
                        self->OnArgumentsArrived(arg); 
                        // storedResult is alive as long as capture exists
                        return storedResult;
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
                std::same_as< OperatorResult, std::optional<CallbackArg> >,
                "Call to lambda shall return void or std::optional<CallbackArg>\n"
                "(present value means result is ready immediately)"
            );

            return ProcessResult<CoResultType, AwaitableToPlaceResult>(
                setupLambda(
                    self->Decorate(
                        [   =,
                            //captured copy shall be able to exist a for long time
                            calculateResult=std::remove_reference_t<Function>(
                                std::forward<Function>(resultGetter)
                            )
                        ](CallbackArg arg){
                            self->EmplaceCoResult(arg); //what will be returned to coroutine
                            handle(); 
                            // calculateResult is alive as long capture exists
                            return calculateResult();
                        }
                    )
                ),
                self
            );
        }
    };
    //single argument version, lambda always starts asynchronous operation
    template<class LambdaType, class CallbackRes, class CallbackArg>
    struct FromOperator<
        void(LambdaType::*)( std::function<CallbackRes(CallbackArg)> )
    >{
        using Signature = CallbackRes(CallbackArg);
        using FunctionType = std::function<Signature>;
        using CoResultType = CallbackArg;
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
                        self->EmplaceCoResult(arg); //what will be returned to coroutine
                        handle(); 
                        // storedResult is alive as long as capture exists
                        return storedResult;
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
                        self->OnArgumentsArrived(arg);
                        // storedResult is alive as long as capture exists
                        return storedResult;
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
                        self->EmplaceCoResult(arg); //what will be returned to coroutine
                        handle(); 
                        // calculateResult is alive as long capture exists
                        return calculateResult();
                    }
                )
            );
            return true; //means stay suspended (this is ok even if already resumed)
        }
    };

    //multiple argument version, lambda optionally can complete synchronously
    template<class OperatorResult, class LambdaType, class CallbackRes, class... CallbackArgs>
    struct FromOperator<
        OperatorResult(LambdaType::*)( std::function<CallbackRes(CallbackArgs...)> )
    >{
        using Signature = CallbackRes(CallbackArgs...);
        using FunctionType = std::function<Signature>;
        using CoResultType = std::tuple<CallbackArgs...>;
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
                "Call to lambda shall return void of std::optional<std::tuple<CallbackArgs...>>\n"
                "(present value means result is ready immediately)"
            );

            return ProcessResult<CoResultType, AwaitableToPlaceResult>(
                setupLambda(
                    self->Decorate(
                        [   =,
                            //captured copy shall be able to exist a for long time
                            storedResult=std::remove_reference_t<Value>( 
                                std::forward<Value>(result)
                            )
                        ](CallbackArgs... args){
                            self->EmplaceCoResult(args...); //what will be returned to coroutine
                            handle(); 
                            // storedResult is alive as long as capture copy exists
                            return storedResult;
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
                std::same_as<OperatorResult, std::optional<CoResultType>>,
                "Call to lambda shall return void of std::optional<std::tuple<CallbackArgs...>>\n"
                "(present value means result is ready immediately)"
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
                        self->OnArgumentsArrived(args...); //what will be returned to coroutine
                        // storedResult is alive as long as capture copy exists
                        return storedResult;
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
                "Call to lambda shall return void of std::optional<std::tuple<CallbackArgs...>>\n"
                "(present value means result is ready immediately)"
            );

            return ProcessResult<CoResultType, AwaitableToPlaceResult>(
                setupLambda(
                    self->Decorate(
                        [   =,
                            //captured copy shall be able to exist a for long time
                            calculateResult=std::remove_reference_t<Function>(
                                std::forward<Function>(resultGetter)
                            )
                        ](CallbackArgs... args){
                            self->EmplaceCoResult(args...); //what will be returned to coroutine
                            handle(); 
                            // calculateResult is alive as long as std::function copy exists
                            return calculateResult();
                        }
                    )
                ),
                self
            );
        }
    };
    //multiple argument version, lambda always starts asynchronous operation
    template<class LambdaType, class CallbackRes, class... CallbackArgs>
    struct FromOperator<
        void(LambdaType::*)( std::function<CallbackRes(CallbackArgs...)> )
    >{
        using Signature = CallbackRes(CallbackArgs...);
        using FunctionType = std::function<Signature>;
        using CoResultType = std::tuple<CallbackArgs...>;
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
                        self->EmplaceCoResult(args...); //what will be returned to coroutine
                        handle(); 
                        // storedResult is alive as long as capture copy exists
                        return storedResult;
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
                        self->OnArgumentsArrived(args...); //what will be returned to coroutine
                        // storedResult is alive as long as capture copy exists
                        return storedResult;
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
                        self->EmplaceCoResult(args...); //what will be returned to coroutine
                        handle(); 
                        // calculateResult is alive as long as std::function copy exists
                        return calculateResult();
                    }
                )
            );
            return true; //means stay suspended (this is ok even if already resumed)
        }
    };


    //__________________________________________________________________________
    // redirect other possible signatures for std::function

    template<class OperatorResult, class LambdaType, class FunctionSignature>
    struct FromOperator<OperatorResult(LambdaType::*)( std::function<FunctionSignature> ) const>
        : public FromOperator<OperatorResult(LambdaType::*)( std::function<FunctionSignature> )> {};

    template<class OperatorResult, class LambdaType, class FunctionSignature>
    struct FromOperator<OperatorResult(LambdaType::*)( const std::function<FunctionSignature>& )>
        : public FromOperator<OperatorResult(LambdaType::*)( std::function<FunctionSignature> )> {};

    template<class OperatorResult, class LambdaType, class FunctionSignature>
    struct FromOperator<OperatorResult(LambdaType::*)( const std::function<FunctionSignature>& ) const>
        : public FromOperator<OperatorResult(LambdaType::*)( std::function<FunctionSignature> )> {};


    //__________________________________________________________________________
    // Raw callback reuses logic applicable to std::function 

    template<class OperatorResult, class LambdaType, class CallbackRes, class... CallbackArgs>
    struct FromOperator<OperatorResult(LambdaType::*)( CallbackRes(*)(CallbackArgs...) )>
        : public FromOperator<
                    OperatorResult(LambdaType::*)( std::function<CallbackRes(CallbackArgs...)> )
                 >
    {
        using RawPointerCallbackType = CallbackRes(*)(CallbackArgs...);
    };
    template<class OperatorResult, class LambdaType, class CallbackRes, class... CallbackArgs>
    struct FromOperator<OperatorResult(LambdaType::*)( CallbackRes(*)(CallbackArgs...) ) const>
        : public FromOperator<
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
    
    /// Awaitable being used by invert_function (simple case)
    /** Setup lambda is alive as long as full-expression exists,
     * thus both setup lambda and Awaitable instance coexist simultaneously,
     * see https://stackoverflow.com/questions/62620147/c-coroutines-temporaries-in-co-await-expressions
     * for more details (temporaries are destroyed at the end of the full-expression) */
    template<
        class Decorator,
        class CoResult,
        class SetupLambdaType
    >
    class AwaitableSimpleCB: public Decorator{
    public:
        /// Tie with setup lambda
        AwaitableSimpleCB(const SetupLambdaType& setupLambdaToUse)
            : setupLambda(setupLambdaToUse) {}

        /// Suspends (await_suspend must see handle before setup lambda is invoked)
        constexpr bool await_ready() const noexcept { return false;  }
        
        /// Pass awaiting handle to be used once callback issued 
        bool await_suspend(
            std::coroutine_handle<> handle ///< represent the current coroutine
        ) {
            return ExtractFromLambda<SetupLambdaType>
                ::SetupSimpleCB(
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

    /// Specialization for waitable being used by invert_function (simple case)
    template<class Decorator, class SetupLambdaType>
    class AwaitableSimpleCB<Decorator, void, SetupLambdaType>: public Decorator{
    public:
        /// Tie with setup lambda
        AwaitableSimpleCB(const SetupLambdaType& setupLambdaToUse)
            : setupLambda(setupLambdaToUse) {}

        /// Always suspends (shall not skip await_suspend)
        constexpr bool await_ready() const noexcept { return false;  }
        
        /// Pass awaiting handle to be used once callback issued 
        bool await_suspend(
            std::coroutine_handle<> handle ///< represent the current coroutine
        ) {
            return ExtractFromLambda<SetupLambdaType>
                ::SetupSimpleCB(
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

    /// Awaitable being used by invert_function (case of value)
    /** Setup lambda is alive as long as full-expression exists,
     * thus both setup lambda and Awaitable instance coexist simultaneously,
     * see https://stackoverflow.com/questions/62620147/c-coroutines-temporaries-in-co-await-expressions
     * for more details (temporaries are destroyed at the end of the full-expression) */
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
            return ExtractFromLambda<SetupLambdaType>
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
        /// Exactly the same thing as is was received by invert_function
        /** Reference is valid since awaitable exists as long as awaitable 
         *  (for the time of full expression) */
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
            return ExtractFromLambda<SetupLambdaType>
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
        /// Exactly the same thing as is was received by invert_function
        /** Reference is valid since awaitable exists as long as awaitable 
         *  (for the time of full expression) */
        std::remove_reference_t<GeneratedCallbackResultType> valueGeneratedCallbackReturns;
    };


    //__________________________________________________________________________
    // Awaitable for generating callback that calls callback for result

    /// Awaitable being used by invert_function (case of callback returning result)
    /** Setup lambda is alive as long as full-expression exists,
     * thus both setup lambda and Awaitable instance coexist simultaneously,
     * see https://stackoverflow.com/questions/62620147/c-coroutines-temporaries-in-co-await-expressions
     * for more details (temporaries are destroyed at the end of the full-expression) */
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
            return ExtractFromLambda<SetupLambdaType>
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
        /// Exactly the same thing as is was received by invert_function
        /** Reference is valid since awaitable exists as long as awaitable 
         *  (for the time of full expression) */
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
            return ExtractFromLambda<SetupLambdaType>
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
        /// Exactly the same thing as is was received by invert_function
        /** Reference is valid since awaitable exists as long as awaitable 
         *  (for the time of full expression) */
        std::remove_reference_t<CallbackToCallFromGeneratedCallbackType> callbackToCallFromGeneratedCallback;
    };


    //__________________________________________________________________________
    //##########################################################################
    // Support for callback_from_lambda 


    template<class Res, class LambdaType, class... Args>
    struct ConvertSignature<Res(LambdaType::*)(Args...)>{
        using SimpleSignature = Res(Args...);
    };
    template<class Res, class LambdaType, class... Args>
    struct ConvertSignature<Res(LambdaType::*)(Args...) const>{
        using SimpleSignature = Res(Args...);
    };

    template<class Res, class LambdaType, class... Args>
    struct ConvertSignature<Res(LambdaType::*)(callback_extend_lifetime&, Args...)>{
        using SimpleSignature = Res(Args...);
    };
    template<class Res, class LambdaType, class... Args>
    struct ConvertSignature<Res(LambdaType::*)(callback_extend_lifetime&, Args...) const>{
        using SimpleSignature = Res(Args...);
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
        /// Signature for corresponding "simple function pointer"
        using SimpleSignature = SimpleSignatureFromLambda<LambdaType>;

        /// Create general allocation node
        LambdaListNode(
            SimpleSignature* individualTrampolineGenerated,
            LambdaListNode* nextNode
        )
            : individualTrampoline(individualTrampolineGenerated), next(nextNode) {}
        
        /// Destructor to do nothing (assume there is no lambda)
        ~LambdaListNode(){}

        /// Hold corresponding "simple function pointer"/callback when is free
        SimpleSignature* const individualTrampoline;


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
    struct ObtainSimpleAutoFreeFunction;

    /// Specialization to obtain code for LambdaType::operator()
    template<class LambdaListNodeType, class Res, class LambdaType, class... Args>
    struct ObtainSimpleAutoFreeFunction<LambdaListNodeType, Res(LambdaType::*)(Args...)>{
        template<void* self>
        struct For{
            static Res apply(Args... args){
                // moved copy is needed to allow recursive allocation
                auto copyOfLambda = static_cast<LambdaType&&>(
                    static_cast<LambdaListNodeType*>(self)->lambda
                );

                // free corresponding node
                static_cast<LambdaListNodeType*>(self)->Free();
                
                //call the mover copy
                return copyOfLambda(static_cast<Args&&>(args)...);
            }
        };
    };
    /// Specialization to obtain code for LambdaType::operator() const
    template<class LambdaListNodeType, class Res, class LambdaType, class... Args>
    struct ObtainSimpleAutoFreeFunction<LambdaListNodeType, Res(LambdaType::*)(Args...) const>:
        public ObtainSimpleAutoFreeFunction<LambdaListNodeType, Res(LambdaType::*)(Args...)> //reuse existing
    {};

    /// Specialization to obtain code for lambda that controls own lifetime
    template<class LambdaListNodeType, class Res, class LambdaType, class... Args>
    struct ObtainSimpleAutoFreeFunction<LambdaListNodeType, Res(LambdaType::*)(callback_extend_lifetime&, Args...)>{
        template<void* self>
        struct For{
            static Res apply(Args... args){
                CallbackExtendLifetimeImpl extendLifetime;
                Res res = static_cast<LambdaListNodeType*>(self)->lambda(extendLifetime, static_cast<Args&&>(args)...);
                
                if( extendLifetime.is_disposed() ){
                    // free corresponding node
                    static_cast<LambdaListNodeType*>(self)->Free();
                }

                return res;
            }
        };
    };
    /// Specialization to obtain code for void lambda that controls own lifetime
    template<class LambdaListNodeType, class LambdaType, class... Args>
    struct ObtainSimpleAutoFreeFunction<LambdaListNodeType, void(LambdaType::*)(callback_extend_lifetime&, Args...)>{
        template<void* self>
        struct For{
            static void apply(Args... args){
                CallbackExtendLifetimeImpl extendLifetime;
                static_cast<LambdaListNodeType*>(self)->lambda(extendLifetime, static_cast<Args&&>(args)...);
                
                if( extendLifetime.is_disposed() ){
                    // free corresponding node
                    static_cast<LambdaListNodeType*>(self)->Free();
                }
            }
        };
    };
    /// Specialization to obtain code for lambda that controls own lifetime
    template<class LambdaListNodeType, class Res, class LambdaType, class... Args>
    struct ObtainSimpleAutoFreeFunction<LambdaListNodeType, Res(LambdaType::*)(callback_extend_lifetime&, Args...) const>:
        public ObtainSimpleAutoFreeFunction<LambdaListNodeType, Res(LambdaType::*)(callback_extend_lifetime&, Args...)>
    {};

    /// Meta function to obtain code for LambdaType
    template<class LambdaListNodeType, class LambdaType, void* obj>
    using ObtainSimpleAutoFreeFunctionFor = typename
        ObtainSimpleAutoFreeFunction<LambdaListNodeType, decltype(&LambdaType::operator())>::template For<obj>;


    // special case of long leaving nodes

    /// Meta function to obtain code for LambdaType::operator()
    template<class LambdaListNodeType, class MethodSignature>
    struct ObtainSimpleNoAutoFreeFunction;

    /// Specialization to obtain code for LambdaType::operator()
    template<class LambdaListNodeType, class Res, class LambdaType, class... Args>
    struct ObtainSimpleNoAutoFreeFunction<LambdaListNodeType, Res(LambdaType::*)(Args...)>{
        template<void* self>
        struct For{
            static Res apply(Args... args){
                // own copy is needed to allow recursive allocation
                auto copyOfLambda = static_cast<LambdaType&&>(static_cast<LambdaListNodeType*>(self)->lambda);

                //Not that copy is able to tp whatever it likes with lambda
                return copyOfLambda(static_cast<Args&&>(args)...);
            }
        };
    };
    /// Specialization to obtain code for LambdaType::operator() const
    template<class LambdaListNodeType, class Res, class LambdaType, class... Args>
    struct ObtainSimpleNoAutoFreeFunction<LambdaListNodeType, Res(LambdaType::*)(Args...) const>:
        public ObtainSimpleNoAutoFreeFunction<LambdaListNodeType, Res(LambdaType::*)(Args...)> //reuse existing
    {};

    /// Meta function to obtain code for LambdaType
    template<class LambdaListNodeType, class LambdaType, void* obj>
    using ObtainSimpleNoAutoFreeFunctionFor = typename
        ObtainSimpleNoAutoFreeFunction<LambdaListNodeType, decltype(&LambdaType::operator())>::template For<obj>;


    //__________________________________________________________________________
    // callback_from_lambda: generate linked list of nodes


    /// Meta function to generate linked list of LambdaListNode and corresponding trampolines
    template<
        class LambdaType, class AdditionalOptionalTag, void (*freeMe)(void*),
        template<class, class, void*> class ObtainSimple, //Simple API is obtained here
        unsigned reservedCount
    >
    struct LambdaListNodeGenerator;

    /// Generate source for individual caller (all except first)
    template<
        class LambdaType, class AdditionalOptionalTag, void (*freeMe)(void*),
        template<class, class, void*> class ObtainSimple,
        unsigned reservedCount
    >
    struct LambdaListNodeGenerator{
        /// Node to store corresponding lambda
        static LambdaListNode<LambdaType, AdditionalOptionalTag, freeMe> node;
    };
    /// Generate source for individual caller (the first one)
    template<
        class LambdaType, class AdditionalOptionalTag, void (*freeMe)(void*),
        template<class, class, void*> class ObtainSimple
    >
    struct LambdaListNodeGenerator<LambdaType, AdditionalOptionalTag, freeMe, ObtainSimple, 1>{
        /// Node to store corresponding lambda
        static LambdaListNode<LambdaType, AdditionalOptionalTag, freeMe> node;
    };

    //The last node storing corresponding lambda is nas no "next" node
    template<
        class LambdaType, class AdditionalOptionalTag, void (*freeMe)(void*),
        template<class, class, void*> class ObtainSimple
    >
    LambdaListNode<LambdaType, AdditionalOptionalTag, freeMe>
    LambdaListNodeGenerator<LambdaType, AdditionalOptionalTag, freeMe, ObtainSimple, 1>::node{
        &ObtainSimple<
            LambdaListNode<LambdaType, AdditionalOptionalTag, freeMe>,
            LambdaType,
            &LambdaListNodeGenerator<LambdaType, AdditionalOptionalTag, freeMe, ObtainSimple, 1>::node
        >::apply,
        nullptr // no "next" node
    };
    //All the rest of the nodes reference 
    template<
        class LambdaType, class AdditionalOptionalTag, void (*freeMe)(void*),
        template<class, class, void*> class ObtainSimple,
        unsigned reservedCount
    >
    LambdaListNode<LambdaType, AdditionalOptionalTag, freeMe>
    LambdaListNodeGenerator<LambdaType, AdditionalOptionalTag, freeMe, ObtainSimple, reservedCount>::node{
        &ObtainSimple<
            LambdaListNode<LambdaType, AdditionalOptionalTag, freeMe>,
            LambdaType,
            &LambdaListNodeGenerator<LambdaType, AdditionalOptionalTag, freeMe, ObtainSimple, 1>::node
        >::apply,
        &LambdaListNodeGenerator<LambdaType, AdditionalOptionalTag, freeMe, reservedCount-1>::node
    };


    //__________________________________________________________________________
    // callback_from_lambda: managing nodes that are able to free self

    /// CallbackAllocator encapsulating linked list operations
    template<
        unsigned reservedCount, class AdditionalOptionalTag, class LambdaType,
        template<class, class, void*> class ObtainSimple
    >
    class CallbackAllocator{
        /// Freeing node (to be passed as template parameter)
        static void FreeNode(void* rawNode);        

        /// Cut off references to ger original lambda type 
        using OriginalLambda = std::remove_reference_t<LambdaType>;
    public:
        /// Type of nodes used for allocation 
        using Node = LambdaListNode<LambdaType, AdditionalOptionalTag, FreeNode>;

        /// Allocate item that forwards from simple function to lambda
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
            ObtainSimple,
            reservedCount
        >;

        /// Head of the list
        static Node* first;

        /// Ensure it possible to allocate from different threads
        static std::mutex protectAllocation;
    };

    template<
        unsigned reservedCount, class AdditionalOptionalTag, class LambdaType,
        template<class, class, void*> class ObtainSimple
    >
    typename CallbackAllocator<reservedCount, AdditionalOptionalTag, LambdaType, ObtainSimple>::Node* 
        CallbackAllocator<reservedCount, AdditionalOptionalTag, LambdaType, ObtainSimple>::first =
            &CallbackAllocator<reservedCount, AdditionalOptionalTag, LambdaType, ObtainSimple>::Generator::node;

    template<
        unsigned reservedCount, class AdditionalOptionalTag, class LambdaType,
        template<class, class, void*> class ObtainSimple
    >
    inline void CallbackAllocator<reservedCount, AdditionalOptionalTag, LambdaType, ObtainSimple>::FreeNode(void* rawNode){
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
        template<class, class, void*> class ObtainSimple
    >
    std::mutex 
    CallbackAllocator<reservedCount, AdditionalOptionalTag, LambdaType, ObtainSimple>::protectAllocation;


    // now specialization for the case of one node
    // for debugging purposes #define COROFORK_SUPPRESS_SPECIALIZATION
#   ifndef COROFORK_SUPPRESS_SPECIALIZATION

    /// CallbackAllocator with single item  
    template<
        class AdditionalOptionalTag, class LambdaType,
        template<class, class, void*> class ObtainSimple
    >
    class CallbackAllocator<1, AdditionalOptionalTag, LambdaType, ObtainSimple>{
        /// Freeing node (to be passed as template parameter)
        static void FreeSingleNode(void* rawNode);

        /// Cut off references to ger original lambda type 
        using OriginalLambda = std::remove_reference_t<LambdaType>;
    public:
        /// Type of nodes used for allocation 
        using Node = LambdaListNode<LambdaType, AdditionalOptionalTag, FreeSingleNode>;

        /// Allocate item that forwards from simple function to lambda
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

        /// Hack for asserting existing allocation
        /** ignore thread safety here))
         *  Used only for assertion purposes with single item allocation.
         *  We assume one will not use single item allocation in case
         *  of multiple threads */
        static bool isAllocated;
    public:
        using ObtainSimpleShortcut = ObtainSimple<
            Node,
            LambdaType, 
            &CallbackAllocator<1, AdditionalOptionalTag, LambdaType, ObtainSimple>::theOnlyNode
        >;
    };

    template<
        class AdditionalOptionalTag, class LambdaType,
        template<class, class, void*> class ObtainSimple
    >
    typename CallbackAllocator<1, AdditionalOptionalTag, LambdaType, ObtainSimple>::Node
        CallbackAllocator<1, AdditionalOptionalTag, LambdaType, ObtainSimple>::theOnlyNode(
            &CallbackAllocator<1, AdditionalOptionalTag, LambdaType, ObtainSimple>::ObtainSimpleShortcut::apply,
            nullptr
        );

    template<
        class AdditionalOptionalTag, class LambdaType,
        template<class, class, void*> class ObtainSimple
    >
    bool CallbackAllocator<1, AdditionalOptionalTag, LambdaType, ObtainSimple>::isAllocated = false;

    template<
        class AdditionalOptionalTag, class LambdaType,
        template<class, class, void*> class ObtainSimple
    >
    inline void CallbackAllocator<1, AdditionalOptionalTag, LambdaType, ObtainSimple>::FreeSingleNode(void* rawNode){
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
        /// Return argument wrapped into "plain C" callback
        template<class WrappedCallback>
        auto Decorate(WrappedCallback&& cb) noexcept{
            using Allocator = CallbackAllocator<
                maxCallbacksCount,
                AdditionalOptionalTag,
                std::remove_reference_t<WrappedCallback>,
                ObtainSimpleNoAutoFreeFunctionFor
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
        /// Setup simple generated subscription that returns nothing
        /** See corresponding invert_subscription */
        template<CoroForkLambdaToSetupFunction LambdaToSetupFunction>
        InvertSubscriptionHolder(
            const LambdaToSetupFunction& setupFunction ///< receives generated callback
        ):  finalCleanup( CoroFork_detail::ExtractFromLambda<LambdaToSetupFunction>
                            ::SubscribeSimpleCB(this, setupFunction)
            ) {}

        /// Setup simple generated subscription that returns value
        /** See corresponding invert_subscription,
         * (Remember here callback returns result immediately once issued
         *  and not when value is actually awaited) */
        template<
            CoroForkLambdaToSetupFunction LambdaToSetupFunction,
            class ResultForCallbackFunctionAsValue
        >
        requires
            std::convertible_to<
                std::remove_cvref_t<ResultForCallbackFunctionAsValue>,
                std::remove_cvref_t<
                    typename CoroFork_detail::ExtractFromLambda<LambdaToSetupFunction>::FunctionResultType
                >
            >
        InvertSubscriptionHolder(
            const LambdaToSetupFunction& setupFunction, ///< Your setup
            ResultForCallbackFunctionAsValue&& resultForCallbackFunction
        ):  finalCleanup( CoroFork_detail::ExtractFromLambda<LambdaToSetupFunction>
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

            /// Awaitable is ready once result is available from the queue
            /** \returns true if result is already there
             *   false if we need to suspend (callback was not issued yet) */
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
            void await_suspend(std::coroutine_handle<> handle){
                /* we still own the lock over holder->protectEventsQueue
                   so result was not placed here for sure! */
                this->handle = handle; //will be used by OnArgumentsArrived
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
            // Instaces are created only by InvertSubscriptionHolder
            friend class InvertSubscriptionHolder;
            /// Constructor for awaitable tied to InvertSubscriptionHolder
            awaitable(InvertSubscriptionHolder* correspondingHolder)
                : holder(correspondingHolder)
                , lockEventsQueue(correspondingHolder->protectEventsQueue)
                {}

            /// Ensure lock os obatined from the moment we are created
            std::unique_lock<std::mutex> lockEventsQueue;

            /// Subscription holder we use in awaitable
            InvertSubscriptionHolder* holder;

            /// What to resume once value arrives (used by InvertSubscriptionHolder)
            /** For scenario once there were no items */
            std::coroutine_handle<> handle;

            /// result to be returned once it is available
            /** Storing value here prevents extra locking */
            std::optional<AwaitedType> result;

            /// Called once result is available, but not under lock!
            void OnResultReady(){
                handle.resume();
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

        /// Protect operations with eventsQueue
        /** Subscription data may arrive from different threads */
        std::mutex protectEventsQueue;
    };


    /// Hold subscription as long as instance exists (void specialization)
    template<class Decorator>
    class InvertSubscriptionHolder<Decorator, void>: public Decorator{
    public:
        /// Setup simple generated subscription that returns nothing
        /** See corresponding invert_subscription */
        template<CoroForkLambdaToSetupFunction LambdaToSetupFunction>
        InvertSubscriptionHolder(
            const LambdaToSetupFunction& setupFunction ///< receives generated callback
        ):  finalCleanup( CoroFork_detail::ExtractFromLambda<LambdaToSetupFunction>
                            ::SubscribeSimpleCB(this, setupFunction)
            ) {}

        /// Setup simple generated subscription that returns value
        /** See corresponding invert_subscription,
         * (Remember here callback returns result immediately once issued
         *  and not when value is actually awaited) */
        template<
            CoroForkLambdaToSetupFunction LambdaToSetupFunction,
            class ResultForCallbackFunctionAsValue
        >
        requires
            std::convertible_to<
                std::remove_cvref_t<ResultForCallbackFunctionAsValue>,
                std::remove_cvref_t<
                    typename CoroFork_detail::ExtractFromLambda<LambdaToSetupFunction>::FunctionResultType
                >
            >
        InvertSubscriptionHolder(
            const LambdaToSetupFunction& setupFunction, ///< Your setup
            ResultForCallbackFunctionAsValue&& resultForCallbackFunction
        ):  finalCleanup( CoroFork_detail::ExtractFromLambda<LambdaToSetupFunction>
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

            /// Awaitable is ready once callback was already issued
            /** \returns true if callback was already issued
             *   false if we need to suspend (callback was not issued yet) */
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
            void await_suspend(std::coroutine_handle<> handle){
                /* we still own the lock over holder->protectEventsQueue
                   so callback did not increment eventsCount here for sure! */
                this->handle = handle; //will be used by OnEventHappened
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
                : holder(correspondingHolder)
                , lockEvents(correspondingHolder->protectEvents)
                {}

            /// Ensure lock os obatined from the moment we are created
            std::unique_lock<std::mutex> lockEvents;

            /// Subscription holder we use in awaitable
            InvertSubscriptionHolder* holder;

            /// What to resume once value arrives (used by InvertSubscriptionHolder)
            /** For scenario once there were no items */
            std::coroutine_handle<> handle;

            /// Called once callback was issued, but not under lock!
            void OnEventHappened(){
                handle.resume();
            }
        };

        /// Allow the same InvertSubscriptionHolder to be awaited multiple times
        awaitable operator co_await(){
            return awaitable(this);
        }

        /// To be called from "somewhere else" once callback is issued
        template<class... CallbackArgs>
        void OnEventHappened(CallbackArgs&&... args){
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

        /// Protect operations with eventsQueue
        /** Subscription data may arrive from different threads */
        std::mutex protectEvents;
    };

} //namespace CoroFork_detail


template<unsigned reservedCount, class AdditionalOptionalTag, class UniqueLambdaType>
auto callback_from_lambda(UniqueLambdaType&& lambda) -> 
    CoroFork_detail::SimpleSignatureFromLambda<UniqueLambdaType>*
{
    /* lambda will be moved to internal block pool
       and "auto free" strategy is applied,
       one shall not allocate nodes manually  */
    return CoroFork_detail::CallbackAllocator<
        reservedCount,
        AdditionalOptionalTag,
        std::remove_reference_t<UniqueLambdaType>,
        CoroFork_detail::ObtainSimpleAutoFreeFunctionFor
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
            CoroFork_detail::ObtainSimpleNoAutoFreeFunctionFor
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
        CoroFork_detail::ObtainSimpleNoAutoFreeFunctionFor
    >::Node;
    static_cast<Node*>(owningBlock)->Free();
}

template<
    unsigned reservedCount, class AdditionalOptionalTag, class UniqueLambdaType
> scoped_callback_holder<reservedCount, AdditionalOptionalTag, UniqueLambdaType>
::scoped_callback_holder(const scoped_callback_holder&& other)
    : owningBlock(other.owningBlock)
{
    other.owningBlock = nullptr; //cleanup other
}
    
template<
    unsigned reservedCount, class AdditionalOptionalTag, class UniqueLambdaType
>
scoped_callback_holder<reservedCount, AdditionalOptionalTag, UniqueLambdaType>&
scoped_callback_holder<reservedCount, AdditionalOptionalTag, UniqueLambdaType>
::operator=(const scoped_callback_holder&& other){
    //the destructor of temporary "other" will take care of cleanup
    std::swap(owningBlock, other.owningBlock);
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
        CoroFork_detail::ObtainSimpleNoAutoFreeFunctionFor
    >::Node;
    //Note: it is expected lambda was attached
    return static_cast<Node*>(owningBlock)->individualTrampoline;
}

template<
    unsigned reservedCount, class AdditionalOptionalTag, class UniqueLambdaType
>
auto scoped_callback(UniqueLambdaType&& lambda) ->
    scoped_callback_holder<
        reservedCount, AdditionalOptionalTag, UniqueLambdaType
    >
{
    return scoped_callback_holder<
        reservedCount, AdditionalOptionalTag, UniqueLambdaType
    >(std::forward<UniqueLambdaType>(lambda));
}


#endif
