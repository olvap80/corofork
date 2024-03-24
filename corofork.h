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
        // optionally more awaits can happen here
    };

    //Code that executes independently once we started awaiting 
 @endcode

There are invert_function and invert_callback API to generate 
std::function of "C style" callbacks respectively, and turn
any asynchronous API into awaitable.

Those "awaitables" are suitable for waiting from any coroutine.

For example one can turn asynchronous communication to linear in thins way:

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
        [](
            std::function<
                void(unsigned age, std::string address)
            > onReady
        ){
            //Pass generated callback to your async API 
            AsyncRequestAgeAndAddress(onReady);
        }
    );
    //here age is unsigned and address is std::string
    //(for multiple callback parameters std::tuple is returned)

    ... //etc
 @endcode
The main rule of such "inversion" is to repeat the signature,
as it is expected by the asynchronous API and then those arguments will be
received as a result of the await operation.

NOTE: remember invert_function and invert_callback will work with
      any coroutine that is able to do co_await operation,
      you can use them without corofork macro.

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
//runtime_error throws when trampoline allocation fails
#include <stdexcept>
//placement new is used for placing lambda to trampoline
#include <new>
//for std::nullptr_t
#include <cstddef>
//Used by invert_subscription to accumulate events (calls) happened so far
#include <queue>
//Used by invert_subscription_holder
#include <condition_variable>



//______________________________________________________________________________
// The macro to start coroutine inline (see samples above)


/// Start a coroutine "inline" (body follows in {})
/** Macro creates a new coroutine optionally capturing parameters  
 * The way how variables are captured is determined by the macro parameters
 * one can provide =, &, or named captures as well */
#define corofork(...) \
    CoroFork_detail::tagClassForLambda ->* \
    /* Note: lambda is not called below, operator->* does the stuff */ \
    [__VA_ARGS__]()->CoroFork_detail::CoroFork /* Lambda body follows here */



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
    // Forward declare awaitables

    template<class Decorator, class CoResult, class SetupLambdaType>
    class AwaitableSimpleCB;

    template<class Decorator, class CoResult,
        class SetupLambdaType, class GeneratedCallbackResultType>
    class AwaitableGeneratedCBReturnsValue;

    template<class Decorator, class CoResult,
        class SetupLambdaType, class CallbackToCallFromGeneratedCallbackType>
    class AwaitableGeneratedCBReturnsResultOfCall;

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
 * See samples above for illustration */
template<CoroForkLambdaToSetupFunction LambdaToSetupFunction>
auto invert_function(
    const LambdaToSetupFunction& setupFunction ///< receives generated callback
){
    return CoroFork_detail::AwaitableSimpleCB<
        CoroFork_detail::TheSame,
        typename CoroFork_detail::ExtractFromLambda<LambdaToSetupFunction>::CoResultType,
        LambdaToSetupFunction
    >(setupFunction);
}


/// Same as above, but generated function callback has a return value
/** In addition to the above logic Here the return value for
 * the generated function callback can be provided.
 * See samples above for illustration. */
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
auto invert_function(
    const LambdaToSetupFunction& setupFunction, ///< Your setup
    ResultForCallbackFunctionAsValue&& resultForCallbackFunction
){
    return CoroFork_detail::AwaitableGeneratedCBReturnsValue<
        CoroFork_detail::TheSame,
        typename CoroFork_detail::ExtractFromLambda<LambdaToSetupFunction>::CoResultType,
        LambdaToSetupFunction,
        typename CoroFork_detail::ExtractFromLambda<LambdaToSetupFunction>::FunctionResultType
    >(
        setupFunction, 
        std::forward<ResultForCallbackFunctionAsValue>(resultForCallbackFunction)
    );
}

/// Same as above, but generated callback returns value returned by functionToObtainResult
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
    return CoroFork_detail::AwaitableGeneratedCBReturnsResultOfCall<
        CoroFork_detail::TheSame,
        typename CoroFork_detail::ExtractFromLambda<LambdaToSetupFunction>::CoResultType,
        LambdaToSetupFunction,
        FunctionToObtainResult
    >(setupFunction, functionToObtainResultForCallbackFunction);
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
    return CoroFork_detail::AwaitableSimpleCB<
        CoroFork_detail::ToPlainC<
            maxCallbacksCount, LambdaToSetupCallback
        >,
        typename CoroFork_detail::ExtractFromLambda<
            LambdaToSetupCallback
        >::CoResultType,
        LambdaToSetupCallback
    >(setupCallback);
}


/// Same as above, but generated callback has a return value
/** In addition to the above logic Here the return value for
 * the generated callback can be provided.
 * See samples above for illustration. */
template<
    std::size_t maxCallbacksCount, ///< Specify callback count per lambda
    CoroForkLambdaToSetupCallback LambdaToSetupCallback, ///< Your setup
    class ResultForCallbackFunctionAsValue
>
requires    
    std::convertible_to<
        std::remove_cvref_t<
            ResultForCallbackFunctionAsValue
        >,
        std::remove_cvref_t<
            typename CoroFork_detail::ExtractFromLambda<LambdaToSetupCallback>::FunctionResultType
        >
    >
auto invert_callback(
    const LambdaToSetupCallback& setupCallback,
    ResultForCallbackFunctionAsValue&& resultForCallback
){
    return CoroFork_detail::AwaitableGeneratedCBReturnsValue<
        CoroFork_detail::ToPlainC<
            maxCallbacksCount, LambdaToSetupCallback
        >,
        typename CoroFork_detail::ExtractFromLambda<LambdaToSetupCallback>::CoResultType,
        LambdaToSetupCallback,
        typename CoroFork_detail::ExtractFromLambda<LambdaToSetupCallback>::FunctionResultType
    >(
        setupCallback, 
        std::forward<ResultForCallbackFunctionAsValue>(resultForCallback)
    );
}

/// Same as above, but generated callback returns value returned by functionToObtainResultForCallback
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
    return CoroFork_detail::AwaitableGeneratedCBReturnsResultOfCall<
        CoroFork_detail::ToPlainC<
            maxCallbacksCount, LambdaToSetupCallback
        >,
        typename CoroFork_detail::ExtractFromLambda<LambdaToSetupCallback>::CoResultType,
        LambdaToSetupCallback,
        FunctionToObtainResult
    >(setupCallback, functionToObtainResultForCallback);
}


//______________________________________________________________________________
// Turning subscription to awaitable for function


/// On subscription as long as instance exists
template<class AwaitedType>
class invert_subscription_holder{
public:
    /// Setup simple generated subscription that returns nothing
    /** See corresponding invert_subscription below */
    template<CoroForkLambdaToSetupFunction LambdaToSetupFunction>
    invert_subscription_holder(
        const LambdaToSetupFunction& setupFunction ///< receives generated callback
    ):  finalCleanup( CoroFork_detail::ExtractFromLambda<LambdaToSetupFunction>
                        ::SubscribeSimpleCB(this, setupFunction) ) {}

    /// Setup simple generated subscription that returns value
    /** See corresponding invert_subscription below */
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
    invert_subscription_holder(
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


    /// Number of items accumulated by subscription so far
    std::size_t count() const;

    // exists only in one place (but takes advantage of RVO)
    invert_subscription_holder(const invert_subscription_holder&) = delete;
    invert_subscription_holder& operator=(const invert_subscription_holder&) = delete;


    /// Awaitable being used 
    class awaitable{
    public:
        // can move, but cannot copy
        awaitable(const awaitable&&);
        awaitable& operator=(const awaitable&&);
        awaitable(const awaitable&) = delete;
        awaitable& operator=(const awaitable&) = delete;


    private:
        friend class invert_subscription_holder;
        ///
        awaitable(invert_subscription_holder* awaitFrom);
    };

    /// Allow the same invert_subscription_holder to be awaited multiple times
    awaitable operator co_await(){
        return awaitable(this);
    }
private:
    ///
    std::function< void() > finalCleanup;

    /// Events that are available so far
    std::queue<AwaitedType> eventsQueue;

    /// Protect operations with eventsQueue
    /** Subscription data may arrive from different threads */
    std::mutex protectEventsQueue;
    /// TODO do we need this?
    std::condition_variable onceItemAdded;
};

/// Turn std::function subscription based API into coroutine based
/** Provided lambda shall receive desired callback with corresponding signature
 * as a parameter. Such callback can be passed to asynchronous subscription API
 * to be called later (likely multiple times) by "something else".
 * Each time once called that generated function callback 
 * will resume the coroutine awaiting for invert_subscription_holder
 * being created by the invert_subscription call,
 * and callback arguments will go to caller.   
 * See samples above for illustration */
template<CoroForkLambdaToSetupFunction LambdaToSetupFunction>
auto invert_subscription(
    const LambdaToSetupFunction& setupFunction ///< receives generated callback
){
    //takes advantage of RVO here
    invert_subscription_holder<
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
    invert_subscription_holder<
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

    class CoroFork{
    public:
        /// Define coroutine behavior
        struct promise_type;

        ///Start with lambda that creates coroutine instance
        /** Ensures lambda is alive as long as coroutine is running */
        template<class Lambda>
        static void Start(Lambda&& lambda);
        
    private:
        /// Tie with promise once created
        CoroFork(promise_type* createdFrom): myPromise(createdFrom) {}

        /// Corresponding promise
        promise_type* myPromise;

        /// Common base for stored lambda
        class LambdaStorageBase;

        /// Store lambda (with captured) as long as coroutine is running
        template<class Lambda>
        class LambdaStorage;
    };

    struct CoroFork::promise_type{
        /// How actually CoroFork is obtained from the coroutine
        CoroFork get_return_object() { return CoroFork(this); }

        /// Shall be suspended (prevent deallocation until the lambda is safe)
        std::suspend_always initial_suspend() const noexcept { return {}; }
        
        /// Shall cleanup here
        std::suspend_never final_suspend() const noexcept {
            // do nothing (lambda will fade away with promise)
            return {}; //let coroutine destruct
        }

        /// Just do nothing
        void return_void() const noexcept {}
        
        /// Once exception happens, let it propagate
        void unhandled_exception() const { throw; }

        /// The way to make sure all lambda captures are alive as long coroutine is running  
        std::unique_ptr<LambdaStorageBase> capturingFunction;
    };


    class CoroFork::LambdaStorageBase{
    public:
        /// Allow cleanup via base class
        virtual ~LambdaStorageBase(){}

        /// Actually starts coroutine
        virtual CoroFork Start() = 0;
    };

    ///Store lambda (with captured) as long as coroutine is running
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
        virtual CoroFork Start() override{
            return lambda();
        }

    private:
        Lambda lambda; ///< Hold lambda, avoid slow std::function here
    };


    template<class Lambda>
    void CoroFork::Start(Lambda&& lambda){
        auto capture = std::unique_ptr<LambdaStorageBase>(
            new LambdaStorage< std::remove_reference_t<Lambda> >(
                    std::forward<Lambda>(lambda)
            )
        );
        //this will create corresponding promise
        CoroFork started = capture->Start();
        //capture will exist as long as the coroutine is running
        started.myPromise->capturingFunction = std::move(capture);

        //now capture is safe, so we can continue with coroutine
        std::coroutine_handle<promise_type>::from_promise(*started.myPromise).resume();

        /* Intentionally do not destroy coroutine here,
        let it cleanup self in natural way, as coroutine execution reaches end
        https://stackoverflow.com/questions/68352718/is-it-necessary-to-call-destroy-on-a-stdcoroutine-handle
        */ 
    }


    ///Helper type to trigger operator ->*
    struct TagClassForLambda{ constexpr TagClassForLambda() = default; };
    ///Use this "instance" to trigger overloaded operator ->*
    /** The trick with tagClassForLambda is needed
        to infer type of the lambda */
    constexpr TagClassForLambda tagClassForLambda;

    ///Helper operator to easy catch lambda for corofork macro
    /** Use template to avoid slow std::function
        (raw lambda is copied/stored here) */
    template<class Lambda>
    constexpr void operator ->* (const TagClassForLambda&, Lambda&& lambda){
        CoroFork::Start(std::move(lambda));
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
            std::coroutine_handle<> handle,
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
        CoResult await_resume() const noexcept {
            return *result;
        }


        /// To be called from "somewhere else" once result is ready
        template<class... CallbackArgs>
        void EmplaceCoResult(CallbackArgs&&... args){
            result.emplace(std::forward<CallbackArgs>(args)...);
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
        CoResult await_resume() const noexcept {
            return *result;
        }


        /// To be called from "somewhere else" once result is ready
        template<class... CallbackArgs>
        void EmplaceCoResult(CallbackArgs&&... args){
            result.emplace(std::forward<CallbackArgs>(args)...);
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
        CoResult await_resume() const noexcept {
            return *result;
        }


        /// To be called from "somewhere else" once result is ready
        template<class... CallbackArgs>
        void EmplaceCoResult(CallbackArgs&&... args){
            result.emplace(std::forward<CallbackArgs>(args)...);
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
