/** @file test_callback_pool.cpp
    @brief Cover the "plain C callback" machinery: callback_from_lambda,
           scoped_callback, callback_extend_lifetime and invert_callback.

The pool of trampolines is per (reservedCount, tag, lambda type), and every
lambda expression is a unique type, so each case below owns its own pool.
To reuse the same pool twice a case wraps the lambda into a maker lambda.
*/

#include "corofork.h"
#include "test_framework.h"

#include <functional>
#include <memory>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <vector>


//______________________________________________________________________________
// callback_from_lambda: single shot trampolines

COROFORK_TEST(callback_single_reserved_slot){
    int seen = 0;
    auto callback = callback_from_lambda<1>([&seen](int value){ seen = value; });

    callback(42);
    CHECK_EQ(seen, 42);
}

COROFORK_TEST(callback_returns_value){
    int factor = 2; //captured, so a real trampoline is generated
    auto callback = callback_from_lambda<1>([factor](int value){ return value * factor; });
    CHECK_EQ(callback(21), 42);
}

COROFORK_TEST(lambdas_without_captures_are_detected_and_rejected){
    /* A capture less lambda already IS a "plain C" callback, so both
       callback_from_lambda and scoped_callback refuse it and tell the caller
       to pass it to the target API directly. That refusal is a static_assert,
       so it cannot be exercised from a test that has to compile: what is
       locked here is the detection those static_asserts are built on. */
    using NoCaptures = decltype( [](int){ } );
    using NoCapturesAskingLifetime = decltype( [](callback_extend_lifetime&, int){ } );
    int captured = 0;
    using WithCaptures = decltype( [captured](int){ (void)captured; } );

    //plain capture less lambda converts to the callback the target API wants
    static_assert(CoroFork_detail::ConvertsToPlainFunctionCallback<NoCaptures>);

    /* capture less lambda asking for lifetime control does NOT convert to the
       plain function signature (that extra parameter), but does convert to the
       declared one, and that combination is what the diagnostic catches */
    static_assert(!CoroFork_detail::ConvertsToPlainFunctionCallback<NoCapturesAskingLifetime>);
    static_assert(CoroFork_detail::ConvertsToLifetimeAwareCallback<NoCapturesAskingLifetime>);

    //a capturing lambda converts to neither, so it goes to the block pool
    static_assert(!CoroFork_detail::ConvertsToPlainFunctionCallback<WithCaptures>);
    static_assert(!CoroFork_detail::ConvertsToLifetimeAwareCallback<WithCaptures>);

    //nothing to do at run time, the whole case is compile time
    CHECK_EQ(captured, 0);
}

COROFORK_TEST(callback_several_reserved_slots_are_independent){
    /* The whole point of reservedCount > 1: every trampoline must own its
       own storage, otherwise all of them would share one captured state. */
    std::vector<int> observed;

    void (*callbacks[3])() = {};
    for( int i = 0; i < 3; ++i ){
        callbacks[i] = callback_from_lambda<3>([&observed, i]{ observed.push_back(i); });
    }

    CHECK(callbacks[0] != callbacks[1]);
    CHECK(callbacks[1] != callbacks[2]);
    CHECK(callbacks[0] != callbacks[2]);

    //issue them out of order, each must report its own captured index
    callbacks[1]();
    callbacks[2]();
    callbacks[0]();

    CHECK_EQ(observed.size(), std::size_t(3));
    if( observed.size() == 3 ){
        CHECK_EQ(observed[0], 1);
        CHECK_EQ(observed[1], 2);
        CHECK_EQ(observed[2], 0);
    }
}

COROFORK_TEST(callback_slots_are_reused_after_call){
    int calls = 0;
    // same lambda expression, so all iterations share one pool of 2 slots
    auto make = [&calls]{ return callback_from_lambda<2>([&calls]{ ++calls; }); };

    for( int round = 0; round < 5; ++round ){
        auto first = make();
        auto second = make();
        first();  //single shot: frees its slot
        second(); //single shot: frees its slot
    }
    CHECK_EQ(calls, 10);
}

COROFORK_TEST(callback_pool_exhaustion_throws){
    //the capture is what forces a real pool slot to be taken
    int sink = 0;
    auto make = [&sink]{ return callback_from_lambda<2>([&sink]{ ++sink; }); };

    auto first = make();
    auto second = make();
    CHECK(first != nullptr);
    CHECK(second != nullptr);

    //third one has no slot left
    CHECK_THROWS(make(), std::runtime_error);
}


//______________________________________________________________________________
// callback_extend_lifetime: lambda decides when the slot is released

COROFORK_TEST(callback_extend_lifetime_controls_disposal){
    int total = 0;
    auto make = [&total]{
        return callback_from_lambda<1>(
            [&total](callback_extend_lifetime& lifetime, int value){
                total += value;
                if( total >= 6 ){
                    lifetime.dispose(); //release the slot, no more calls expected
                }
            }
        );
    };

    auto callback = make();
    callback(1);
    callback(2);
    CHECK_EQ(total, 3);
    callback(3); //reaches 6 and disposes
    CHECK_EQ(total, 6);

    //slot must be free again, otherwise this throws
    auto second = make();
    second(10);
    CHECK_EQ(total, 16);
}


//______________________________________________________________________________
// scoped_callback: slot is tied to the holder lifetime

COROFORK_TEST(scoped_callback_lives_as_long_as_holder){
    int seen = 0;
    auto make = [&seen]{ return scoped_callback<1>([&seen](int value){ seen = value; }); };

    {
        auto holder = make();
        holder.Callback()(5);
        CHECK_EQ(seen, 5);
        holder.Callback()(6); //unlike callback_from_lambda it survives the call
        CHECK_EQ(seen, 6);
    }

    //holder is gone, its slot must be free again
    auto second = make();
    second.Callback()(7);
    CHECK_EQ(seen, 7);
}

COROFORK_TEST(scoped_callback_holder_stays_where_it_was_created){
    /* Locks in the "deliberately narrow scope" decision documented on
       scoped_callback_holder in corofork.h: a holder is created and dies in
       the scope that subscribes, it is never moved, never reassigned another
       lambda and never collected into a container. If one of the assertions
       below starts failing, read that note before relaxing anything. */
    int seen = 0;
    auto make = [&seen]{ return scoped_callback<1>([&seen](int value){ seen = value; }); };
    using Holder = decltype(make());

    static_assert(!std::is_copy_constructible_v<Holder>);
    static_assert(!std::is_copy_assignable_v<Holder>);
    static_assert(!std::is_move_constructible_v<Holder>);
    static_assert(!std::is_move_assignable_v<Holder>);

    //returning by value still works, guaranteed elision needs no move
    auto holder = make();
    holder.Callback()(3);
    CHECK_EQ(seen, 3);
}


//______________________________________________________________________________
// invert_callback: awaiting on a "plain C" callback

COROFORK_TEST(invert_callback_no_arguments){
    void (*stored)() = nullptr;
    bool resumed = false;

    corofork(&){
        co_await invert_callback<1>([&](void (*cb)()){ stored = cb; });
        resumed = true;
        co_return;
    };

    CHECK(stored != nullptr);
    CHECK_EQ(resumed, false);
    stored();
    CHECK_EQ(resumed, true);
}

COROFORK_TEST(invert_callback_single_argument){
    void (*stored)(int) = nullptr;
    int received = 0;

    corofork(&){
        received = co_await invert_callback<1>([&](void (*cb)(int)){ stored = cb; });
        co_return;
    };

    stored(55);
    CHECK_EQ(received, 55);
}

COROFORK_TEST(invert_callback_multiple_arguments){
    void (*stored)(int, int) = nullptr;
    int sum = 0;

    corofork(&){
        auto [a, b] = co_await invert_callback<1>([&](void (*cb)(int, int)){ stored = cb; });
        sum = a + b;
        co_return;
    };

    stored(30, 12);
    CHECK_EQ(sum, 42);
}

COROFORK_TEST(invert_callback_returning_value){
    int (*stored)(int) = nullptr;
    int received = 0;

    corofork(&){
        received = co_await invert_callback<1>(
            [&](int (*cb)(int)){ stored = cb; },
            99 //what the generated callback returns to the C API
        );
        co_return;
    };

    CHECK_EQ(stored(7), 99);
    CHECK_EQ(received, 7);
}

COROFORK_TEST(invert_callback_argument_of_move_only_type){
    void (*stored)(std::unique_ptr<int>) = nullptr;
    int received = 0;

    corofork(&){
        std::unique_ptr<int> value = co_await invert_callback<1>(
            [&](void (*cb)(std::unique_ptr<int>)){ stored = cb; }
        );
        received = *value;
        co_return;
    };

    stored(std::make_unique<int>(31));
    CHECK_EQ(received, 31);
}


//______________________________________________________________________________
// resuming frees the trampoline node (the awaiter dies at the end of the
// co_await expression), so what goes back to the C API is taken before resuming

COROFORK_TEST(invert_callback_returned_value_outlives_freed_trampoline){
    using corofork_test::LifetimeTrackedValue;
    LifetimeTrackedValue (*stored)(int) = nullptr;
    int received = 0;

    corofork(&){
        received = co_await invert_callback<1>(
            [&](LifetimeTrackedValue (*cb)(int)){ stored = cb; },
            LifetimeTrackedValue(99)
        );
        co_return;
    };

    LifetimeTrackedValue::copiesFromDestroyedSource = 0;
    LifetimeTrackedValue returned = stored(7);

    CHECK_EQ(received, 7);
    CHECK_EQ(returned.payload, 99);
    CHECK_EQ(LifetimeTrackedValue::copiesFromDestroyedSource, 0);
}

COROFORK_TEST(invert_callback_result_of_call_outlives_freed_trampoline){
    using corofork_test::LifetimeTrackedValue;
    LifetimeTrackedValue (*stored)(int) = nullptr;
    int received = 0;

    corofork(&){
        received = co_await invert_callback<1>(
            [&](LifetimeTrackedValue (*cb)(int)){ stored = cb; },
            [trackedPayload = LifetimeTrackedValue(55)]{ return trackedPayload; }
        );
        co_return;
    };

    LifetimeTrackedValue::copiesFromDestroyedSource = 0;
    LifetimeTrackedValue returned = stored(8);

    CHECK_EQ(received, 8);
    CHECK_EQ(returned.payload, 55);
    CHECK_EQ(LifetimeTrackedValue::copiesFromDestroyedSource, 0);
}


//______________________________________________________________________________
// all trampolines of one invert_callback place in use: catchable exception

COROFORK_TEST(invert_callback_exhausted_pool_throws_inside_coroutine){
    void (*storedCallbacks[2])() = {nullptr, nullptr};
    std::string errorMessages[2];

    for( int coroutineIndex = 0; coroutineIndex < 2; ++coroutineIndex ){
        //both coroutines come from one place, so both use one pool of 1 trampoline
        corofork(&, coroutineIndex){
            try{
                co_await invert_callback<1>([&](void (*cb)()){
                    storedCallbacks[coroutineIndex] = cb;
                });
            }
            catch(const std::runtime_error& e){
                errorMessages[coroutineIndex] = e.what();
            }
            co_return;
        };
    }

    CHECK(storedCallbacks[0] != nullptr);
    CHECK(storedCallbacks[1] == nullptr);
    CHECK_EQ(errorMessages[0], std::string());
    CHECK_EQ(errorMessages[1], std::string("callback_from_lambda cannot allocate trampoline"));

    if( storedCallbacks[0] ){
        storedCallbacks[0](); //finishes the first coroutine, frees the only trampoline
    }
}


//______________________________________________________________________________
// the awaitable owns the setup lambda: helpers may return invert_callback(...),
// and the setup lambda is moved, never copied

namespace{
    /// Helper returning invert_callback(...): the setup lambda dies with the helper unless owned
    auto storePlainCallbackAsync(
        void (*&stored)(int),
        corofork_test::LifetimeTrackedValue tracked,
        int& payloadSeenBySetup
    ){
        return invert_callback<1>(
            [&stored, &payloadSeenBySetup, tracked](void (*cb)(int)){
                corofork_test::LifetimeTrackedValue copyInSetup = tracked;
                payloadSeenBySetup = copyInSetup.payload;
                stored = cb;
            }
        );
    }
}

COROFORK_TEST(invert_callback_helper_may_return_invert_callback){
    using corofork_test::LifetimeTrackedValue;
    void (*stored)(int) = nullptr;
    int payloadSeenBySetup = 0;
    int received = 0;

    LifetimeTrackedValue::copiesFromDestroyedSource = 0;
    corofork(&){
        received = co_await storePlainCallbackAsync(stored, LifetimeTrackedValue(65), payloadSeenBySetup);
        co_return;
    };

    CHECK_EQ(payloadSeenBySetup, 65);
    CHECK_EQ(LifetimeTrackedValue::copiesFromDestroyedSource, 0);
    if( stored ){
        stored(6);
    }
    CHECK_EQ(received, 6);
}

COROFORK_TEST(invert_callback_setup_lambda_is_moved_not_copied){
    using corofork_test::CopyMoveCounter;
    void (*stored)() = nullptr;

    CopyMoveCounter::Reset();
    corofork(&){
        co_await invert_callback<1>(
            [counter = CopyMoveCounter(), &stored](void (*cb)()){ stored = cb; }
        );
        co_return;
    };

    CHECK_EQ(CopyMoveCounter::copies, 0);
    CHECK(CopyMoveCounter::moves <= 2); //same 2 moves as invert_function
    stored();
}


//______________________________________________________________________________
// the node behind a scoped_callback is not freed by the call, so the lambda
// must be invoked where it lives (moving it out would leave everything it
// captured by value in a "moved from" state for every next call)

COROFORK_TEST(scoped_callback_keeps_captured_state_across_calls){
    const std::string expected = "captured by value, long enough to avoid the small string buffer";

    std::string payload = expected;
    auto holder = scoped_callback<1>(
        [payload](int) -> std::string { return payload; }
    );

    CHECK_EQ(holder.Callback()(1), expected);
    CHECK_EQ(holder.Callback()(2), expected); //used to come back empty
    CHECK_EQ(holder.Callback()(3), expected);
}

COROFORK_TEST(callback_extend_lifetime_keeps_captured_state_until_disposed){
    //same property for the other trampoline that survives its own call
    const std::string expected = "captured by value, long enough to avoid the small string buffer";
    std::vector<std::string> seen;

    std::string payload = expected;
    auto callback = callback_from_lambda<1>(
        [payload, &seen](callback_extend_lifetime& lifetime, int last){
            seen.push_back(payload);
            if( last ){
                lifetime.dispose();
            }
        }
    );

    callback(0);
    callback(0);
    callback(1); //disposes, freeing the slot

    CHECK_EQ(seen.size(), std::size_t(3));
    for( const std::string& one : seen ){
        CHECK_EQ(one, expected);
    }
}
