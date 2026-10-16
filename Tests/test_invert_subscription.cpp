/** @file test_invert_subscription.cpp
    @brief Cover invert_subscription: repeated awaits on one subscription,
           events accumulated while nobody awaits, optional cleanup on scope
           exit, callbacks with a return value and cross thread delivery.
*/

#include "corofork.h"
#include "test_framework.h"

#include <cstddef>
#include <functional>
#include <future>
#include <memory>
#include <string>
#include <thread>
#include <vector>


//______________________________________________________________________________
// events delivered while the coroutine is already awaiting

COROFORK_TEST(subscription_resumes_on_every_event){
    std::function<void(int)> fire;
    std::vector<int> received;

    corofork(&){
        auto subscription = invert_subscription(
            [&](std::function<void(int)> cb){ fire = cb; }
        );
        for( int i = 0; i < 3; ++i ){
            received.push_back(co_await subscription);
        }
        co_return;
    };

    CHECK(bool(fire));
    CHECK_EQ(received.size(), std::size_t(0)); //suspended on the first await

    fire(10);
    fire(20);
    fire(30);

    CHECK_EQ(received.size(), std::size_t(3));
    if( received.size() == 3 ){
        CHECK_EQ(received[0], 10);
        CHECK_EQ(received[1], 20);
        CHECK_EQ(received[2], 30);
    }
}


//______________________________________________________________________________
// events accumulated before anybody awaits

COROFORK_TEST(subscription_queues_events_and_reports_count){
    std::vector<int> received;
    std::size_t countAfterThree = 0;
    std::size_t countAfterOneConsumed = 0;

    corofork(&){
        std::function<void(int)> fire;
        auto subscription = invert_subscription(
            [&](std::function<void(int)> cb){ fire = cb; }
        );

        //nobody awaits right now, everything goes to the queue
        fire(1);
        fire(2);
        fire(3);
        countAfterThree = subscription.count();

        received.push_back(co_await subscription); //taken from the queue
        countAfterOneConsumed = subscription.count();

        received.push_back(co_await subscription);
        received.push_back(co_await subscription);
        co_return;
    };

    CHECK_EQ(countAfterThree, std::size_t(3));
    CHECK_EQ(countAfterOneConsumed, std::size_t(2));
    CHECK_EQ(received.size(), std::size_t(3));
    if( received.size() == 3 ){
        CHECK_EQ(received[0], 1);
        CHECK_EQ(received[1], 2);
        CHECK_EQ(received[2], 3);
    }
}

COROFORK_TEST(subscription_queues_events_without_arguments){
    unsigned resumeCount = 0;
    std::size_t countBefore = 0;

    corofork(&){
        std::function<void()> fire;
        auto subscription = invert_subscription(
            [&](std::function<void()> cb){ fire = cb; }
        );

        fire();
        fire();
        countBefore = subscription.count();

        co_await subscription;
        ++resumeCount;
        co_await subscription;
        ++resumeCount;
        co_return;
    };

    CHECK_EQ(countBefore, std::size_t(2));
    CHECK_EQ(resumeCount, 2u);
}


//______________________________________________________________________________
// queued events must survive the death of what the callback referenced

COROFORK_TEST(subscription_queue_owns_reference_arguments){
    std::vector<std::string> names;
    std::vector<int> numbers;

    corofork(&){
        std::function<void(const std::string&, int)> fire;
        auto subscription = invert_subscription(
            [&](std::function<void(const std::string&, int)> cb){ fire = cb; }
        );

        //both payloads are gone by the time they are awaited
        {
            std::string payload = "first payload, long enough to avoid small string buffer";
            fire(payload, 1);
        }
        {
            std::string payload = "second payload, long enough to avoid small string buffer";
            fire(payload, 2);
        }

        for( int i = 0; i < 2; ++i ){
            auto [name, number] = co_await subscription;
            names.push_back(name);
            numbers.push_back(number);
        }
        co_return;
    };

    CHECK_EQ(names.size(), std::size_t(2));
    if( names.size() == 2 ){
        CHECK_EQ(names[0], std::string("first payload, long enough to avoid small string buffer"));
        CHECK_EQ(names[1], std::string("second payload, long enough to avoid small string buffer"));
        CHECK_EQ(numbers[0], 1);
        CHECK_EQ(numbers[1], 2);
    }
}

COROFORK_TEST(subscription_queue_owns_single_reference_argument){
    std::vector<std::string> names;

    corofork(&){
        std::function<void(const std::string&)> fire;
        auto subscription = invert_subscription(
            [&](std::function<void(const std::string&)> cb){ fire = cb; }
        );

        {
            std::string payload = "only payload, long enough to avoid small string buffer";
            fire(payload);
        }
        names.push_back(co_await subscription);
        co_return;
    };

    CHECK_EQ(names.size(), std::size_t(1));
    if( names.size() == 1 ){
        CHECK_EQ(names[0], std::string("only payload, long enough to avoid small string buffer"));
    }
}


//______________________________________________________________________________
// optional cleanup returned by the setup lambda

COROFORK_TEST(subscription_cleanup_runs_on_scope_exit){
    bool cleaned = false;
    bool cleanedWhileInScope = true;

    corofork(&){
        std::function<void()> fire;
        {
            auto subscription = invert_subscription(
                [&](std::function<void()> cb){
                    fire = cb;
                    return [&]{ cleaned = true; }; //unsubscribe on scope exit
                }
            );
            fire();
            co_await subscription;
            cleanedWhileInScope = cleaned;
        }
        co_return;
    };

    CHECK_EQ(cleanedWhileInScope, false);
    CHECK_EQ(cleaned, true);
}

COROFORK_TEST(subscription_cleanup_with_multiple_arguments){
    //same shape as above, but the callback carries several arguments
    bool cleaned = false;
    int sum = 0;

    corofork(&){
        std::function<void(int, int)> fire;
        {
            auto subscription = invert_subscription(
                [&](std::function<void(int, int)> cb){
                    fire = cb;
                    return [&]{ cleaned = true; };
                }
            );
            fire(2, 3);
            auto [a, b] = co_await subscription;
            sum = a + b;
        }
        co_return;
    };

    CHECK_EQ(sum, 5);
    CHECK_EQ(cleaned, true);
}


//______________________________________________________________________________
// subscription whose generated callback returns a value

COROFORK_TEST(subscription_callback_returns_value){
    std::function<bool(const std::string&, unsigned)> fire;
    std::vector<std::string> names;
    std::vector<unsigned> iterations;

    corofork(&){
        auto subscription = invert_subscription(
            [&](std::function<bool(const std::string&, unsigned)> cb){ fire = cb; },
            true //what the generated callback answers to the event source
        );
        for( int i = 0; i < 2; ++i ){
            auto [name, iteration] = co_await subscription;
            names.push_back(name);
            iterations.push_back(iteration);
        }
        co_return;
    };

    CHECK(bool(fire));
    {
        std::string payload = "tick, long enough to avoid the small string buffer";
        CHECK_EQ(fire(payload, 0u), true); //answer is available immediately
        CHECK_EQ(fire(payload, 1u), true);
    }

    CHECK_EQ(names.size(), std::size_t(2));
    if( names.size() == 2 ){
        CHECK_EQ(names[0], std::string("tick, long enough to avoid the small string buffer"));
        CHECK_EQ(iterations[0], 0u);
        CHECK_EQ(iterations[1], 1u);
    }
}


//______________________________________________________________________________
// events produced by another thread

COROFORK_TEST(subscription_delivers_from_another_thread){
    std::promise< std::function<void(int)> > handoff;
    std::future< std::function<void(int)> > subscribed = handoff.get_future();
    std::vector<int> received;

    std::thread worker([&]{
        std::function<void(int)> fire = subscribed.get();
        for( int i = 0; i < 5; ++i ){
            fire(i); //each call resumes the coroutine right in this thread
        }
    });

    corosync(&){
        auto subscription = invert_subscription(
            [&](std::function<void(int)> cb){ handoff.set_value(cb); }
        );
        for( int i = 0; i < 5; ++i ){
            received.push_back(co_await subscription);
        }

        co_await back_to_thread; //continue in the thread that started corosync

        /* joining here (and not after corosync) guarantees the producer is
           done before the subscription holder goes out of scope */
        worker.join();
        co_return;
    };

    if( worker.joinable() ){
        worker.join(); //safety net if the coroutine never got that far
    }

    CHECK_EQ(received.size(), std::size_t(5));
    for( int i = 0; i < int(received.size()); ++i ){
        CHECK_EQ(received[std::size_t(i)], i);
    }
}


//______________________________________________________________________________
// event arguments of move only types, both through the queue and directly

COROFORK_TEST(subscription_argument_of_move_only_type){
    std::function<void(std::unique_ptr<int>)> fire;
    std::vector<int> received;

    corofork(&){
        auto subscription = invert_subscription(
            [&](std::function<void(std::unique_ptr<int>)> cb){ fire = cb; }
        );
        fire(std::make_unique<int>(1)); //nobody awaits yet: goes through the queue
        for( int eventIndex = 0; eventIndex < 2; ++eventIndex ){
            std::unique_ptr<int> value = co_await subscription;
            received.push_back(*value);
        }
        co_return;
    };

    fire(std::make_unique<int>(2)); //the coroutine awaits: handed over directly
    CHECK((received == std::vector<int>{1, 2}));
}
