/** @file test_invert_function.cpp
    @brief Cover invert_function: 0/1/N callback arguments, callbacks with
           and without a return value, synchronous completion and the
           "callback takes a reference" shapes real APIs actually use.

The pattern used almost everywhere below is:
    corofork(&){ ... co_await invert_function([&](Signature cb){ stored = cb; }); ... };
    // coroutine is suspended here, nothing ran yet
    stored(...);                 // resumes the coroutine in this very thread
That keeps every case single threaded and fully deterministic.
*/

#include "corofork.h"
#include "test_framework.h"

#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <tuple>
#include <vector>


//______________________________________________________________________________
// callbacks that return nothing

COROFORK_TEST(function_no_arguments){
    std::function<void()> stored;
    bool resumed = false;

    corofork(&){
        co_await invert_function([&](std::function<void()> cb){
            stored = cb;
        });
        resumed = true;
        co_return;
    };

    CHECK(bool(stored));
    CHECK_EQ(resumed, false); //still suspended, callback was not issued yet

    stored();
    CHECK_EQ(resumed, true);
}

COROFORK_TEST(function_single_argument){
    std::function<void(int)> stored;
    int received = 0;

    corofork(&){
        received = co_await invert_function([&](std::function<void(int)> cb){
            stored = cb;
        });
        co_return;
    };

    stored(4202);
    CHECK_EQ(received, 4202);
}

COROFORK_TEST(function_multiple_arguments){
    std::function<void(int, double)> stored;
    int receivedInt = 0;
    double receivedDouble = 0;

    corofork(&){
        auto [i, d] = co_await invert_function([&](std::function<void(int, double)> cb){
            stored = cb;
        });
        receivedInt = i;
        receivedDouble = d;
        co_return;
    };

    stored(17, 2.5);
    CHECK_EQ(receivedInt, 17);
    CHECK_EQ(receivedDouble, 2.5);
}

COROFORK_TEST(function_const_reference_argument){
    // void(const T&) is the single most common shape in real callback APIs
    std::function<void(const std::string&)> stored;
    std::string received;

    corofork(&){
        received = co_await invert_function([&](std::function<void(const std::string&)> cb){
            stored = cb;
        });
        co_return;
    };

    {
        std::string temporary = "payload that dies right away";
        stored(temporary);
    }
    CHECK_EQ(received, std::string("payload that dies right away"));
}

COROFORK_TEST(function_mixed_reference_arguments){
    std::function<void(const std::string&, int)> stored;
    std::string receivedName;
    int receivedValue = 0;

    corofork(&){
        auto [name, value] = co_await invert_function(
            [&](std::function<void(const std::string&, int)> cb){ stored = cb; }
        );
        receivedName = name;
        receivedValue = value;
        co_return;
    };

    {
        std::string temporary = "named event";
        stored(temporary, 9);
    }
    CHECK_EQ(receivedName, std::string("named event"));
    CHECK_EQ(receivedValue, 9);
}


//______________________________________________________________________________
// callbacks that return a value

COROFORK_TEST(function_returning_fixed_value){
    std::function<int(int)> stored;
    int received = 0;

    corofork(&){
        received = co_await invert_function(
            [&](std::function<int(int)> cb){ stored = cb; },
            77 //what the generated callback returns to its caller
        );
        co_return;
    };

    CHECK_EQ(stored(5), 77);
    CHECK_EQ(received, 5);
}

COROFORK_TEST(function_returning_result_of_call){
    std::function<int()> stored;
    bool resumed = false;

    corofork(&){
        co_await invert_function(
            [&](std::function<int()> cb){ stored = cb; },
            []{ return 123; } //calculated when the generated callback is issued
        );
        resumed = true;
        co_return;
    };

    CHECK_EQ(stored(), 123);
    CHECK_EQ(resumed, true);
}


//______________________________________________________________________________
// setup that already knows the answer (no suspension at all)

COROFORK_TEST(function_completes_synchronously_without_value){
    bool setupCalled = false;
    bool resumed = false;

    corofork(&){
        co_await invert_function([&](std::function<void()>) -> bool {
            setupCalled = true;
            return true; //completed synchronously, callback was not scheduled
        });
        resumed = true;
        co_return;
    };

    CHECK_EQ(setupCalled, true);
    CHECK_EQ(resumed, true); //ran to the end without anybody resuming it
}

COROFORK_TEST(function_completes_synchronously_with_value){
    int received = 0;

    corofork(&){
        received = co_await invert_function(
            [&](std::function<void(int)>) -> std::optional<int> {
                return 31337; //result is available immediately
            }
        );
        co_return;
    };

    CHECK_EQ(received, 31337);
}

COROFORK_TEST(function_completes_synchronously_with_tuple){
    int receivedInt = 0;
    std::string receivedText;

    corofork(&){
        auto [i, text] = co_await invert_function(
            [&](std::function<void(int, const std::string&)>)
                -> std::optional<std::tuple<int, std::string>>
            {
                return std::tuple<int, std::string>{8, "ready"};
            }
        );
        receivedInt = i;
        receivedText = text;
        co_return;
    };

    CHECK_EQ(receivedInt, 8);
    CHECK_EQ(receivedText, std::string("ready"));
}


//______________________________________________________________________________
// sequencing: several inversions in a row inside one coroutine

COROFORK_TEST(function_sequence_of_awaits){
    std::function<void(int)> first;
    std::function<void(int)> second;
    int sum = 0;

    corofork(&){
        int a = co_await invert_function([&](std::function<void(int)> cb){ first = cb; });
        int b = co_await invert_function([&](std::function<void(int)> cb){ second = cb; });
        sum = a + b;
        co_return;
    };

    CHECK(bool(first));
    CHECK(!second); //second setup only runs once the first await completes

    first(40);
    CHECK(bool(second));
    CHECK_EQ(sum, 0);

    second(2);
    CHECK_EQ(sum, 42);
}


//______________________________________________________________________________
// callback arguments of move only types are moved through, never copied

COROFORK_TEST(function_argument_of_move_only_type){
    std::function<void(std::unique_ptr<int>)> stored;
    int received = 0;

    corofork(&){
        std::unique_ptr<int> value = co_await invert_function(
            [&](std::function<void(std::unique_ptr<int>)> cb){ stored = cb; }
        );
        received = *value;
        co_return;
    };

    stored(std::make_unique<int>(77));
    CHECK_EQ(received, 77);
}

COROFORK_TEST(function_multiple_arguments_of_move_only_type){
    std::function<void(std::unique_ptr<int>, std::string)> stored;
    int receivedNumber = 0;
    std::string receivedText;

    corofork(&){
        auto [number, text] = co_await invert_function(
            [&](std::function<void(std::unique_ptr<int>, std::string)> cb){ stored = cb; }
        );
        receivedNumber = *number;
        receivedText = text;
        co_return;
    };

    stored(std::make_unique<int>(5), std::string("five"));
    CHECK_EQ(receivedNumber, 5);
    CHECK_EQ(receivedText, std::string("five"));
}


//______________________________________________________________________________
// the coroutine replaces the std::function being executed right now,
// the generated callback must not read own captures after resuming

COROFORK_TEST(function_returned_value_survives_callback_replaced_while_resumed){
    using corofork_test::LifetimeTrackedValue;
    std::function<LifetimeTrackedValue(int)> stored;
    std::vector<int> received;

    corofork(&){
        for( int round = 0; round < 2; ++round ){
            received.push_back(co_await invert_function(
                [&](std::function<LifetimeTrackedValue(int)> cb){ stored = cb; },
                LifetimeTrackedValue(100 + round)
            ));
        }
        co_return;
    };

    LifetimeTrackedValue::copiesFromDestroyedSource = 0;
    LifetimeTrackedValue firstReturned = stored(1); //the coroutine replaces stored inside
    LifetimeTrackedValue secondReturned = stored(2);

    CHECK_EQ(firstReturned.payload, 100);
    CHECK_EQ(secondReturned.payload, 101);
    CHECK_EQ(LifetimeTrackedValue::copiesFromDestroyedSource, 0);
    CHECK((received == std::vector<int>{1, 2}));
}
