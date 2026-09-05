/** @file test_corofork.cpp
    @brief Cover the corofork and corosync macros themselves: capture
           lifetime, move only captures, thread switching with back_to_thread
           and exception propagation back to the initial thread.
*/

#include "corofork.h"
#include "test_framework.h"

#include <functional>
#include <future>
#include <memory>
#include <stdexcept>
#include <string>
#include <thread>


//______________________________________________________________________________
// corofork: starts in the current thread, does not block the caller

COROFORK_TEST(corofork_runs_body_immediately){
    int observed = 0;

    corofork(&){
        observed = 1;
        co_return;
    };

    CHECK_EQ(observed, 1); //body already ran, no suspension happened
}

COROFORK_TEST(corofork_returns_control_on_suspension){
    std::function<void()> stored;
    int stage = 0;

    corofork(&){
        stage = 1;
        co_await invert_function([&](std::function<void()> cb){ stored = cb; });
        stage = 2;
        co_return;
    };

    CHECK_EQ(stage, 1); //caller continues while the coroutine waits
    stored();
    CHECK_EQ(stage, 2);
}

COROFORK_TEST(corofork_keeps_captured_copies_alive){
    std::function<void()> stored;
    int observed = 0;

    {
        int local = 7;
        corofork(=, &stored, &observed){
            co_await invert_function([&](std::function<void()> cb){ stored = cb; });
            observed = local; //the copy must outlive the scope below
            co_return;
        };
    } //local is gone from here

    stored();
    CHECK_EQ(observed, 7);
}

COROFORK_TEST(corofork_accepts_move_only_captures){
    int observed = 0;
    auto owned = std::make_unique<int>(5);

    corofork(owned = std::move(owned), &observed){
        observed = *owned;
        co_return;
    };

    CHECK_EQ(observed, 5);
}

COROFORK_TEST(corofork_move_only_capture_survives_suspension){
    std::function<void()> stored;
    int observed = 0;

    {
        auto owned = std::make_unique<int>(11);
        corofork(owned = std::move(owned), &stored, &observed){
            co_await invert_function([&](std::function<void()> cb){ stored = cb; });
            observed = *owned;
            co_return;
        };
    }

    stored();
    CHECK_EQ(observed, 11);
}

COROFORK_TEST(corofork_coroutines_are_independent){
    std::function<void()> firstResume;
    std::function<void()> secondResume;
    int order = 0;
    int firstDone = 0;
    int secondDone = 0;

    corofork(&){
        co_await invert_function([&](std::function<void()> cb){ firstResume = cb; });
        firstDone = ++order;
        co_return;
    };
    corofork(&){
        co_await invert_function([&](std::function<void()> cb){ secondResume = cb; });
        secondDone = ++order;
        co_return;
    };

    secondResume(); //resumed in the opposite order
    firstResume();

    CHECK_EQ(secondDone, 1);
    CHECK_EQ(firstDone, 2);
}


//______________________________________________________________________________
// corosync: blocks the caller until the coroutine is done

COROFORK_TEST(corosync_blocks_until_completion){
    bool finished = false;

    corosync(&){
        finished = true;
        co_return;
    };

    CHECK_EQ(finished, true);
}

COROFORK_TEST(corosync_switches_thread_and_comes_back){
    const std::thread::id initialThread = std::this_thread::get_id();
    std::thread::id threadInside;
    std::thread::id threadAfterBack;

    std::promise< std::function<void()> > handoff;
    std::future< std::function<void()> > resumerReady = handoff.get_future();

    std::thread worker([&]{
        std::function<void()> resumer = resumerReady.get();
        resumer(); //the coroutine continues in this thread
    });

    corosync(&){
        co_await invert_function([&](std::function<void()> resumer){
            handoff.set_value(resumer);
        });
        threadInside = std::this_thread::get_id();

        co_await back_to_thread;
        threadAfterBack = std::this_thread::get_id();

        worker.join(); //we are back in the initial thread, producer is done
        co_return;
    };

    if( worker.joinable() ){
        worker.join(); //safety net if the coroutine never got that far
    }

    CHECK(threadInside != initialThread);
    CHECK_EQ(threadAfterBack, initialThread);
}

COROFORK_TEST(corosync_propagates_exception_from_initial_thread){
    std::string message;

    try{
        corosync(&){
            co_await invert_function([](std::function<void()>) -> bool {
                return true; //completes synchronously, no suspension
            });
            throw std::runtime_error("thrown in the initial thread");
        };
    }
    catch(const std::runtime_error& e){
        message = e.what();
    }

    CHECK_EQ(message, std::string("thrown in the initial thread"));
}

COROFORK_TEST(corosync_propagates_exception_from_another_thread){
    std::promise< std::function<void()> > handoff;
    std::future< std::function<void()> > resumerReady = handoff.get_future();

    std::thread worker([&]{
        std::function<void()> resumer = resumerReady.get();
        resumer(); //the coroutine throws while running in this thread
    });

    std::string message;
    try{
        corosync(&){
            co_await invert_function([&](std::function<void()> resumer){
                handoff.set_value(resumer);
            });
            throw std::runtime_error("thrown in another thread");
        };
    }
    catch(const std::runtime_error& e){
        message = e.what();
    }

    worker.join();
    CHECK_EQ(message, std::string("thrown in another thread"));
}
