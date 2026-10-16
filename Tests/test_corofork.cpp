/** @file test_corofork.cpp
    @brief Cover the corofork and corosync macros themselves: capture
           lifetime, move only captures, thread switching with back_to_thread
           and exception propagation back to the initial thread.
*/

#include "corofork.h"
#include "test_framework.h"

#include <atomic>
#include <chrono>
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

namespace{
    /**Awaitable of "another library": resumes the coroutine with plain
    handle.resume() from a new thread, no corofork code is involved */
    struct ResumesInNewThreadWithPlainResume{
        std::thread& resumingThread;
        bool& resumeReturnedNormally;

        bool await_ready() const noexcept { return false; }

        void await_suspend(std::coroutine_handle<> handle){
            //read the references first, the coroutine may run in the new thread at once
            std::thread& threadToStart = resumingThread;
            bool& returnedNormally = resumeReturnedNormally;

            threadToStart = std::thread([handle, &returnedNormally]{
                try{
                    handle.resume();
                    returnedNormally = true;
                }
                catch(...){
                    returnedNormally = false;
                }
            });
        }

        void await_resume() const noexcept {}
    };
}

COROFORK_TEST(corosync_propagates_exception_when_resumed_by_foreign_awaitable){
    /* The guarantee corofork does not give: whatever code resumes the corosync
       body (here plain handle.resume() of another library in another thread),
       the exception reaches the corosync caller, and resume() in that thread
       returns normally */
    std::thread resumingThread;
    bool resumeReturnedNormally = false;
    std::string message;

    try{
        corosync(&){
            co_await ResumesInNewThreadWithPlainResume{resumingThread, resumeReturnedNormally};
            throw std::runtime_error("thrown after plain resume in another thread");
        };
    }
    catch(const std::runtime_error& e){
        message = e.what();
    }
    if( resumingThread.joinable() ){
        resumingThread.join();
    }

    CHECK_EQ(message, std::string("thrown after plain resume in another thread"));
    CHECK_EQ(resumeReturnedNormally, true);
}

namespace{
    /**Body local whose destructor watches for corosync returning to the caller.
    The destructor waits (bounded) for corosyncReturned: a correct corosync
    returns only after this destructor is done, so the wait always runs out,
    and destructorSawCorosyncReturned stays false */
    struct WatchesCorosyncReturn{
        const std::atomic<bool>& corosyncReturned;
        bool& destructorSawCorosyncReturned;

        ~WatchesCorosyncReturn(){
            const auto deadline =
                std::chrono::steady_clock::now() + std::chrono::milliseconds(200);
            while( std::chrono::steady_clock::now() < deadline ){
                if( corosyncReturned.load() ){
                    destructorSawCorosyncReturned = true;
                    return;
                }
                std::this_thread::yield();
            }
        }
    };
}

COROFORK_TEST(corosync_returns_only_after_body_locals_are_destroyed){
    /* The only case with a timeout: the wrong order (corosync returns while
       the destructor of a body local still runs in another thread) can only be
       observed by letting that destructor wait for the caller */
    std::atomic<bool> corosyncReturned{false};
    bool destructorSawCorosyncReturned = false;
    std::thread worker;

    corosync(&){
        WatchesCorosyncReturn bodyLocal{corosyncReturned, destructorSawCorosyncReturned};
        co_await invert_function([&](std::function<void()> resumer){
            worker = std::thread(resumer); //the body finishes in the worker thread
        });
        co_return; //explicit co_return: return_void runs before ~WatchesCorosyncReturn
    };
    corosyncReturned = true;
    worker.join();

    CHECK_EQ(destructorSawCorosyncReturned, false);
}


//______________________________________________________________________________
// corofork: exception escaping the body frees the frame and the captures

COROFORK_TEST(corofork_exception_before_suspension_reaches_caller_and_frees_captures){
    auto capturedOwner = std::make_shared<int>(1);
    std::weak_ptr<int> capturedObserver = capturedOwner;
    std::string message;

    try{
        corofork(capturedOwner){
            if( *capturedOwner == 1 ){
                throw std::runtime_error("thrown before the first suspension");
            }
            co_return;
        };
    }
    catch(const std::runtime_error& e){
        message = e.what();
    }
    capturedOwner.reset(); //from here only the copy captured by the coroutine may hold it

    CHECK_EQ(message, std::string("thrown before the first suspension"));
    CHECK(capturedObserver.expired());
}

COROFORK_TEST(corofork_exception_after_resume_reaches_callback_caller_and_frees_captures){
    std::function<void()> stored;
    auto capturedOwner = std::make_shared<int>(1);
    std::weak_ptr<int> capturedObserver = capturedOwner;

    corofork(&stored, capturedOwner){
        co_await invert_function([&](std::function<void()> cb){ stored = cb; });
        if( *capturedOwner == 1 ){
            throw std::runtime_error("thrown after resume");
        }
    };
    capturedOwner.reset();
    CHECK(!capturedObserver.expired()); //the suspended coroutine still holds the capture

    std::string message;
    try{
        stored(); //resumes the coroutine, the body throws
    }
    catch(const std::runtime_error& e){
        message = e.what();
    }

    CHECK_EQ(message, std::string("thrown after resume"));
    CHECK(capturedObserver.expired());
}
