/** @file corofork_invert_subscription_demo.cpp
    @brief Demonstrate how to drive coroutine in a loop.  

    Watch for console outputs and thread IDs,
    printouts here are for illustration purposes ))
*/

#include "corofork.h"
#include <iostream>
#include <thread>
#include <chrono>
#include <functional>

// replace by corosync macro
#include <future>

int main(){
    std::cout<<"Testing coroutines, main enter in "<<std::this_thread::get_id()<<"\n"<<std::endl;

    /* thats definitely for demo purposes (coroutine going loop in thread)
       to show how coroutine is driven by thread */
    std::promise<void> p;

    corofork(&){ //capturing by & is safe because outer waits for completion
        std::cout<<"\nCorofork coroutine entered in "<<std::this_thread::get_id()<<"\n"<<std::endl;

        // actually such co_await can be done in any coroutine
        // it is not necessary to use corofork to have invert_function))
        auto subscription1 = invert_subscription([](std::function<void(int)> resumer){
            std::cout<<"before creating thread in "<<std::this_thread::get_id()<<std::endl;
            
            // Note: resumer will continue execution of the coroutine in thread
            std::thread(
                [resumer](){
                    std::cout<<"in new thread "<<std::this_thread::get_id()<<std::endl;
                    for(int i=0; i<10; ++i){
                        std::this_thread::sleep_for(std::chrono::seconds(1));
                        std::cout<<"thread iteration before resume"<<i<<std::endl;
                        resumer(i);
                    }
                    std::cout<<"thread end"<<std::endl;
                }
            ).detach(); // the only necessary line inside
            
            std::cout<<"after creating thread in "<<std::this_thread::get_id()<<std::endl;
        });

        std::cout<<"\nbefore co_await loop in "<<std::this_thread::get_id()<<std::endl;
        
        for( ;; ){
            //each co_await obtains the value from the thread
            auto val = co_await subscription1;
            
            // from now coroutine executes in thread 
            std::cout<<"\nafter co_await in 'their' thread "<<std::this_thread::get_id()<<std::endl;
            std::cout<<"val="<<val<<"\n"<<std::endl;

            if( val == 9 ){
                break;
            }
        }

        std::cout<<"\nbefore p.set_value() in "<<std::this_thread::get_id()<<"\n"<<std::endl;

        p.set_value(); // signal we are ready here
        
        // thread continues running here
        co_return;
    };
    std::cout<<"returned from corofork setup in "<<std::this_thread::get_id()<<"\n"<<std::endl;

    //Just ensure other threads end before we exit
    p.get_future().get();
    std::cout<<"Once coroutine signalled, we are in "<<std::this_thread::get_id()<<"\n"<<std::endl;

    std::cout<<"Testing coroutines, main leave in "<<std::this_thread::get_id()<<"\n"<<std::endl;
    return 0;
}
