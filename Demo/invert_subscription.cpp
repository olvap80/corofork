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


int main(){
    std::cout<<"Testing coroutines, main enter in "<<std::this_thread::get_id()<<"\n"<<std::endl;

    /* thats definitely for demo purposes (coroutine going loop in thread)
       to show how coroutine is driven by thread */

    corosync(&){ //capturing by & is safe because outer waits for completion
        std::cout<<"\nCorosync coroutine entered in "<<std::this_thread::get_id()<<"\n"<<std::endl;

        // actually such co_await can be done in any coroutine
        // it is not necessary to use corosync to have invert_function))
        auto subscription1 = invert_subscription([](std::function<void(int)> resumer){
            std::cout<<"before creating thread in "<<std::this_thread::get_id()<<std::endl;
            
            // Note: resumer will continue execution of the coroutine in thread
            std::thread(
                [resumer](){
                    std::cout<<"\nin new thread "<<std::this_thread::get_id()<<std::endl;
                    
                    for(int i=0; i<10; ++i){
                        std::this_thread::sleep_for(std::chrono::seconds(1));
                        std::cout<<"thread iteration before resume i="<<i<<std::endl;
                        resumer(i); // stuff after co_await will be executed here
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

        std::cout<<"\nLoop exited in "<<std::this_thread::get_id()<<"\n"<<std::endl;

        // other thread continues running here, and will exit when coroutine is done
    };
    std::cout<<"Returned from corosync setup in "<<std::this_thread::get_id()<<"\n"<<std::endl;

    std::cout<<"Testing coroutines, main leave in "<<std::this_thread::get_id()<<"\n"<<std::endl;
    return 0;
}
