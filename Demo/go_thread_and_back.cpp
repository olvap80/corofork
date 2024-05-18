/** @file corofork_thread_demo.h
    @brief  Demonstrate how callback shall issue resumer API
            to drive the coroutine.  

    Watch for console outputs and thread IDs,
    printouts here are for illustration purposes ))
*/

#include "corofork.h"
#include <iostream>
#include <thread>
#include <future>


int main(){
    std::cout<<"Testing coroutines, main enter in "<<std::this_thread::get_id()<<"\n"<<std::endl;

    /* thats definitely for coroutine going in thread demo purposes
       just to show how coroutine switches to other thread */

    corosync(){ //capturing by & is safe because outer waits for completion
        std::cout<<"\nCorosync coroutine entered in "<<std::this_thread::get_id()<<"\n"<<std::endl;

        // actually such co_await can be done in any coroutine
        // it is not necessary to use corofork/corosync to have invert_function))
        co_await invert_function([](std::function<void()> resumer){
            std::cout<<"before creating thread in "<<std::this_thread::get_id()<<std::endl;
            // Note: resumer will continue execution of the coroutine in thread
            std::thread(resumer).detach(); // the only necessary line inside  
            std::cout<<"after creating thread in "<<std::this_thread::get_id()<<std::endl;
        });
        // from now coroutine executes in thread 
        std::cout<<"\nafter co_await in a new thread "<<std::this_thread::get_id()<<std::endl;

        // just a demo, not doing anything useful here

        std::cout<<"\nbefore co_await back_to_thread in "<<std::this_thread::get_id()<<"\n"<<std::endl;
        co_await back_to_thread;
        std::cout<<"\nafter co_await back_to_thread in "<<std::this_thread::get_id()<<"\n"<<std::endl;
        
        // thread continues running here
    };
    std::cout<<"returned from corosync setup in "<<std::this_thread::get_id()<<"\n"<<std::endl;

    std::cout<<"Testing coroutines, main leave in "<<std::this_thread::get_id()<<"\n"<<std::endl;
    return 0;
}
