/** @file corofork_thread_demo.h
    @brief Demonstrate how callback shall issue 

    Watch for console outputs and thread IDs,
    printouts are for illustration purposes ))
*/

#include "corofork.h"
#include <iostream>
#include <thread>
#include <future>


int main(){
    std::cout<<"Testing coroutines, main enter in "<<std::this_thread::get_id()<<"\n"<<std::endl;

    /* thats definitely for coroutine going in thread demo purposes
       to show how coroutine switches to other thread
       (std::async is better in current case, but what is just for sample) */
    std::promise<void> p;

    corofork(&){ //capturing by & is safe because outer waits for completion
        std::cout<<"\nCorofork coroutine entered in "<<std::this_thread::get_id()<<"\n"<<std::endl;

        // actually such co_await can be done in any coroutine
        // it is not necessary to use corofork to have invert_function))
        co_await invert_function([](std::function<void()> resumer){
            std::cout<<"before creating thread in "<<std::this_thread::get_id()<<std::endl;
            
            std::thread(resumer).detach(); // the only necessary line inside
            
            std::cout<<"after creating thread in "<<std::this_thread::get_id()<<std::endl;
        });
        // from now coroutine executes in thread 
        std::cout<<"\nafter co_await in a new thread "<<std::this_thread::get_id()<<std::endl;

        // just a demo, not doing anything useful here

        std::cout<<"\nbefore p.set_value() in "<<std::this_thread::get_id()<<"\n"<<std::endl;

        p.set_value(); // signal we are ready here
        
        // thread continues running here
    };
    std::cout<<"returned from corofork setup in "<<std::this_thread::get_id()<<"\n"<<std::endl;

    //Just ensure other thread ends before we exit
    p.get_future().get();

    std::cout<<"Testing coroutines, main leave in "<<std::this_thread::get_id()<<"\n"<<std::endl;
    return 0;
}
