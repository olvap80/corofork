/** @file corofork_multimpe_threads_demo.cpp
    @brief Demonstrate driving coroutine from multimple threads.  

    Watch for console outputs and thread IDs,
    printouts here are for illustration purposes ))
*/

#include "corofork.h"
#include <iostream>
#include <thread>
#include <future>
#include <chrono>
#include <functional>

/// Straight forward class for demo purposes
class PeriodicEventSource{
public:
    using Subscriber = std::function<void(const std::string& name, unsigned iteration)>;

    PeriodicEventSource(std::string name, unsigned periodSeconds)
    {
        std::thread([=]{
            std::cout<<"Thread "<<name<<" CREATED, id="<<std::this_thread::get_id()<<std::endl;
            for( unsigned iteration = 0; ; ++iteration ){
                std::this_thread::sleep_for(
                    std::chrono::seconds(periodSeconds)
                );
                std::cout<<"Thread "<<name<<", id="<<std::this_thread::get_id()<<", iteration="<<iteration<<std::endl;
                
                Subscriber copySubscription;
                {std::lock_guard lock(protect);
                    copySubscription = subscription;
                }
                if( copySubscription ){
                     copySubscription(name, iteration);                   
                }
                break;
            }
        }).detach();
    }

    void Subscribe(Subscriber newSubscription){
        std::lock_guard lock(protect);
        subscription = newSubscription;
    }

private:
    std::mutex protect;
    Subscriber subscription;
};


int main(){
    std::cout<<"Testing coroutines, main enter in "<<std::this_thread::get_id()<<"\n"<<std::endl;


    //PeriodicEventSource s1("1secondEvents", 1);
    //PeriodicEventSource s7("7secondsEvents", 7);

    /* thats definitely for coroutine going in thread demo purposes
       to show how coroutine switches to other thread
       (std::async is better in current case, but what is just for sample) */
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
            auto val = co_await subscription1;
            
            // from now coroutine executes in thread 
            std::cout<<"\nafter co_await in 'their' thread "<<std::this_thread::get_id()<<std::endl;
            std::cout<<"val="<<val<<"\n"<<std::endl;

            if( val == 9 ){
                break;
            }
        }

        // just a demo, not doing anything useful here

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
