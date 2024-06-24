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
    using Subscriber = std::function<bool(const std::string& name, unsigned iteration)>;

    PeriodicEventSource(std::string name, unsigned periodSeconds)
    {
        std::thread([=]{
            std::cout<<"Thread "<<name<<" CREATED, id="<<std::this_thread::get_id()<<std::endl;
            for( unsigned iteration = 0; ; ++iteration ){
                std::this_thread::sleep_for( std::chrono::seconds(periodSeconds) );
                std::cout<<"Thread_"<<name<<", id="<<std::this_thread::get_id()<<", iteration="<<iteration<<std::endl;
                
                Subscriber copySubscription;
                {std::lock_guard lock(protect);
                    copySubscription = subscription;
                }
                if( copySubscription ){
                    if( !copySubscription(name, iteration) ){
                        break;
                    }
                }
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

    corosync(){ //capturing by & is safe because outer waits for completion
        std::cout<<"\nCorosync coroutine entered in "<<std::this_thread::get_id()<<"\n"<<std::endl;

        PeriodicEventSource s1("1secondEvents", 1);
        auto subscription1 = invert_subscription(
            [&](PeriodicEventSource::Subscriber resumer){
                s1.Subscribe(resumer);

                return [&]{
                    std::cout<<"subscription1 cleanup in "<<std::this_thread::get_id()<<std::endl;
                    //this will break the loop
                    s1.Subscribe([](const std::string&, unsigned){
                        return false; // new subscription that will break the loop
                    });
                };
            },
            true //True causes the subscription to continue
        );

        PeriodicEventSource s7("7secondsEvents", 7);
        auto subscription7 = invert_subscription(
            [&](std::function<bool(const std::string&, unsigned)> resumer){
                s7.Subscribe(resumer);

                return [&]{
                    std::cout<<"subscription2 cleanup in "<<std::this_thread::get_id()<<std::endl;
                    //this will break the loop
                    s7.Subscribe([](const std::string&, unsigned){
                        return false; // new subscription that will break the loop
                    });
                };
            },
            true//True causes the subscription to continue
        );
        
        std::cout<<"\nbefore co_await loop in "<<std::this_thread::get_id()<<std::endl;
        
        {
            auto [name, iteration] = co_await subscription1;
            std::cout<<"\nafter first co_await for subscription1 in 'their' thread "<<std::this_thread::get_id()<<std::endl;
            std::cout<<"name="<<name<<" iteration="<<iteration<<"\n"<<std::endl;
        }

        {
            auto [name, iteration] = co_await subscription1;
            std::cout<<"\nafter second co_await for subscription1 in 'their' thread "<<std::this_thread::get_id()<<std::endl;
            std::cout<<"name="<<name<<" iteration="<<iteration<<"\n"<<std::endl;
        }

        {
            std::cout   <<"\nbefore co_await for subscription7 in "<<std::this_thread::get_id()
                        <<"\nNow all the stuff from subscription1 is stored in the queue"<<std::endl;
            auto [name, iteration] = co_await subscription7;
            std::cout<<"\nafter first co_await for subscription7 in 'their' thread "<<std::this_thread::get_id()<<std::endl;
            std::cout<<"name="<<name<<" iteration="<<iteration<<"\n"<<std::endl;
        }

        std::cout
                <<"\n\nRead accumulated events from subscription1 in "<<std::this_thread::get_id()<<std::endl
                <<  "\nNow all the stuff from subscription1 is extracted from the queue and consumed\n"
                <<  "\nmore subscription1 will be extracted directly from the source\n"
                    "\nbut subscription7 is stored in the queue"<<std::endl;
        for( int i = 0; i < 17; ++i ){
            auto [name, iteration] = co_await subscription1;
            std::cout<<"after co_await for subscription1 in 'their' thread "<<std::this_thread::get_id()<<std::endl;
            std::cout<<"name="<<name<<" iteration="<<iteration<<"\n"<<std::endl;
        }
        
        // thread continues running here, as we go out of scope and cleanup subscriptions
    };
    std::cout<<"returned from corosync setup in "<<std::this_thread::get_id()<<"\n"<<std::endl;

    std::cout<<"Testing coroutines, main leave in "<<std::this_thread::get_id()<<"\n"<<std::endl;
    return 0;
}
