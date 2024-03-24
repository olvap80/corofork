/** @file corofork_all_demo.h
    @brief Run all the features in one place
*/

#include "corofork.h"
#include <iostream>


int main(){
    //for(int i = 0; i < 10; ++i){
    //std::cout<<"ITERATION:"<<i<<std::endl;

    
    std::cout<<"Testing coroutines, main enter\n"<<std::endl;

    corofork(){
        std::cout<<"\n"
        "##############################################################################\n"
        "Corofork coroutine entered\n"<<std::endl;

        //======================================================================
        std::cout<<"\nSIMPLE CASE OF GENERATING FUNCTION CALLBACK THAT RETURNS NOTHING"<<std::endl;
        {
            std::cout<<"\nBEFORE1 invert_function for std::function<void()>"<<std::endl;
            co_await invert_function([](const std::function<void()>& function){
                std::cout<<"Setup1 void() enter"<<std::endl;
                function(); //invokes immediately, this causes recursion, but that is ok for demo
                std::cout<<"Setup1 void() leave"<<std::endl;
            });
            std::cout<<"AFTER1 invert_function for std::function<void()>"<<std::endl;


            std::cout<<"\nBEFORE2 invert_function for std::function<void(int)>"<<std::endl;
            auto val1 = co_await invert_function([](const std::function<void(int)>& function){
                std::cout<<"Setup2 void(int) enter"<<std::endl;
                function(4202); //invokes immediately, this causes recursion, but that is ok for demo
                std::cout<<"Setup2 void(int) leave"<<std::endl;
            });
            std::cout<<"AFTER2 invert_function for std::function<void(int)>, val1 = "<<val1<<std::endl;


            std::cout<<"\nBEFORE3 invert_function for std::function<void(int, double)>"<<std::endl;
            auto [val2a, val2b] = co_await invert_function([](const std::function<void(int, double)>& function){
                std::cout<<"Setup3 void(int, double) enter"<<std::endl;
                function(4203, 4.203); //invokes immediately, this causes recursion, but that is ok for demo
                std::cout<<"Setup3 void(int, double) leave"<<std::endl;
            });
            std::cout<<"AFTER3 invert_function for std::function<void(int, double)> [val2a, val2b] = ["
                <<val2a<<", "<<val2b<<']'<<std::endl;
        }

        //======================================================================
        std::cout<<"\nCASE OF GENERATING FUNCTION CALLBACK THAT RETURNS PREDEFINED VALUE"<<std::endl;
        {
            std::cout<<"\nBEFORE4 invert_function for std::function<int()>"<<std::endl;
            co_await invert_function(
                [](const std::function<int()>& function){
                    std::cout<<"Setup4 int() enter"<<std::endl;
                    auto res = function(); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup4 int() leave with res="<<res<<std::endl;
                },
                42004
            );
            std::cout<<"AFTER4 invert_function for std::function<int()>"<<std::endl;


            std::cout<<"\nBEFORE5 invert_function for std::function<double(int)>"<<std::endl;
            auto val1 = co_await invert_function(
                [](const std::function<double(int)>& function){
                    std::cout<<"Setup5 double(int) enter"<<std::endl;
                    auto res = function(4205); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup5 double(int) leave with res="<<res<<std::endl;
                },
                0.42005
            );
            std::cout<<"AFTER5 invert_function for std::function<double(int)>, val1 = "<<val1<<std::endl;


            std::cout<<"\nBEFORE6 invert_function for std::function<string(int, double)>"<<std::endl;
            auto [val2a, val2b] = co_await invert_function(
                [](const std::function<std::string(int, double)>& function){
                    std::cout<<"Setup6 string(int, double) enter"<<std::endl;
                    auto res = function(4206, 4.206); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup6 string(int, double) leave with res="<<res<<std::endl;
                },
                "demo_string_result06"
            );
            std::cout<<"AFTER6 invert_function for std::function<string(int, double)> [val2a, val2b] = ["
                <<val2a<<", "<<val2b<<']'<<std::endl;
        }

        //======================================================================
        std::cout<<"\nCASE OF GENERATING FUNCTION CALLBACK THAT RETURNS RESULT OF FUNCTION CALL"<<std::endl;
        {
            std::cout<<"\nBEFORE7 invert_function for std::function<int()>"<<std::endl;
            co_await invert_function(
                [](const std::function<int()>& function){
                    std::cout<<"Setup7 int() enter"<<std::endl;
                    auto res = function(); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup7 int() leave with res="<<res<<std::endl;
                },
                []{ return 42007; }
            );
            std::cout<<"AFTER7 invert_function for std::function<int()>"<<std::endl;


            std::cout<<"\nBEFORE8 invert_function for std::function<double(int)>"<<std::endl;
            auto val1 = co_await invert_function(
                [](const std::function<double(int)>& function){
                    std::cout<<"Setup8 double(int) enter"<<std::endl;
                    auto res = function(4208); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup8 double(int) leave with res="<<res<<std::endl;
                },
                []{ return 0.42008; }
            );
            std::cout<<"AFTER8 invert_function for std::function<double(int)>, val1 = "<<val1<<std::endl;


            std::cout<<"\nBEFORE9 invert_function for std::function<string(int, double)>"<<std::endl;
            auto [val2a, val2b] = co_await invert_function(
                [](const std::function<std::string(int, double)>& function){
                    std::cout<<"Setup9 string(int, double) enter"<<std::endl;
                    auto res = function(4209, 4.209); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup9 string(int, double) leave with res="<<res<<std::endl;
                },
                []()-> std::string { return "another_demo_string_result09"; }
            );
            std::cout<<"AFTER9 invert_function for std::function<string(int, double)> [val2a, val2b] = ["
                <<val2a<<", "<<val2b<<']'<<std::endl;
        }


        std::cout<<
            "\n\n#################################################################\n"
            "NOW THE SAME BUT WITH \"PLAIN\" CALLBACKS\n"
        <<std::endl;
        
        //======================================================================
        std::cout<<"\nSIMPLE CASE OF GENERATING CALLBACK THAT RETURNS NOTHING"<<std::endl;
        {
            std::cout<<"\nBEFORE10 invert_callback for void(*callback)()"<<std::endl;
            co_await invert_callback<1>([](void(*callback)()){
                std::cout<<"Setup10 void() enter"<<std::endl;
                callback(); //invokes immediately, this causes recursion, but that is ok for demo
                std::cout<<"Setup10 void() leave"<<std::endl;
            });
            std::cout<<"AFTER10 invert_callback for void(*callback)()>"<<std::endl;


            std::cout<<"\nBEFORE11 invert_callback for void(*callback)(int)>"<<std::endl;
            auto val1 = co_await invert_callback<1>([](void(*callback)(int)){
                std::cout<<"Setup11 void(int) enter"<<std::endl;
                callback(4211); //invokes immediately, this causes recursion, but that is ok for demo
                std::cout<<"Setup11 void(int) leave"<<std::endl;
            });
            std::cout<<"AFTER11 invert_callback for void(*callback)(int)>, val1 = "<<val1<<std::endl;


            std::cout<<"\nBEFORE12 invert_callback for void(*callback)(int, double)>"<<std::endl;
            auto [val2a, val2b] = co_await invert_callback<1>([](void(*callback)(int, double)){
                std::cout<<"Setup12 void(int, double) enter"<<std::endl;
                callback(4213, 4.212); //invokes immediately, this causes recursion, but that is ok for demo
                std::cout<<"Setup12 void(int, double) leave"<<std::endl;
            });
            std::cout<<"AFTER12 invert_callback for void(*callback)(int, double)> [val2a, val2b] = ["
                <<val2a<<", "<<val2b<<']'<<std::endl;
        }

        //======================================================================
        std::cout<<"\nCASE OF GENERATING CALLBACK THAT RETURNS PREDEFINED VALUE"<<std::endl;
        {
            std::cout<<"\nBEFORE13 invert_callback for int(*callback)()"<<std::endl;
            co_await invert_callback<1>(
                [](int(*callback)()){
                    std::cout<<"Setup13 int() enter"<<std::endl;
                    auto res = callback(); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup13 int() leave with res="<<res<<std::endl;
                },
                42013
            );
            std::cout<<"AFTER13 invert_callback for int(*callback)()"<<std::endl;


            std::cout<<"\nBEFORE14 invert_callback for double(*callback)(int)>"<<std::endl;
            auto val1 = co_await invert_callback<1>(
                [](double(*callback)(int)){
                    std::cout<<"Setup14 double(int) enter"<<std::endl;
                    auto res = callback(4214); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup14 double(int) leave with res="<<res<<std::endl;
                },
                4.2014
            );
            std::cout<<"AFTER14 invert_callback for double(*callback)(int)>, val1 = "<<val1<<std::endl;


            std::cout<<"\nBEFORE15 invert_callback for std::string(*callback)(int, double)>"<<std::endl;
            auto [val2a, val2b] = co_await invert_callback<1>(
                [](std::string(*callback)(int, double)){
                    std::cout<<"Setup15 string(int, double) enter"<<std::endl;
                    auto res = callback(4215, 4.215); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup15 string(int, double) leave with res="<<res<<std::endl;
                },
                "demo_string_result15"
            );
            std::cout<<"AFTER15 invert_callback for std::string(*callback)(int, double) [val2a, val2b] = ["
                <<val2a<<", "<<val2b<<']'<<std::endl;
        }

        //======================================================================
        std::cout<<"\nCASE OF GENERATING CALLBACK THAT RETURNS RESULT OF FUNCTION CALL"<<std::endl;
        {
            std::cout<<"\nBEFORE16 invert_callback for int(*callback)()>"<<std::endl;
            co_await invert_callback<1>(
                [](int(*callback)()){
                    std::cout<<"Setup16 int() enter"<<std::endl;
                    auto res = callback(); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup16 int() leave with res="<<res<<std::endl;
                },
                []{ return 54016; }
            );
            std::cout<<"AFTER16 invert_callback for int(*callback)()"<<std::endl;


            std::cout<<"\nBEFORE17 invert_callback for double(*callback)(int)>"<<std::endl;
            auto val1 = co_await invert_callback<1>(
                [](double(*callback)(int)){
                    std::cout<<"Setup17 double(int) enter"<<std::endl;
                    auto res = callback(542); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup17 double(int) leave with res="<<res<<std::endl;
                },
                []{ return 0.42017; }
            );
            std::cout<<"AFTER17 invert_callback for double(*callback)(int)>, val1 = "<<val1<<std::endl;


            std::cout<<"\nBEFORE18 invert_callback for std::string(*callback)(int, double)>"<<std::endl;
            auto [val2a, val2b] = co_await invert_callback<1>(
                [](std::string(*callback)(int, double)){
                    std::cout<<"Setup18 string(int, double) enter"<<std::endl;
                    auto res = callback(4218, 4.218); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup18 string(int, double) leave with res="<<res<<std::endl;
                },
                []()-> std::string { return "another_demo_string_result18"; }
            );
            std::cout<<"AFTER18 invert_callback for std::string(*callback)(int, double)> [val2a, val2b] = ["
                <<val2a<<", "<<val2b<<']'<<std::endl;
        }



        std::cout<<"\nCorofork coroutine exited\n"
        "##############################################################################\n"
        <<std::endl;
    };

    std::cout<<"\nNote: all the stuff above is from nested awaitables setup\n"
               "\nthis is expected due to the way how coroutines are resumed"<<std::endl;

    std::cout<<"\n\n\nNow test the same stuff but with setup result not available immediately\n"
    "##################################################################################\n"
    "##################################################################################\n"
    <<std::endl;


    corofork(){
        std::cout<<"\n"
        "##############################################################################\n"
        "Corofork coroutine entered\n"<<std::endl;

        //======================================================================
        std::cout<<"\nSIMPLE CASE OF GENERATING FUNCTION CALLBACK THAT RETURNS NOTHING"<<std::endl;
        {
            std::cout<<"\nBEFORE1a invert_function for std::function<void()>"<<std::endl;
            co_await invert_function([](const std::function<void()>& function){
                std::cout<<"Setup1a void() enter"<<std::endl;
                function(); //invokes immediately, this causes recursion, but that is ok for demo
                std::cout<<"Setup1a void() leave"<<std::endl;
                return false; //result will go to function
            });
            std::cout<<"AFTER1a invert_function for std::function<void()>"<<std::endl;


            std::cout<<"\nBEFORE2a invert_function for std::function<void(int)>"<<std::endl;
            auto val1 = co_await invert_function([](const std::function<void(int)>& function){
                std::cout<<"Setup2a void(int) enter"<<std::endl;
                function(4202); //invokes immediately, this causes recursion, but that is ok for demo
                std::cout<<"Setup2a void(int) leave"<<std::endl;
                return std::optional<int>(); //result will go to function (empty optional)
            });
            std::cout<<"AFTER2a invert_function for std::function<void(int)>, val1 = "<<val1<<std::endl;


            std::cout<<"\nBEFORE3a invert_function for std::function<void(int, double)>"<<std::endl;
            auto [val2a, val2b] = co_await invert_function([](const std::function<void(int, double)>& function){
                std::cout<<"Setup3 void(int, double) enter"<<std::endl;
                function(4203, 4.203); //invokes immediately, this causes recursion, but that is ok for demo
                std::cout<<"Setup3a void(int, double) leave"<<std::endl;
                return std::optional< std::tuple<int, double> >(); //result will go to function (empty optional)
            });
            std::cout<<"AFTER3a invert_function for std::function<void(int, double)> [val2a, val2b] = ["
                <<val2a<<", "<<val2b<<']'<<std::endl;
        }

        //======================================================================
        std::cout<<"\nCASE OF GENERATING FUNCTION CALLBACK THAT RETURNS PREDEFINED VALUE"<<std::endl;
        {
            std::cout<<"\nBEFORE4a invert_function for std::function<int()>"<<std::endl;
            co_await invert_function(
                [](const std::function<int()>& function){
                    std::cout<<"Setup4a int() enter"<<std::endl;
                    auto res = function(); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup4a int() leave with res="<<res<<std::endl;
                    return false; //result will go to function
                },
                42004
            );
            std::cout<<"AFTER4a invert_function for std::function<int()>"<<std::endl;


            std::cout<<"\nBEFORE5a invert_function for std::function<double(int)>"<<std::endl;
            auto val1 = co_await invert_function(
                [](const std::function<double(int)>& function){
                    std::cout<<"Setup5a double(int) enter"<<std::endl;
                    auto res = function(4205); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup5a double(int) leave with res="<<res<<std::endl;
                    return std::optional<int>(); //result will go to function (empty optional)
                },
                0.42005
            );
            std::cout<<"AFTER5a invert_function for std::function<double(int)>, val1 = "<<val1<<std::endl;


            std::cout<<"\nBEFORE6a invert_function for std::function<string(int, double)>"<<std::endl;
            auto [val2a, val2b] = co_await invert_function(
                [](const std::function<std::string(int, double)>& function){
                    std::cout<<"Setup6a string(int, double) enter"<<std::endl;
                    auto res = function(4206, 4.206); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup6a string(int, double) leave with res="<<res<<std::endl;
                    return std::optional< std::tuple<int, double> >(); //result will go to function (empty optional)
                },
                "demo_string_result06"
            );
            std::cout<<"AFTER6a invert_function for std::function<string(int, double)> [val2a, val2b] = ["
                <<val2a<<", "<<val2b<<']'<<std::endl;
        }

        //======================================================================
        std::cout<<"\nCASE OF GENERATING FUNCTION CALLBACK THAT RETURNS RESULT OF FUNCTION CALL"<<std::endl;
        {
            std::cout<<"\nBEFORE7a invert_function for std::function<int()>"<<std::endl;
            co_await invert_function(
                [](const std::function<int()>& function){
                    std::cout<<"Setup7a int() enter"<<std::endl;
                    auto res = function(); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup7a int() leave with res="<<res<<std::endl;
                    return false; //result will go to function
                },
                []{ return 42007; }
            );
            std::cout<<"AFTER7a invert_function for std::function<int()>"<<std::endl;


            std::cout<<"\nBEFORE8a invert_function for std::function<double(int)>"<<std::endl;
            auto val1 = co_await invert_function(
                [](const std::function<double(int)>& function){
                    std::cout<<"Setup8a double(int) enter"<<std::endl;
                    auto res = function(4208); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup8a double(int) leave with res="<<res<<std::endl;
                    return std::optional<int>(); //result will go to function (empty optional)
                },
                []{ return 0.42008; }
            );
            std::cout<<"AFTER8a invert_function for std::function<double(int)>, val1 = "<<val1<<std::endl;


            std::cout<<"\nBEFORE9a invert_function for std::function<string(int, double)>"<<std::endl;
            auto [val2a, val2b] = co_await invert_function(
                [](const std::function<std::string(int, double)>& function){
                    std::cout<<"Setup9a string(int, double) enter"<<std::endl;
                    auto res = function(4209, 4.209); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup9a string(int, double) leave with res="<<res<<std::endl;
                    return std::optional< std::tuple<int, double> >(); //result will go to function (empty optional)
                },
                []()-> std::string { return "another_demo_string_result09"; }
            );
            std::cout<<"AFTER9a invert_function for std::function<string(int, double)> [val2a, val2b] = ["
                <<val2a<<", "<<val2b<<']'<<std::endl;
        }


        std::cout<<
            "\n\n#################################################################\n"
            "NOW THE SAME BUT WITH \"PLAIN\" CALLBACKS\n"
        <<std::endl;
        
        //======================================================================
        std::cout<<"\nSIMPLE CASE OF GENERATING CALLBACK THAT RETURNS NOTHING"<<std::endl;
        {
            std::cout<<"\nBEFORE10a invert_callback for void(*callback)()"<<std::endl;
            co_await invert_callback<1>([](void(*callback)()){
                std::cout<<"Setup10a void() enter"<<std::endl;
                callback(); //invokes immediately, this causes recursion, but that is ok for demo
                std::cout<<"Setup10a void() leave"<<std::endl;
                return false; //result will go to callback
            });
            std::cout<<"AFTER10a invert_callback for void(*callback)()>"<<std::endl;


            std::cout<<"\nBEFORE11a invert_callback for void(*callback)(int)>"<<std::endl;
            auto val1 = co_await invert_callback<1>([](void(*callback)(int)){
                std::cout<<"Setup11a void(int) enter"<<std::endl;
                callback(4211); //invokes immediately, this causes recursion, but that is ok for demo
                std::cout<<"Setup11a void(int) leave"<<std::endl;
                return std::optional<int>(); //result will go to callback (empty optional)
            });
            std::cout<<"AFTER11a invert_callback for void(*callback)(int)>, val1 = "<<val1<<std::endl;


            std::cout<<"\nBEFORE12a invert_callback for void(*callback)(int, double)>"<<std::endl;
            auto [val2a, val2b] = co_await invert_callback<1>([](void(*callback)(int, double)){
                std::cout<<"Setup12a void(int, double) enter"<<std::endl;
                callback(4213, 4.212); //invokes immediately, this causes recursion, but that is ok for demo
                std::cout<<"Setup12a void(int, double) leave"<<std::endl;
                return std::optional< std::tuple<int, double> >(); //result will go to callback (empty optional)
            });
            std::cout<<"AFTER12a invert_callback for void(*callback)(int, double)> [val2a, val2b] = ["
                <<val2a<<", "<<val2b<<']'<<std::endl;
        }

        //======================================================================
        std::cout<<"\nCASE OF GENERATING CALLBACK THAT RETURNS PREDEFINED VALUE"<<std::endl;
        {
            std::cout<<"\nBEFORE13a invert_callback for int(*callback)()"<<std::endl;
            co_await invert_callback<1>(
                [](int(*callback)()){
                    std::cout<<"Setup13a int() enter"<<std::endl;
                    auto res = callback(); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup13a int() leave with res="<<res<<std::endl;
                    return false; //result will go to callback
                },
                42013
            );
            std::cout<<"AFTER13a invert_callback for int(*callback)()"<<std::endl;


            std::cout<<"\nBEFORE14a invert_callback for double(*callback)(int)>"<<std::endl;
            auto val1 = co_await invert_callback<1>(
                [](double(*callback)(int)){
                    std::cout<<"Setup14a double(int) enter"<<std::endl;
                    auto res = callback(4214); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup14a double(int) leave with res="<<res<<std::endl;
                    return std::optional<int>(); //result will go to callback (empty optional)
                },
                4.2014
            );
            std::cout<<"AFTER14a invert_callback for double(*callback)(int)>, val1 = "<<val1<<std::endl;


            std::cout<<"\nBEFORE15a invert_callback for std::string(*callback)(int, double)>"<<std::endl;
            auto [val2a, val2b] = co_await invert_callback<1>(
                [](std::string(*callback)(int, double)){
                    std::cout<<"Setup15a string(int, double) enter"<<std::endl;
                    auto res = callback(4215, 4.215); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup15a string(int, double) leave with res="<<res<<std::endl;
                    return std::optional< std::tuple<int, double> >(); //result will go to callback (empty optional)
                },
                "demo_string_result15"
            );
            std::cout<<"AFTER15a invert_callback for std::string(*callback)(int, double) [val2a, val2b] = ["
                <<val2a<<", "<<val2b<<']'<<std::endl;
        }

        //======================================================================
        std::cout<<"\nCASE OF GENERATING CALLBACK THAT RETURNS RESULT OF FUNCTION CALL"<<std::endl;
        {
            std::cout<<"\nBEFORE16a invert_callback for int(*callback)()>"<<std::endl;
            co_await invert_callback<1>(
                [](int(*callback)()){
                    std::cout<<"Setup16a int() enter"<<std::endl;
                    auto res = callback(); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup16a int() leave with res="<<res<<std::endl;
                    return false; //result will go to callback
                },
                []{ return 54016; }
            );
            std::cout<<"AFTER16a invert_callback for int(*callback)()"<<std::endl;


            std::cout<<"\nBEFORE17a invert_callback for double(*callback)(int)>"<<std::endl;
            auto val1 = co_await invert_callback<1>(
                [](double(*callback)(int)){
                    std::cout<<"Setup17a double(int) enter"<<std::endl;
                    auto res = callback(542); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup17a double(int) leave with res="<<res<<std::endl;
                    return std::optional<int>(); //result will go to callback (empty optional)
                },
                []{ return 0.42017; }
            );
            std::cout<<"AFTER17 invert_callback for double(*callback)(int)>, val1 = "<<val1<<std::endl;


            std::cout<<"\nBEFORE18a invert_callback for std::string(*callback)(int, double)>"<<std::endl;
            auto [val2a, val2b] = co_await invert_callback<1>(
                [](std::string(*callback)(int, double)){
                    std::cout<<"Setup18a string(int, double) enter"<<std::endl;
                    auto res = callback(4218, 4.218); //invokes immediately, this causes recursion, but that is ok for demo
                    std::cout<<"Setup18a string(int, double) leave with res="<<res<<std::endl;
                    return std::optional< std::tuple<int, double> >(); //result will go to callback (empty optional)
                },
                []()-> std::string { return "another_demo_string_result18"; }
            );
            std::cout<<"AFTER18a invert_callback for std::string(*callback)(int, double)> [val2a, val2b] = ["
                <<val2a<<", "<<val2b<<']'<<std::endl;
        }



        std::cout<<"\nCorofork coroutine exited\n"
        "##############################################################################\n"
        <<std::endl;
    };

    std::cout<<"\nNote: all the stuff above is from nested awaitables setup\n"
               "\nthis is expected due to the way how coroutines are resumed"<<std::endl;

    std::cout<<"\n\n\nNow test the same stuff but result is available immediately\n"
    "##################################################################################\n"
    "##################################################################################\n"
    <<std::endl;

    corofork(){
        std::cout<<"\n"
        "##############################################################################\n"
        "Corofork coroutine entered\n"<<std::endl;

        //======================================================================
        std::cout<<"\nSIMPLE CASE OF GENERATING FUNCTION CALLBACK THAT RETURNS NOTHING"<<std::endl;
        {
            std::cout<<"\nBEFORE1b invert_function for std::function<void()>"<<std::endl;
            co_await invert_function([](const std::function<void()>& function){
                return true; //resolved immediately
            });
            std::cout<<"AFTER1b invert_function for std::function<void()>"<<std::endl;


            std::cout<<"\nBEFORE2b invert_function for std::function<void(int)>"<<std::endl;
            auto val1 = co_await invert_function([](const std::function<void(int)>& function){
                return std::optional<int>(4202); //resolved immediately
            });
            std::cout<<"AFTER2b invert_function for std::function<void(int)>, val1 = "<<val1<<std::endl;


            std::cout<<"\nBEFORE3b invert_function for std::function<void(int, double)>"<<std::endl;
            auto [val2a, val2b] = co_await invert_function([](const std::function<void(int, double)>& function){
                return std::optional< std::tuple<int, double> >(
                    std::tuple(4203, 4.203)
                ); //resolved immediately
            });
            std::cout<<"AFTER3b invert_function for std::function<void(int, double)> [val2a, val2b] = ["
                <<val2a<<", "<<val2b<<']'<<std::endl;
        }

        //======================================================================
        std::cout<<"\nCASE OF GENERATING FUNCTION CALLBACK THAT RETURNS PREDEFINED VALUE"<<std::endl;
        {
            std::cout<<"\nBEFORE4b invert_function for std::function<int()>"<<std::endl;
            co_await invert_function(
                [](const std::function<int()>& function){
                    return true; //resolved immediately
                },
                42004
            );
            std::cout<<"AFTER4b invert_function for std::function<int()>"<<std::endl;


            std::cout<<"\nBEFORE5b invert_function for std::function<double(int)>"<<std::endl;
            auto val1 = co_await invert_function(
                [](const std::function<double(int)>& function){
                    return std::optional<int>(4205); //resolved immediately
                },
                0.42005
            );
            std::cout<<"AFTER5b invert_function for std::function<double(int)>, val1 = "<<val1<<std::endl;


            std::cout<<"\nBEFORE6b invert_function for std::function<string(int, double)>"<<std::endl;
            auto [val2a, val2b] = co_await invert_function(
                [](const std::function<std::string(int, double)>& function){
                    return std::optional< std::tuple<int, double> >(
                        std::tuple(4206, 4.206)
                    ); //resolved immediately
                },
                "demo_string_result06"
            );
            std::cout<<"AFTER6b invert_function for std::function<string(int, double)> [val2a, val2b] = ["
                <<val2a<<", "<<val2b<<']'<<std::endl;
        }

        //======================================================================
        std::cout<<"\nCASE OF GENERATING FUNCTION CALLBACK THAT RETURNS RESULT OF FUNCTION CALL"<<std::endl;
        {
            std::cout<<"\nBEFORE7b invert_function for std::function<int()>"<<std::endl;
            co_await invert_function(
                [](const std::function<int()>& function){
                    return true; //resolved immediately
                },
                []{ return 42007; }
            );
            std::cout<<"AFTER7b invert_function for std::function<int()>"<<std::endl;


            std::cout<<"\nBEFORE8b invert_function for std::function<double(int)>"<<std::endl;
            auto val1 = co_await invert_function(
                [](const std::function<double(int)>& function){
                    return std::optional<int>(4208); //resolved immediately
                },
                []{ return 0.42008; }
            );
            std::cout<<"AFTER8b invert_function for std::function<double(int)>, val1 = "<<val1<<std::endl;


            std::cout<<"\nBEFORE9b invert_function for std::function<string(int, double)>"<<std::endl;
            auto [val2a, val2b] = co_await invert_function(
                [](const std::function<std::string(int, double)>& function){
                    return std::optional< std::tuple<int, double> >(
                        std::tuple(4209, 4.209)
                    ); //resolved immediately
                },
                []()-> std::string { return "another_demo_string_result09"; }
            );
            std::cout<<"AFTER9b invert_function for std::function<string(int, double)> [val2a, val2b] = ["
                <<val2a<<", "<<val2b<<']'<<std::endl;
        }


        std::cout<<
            "\n\n#################################################################\n"
            "NOW THE SAME BUT WITH \"PLAIN\" CALLBACKS\n"
        <<std::endl;
        
        //======================================================================
        std::cout<<"\nSIMPLE CASE OF GENERATING CALLBACK THAT RETURNS NOTHING"<<std::endl;
        {
            std::cout<<"\nBEFORE10b invert_callback for void(*callback)()"<<std::endl;
            co_await invert_callback<1>([](void(*callback)()){
                return true; //resolved immediately
            });
            std::cout<<"AFTER10b invert_callback for void(*callback)()>"<<std::endl;


            std::cout<<"\nBEFORE11b invert_callback for void(*callback)(int)>"<<std::endl;
            auto val1 = co_await invert_callback<1>([](void(*callback)(int)){
                return std::optional<int>(4211); //resolved immediately
            });
            std::cout<<"AFTER11b invert_callback for void(*callback)(int)>, val1 = "<<val1<<std::endl;


            std::cout<<"\nBEFORE12b invert_callback for void(*callback)(int, double)>"<<std::endl;
            auto [val2a, val2b] = co_await invert_callback<1>([](void(*callback)(int, double)){
                return std::optional< std::tuple<int, double> >(
                    std::tuple(4213, 4.212)
                ); //resolved immediately
            });
            std::cout<<"AFTER12b invert_callback for void(*callback)(int, double)> [val2a, val2b] = ["
                <<val2a<<", "<<val2b<<']'<<std::endl;
        }

        //======================================================================
        std::cout<<"\nCASE OF GENERATING CALLBACK THAT RETURNS PREDEFINED VALUE"<<std::endl;
        {
            std::cout<<"\nBEFORE13b invert_callback for int(*callback)()"<<std::endl;
            co_await invert_callback<1>(
                [](int(*callback)()){
                    return true; //resolved immediately
                },
                42013
            );
            std::cout<<"AFTER13b invert_callback for int(*callback)()"<<std::endl;


            std::cout<<"\nBEFORE14b invert_callback for double(*callback)(int)>"<<std::endl;
            auto val1 = co_await invert_callback<1>(
                [](double(*callback)(int)){
                    return std::optional<int>(4214); //resolved immediately
                },
                4.2014
            );
            std::cout<<"AFTER14b invert_callback for double(*callback)(int)>, val1 = "<<val1<<std::endl;


            std::cout<<"\nBEFORE15b invert_callback for std::string(*callback)(int, double)>"<<std::endl;
            auto [val2a, val2b] = co_await invert_callback<1>(
                [](std::string(*callback)(int, double)){
                    return std::optional< std::tuple<int, double> >(
                        std::tuple(4215, 4.215)
                    ); //resolved immediately
                },
                "demo_string_result15"
            );
            std::cout<<"AFTER15b invert_callback for std::string(*callback)(int, double) [val2a, val2b] = ["
                <<val2a<<", "<<val2b<<']'<<std::endl;
        }

        //======================================================================
        std::cout<<"\nCASE OF GENERATING CALLBACK THAT RETURNS RESULT OF FUNCTION CALL"<<std::endl;
        {
            std::cout<<"\nBEFORE16b invert_callback for int(*callback)()>"<<std::endl;
            co_await invert_callback<1>(
                [](int(*callback)()){
                    return true; //resolved immediately
                },
                []{ return 54016; }
            );
            std::cout<<"AFTER16b invert_callback for int(*callback)()"<<std::endl;


            std::cout<<"\nBEFORE17b invert_callback for double(*callback)(int)>"<<std::endl;
            auto val1 = co_await invert_callback<1>(
                [](double(*callback)(int)){
                    return std::optional<int>(542); //resolved immediately
                },
                []{ return 0.42017; }
            );
            std::cout<<"AFTER17 invert_callback for double(*callback)(int)>, val1 = "<<val1<<std::endl;


            std::cout<<"\nBEFORE18b invert_callback for std::string(*callback)(int, double)>"<<std::endl;
            auto [val2a, val2b] = co_await invert_callback<1>(
                [](std::string(*callback)(int, double)){
                    return std::optional< std::tuple<int, double> >(
                        std::tuple(4218, 4.218)
                    ); //resolved immediately
                },
                []()-> std::string { return "another_demo_string_result18"; }
            );
            std::cout<<"AFTER18b invert_callback for std::string(*callback)(int, double)> [val2a, val2b] = ["
                <<val2a<<", "<<val2b<<']'<<std::endl;
        }



        std::cout<<"\nCorofork coroutine exited\n"
        "##############################################################################\n"
        <<std::endl;
    };

#if 1

    /**
    Q:  When setup happens?
    A:  buffered can setup immediately, callbacks will accumulate;
        unbuffered also can happen immediately, callbacks will delay (DRAWBACK)
    
    Q:  What if source provides other call while previous is not completed?
    A:  buffered shall place them it into buffer;
        unbuffered shall cause the source to stall (DRAWBACK)

    Q:  What about "awaiting for something else"
    A:  buffered can await, callbacks will accumulate;
        unbuffered will cause source to stall 
        while awaiting for "something else"
        (probably for indefinite time, DRAWBACK)

    Q:  How callback returns a value?
    A:  buffered can only return immediately, 
        since every call needs immediate answer (DRAWBACK);
        unbuffered can provide result "in time" with function
        (but likely nobody will use that feature, just abandon it)

    The final decision is to make calls to subscription to be accumulated,
    await on subscription shall extract item from the buffer
    
    
    {
        auto waiter = invert_subscription([](std::function<void()> f){
            ... //manual setup for f

            //returns "unsubscriber"
            return [=]{
                ... //(manual) teardown
            }
        });

        //hmm... co_await for "something else" here
        //means calls to subscription shall be queued?

        for(int i = 0; i < 10; ++i){
            co_await waiter; //
        }

        //waiter goes out of scope, subscription disappears, generated API shall be deallocated
    }
    
    //actual return to issuer happens 
    //only when we co_await on "something else"
    //but here we are unsubscribed for sure

    co_await ...
    
    */

    /*{
        auto file = deferred{
            auto file = fopen(...)
            co_yield file;
            fclose(file)
        }

        //work *file

        //file goes out of scope and cleanup
        //seems deferpp works better for this  
    }*/
       
#endif

    std::cout<<"\nTesting coroutines, main leave"<<std::endl;

    //}
    
    return 0;
}
