cmake_minimum_required(VERSION 3.25)
project(corofork_demo LANGUAGES CXX)

# While in VSCode, use recommended extentions from extensions.json,
# they add context menus and buttons into status bar. 
# The set(CMAKE_BUILD_TYPE Release) has no effect on milticonfig builds.
# To provide --config ConfigurationName in VSCode's status bar 
# click CMake: [ConfigurationName] to select one of the configurations.
# There are also debug and run pictograms in status bar (hover to see hint)
# (note: those buttons are not related to debug tab))
message("Generated with config types: ${CMAKE_CONFIGURATION_TYPES}")

set(CMAKE_CXX_STANDARD 20)

if(MSVC)
    # Remember _HAS_STATIC_RTTI=0 shall also do /GR-
    # add_link_options("/DEBUG:FULL")
endif()

# While in VSCode click target in status bar to select the one to run
add_executable(corofork_all_demo corofork_all_demo.cpp)
add_executable(corofork_thread_demo corofork_thread_demo.cpp)
add_executable(corofork_multimpe_threads_demo corofork_multimpe_threads_demo.cpp)
