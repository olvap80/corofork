cmake_minimum_required(VERSION 3.25)
project(corofork_demo LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 20)

# While in VSCode, use recommended extentions from extensions.json,
# they add context menus and buttons into status bar. 
# The set(CMAKE_BUILD_TYPE Release) has no effect on milticonfig builds.
# To provide --config ConfigurationName in VSCode's status bar 
# click CMake: [ConfigurationName] to select one of the configurations.
# There are also debug and run pictograms in status bar (hover to see hint)
# (note: those buttons are not related to debug tab))
# See also https://github.com/microsoft/vscode-cmake-tools/blob/58a1962bf47dbd53b4798734a72192f7de12512b/docs/debug-launch.md
message("Generated with config types: ${CMAKE_CONFIGURATION_TYPES}")


if(MSVC)
    # Remember _HAS_STATIC_RTTI=0 shall also do /GR-

    # uncomment below to have PDB in addition to execitable even in release 
    set(CMAKE_MSVC_DEBUG_INFORMATION_FORMAT "ProgramDatabase")
    add_link_options("/DEBUG:FULL")
endif()

include_directories(${CMAKE_CURRENT_SOURCE_DIR})

# While in VSCode click "Activity Bar" on the left, then "CMake" 
# to see the list of targets and select the one to run.
# Find Build/Debug/Run buttons in the status bar below
add_executable(all_invert_function_and_callback Demo/all_invert_function_and_callback.cpp)
add_executable(go_thread_and_back Demo/go_thread_and_back.cpp)
add_executable(invert_subscription Demo/invert_subscription.cpp)
add_executable(multimpe_threads_invert_subscription Demo/multimpe_threads_invert_subscription.cpp)
