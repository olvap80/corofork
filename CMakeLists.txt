cmake_minimum_required(VERSION 3.25)
project(corofork_demo LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 20)

# While in VSCode, use recommended extentions from extensions.json,
# they add context menus and buttons into status bar. 
# To provide --config ConfigurationName in VSCode's
# click "Activity Bar" on the left, then go "CMake" icon.
# See also https://github.com/microsoft/vscode-cmake-tools/blob/main/docs/debug-launch.md
message("Generated with config types: ${CMAKE_CONFIGURATION_TYPES}")


if(MSVC)
    # Remember _HAS_STATIC_RTTI=0 shall also do /GR-
    # The set(CMAKE_BUILD_TYPE Release) has no effect on milticonfig builds.

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


# Automated tests (no external dependencies, see Tests/test_framework.h).
# Build and run them all with:
#     cmake --build build && ctest --test-dir build --output-on-failure
# Every test executable also runs standalone and returns the number of
# failed cases as its exit code.
enable_testing()

set(COROFORK_TESTS
    test_corofork
    test_invert_function
    test_invert_subscription
    test_callback_pool
)

find_package(Threads REQUIRED)

foreach(corofork_test IN LISTS COROFORK_TESTS)
    add_executable(${corofork_test} Tests/${corofork_test}.cpp)
    target_include_directories(${corofork_test} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/Tests)
    target_link_libraries(${corofork_test} PRIVATE Threads::Threads)
    if(MSVC)
        target_compile_options(${corofork_test} PRIVATE /W4)
    else()
        target_compile_options(${corofork_test} PRIVATE -Wall -Wextra)
    endif()
    add_test(NAME ${corofork_test} COMMAND ${corofork_test})
endforeach()
