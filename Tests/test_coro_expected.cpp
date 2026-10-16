/** @file test_coro_expected.cpp
    @brief Cover coro_expected: value and error paths, co_await returning the
           error right away, destruction of the body locals, void Value,
           error type conversion, move only values, exceptions,
           calls from a corofork body and (C++23) std::expected interop.

Built twice by CMake: as C++20 (test_coro_expected) and as C++23
(test_coro_expected_cpp23, the std::expected cases exist only there and only
with a standard library providing <expected>).
*/

#include "coro_expected.h"
#include "corofork.h"
#include "test_framework.h"

#include <functional>
#include <memory>
#include <stdexcept>
#include <string>
#include <system_error>
#include <vector>


//______________________________________________________________________________
// functions under test

namespace{

    coro_expected<int, std::errc> parseDigit(char text){
        if( text < '0' || text > '9' ){
            co_return coro_unexpected{std::errc::invalid_argument};
        }
        co_return text - '0';
    }

    /// Body local that writes own name into the log once destroyed
    struct LoggedLocal{
        std::vector<std::string>& destructionLog;
        const char* name;
        ~LoggedLocal(){ destructionLog.push_back(name); }
    };

    coro_expected<int, std::errc> parseTwoDigits(
        const char* text,
        std::vector<std::string>& destructionLog,
        int& statementsAfterSecondAwait
    ){
        LoggedLocal first{destructionLog, "first"};
        int tens = co_await parseDigit(text[0]);
        LoggedLocal second{destructionLog, "second"};
        int ones = co_await parseDigit(text[1]);
        ++statementsAfterSecondAwait;
        co_return tens * 10 + ones;
    }

    coro_expected<void, std::errc> checkNotEmpty(const char* text){
        if( !*text ){
            co_await coro_unexpected{std::errc::result_out_of_range};
        }
    }

    coro_expected<int, std::errc> parseNonEmpty(const char* text){
        co_await checkNotEmpty(text);
        int tens = co_await parseDigit(text[0]);
        int ones = co_await parseDigit(text[1]);
        co_return tens * 10 + ones;
    }

    /// std::errc converts to std::error_condition: the error is converted on the way up
    coro_expected<int, std::error_condition> parseDigitAsErrorCondition(char text){
        co_return co_await parseDigit(text);
    }

    coro_expected<std::unique_ptr<int>, std::errc> makeOwnedDigit(char text){
        co_return std::make_unique<int>(co_await parseDigit(text));
    }

    coro_expected<int, std::errc> takeOwnedDigit(char text){
        std::unique_ptr<int> owned = co_await makeOwnedDigit(text);
        co_return *owned;
    }

    coro_expected<int, std::errc> throwsInBody(bool shallThrow){
        if( shallThrow ){
            throw std::runtime_error("thrown in coro_expected body");
        }
        co_return 1;
    }

} //namespace


//______________________________________________________________________________
// value and error paths

COROFORK_TEST(coro_expected_returns_value){
    auto parsed = parseDigit('7');
    CHECK(parsed.has_value());
    CHECK(bool(parsed));
    CHECK_EQ(parsed.value(), 7);
}

COROFORK_TEST(coro_expected_returns_error){
    auto parsed = parseDigit('x');
    CHECK(!parsed.has_value());
    CHECK(parsed.error() == std::errc::invalid_argument);
}

COROFORK_TEST(coro_expected_co_await_returns_error_right_away){
    std::vector<std::string> destructionLog;
    int statementsAfterSecondAwait = 0;

    auto parsed = parseTwoDigits("4x", destructionLog, statementsAfterSecondAwait);

    CHECK(!parsed.has_value());
    CHECK(parsed.error() == std::errc::invalid_argument);
    CHECK_EQ(statementsAfterSecondAwait, 0); //nothing after the failed co_await ran
}

COROFORK_TEST(coro_expected_co_await_gives_value){
    std::vector<std::string> destructionLog;
    int statementsAfterSecondAwait = 0;

    CHECK_EQ(parseTwoDigits("42", destructionLog, statementsAfterSecondAwait).value(), 42);
    CHECK_EQ(statementsAfterSecondAwait, 1);
}


//______________________________________________________________________________
// locals alive at the failed co_await are destroyed together with coro_expected

COROFORK_TEST(coro_expected_destroys_body_locals_in_reverse_order){
    std::vector<std::string> destructionLog;
    int statementsAfterSecondAwait = 0;

    {
        auto parsed = parseTwoDigits("4x", destructionLog, statementsAfterSecondAwait);
        CHECK(!parsed.has_value());
        CHECK(destructionLog.empty()); //still alive inside the suspended frame
    }

    CHECK((destructionLog == std::vector<std::string>{"second", "first"}));
}

COROFORK_TEST(coro_expected_moved_keeps_outcome){
    auto parsed = parseDigit('3');
    coro_expected<int, std::errc> movedTo = std::move(parsed);
    CHECK_EQ(movedTo.value(), 3);

    movedTo = parseDigit('z');
    CHECK(movedTo.error() == std::errc::invalid_argument);
}


//______________________________________________________________________________
// void Value, error conversion, move only values

COROFORK_TEST(coro_expected_void_value_returns_error_through_co_await){
    CHECK(checkNotEmpty("a").has_value());
    CHECK(checkNotEmpty("").error() == std::errc::result_out_of_range);

    CHECK_EQ(parseNonEmpty("17").value(), 17);
    CHECK(parseNonEmpty("").error() == std::errc::result_out_of_range);
    CHECK(parseNonEmpty("1?").error() == std::errc::invalid_argument);
}

COROFORK_TEST(coro_expected_converts_error_of_awaited_result){
    CHECK_EQ(parseDigitAsErrorCondition('5').value(), 5);
    CHECK(parseDigitAsErrorCondition('q').error() == std::errc::invalid_argument);
}

COROFORK_TEST(coro_expected_moves_move_only_value){
    CHECK_EQ(takeOwnedDigit('9').value(), 9);
    CHECK(takeOwnedDigit('-').error() == std::errc::invalid_argument);
}


//______________________________________________________________________________
// exception escaping the body is rethrown on access

COROFORK_TEST(coro_expected_rethrows_exception_on_access){
    auto failed = throwsInBody(true);
    CHECK_THROWS(failed.has_value(), std::runtime_error);
    CHECK_THROWS(failed.error(), std::runtime_error);

    CHECK_EQ(throwsInBody(false).value(), 1);
}


//______________________________________________________________________________
// coro_expected functions are plain calls inside a corofork body

COROFORK_TEST(coro_expected_called_from_corofork_body){
    std::function<void(char)> stored;
    int received = 0;
    std::errc receivedError = {};

    corofork(&){
        for( int round = 0; round < 2; ++round ){
            char text = co_await invert_function([&](std::function<void(char)> cb){ stored = cb; });
            auto parsed = parseDigit(text);
            if( parsed ){
                received = parsed.value();
            }
            else{
                receivedError = parsed.error();
            }
        }
        co_return;
    };

    stored('6');
    stored('?');
    CHECK_EQ(received, 6);
    CHECK(receivedError == std::errc::invalid_argument);
}


//______________________________________________________________________________
// std::expected interop (C++23)

#if defined(__cpp_lib_expected)

namespace{

    std::expected<int, std::errc> stdExpectedDigit(char text){
        if( text < '0' || text > '9' ){
            return std::unexpected(std::errc::invalid_argument);
        }
        return text - '0';
    }

    coro_expected<int, std::errc> awaitsStdExpected(char text){
        int digit = co_await stdExpectedDigit(text);
        co_return digit + 1;
    }

} //namespace

COROFORK_TEST(coro_expected_awaits_std_expected){
    CHECK_EQ(awaitsStdExpected('4').value(), 5);
    CHECK(awaitsStdExpected('x').error() == std::errc::invalid_argument);
}

COROFORK_TEST(coro_expected_converts_to_std_expected){
    std::expected<int, std::errc> convertedValue = parseDigit('8');
    std::expected<int, std::errc> convertedError = parseDigit('#');
    std::expected<void, std::errc> convertedVoid = checkNotEmpty("");

    CHECK(convertedValue.has_value() && *convertedValue == 8);
    CHECK(!convertedError.has_value() && convertedError.error() == std::errc::invalid_argument);
    CHECK(!convertedVoid.has_value() && convertedVoid.error() == std::errc::result_out_of_range);
}

#endif
