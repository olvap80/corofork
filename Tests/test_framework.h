/** @file test_framework.h
    @brief Minimal dependency free test harness for corofork.

Each test file is a standalone executable: include this header,
declare cases with COROFORK_TEST and the main() below runs them all.
Exit code is the number of failed cases (0 means everything passed),
so CTest (or any CI) picks failures up automatically.

Rules for tests living here:
  - no sleeps, no timing assumptions (use explicit handshakes instead),
  - every test must be able to fail loudly, never hang silently.
*/

#ifndef COROFORK_TEST_FRAMEWORK_H_INCLUDED
#define COROFORK_TEST_FRAMEWORK_H_INCLUDED

#include <cstdio>
#include <exception>
#include <sstream>
#include <string>
#include <vector>


namespace corofork_test{

    /// One registered test case
    struct Case{
        const char* name;
        void (*run)();
    };

    /// All cases registered so far (function local to avoid init order issues)
    inline std::vector<Case>& registry(){
        static std::vector<Case> all;
        return all;
    }

    /// Failed checks of the case being executed right now
    inline unsigned& failuresInCase(){
        static unsigned count = 0;
        return count;
    }

    /// Self registering helper behind COROFORK_TEST
    struct Registrar{
        Registrar(const char* name, void (*run)()){
            registry().push_back(Case{name, run});
        }
    };

    /// Render a value for the failure message (falls back for non printable ones)
    template<class T>
    std::string describe(const T& value){
        if constexpr( requires(std::ostream& os, const T& v){ os << v; } ){
            std::ostringstream os;
            os << value;
            return os.str();
        }
        else{
            return "<not printable>";
        }
    }

    /// Report one failed check (does not abort, the case runs to the end)
    inline void reportFailure(
        const char* file, int line, const char* expression, const std::string& detail
    ){
        ++failuresInCase();
        std::printf("      FAIL %s(%d): %s%s\n",
                    file, line, expression, detail.c_str());
    }

    /**Value that detects being copied after its destruction.
    The destructor overwrites aliveMarker (volatile, so the compiler keeps
    that store), the copy operations count copies made from such a source.
    Used to catch generated callbacks that read their captures after
    resuming the coroutine (resuming may destroy those captures) */
    struct LifetimeTrackedValue{
        static constexpr int aliveMarkerValue = 0x600D;
        static constexpr int destroyedMarkerValue = 0xDEAD;

        /// Copies made from an already destroyed instance (reset by the test)
        static inline int copiesFromDestroyedSource = 0;

        explicit LifetimeTrackedValue(int payloadToStore): payload(payloadToStore) {}

        LifetimeTrackedValue(const LifetimeTrackedValue& source): payload(source.payload){
            CountIfDestroyed(source);
        }

        LifetimeTrackedValue& operator=(const LifetimeTrackedValue& source){
            CountIfDestroyed(source);
            payload = source.payload;
            return *this;
        }

        ~LifetimeTrackedValue(){ aliveMarker = destroyedMarkerValue; }

        /// Overwritten by the destructor
        volatile int aliveMarker = aliveMarkerValue;

        /// What the test checks for
        int payload;

    private:
        static void CountIfDestroyed(const LifetimeTrackedValue& source){
            if( source.aliveMarker != aliveMarkerValue ){
                ++copiesFromDestroyedSource;
            }
        }
    };

    /**Counts copies and moves of itself (reset by the test).
    Captured by a setup lambda, the counters tell how often invert_*
    copies and moves the setup lambda */
    struct CopyMoveCounter{
        static inline int copies = 0;
        static inline int moves = 0;

        static void Reset(){
            copies = 0;
            moves = 0;
        }

        CopyMoveCounter() = default;
        CopyMoveCounter(const CopyMoveCounter&){ ++copies; }
        CopyMoveCounter(CopyMoveCounter&&) noexcept { ++moves; }
        CopyMoveCounter& operator=(const CopyMoveCounter&){ ++copies; return *this; }
        CopyMoveCounter& operator=(CopyMoveCounter&&) noexcept { ++moves; return *this; }
    };

} //namespace corofork_test


/// Declare a test case (body follows in {})
#define COROFORK_TEST(name) \
    static void name(); \
    static corofork_test::Registrar corofork_test_registrar_##name(#name, &name); \
    static void name()

/// Check a condition holds
#define CHECK(condition) \
    do{ \
        if( !(condition) ){ \
            corofork_test::reportFailure(__FILE__, __LINE__, #condition, ""); \
        } \
    }while(false)

/// Check two values compare equal (prints both on mismatch)
#define CHECK_EQ(actual, expected) \
    do{ \
        auto&& corofork_test_a = (actual); \
        auto&& corofork_test_e = (expected); \
        if( !(corofork_test_a == corofork_test_e) ){ \
            corofork_test::reportFailure( \
                __FILE__, __LINE__, #actual " == " #expected, \
                "\n           actual: " + corofork_test::describe(corofork_test_a) + \
                "\n         expected: " + corofork_test::describe(corofork_test_e) \
            ); \
        } \
    }while(false)

/// Check expression throws the expected exception type
#define CHECK_THROWS(expression, ExceptionType) \
    do{ \
        bool corofork_test_thrown = false; \
        try{ (void)(expression); } \
        catch(const ExceptionType&){ corofork_test_thrown = true; } \
        catch(...){} \
        if( !corofork_test_thrown ){ \
            corofork_test::reportFailure( \
                __FILE__, __LINE__, #expression, \
                " did not throw " #ExceptionType \
            ); \
        } \
    }while(false)


/// Runs every registered case, one line per case, details on failures
int main(){
    unsigned failedCases = 0;

    std::printf("Running %u case(s)\n",
                unsigned(corofork_test::registry().size()));

    for( const corofork_test::Case& testCase : corofork_test::registry() ){
        corofork_test::failuresInCase() = 0;
        std::printf("  %s\n", testCase.name);

        try{
            testCase.run();
        }
        catch(const std::exception& e){
            corofork_test::reportFailure(
                __FILE__, __LINE__, testCase.name,
                std::string(" escaped with std::exception: ") + e.what()
            );
        }
        catch(...){
            corofork_test::reportFailure(
                __FILE__, __LINE__, testCase.name, " escaped with unknown exception"
            );
        }

        if( corofork_test::failuresInCase() ){
            ++failedCases;
            std::printf("    -> FAILED (%u check(s))\n", corofork_test::failuresInCase());
        }
        else{
            std::printf("    -> ok\n");
        }
    }

    if( failedCases ){
        std::printf("\n%u case(s) FAILED\n", failedCases);
    }
    else{
        std::printf("\nAll cases passed\n");
    }
    return int(failedCases);
}

#endif
