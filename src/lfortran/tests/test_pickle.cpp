#include <tests/doctest.h>
#include <iostream>

#include <lfortran/pickle.h>
#include <lfortran/ast_to_src.h>
#include <lfortran/parser/parser.h>
#include <lfortran/fortran_evaluator.h>
#include <libasr/asr_deserialization_visitor.h>
#include <libasr/bwriter.h>
#include <libasr/checked_cast.h>

#include <cstdint>
#include <limits>
#include <stdexcept>
#include <vector>

void section(const std::string &s)
{
    std::cout << color(LCompilers::style::bold) << color(LCompilers::fg::blue) << s << color(LCompilers::style::reset) << color(LCompilers::fg::reset) << std::endl;
}

namespace {
    class ParserError0 {
    };
}

std::string p(Allocator &al, const std::string &s)
{
    LCompilers::LFortran::AST::TranslationUnit_t* result;
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::CompilerOptions co;
    co.interactive = true;
    LCompilers::Result<LCompilers::LFortran::AST::TranslationUnit_t*> res
        = LCompilers::LFortran::parse(al, s, diagnostics, co);
    if (res.ok) {
        result = res.result;
    } else {
        throw ParserError0();
    }
    LCompilers::LFortran::AST::ast_t* ast;
    if (result->n_items >= 1) {
        ast = result->m_items[0];
    } else {
        ast = (LCompilers::LFortran::AST::ast_t*)result;
    }
    std::string pickle = LCompilers::LFortran::pickle(*ast);
    std::string src = LCompilers::LFortran::ast_to_src(*result);

    /*
    // Print the test nicely:
    section("--------------------------------------------------------------------------------");
    section("SRC:");
    std::cout << s << std::endl;
    section("SRC -> AST:");
    std::cout << pickle << std::endl;
    section("AST -> SRC:");
    std::cout << src << std::endl;
    */

    return pickle;
}

#define P(x) p(al, x)

TEST_CASE("Names") {
    Allocator al(4*1024);

    CHECK(P("2*y")   == "(* 2 y)");
    CHECK(P("2*yz")   == "(* 2 yz)");
    CHECK(P("abc*xyz")   == "(* abc xyz)");
    CHECK(P("abc*function")   == "(* abc function)");
    CHECK(P("abc*subroutine")   == "(* abc subroutine)");

    CHECK_THROWS_AS(P("abc*2xyz"), ParserError0);
}

TEST_CASE("Symbolic expressions") {
    Allocator al(4*1024);

    CHECK(P("2*x")   == "(* 2 x)");
    CHECK(P("(2*x)") == "(Parenthesis (* 2 x))");
    CHECK(P("3*x**y") == "(* 3 (** x y))");
    CHECK(P("a+b*c") == "(+ a (* b c))");
    CHECK(P("(a+b)*c") == "(* (Parenthesis (+ a b)) c)");

    CHECK_THROWS_AS(P("2*"), ParserError0);
    CHECK_THROWS_AS(P("(2*x"), ParserError0);
    CHECK_THROWS_AS(P("2*x)"), ParserError0);
    CHECK_THROWS_AS(P("3*x**"), ParserError0);
}

TEST_CASE("Symbolic assignments") {
    Allocator al(4*1024);
    CHECK(P("x = y") == "(Assignment 0 x y ())");
    CHECK(P("x = 2*y") == "(Assignment 0 x (* 2 y) ())");

    CHECK_THROWS_AS(P("x ="), ParserError0);
    CHECK_THROWS_AS(P("x = 2*"), ParserError0);
    CHECK_THROWS_AS(P(" = 2*y"), ParserError0);
}

TEST_CASE("Arithmetics") {
    Allocator al(4*1024);

    CHECK_THROWS_AS(P("1+2**(*4)"), ParserError0);
    CHECK_THROWS_AS(P("1x"), ParserError0);
    CHECK_THROWS_AS(P("1+"), ParserError0);
    CHECK_THROWS_AS(P("(1+"), ParserError0);
    CHECK_THROWS_AS(P("(1+2"), ParserError0);
    CHECK_THROWS_AS(P("1+2*"), ParserError0);
    CHECK_THROWS_AS(P("f(3+6"), ParserError0);
}

TEST_CASE("Comparison") {
    Allocator al(4*1024);

    CHECK(P("1 == 2") == "(== 1 2)");
    CHECK(P("1 /= 2") == "(/= 1 2)");
    CHECK(P("1 < 2") == "(< 1 2)");
    CHECK(P("1 <= 2") == "(<= 1 2)");
    CHECK(P("1 > 2") == "(> 1 2)");
    CHECK(P("1 >= 2") == "(>= 1 2)");

    CHECK(P("1 .eq. 2") == "(== 1 2)");
    CHECK(P("1 .ne. 2") == "(/= 1 2)");
    CHECK(P("1 .lt. 2") == "(< 1 2)");
    CHECK(P("1 .le. 2") == "(<= 1 2)");
    CHECK(P("1 .gt. 2") == "(> 1 2)");
    CHECK(P("1 .ge. 2") == "(>= 1 2)");

    CHECK(P("1.eq.2") == "(== 1 2)");
    CHECK(P("1.ne.2") == "(/= 1 2)");
    CHECK(P("1.lt.2") == "(< 1 2)");
    CHECK(P("1.le.2") == "(<= 1 2)");
    CHECK(P("1.gt.2") == "(> 1 2)");
    CHECK(P("1.ge.2") == "(>= 1 2)");

    CHECK(P("1 == 2 + 3") == "(== 1 (+ 2 3))");
    CHECK(P("1 + 3*4 <= 2 + 3") == "(<= (+ 1 (* 3 4)) (+ 2 3))");
    CHECK(P("1 .eq. 2 + 3") == "(== 1 (+ 2 3))");
    CHECK(P("1 + 3*4 .le. 2 + 3") == "(<= (+ 1 (* 3 4)) (+ 2 3))");
    CHECK(P("1.eq.2 + 3") == "(== 1 (+ 2 3))");
    CHECK(P("1 + 3*4.le.2 + 3") == "(<= (+ 1 (* 3 4)) (+ 2 3))");

    // These are not valid Fortran, but we test that the parser follows the
    // precedence rules correctly
    CHECK(P("1 == 2 + 3 == 2") == "(== (== 1 (+ 2 3)) 2)");
    CHECK(P("(1 == 2) + 3") == "(+ (Parenthesis (== 1 2)) 3)");
}


TEST_CASE("Multiple units") {
    Allocator al(4*1024);
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::CompilerOptions co;
    co.interactive = true;
    LCompilers::LFortran::AST::TranslationUnit_t* results;
    std::string s = R"(x = x+1
        y = z+1)";
    results = LCompilers::TRY(LCompilers::LFortran::parse(al, s, diagnostics, co));
    CHECK(results->n_items == 2);
    CHECK(LCompilers::LFortran::pickle(*results->m_items[0]) == "(Assignment 0 x (+ x 1) ())");
    CHECK(LCompilers::LFortran::pickle(*results->m_items[1]) == "(Assignment 0 y (+ z 1) ())");

    s = "x = x+1; ; y = z+1";
    results = LCompilers::TRY(LCompilers::LFortran::parse(al, s, diagnostics, co));
    CHECK(results->n_items == 2);
    CHECK(LCompilers::LFortran::pickle(*results->m_items[0]) == "(Assignment 0 x (+ x 1) (TriviaNode [] [(Semicolon) (Semicolon)]))");
    CHECK(LCompilers::LFortran::pickle(*results->m_items[1]) == "(Assignment 0 y (+ z 1) ())");

    s = R"(x = x+1;

    ; y = z+1)";
    results = LCompilers::TRY(LCompilers::LFortran::parse(al, s, diagnostics, co));
    CHECK(results->n_items == 2);
    CHECK(LCompilers::LFortran::pickle(*results->m_items[0]) == "(Assignment 0 x (+ x 1) (TriviaNode [] [(Semicolon) (EndOfLine) (EndOfLine) (Semicolon)]))");
    CHECK(LCompilers::LFortran::pickle(*results->m_items[1]) == "(Assignment 0 y (+ z 1) ())");

    s = R"(x+1
    y = z+1
    a)";
    results = LCompilers::TRY(LCompilers::LFortran::parse(al, s, diagnostics, co));
    CHECK(results->n_items == 3);
    CHECK(LCompilers::LFortran::pickle(*results->m_items[0]) == "(+ x 1)");
    CHECK(LCompilers::LFortran::pickle(*results->m_items[1]) == "(Assignment 0 y (+ z 1) ())");
    CHECK(LCompilers::LFortran::pickle(*results->m_items[2]) == "a");

    s = R"(function g()
    x = y
    x = 2*y
    end function
    s = x
    y = z+1
    a)";
    results = LCompilers::TRY(LCompilers::LFortran::parse(al, s, diagnostics, co));
    CHECK(results->n_items == 4);
    CHECK(LCompilers::LFortran::pickle(*results->m_items[0]) == "(Function g [] [] () () () [(Assignment 0 x y ()) (Assignment 0 x (* 2 y) ())] [] [])");
    CHECK(LCompilers::LFortran::pickle(*results->m_items[1]) == "(Assignment 0 s x ())");
    CHECK(LCompilers::LFortran::pickle(*results->m_items[2]) == "(Assignment 0 y (+ z 1) ())");
    CHECK(LCompilers::LFortran::pickle(*results->m_items[3]) == "a");
}

TEST_CASE("if") {
    Allocator al(16*1024);

    CHECK_THROWS_AS(P(R"(subroutine g
    if (x) then
        end if = 5
    end if
    end subroutine)"), ParserError0);

    CHECK_THROWS_AS(P(R"(subroutine g
    if (else) then
        else = 5
    else if (else) then
        else if (else)
    else if (else) then
        else = 3
    end if
    end subroutine)"), ParserError0);

}


TEST_CASE("while") {
    Allocator al(4*1024);

    CHECK_THROWS_AS(P(
 R"(do while (x)
        end do = 5
    enddo)"), ParserError0);

}

TEST_CASE("do loop") {
    Allocator al(4*1024);


    CHECK(P(
 R"(do
    a = a + i
    b = 3)") == "do");
}

TEST_CASE("exit") {
    Allocator al(4*1024);

    // (exit) ... statement exit
    // exit   ... variable "exit"

    CHECK(P(
 R"(exit
    enddo)") == "exit");

}

TEST_CASE("cycle") {
    Allocator al(4*1024);

    CHECK(P(
 R"(cycle
    enddo)") == "cycle");

}

TEST_CASE("return") {
    Allocator al(4*1024);

    CHECK(P(
 R"(return
    enddo)") == "return");

}

TEST_CASE("declaration") {
    Allocator al(1024*1024);

    CHECK_THROWS_AS(P("integer, parameter, pointer x"),
            ParserError0);
    CHECK_THROWS_AS(P("integer x(5,:,:3,3:) = x y"), ParserError0);

}

TEST_CASE("checked integer conversions") {
    using LCompilers::integer_fits;
    using LCompilers::checked_integer_cast;
    using LCompilers::checked_bit_pattern_cast;

    CHECK(integer_fits<int32_t>(int64_t{2147483647}));
    CHECK(!integer_fits<int32_t>(int64_t{2147483648}));
    CHECK(integer_fits<int32_t>(int64_t{-2147483647} - 1));
    CHECK(!integer_fits<int32_t>(int64_t{-2147483647} - 2));
    CHECK(integer_fits<uint32_t>(uint64_t{4294967295}));
    CHECK(!integer_fits<uint32_t>(uint64_t{4294967296}));
    CHECK(!integer_fits<uint64_t>(int64_t{-1}));
    CHECK(!integer_fits<uint8_t>(int8_t{-1}));
    CHECK(integer_fits<int64_t>(uint64_t{9223372036854775807}));
    CHECK(!integer_fits<int64_t>(std::numeric_limits<uint64_t>::max()));
    CHECK(!integer_fits<int8_t>(uint8_t{128}));

    CHECK(checked_integer_cast<int>(size_t{42}) == 42);
    CHECK(checked_bit_pattern_cast<uint8_t>(int64_t{-1}) == 255);
    CHECK(checked_bit_pattern_cast<int8_t>(int64_t{255}) == -1);
    CHECK(checked_bit_pattern_cast<int8_t>(int64_t{-128}) == -128);

    // The range checks are assertions: without WITH_LFORTRAN_ASSERT the
    // conversions are plain static_casts.
#if defined(WITH_LFORTRAN_ASSERT)
    CHECK_THROWS_AS(checked_integer_cast<int>(int64_t{2147483648}),
        LCompilers::LCompilersException);
    CHECK_THROWS_AS(checked_integer_cast<uint32_t>(int64_t{-1}),
        LCompilers::LCompilersException);
    CHECK_THROWS_AS(checked_bit_pattern_cast<int8_t>(int64_t{256}),
        LCompilers::LCompilersException);
    CHECK_THROWS_AS(checked_bit_pattern_cast<uint16_t>(int64_t{-32769}),
        LCompilers::LCompilersException);
#else
    CHECK(checked_integer_cast<uint32_t>(int64_t{-1}) == 4294967295u);
    CHECK(checked_bit_pattern_cast<uint8_t>(int64_t{256}) == 0);
#endif
}

TEST_CASE("location sizes must fit 32 bits") {
    CHECK(LCompilers::location_size_to_uint32(
        size_t{std::numeric_limits<uint32_t>::max()})
        == std::numeric_limits<uint32_t>::max());
    if (sizeof(size_t) > sizeof(uint32_t)) {
        CHECK_THROWS_AS(LCompilers::location_size_to_uint32(
            static_cast<size_t>(std::numeric_limits<uint32_t>::max()) + 1),
            std::length_error);
    }
}

namespace {
    // Feeds the given 64-bit integers to the generated deserializer helpers.
    class IntQueueDeserializer
        : public LCompilers::ASR::DeserializationBaseVisitor<IntQueueDeserializer> {
    public:
        std::vector<uint64_t> values;
        size_t next = 0;
        IntQueueDeserializer(Allocator &al, const std::vector<uint64_t> &values)
            : DeserializationBaseVisitor(al, true, 0), values(values) {}
        uint64_t read_int64() {
            return values.at(next++);
        }
    };
}

TEST_CASE("deserialization rejects out of range locations and counters") {
    Allocator al(1024);
    const uint64_t u32_max = std::numeric_limits<uint32_t>::max();
    IntQueueDeserializer d(al, {5, u32_max, u32_max + 1});
    CHECK(d.read_location_position(10) == 15);
    CHECK(d.read_location_position(0) == u32_max);
    CHECK_THROWS_AS(d.read_location_position(0),
        LCompilers::LCompilersException);

    CHECK(d.symtab_counter_from_serialized(u32_max) == u32_max);
    CHECK_THROWS_AS(d.symtab_counter_from_serialized(u32_max + 1),
        LCompilers::LCompilersException);
}

TEST_CASE("text reader accepts the full unsigned range of narrow integers") {
    LCompilers::TextReader r("255 65535 4294967295 256 ");
    CHECK(r.read_int8() == 255);
    CHECK(r.read_int16() == 65535);
    CHECK(r.read_int32() == 4294967295u);
    CHECK_THROWS_AS(r.read_int8(), LCompilers::LCompilersException);
}

TEST_CASE("kind value out of integer(4) range") {
    std::string src = R"(program kind_range
integer(8), parameter :: k = 4294967300_8
real(k) :: x
end program
)";
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::CompilerOptions compiler_options;
    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = "input.f90";
        lm.files.push_back(fl);
    }
    LCompilers::FortranEvaluator e(compiler_options);
    LCompilers::Result<LCompilers::ASR::TranslationUnit_t*> r
        = e.get_asr2(src, lm, diagnostics);
    CHECK(!r.ok);
    bool found = false;
    for (const auto &d : diagnostics.diagnostics) {
        if (d.message.find(
                "kind value 4294967300 is out of range for integer(4)")
                != std::string::npos) {
            found = true;
        }
    }
    CHECK(found);
}
