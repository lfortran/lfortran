#include <tests/doctest.h>
#include <server/lsp_specification.h>

#include <cstring>
#include <memory>
#include <new>
#include <utility>

using namespace LCompilers::LanguageServerProtocol;

TEST_CASE("LSP tuple label copy starts the union member lifetime")
{
    ParameterInformation_label source;
    source = std::pair<uinteger_t, uinteger_t>{3, 7};

    alignas(ParameterInformation_label)
        unsigned char storage[sizeof(ParameterInformation_label)];
    // A copy constructor must not read the previous contents of its storage.
    std::memset(storage, 0xa5, sizeof(storage));
    auto *copy = new (storage) ParameterInformation_label(source);

    CHECK(copy->type() == ParameterInformation_labelType::PairOfUIntegerAndUInteger);
    const auto &value = copy->pairOfUIntegerAndUInteger();
    CHECK(value.first == 3);
    CHECK(value.second == 7);
    CHECK(&value != &source.pairOfUIntegerAndUInteger());
    copy->~ParameterInformation_label();
    CHECK(source.pairOfUIntegerAndUInteger().first == 3);
    CHECK(source.pairOfUIntegerAndUInteger().second == 7);
}

TEST_CASE("LSP tuple label lvalue assignment constructs the union member")
{
    using Pair = std::pair<uinteger_t, uinteger_t>;
    const Pair pair{3, 7};
    const auto pointer = std::make_unique<Pair>(pair);
    ParameterInformation_label source;
    source = Pair(pair);

    alignas(ParameterInformation_label)
        unsigned char storage[sizeof(ParameterInformation_label)];
    // Default construction leaves the inactive union storage untouched.
    std::memset(storage, 0xa5, sizeof(storage));
    auto *destination = new (storage) ParameterInformation_label;

    SUBCASE("label copy assignment") {
        *destination = source;
    }
    SUBCASE("pair lvalue assignment") {
        *destination = pair;
    }
    SUBCASE("unique_ptr lvalue assignment") {
        *destination = pointer;
    }
    SUBCASE("replace a string with a pair lvalue") {
        *destination = string_t("parameter label");
        *destination = pair;
    }

    CHECK(destination->type() == ParameterInformation_labelType::PairOfUIntegerAndUInteger);
    const auto &value = destination->pairOfUIntegerAndUInteger();
    CHECK(value.first == 3);
    CHECK(value.second == 7);
    CHECK(&value != &source.pairOfUIntegerAndUInteger());
    CHECK(&value != pointer.get());
    *destination = string_t("after copy");
    CHECK(destination->type() == ParameterInformation_labelType::String);
    CHECK(destination->string() == "after copy");
    destination->~ParameterInformation_label();
    CHECK(source.pairOfUIntegerAndUInteger() == pair);
    CHECK(*pointer == pair);
}
