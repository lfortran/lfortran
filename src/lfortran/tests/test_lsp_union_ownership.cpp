#include <server/lsp_specification.h>

#include <cstdio>
#include <cstdlib>
#include <new>
#include <utility>

namespace {
void *watched = nullptr;
}

// Keep allocation tracking isolated from the other unit-test executables.
void *operator new(std::size_t size)
{
    if (void *pointer = std::malloc(size ? size : 1)) return pointer;
    throw std::bad_alloc();
}

void operator delete(void *pointer) noexcept
{
    if (pointer == watched) watched = nullptr;
    std::free(pointer);
}

void operator delete(void *pointer, std::size_t) noexcept
{
    ::operator delete(pointer);
}

using namespace LCompilers::LanguageServerProtocol;

template<typename T>
bool releases_previous_value(ParameterInformation_label &destination,
    const ParameterInformation_label &source, const T &previous)
{
    watched = const_cast<T *>(&previous);
    destination = source;
    const bool released = watched == nullptr;
    if (!released) {
        watched = nullptr;
        delete &previous;
        std::fputs("copy assignment leaked the previous label value\n", stderr);
    }
    return released;
}

int main()
{
    using Pair = std::pair<uinteger_t, uinteger_t>;
    const Pair pair{3, 7};
    ParameterInformation_label source, destination;
    source = Pair(pair);
    destination = Pair{11, 13};
    if (!releases_previous_value(destination, source,
            destination.pairOfUIntegerAndUInteger())) return 1;
    if (destination.pairOfUIntegerAndUInteger() != pair
            || &destination.pairOfUIntegerAndUInteger()
                == &source.pairOfUIntegerAndUInteger()) return 2;

    watched = const_cast<Pair *>(&destination.pairOfUIntegerAndUInteger());
    const auto &self = destination;
    destination = self;
    if (watched != &destination.pairOfUIntegerAndUInteger()
            || destination.pairOfUIntegerAndUInteger() != pair) return 3;
    watched = nullptr;

    destination = string_t("previous string");
    if (!releases_previous_value(destination, source, destination.string())) return 4;
    if (destination.pairOfUIntegerAndUInteger() != pair) return 5;

    source = string_t("source string");
    if (!releases_previous_value(destination, source,
            destination.pairOfUIntegerAndUInteger())) return 6;
    if (destination.string() != source.string()
            || &destination.string() == &source.string()) return 7;

    ParameterInformation_label empty;
    if (!releases_previous_value(destination, empty, destination.string())) return 8;
    if (destination.type() != ParameterInformation_labelType::Uninitialized) return 9;

    LSPAny recursive;
    LSPArray inner, outer;
    auto leaf = std::make_unique<LSPAny>();
    *leaf = integer_t(42);
    inner.push_back(std::move(leaf));
    auto child = std::make_unique<LSPAny>();
    *child = std::move(inner);
    outer.push_back(std::move(child));
    recursive = std::move(outer);
    const auto &recursive_self = recursive;
    const auto *owned_child = recursive.array()[0].get();
    recursive = recursive_self;
    if (recursive.array()[0].get() != owned_child
            || recursive.array()[0]->array()[0]->integer() != 42) return 10;

    recursive = *recursive.array()[0];
    if (recursive.type() != LSPAnyType::Array
            || recursive.array().size() != 1
            || recursive.array()[0]->integer() != 42) return 11;
    recursive = *recursive.array()[0];
    if (recursive.type() != LSPAnyType::Integer
            || recursive.integer() != 42) return 12;
    return 0;
}
