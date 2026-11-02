// Copying a generated LSP union must construct the copied alternative in the
// union storage rather than assign to a std::unique_ptr that is not alive.
#include <server/lsp_specification.h>

#include <cstring>
#include <iostream>
#include <new>
#include <string>
#include <vector>

using namespace LCompilers::LanguageServerProtocol;

using Section = DidChangeConfigurationRegistrationOptions_section;

namespace {

int failures = 0;

void check(bool condition, const char *what)
{
    if (!condition) {
        std::cerr << "FAILED: " << what << std::endl;
        ++failures;
    }
}

Section make_source()
{
    Section source;
    source = std::vector<string_t>{"fortran", "lfortran"};
    return source;
}

void check_copy(const Section &copy, const Section &source)
{
    check(copy.type() == DidChangeConfigurationRegistrationOptions_sectionType::StringArray,
        "copy holds the array alternative");
    check(copy.stringArray() == std::vector<string_t>{"fortran", "lfortran"},
        "copy holds the array elements");
    check(&copy.stringArray() != &source.stringArray(), "copy owns its array");
}

void test_copy_constructor()
{
    const Section source = make_source();
    alignas(Section) unsigned char storage[sizeof(Section)];
    // The copy constructor must not read the previous contents of its storage.
    std::memset(storage, 0xa5, sizeof(storage));
    auto *copy = new (storage) Section(source);
    check_copy(*copy, source);
    copy->~Section();
}

void test_copy_assignment()
{
    const Section source = make_source();
    alignas(Section) unsigned char storage[sizeof(Section)];
    std::memset(storage, 0xa5, sizeof(storage));
    // Default construction leaves the inactive union storage untouched.
    auto *copy = new (storage) Section();
    *copy = source;
    check_copy(*copy, source);
    copy->~Section();
}

}

int main()
{
    test_copy_constructor();
    test_copy_assignment();
    if (failures != 0) {
        return 1;
    }
    std::cout << "test_lsp_union_copy: OK" << std::endl;
    return 0;
}
