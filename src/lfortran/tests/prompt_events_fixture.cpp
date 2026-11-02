#include <cstdio>
#include <cpp-terminal/prompt0.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/Unicode.h>

int main()
{
    std::string text = "\x1b[0m";
    for (int i = 0; i < 40; i++) text += "\xe7\x95\x8c";
    text += std::string(40, 'a') + "Z\n" + std::string(79, 'x')
        + "e\xcc\x81\n";
    size_t next = 0;
    int ticks = 0;
    const PromptEvents events = [&](const PromptOutput &display) {
        if (++ticks < 5 || next == text.size()) return PromptEvent::none;
        // One byte per editor tick splits both UTF-8 units and SGR, unlike
        // valid JSON messages, whose strings already contain complete UTF-8.
        display(text.substr(next++, 1), false);
        return PromptEvent::updates;
    };
    const std::function<int(const std::string &)> width =
        [](const std::string &character) {
            return llvm::sys::unicode::columnWidthUTF8(character);
        };
    std::function<bool(std::string)> complete = [](std::string) { return true; };
    std::vector<std::string> history;
    try {
        Terminal term(true);
        prompt0(term, ">>> ", history, complete, width, events);
    } catch (const std::runtime_error &error) {
        std::cerr << "error: " << error.what() << "\n";
        return 1;
    }
}
