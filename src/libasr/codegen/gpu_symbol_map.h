#ifndef LIBASR_CODEGEN_GPU_SYMBOL_MAP_H
#define LIBASR_CODEGEN_GPU_SYMBOL_MAP_H

#include <libasr/asr_utils.h>
#include <map>

namespace LCompilers {

class GpuEmissionScope {
    SymbolTable *&current;
    SymbolTable *previous;
public:
    GpuEmissionScope(SymbolTable *&current, SymbolTable *scope)
        : current(current), previous(current) { current = scope; }
    ~GpuEmissionScope() { current = previous; }
};

template <typename T>
class GpuSymbolMap {
    SymbolTable **scope;
    std::map<ASR::symbol_t*, T> values;

    ASR::symbol_t* resolve(const std::string &name) const {
        return *scope ? ASRUtils::symbol_get_past_external(
            (*scope)->resolve_symbol(name)) : nullptr;
    }
public:
    explicit GpuSymbolMap(SymbolTable **scope) : scope(scope) {}
    auto find(const std::string &name) { return values.find(resolve(name)); }
    auto find(ASR::symbol_t *symbol) {
        return values.find(ASRUtils::symbol_get_past_external(symbol));
    }
    auto end() { return values.end(); }
    size_t count(const std::string &name) const {
        return values.count(resolve(name));
    }
    T &operator[](const std::string &name) {
        ASR::symbol_t *symbol = resolve(name);
        LCOMPILERS_ASSERT(symbol != nullptr);
        return values[symbol];
    }
    T &operator[](ASR::symbol_t *symbol) {
        LCOMPILERS_ASSERT(symbol != nullptr);
        return values[ASRUtils::symbol_get_past_external(symbol)];
    }
    void clear() { values.clear(); }
};

} // namespace LCompilers

#endif
