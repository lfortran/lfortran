#ifndef LIBASR_CODEGEN_GPU_SYMBOL_MAP_H
#define LIBASR_CODEGEN_GPU_SYMBOL_MAP_H

#include <libasr/asr_utils.h>
#include <libasr/pass/gpu_data_layout.h>
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

// Shared plumbing for the maps that answer with an entry of the kernel
// layout. The dummy is always identified by its symbol -- it is the same
// symbol on both sides, so a same-named variable in another scope cannot
// answer for it -- and a caller holding only a name has it resolved
// through the scope being emitted.
class GpuArgumentLookup {
protected:
    SymbolTable **scope;

    explicit GpuArgumentLookup(SymbolTable **scope) : scope(scope) {}
    static ASR::symbol_t* canonical(ASR::symbol_t *symbol) {
        return symbol ? ASRUtils::symbol_get_past_external(symbol) : nullptr;
    }
    ASR::symbol_t* resolve(const std::string &name) const {
        return *scope ? canonical((*scope)->resolve_symbol(name)) : nullptr;
    }
};

// How the kernel layout hands over the components of a struct-typed dummy
// that get device buffers of their own, found by the dummy and the
// component rather than by the name the signature spells the two into and
// matches back on.
//
// Both halves are symbols: the offload pass gives the kernel its own copy
// of every derived-type definition it needs, and a body that reached a
// component through the host's copy would describe the dummy's component
// through a `Variable_t` the dummy's `m_type_declaration` cannot reach.
//
// One description answers for the component's data buffer, its per-element
// offsets and its per-element extents, and for what its elements are and
// how many dimensions index them, so the emitter cannot end up asking for
// those separately and getting answers that disagree.
class GpuComponentMap : public GpuArgumentLookup {
    std::map<std::pair<ASR::symbol_t*, ASR::symbol_t*>,
        GpuComponentLayout> values;

    const GpuComponentLayout* lookup(ASR::symbol_t *variable,
            ASR::symbol_t *member) const {
        if (!variable || !member) return nullptr;
        auto it = values.find({variable, canonical(member)});
        return it == values.end() ? nullptr : &it->second;
    }
public:
    explicit GpuComponentMap(SymbolTable **scope)
        : GpuArgumentLookup(scope) {}
    void add(const ASR::gpu_kernel_layout_t &layout,
            ASR::symbol_t *variable) {
        for (const GpuComponentLayout &component :
                gpu_component_layouts(layout, variable)) {
            values[{canonical(variable),
                canonical(component.component)}] = component;
        }
    }
    const GpuComponentLayout* find(ASR::symbol_t *variable,
            ASR::symbol_t *member) const {
        return lookup(canonical(variable), member);
    }
    const GpuComponentLayout* find(const std::string &variable,
            ASR::symbol_t *member) const {
        return lookup(resolve(variable), member);
    }
    void clear() { values.clear(); }
};

// The kernel arguments that carry one dimension's extent of an array
// dummy whose type states no extent of its own, found by the dummy and
// the 0-based dimension.
class GpuExtentArgumentMap : public GpuArgumentLookup {
    std::map<std::pair<ASR::symbol_t*, size_t>,
        const ASR::gpu_kernel_argument_t*> values;

    const ASR::gpu_kernel_argument_t* lookup(ASR::symbol_t *variable,
            size_t dimension) const {
        if (!variable) return nullptr;
        auto it = values.find({variable, dimension});
        return it == values.end() ? nullptr : it->second;
    }
public:
    explicit GpuExtentArgumentMap(SymbolTable **scope)
        : GpuArgumentLookup(scope) {}
    void add(const ASR::gpu_kernel_argument_t *argument) {
        LCOMPILERS_ASSERT(argument->m_variable && argument->m_dimension >= 0);
        values[{canonical(argument->m_variable),
            (size_t)argument->m_dimension}] = argument;
    }
    const ASR::gpu_kernel_argument_t* find(ASR::symbol_t *variable,
            size_t dimension) const {
        return lookup(canonical(variable), dimension);
    }
    const ASR::gpu_kernel_argument_t* find(const std::string &variable,
            size_t dimension) const {
        return lookup(resolve(variable), dimension);
    }
    void clear() { values.clear(); }
};

} // namespace LCompilers

#endif
