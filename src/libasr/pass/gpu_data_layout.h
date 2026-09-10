#ifndef LIBASR_PASS_GPU_DATA_LAYOUT_H
#define LIBASR_PASS_GPU_DATA_LAYOUT_H

#include <libasr/asr.h>
#include <string>
#include <vector>

namespace LCompilers {

// How one component of a derived-type kernel argument reaches the device.
//
// A kernel carries its own copy of every derived-type definition it uses,
// so the component the kernel layout names and the component a launch site
// has to read out of the caller's variable are two `Variable_t` of one
// name in two copies of one type. This pairs them up, and is the single
// answer to what the component's elements are, how many dimensions they
// are indexed by, and which kernel arguments carry the component's data,
// its per-element offsets and its per-element extents.
struct GpuComponentLayout {
    // The component in the definition asked for -- the launch's copy when
    // one was named, the kernel's own otherwise.
    ASR::symbol_t *component = nullptr;
    // Its type with the allocatable wrapper taken off, and the type of one
    // element of it.
    ASR::ttype_t *type = nullptr;
    ASR::ttype_t *element_type = nullptr;
    // The definition of the element type, when the elements are themselves
    // of a derived type.
    ASR::Struct_t *element_struct = nullptr;
    size_t rank = 0;
    bool allocatable = false;
    // A derived type with no data members occupies no bytes on the host and
    // one byte in the device languages, so its elements are sized to stay
    // addressable and never copied.
    bool element_is_empty = false;
    // The kernel arguments backing it: the elements' data laid end to end,
    // where each element's component starts, and how far each one runs.
    const ASR::gpu_kernel_argument_t *data = nullptr;
    const ASR::gpu_kernel_argument_t *offsets = nullptr;
    const ASR::gpu_kernel_argument_t *sizes = nullptr;

    std::string name() const;
};

// The components of `definition` that a derived-type kernel argument's
// device layout hands over as device buffers of their own, in the order
// they are laid out: the components it inherits first, then its own.
//
// This is where the GPU path decides which components are decomposed.
// Widening it -- to an allocatable scalar component, a pointer component,
// or an allocatable component of an allocatable component -- is a change
// here and in what the launch and the emitters make of a descriptor,
// rather than in each place that walks a derived type of its own accord.
std::vector<GpuComponentLayout> gpu_decomposed_components(
    ASR::Struct_t *definition);

// The components of `variable`'s derived type that `layout` hands over as
// device buffers of their own, in the order the layout lists them.
//
// `in_struct`, when given, is the definition the components are taken
// from: a launch site holds the caller's copy of the type, not the
// kernel's, and reads the component out of that one.
std::vector<GpuComponentLayout> gpu_component_layouts(
    const ASR::gpu_kernel_layout_t &layout, ASR::symbol_t *variable,
    ASR::Struct_t *in_struct = nullptr);

ASR::Struct_t* gpu_struct_definition(ASR::symbol_t *symbol);
void gpu_collect_data_members(ASR::Struct_t *type,
    std::vector<ASR::symbol_t*> &members);
bool gpu_struct_has_allocatable_parts(ASR::symbol_t *symbol);
ASR::ttype_t* gpu_size_of_type_argument(Allocator &al, ASR::expr_t *value,
    ASR::ttype_t *type);

} // namespace LCompilers

#endif
