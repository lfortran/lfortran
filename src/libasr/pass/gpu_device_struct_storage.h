#ifndef LIBASR_PASS_GPU_DEVICE_STRUCT_STORAGE_H
#define LIBASR_PASS_GPU_DEVICE_STRUCT_STORAGE_H

#include <libasr/asr.h>
#include <libasr/utils.h>

#include <vector>

namespace LCompilers {

// The routine's own scopes: the routine itself and the BLOCK and ASSOCIATE
// scopes of its body, but not the routines it contains.
void gpu_routine_scopes(SymbolTable *scope, std::vector<SymbolTable*> &out);

// Gives every derived-type local of device code the storage of its
// allocatable array components, as arrays of the shape the routine gives
// them (see gpu_device_struct_storage.cpp). Returns whether it gave any.
bool gpu_give_struct_component_storage(Allocator &al,
    ASR::TranslationUnit_t &unit);

// Gives an allocatable rank-one local of a device routine that the
// routine allocates with an extent it only knows where it allocates it --
// one that reads the index of a loop -- a buffer whose extent is an upper
// bound of every such extent known on entry to the routine, and makes the
// local a pointer to the leading part of the buffer each allocation asks
// for. Returns whether it gave any.
bool gpu_bind_allocations_to_buffers(Allocator &al,
    ASR::TranslationUnit_t &unit);

// Whether `stmt` binds the allocatable array component of a derived-type
// local of device code to the array that stores it: `s%v => storage`, as
// gpu_give_struct_component_storage writes it. `component` is `s%v`;
// `storage` is the bare array variable, past any physical casts.
bool gpu_struct_component_storage(ASR::stmt_t *stmt,
    ASR::StructInstanceMember_t *&component, ASR::expr_t *&storage);

// Writes the dimensions `dims` of an array of the device routine `fn` as
// the values they have on entry to the routine, over its dummy arguments
// and named constants alone: a local the routine binds once stands for
// what it is bound to, and the shape of a local array of an explicit shape,
// of a section or of a component stored in an array of its own for its
// extents. Leaves `dims` alone and returns false when that cannot be done.
bool gpu_write_extents_at_entry(Allocator &al, ASR::Function_t &fn,
    ASR::dimension_t *dims, size_t n_dims);

// The derived-type local of `fn` (of one of its BLOCK or ASSOCIATE scopes)
// whose allocatable array component `component` has no storage, or nullptr
// when every such local that the routine references has it.
ASR::Variable_t* gpu_struct_local_without_storage(ASR::Function_t &fn,
    std::string &component);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_DEVICE_STRUCT_STORAGE_H
