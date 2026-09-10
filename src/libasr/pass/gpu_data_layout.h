#ifndef LIBASR_PASS_GPU_DATA_LAYOUT_H
#define LIBASR_PASS_GPU_DATA_LAYOUT_H

#include <libasr/asr.h>
#include <vector>

namespace LCompilers {

ASR::Struct_t* gpu_struct_definition(ASR::symbol_t *symbol);
void gpu_collect_data_members(ASR::Struct_t *type,
    std::vector<ASR::symbol_t*> &members);
bool gpu_struct_has_allocatable_parts(ASR::symbol_t *symbol);
ASR::ttype_t* gpu_size_of_type_argument(Allocator &al, ASR::expr_t *value,
    ASR::ttype_t *type);

} // namespace LCompilers

#endif
