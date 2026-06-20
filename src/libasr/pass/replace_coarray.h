#ifndef LCOMPILERS_PASS_REPLACE_COARRAY_PRIF_H
#define LCOMPILERS_PASS_REPLACE_COARRAY_PRIF_H

#include <libasr/asr.h>
#include <libasr/alloc.h>
#include <libasr/pass/pass_utils.h>

namespace LCompilers {

	void pass_replace_coarray(Allocator &al, ASR::TranslationUnit_t &unit,
								   const LCompilers::PassOptions &pass_options);

} // namespace LCompilers

#endif
