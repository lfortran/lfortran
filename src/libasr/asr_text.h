#ifndef LIBASR_ASR_TEXT_H
#define LIBASR_ASR_TEXT_H

#include <string>

#include <libasr/asr.h>
#include <libasr/diagnostics.h>
#include <libasr/exception.h>
#include <libasr/location.h>

namespace LCompilers {

enum class ASRTextForm {
    Named,
    Positional
};

struct ASRTextOptions {
    ASRTextForm form = ASRTextForm::Named;
    bool indent = true;
};

std::string asr_to_text(const ASR::asr_t &asr,
    const ASRTextOptions &options = {});

std::string asr_to_text(const ASR::TranslationUnit_t &asr,
    const ASRTextOptions &options = {});

Result<ASR::TranslationUnit_t *> asr_from_text(
    Allocator &al, const std::string &text, const std::string &filename,
    LocationManager &lm, diag::Diagnostics &diagnostics);

} // namespace LCompilers

#endif // LIBASR_ASR_TEXT_H
