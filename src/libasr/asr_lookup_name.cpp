#include <iostream>
#include <stdint.h>
#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/asr_builder.h>

namespace LCompilers::LFortran {
    class LookupNameVisitor : public ASR::DefaultLookupNameVisitor<LookupNameVisitor> {
        public:
            LookupNameVisitor(uint16_t pos) {
                this->pos = pos;
            }
    };
}
