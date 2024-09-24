#include <iostream>
#include <stdint.h>
#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/asr_builder.h>

namespace LCompilers::LFortran {
    class LookupNameVisitor : public ASR::BaseWalkVisitor<LookupNameVisitor> {
        public:
            uint16_t pos;
            uint32_t min_span = UINT32_MAX;
            ASR::asr_t* node_to_return = nullptr;

            LookupNameVisitor(uint16_t pos) : pos(pos) {}

            bool test_loc_and_set_span(Location loc) {
                uint32_t first = loc.first;
                uint32_t last = loc.last;
                if (first <= pos && pos <= last) {
                    uint32_t span = last - first;
                    if (span < min_span) {
                        min_span = span;
                        return true;
                    }
                }
                return false;
            }

            void visit_Variable(const ASR::Variable_t& x) {
                if (test_loc_and_set_span(x.base.base.loc)) {
                    node_to_return = (ASR::asr_t*) &x;
                }
            }
    };
}
