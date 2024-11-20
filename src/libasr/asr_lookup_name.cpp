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
            void visit_FunctionCall(const ASR::FunctionCall_t &x) {
                for (size_t i=0; i<x.n_args; i++) {
                    this->visit_call_arg(x.m_args[i]);
                }
                this->visit_ttype(*x.m_type);
                if (x.m_value)
                    this->visit_expr(*x.m_value);
                if (x.m_dt)
                    this->visit_expr(*x.m_dt);
                if (test_loc_and_set_span(x.base.base.loc)) {
                    const ASR::symbol_t* sym = this->symbol_get_past_external_(x.m_name);
                    this->handle_symbol(sym);
                    if ( ASR::is_a<ASR::ClassProcedure_t>(*sym) ) {
                        this->handle_symbol(ASR::down_cast<ASR::ClassProcedure_t>(sym)->m_proc);
                    }
                }
            }
    };
}
