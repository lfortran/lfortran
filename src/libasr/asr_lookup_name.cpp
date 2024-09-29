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
            void handle_symbol(ASR::symbol_t* sym) {
                switch(sym->type) {
                    case ASR::symbolType::Program: {
                        node_to_return = ( LCompilers::ASR::asr_t* ) ((ASR::Program_t*)sym);
                        return;
                    }
                    case ASR::symbolType::Module: {
                        node_to_return = ( LCompilers::ASR::asr_t* ) ((ASR::Module_t*)sym);
                        return;
                    }
                    case ASR::symbolType::Function: {
                        node_to_return = ( LCompilers::ASR::asr_t* ) ((ASR::Function_t*)sym);
                        return;
                    }
                    case ASR::symbolType::GenericProcedure: {
                        node_to_return = ( LCompilers::ASR::asr_t* ) ((ASR::GenericProcedure_t*)sym);
                        return;
                    }
                    case ASR::symbolType::CustomOperator: {
                        node_to_return = ( LCompilers::ASR::asr_t* ) ((ASR::CustomOperator_t*)sym);
                        return;
                    }
                    case ASR::symbolType::ExternalSymbol: {
                        node_to_return = ( LCompilers::ASR::asr_t* ) ((ASR::ExternalSymbol_t*)sym);
                        return;
                    }
                    case ASR::symbolType::Struct: {
                        node_to_return = ( LCompilers::ASR::asr_t* ) ((ASR::Struct_t*)sym);
                        return;
                    }
                    case ASR::symbolType::Enum: {
                        node_to_return = ( LCompilers::ASR::asr_t* ) ((ASR::Enum_t*)sym);
                        return;
                    }
                    case ASR::symbolType::UnionType: {
                        node_to_return = ( LCompilers::ASR::asr_t* ) ((ASR::UnionType_t*)sym);
                        return;
                    }
                    case ASR::symbolType::Variable: {
                        node_to_return = ( LCompilers::ASR::asr_t* ) ((ASR::Variable_t*)sym);
                        return;
                    }
                    case ASR::symbolType::Class: {
                        node_to_return = ( LCompilers::ASR::asr_t* ) ((ASR::Class_t*)sym);
                        return;
                    }
                    case ASR::symbolType::ClassProcedure: {
                        node_to_return = ( LCompilers::ASR::asr_t* ) ((ASR::ClassProcedure_t*)sym);
                        return;
                    }
                    case ASR::symbolType::AssociateBlock: {
                        node_to_return = ( LCompilers::ASR::asr_t* ) ((ASR::AssociateBlock_t*)sym);
                        return;
                    }
                    case ASR::symbolType::Block: {
                        node_to_return = ( LCompilers::ASR::asr_t* ) ((ASR::Block_t*)sym);
                        return;
                    }
                    case ASR::symbolType::Requirement: {
                        node_to_return = ( LCompilers::ASR::asr_t* ) ((ASR::Requirement_t*)sym);
                        return;
                    }
                    case ASR::symbolType::Template: {
                        node_to_return = ( LCompilers::ASR::asr_t* ) ((ASR::Template_t*)sym);
                        return;
                    }
                }
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
                    this->handle_symbol(ASRUtils::symbol_get_past_external(x.m_name));
                }
            }
            void visit_Var(const ASR::Var_t &x) {
                if ((bool&)x) { } // Suppress unused warning
                if (test_loc_and_set_span(x.base.base.loc)) {
                    this->handle_symbol(ASRUtils::symbol_get_past_external(x.m_v));
                }
            }
    };
}
