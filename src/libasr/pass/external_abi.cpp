#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/external_abi.h>

#include <set>
#include <string>
#include <vector>

namespace LCompilers {

/*
    Finalizes `FunctionType::external_abi`, the flag that says a procedure uses
    the classic Fortran external ABI (a CHARACTER dummy arrives as a bare data
    pointer with its per-element length as a hidden trailing argument, as
    gfortran and flang do).

    The frontend sets the flag from the declaration alone: a subprogram defined
    at the top level, or an interface body in a plain (non-abstract,
    non-module, non-bind(c)) interface block. Two exclusions cannot be decided
    there, because they are properties of how the symbol is *used* rather than
    of how it was declared, and both need to look at the whole translation
    unit:

      * The procedure is a dummy procedure of its owner. Its interface body
        describes whatever procedure is bound to it, which is usually an
        ordinary module or contained procedure, so a call through it must use
        LFortran's ordinary string-descriptor ABI.
      * The procedure's address is taken somewhere in the translation unit (it
        is passed as an actual argument or assigned to a procedure pointer).
        The resulting function pointer has to match the dummy procedure or
        procedure pointer it is bound to, which uses the ordinary ABI by the
        previous rule.

    This pass applies both, so that after it the flag alone describes the ABI a
    backend must emit and no backend has to re-derive it. It runs first in the
    pipeline, on the ASR as written, before any pass synthesizes helper
    procedures.

    The address-taken exclusion is keyed by procedure name, because an
    interface body and the definition it describes are two ASR symbols for one
    linked procedure and both have to reach the same decision. Only procedures
    that still declare the external ABI are keys, so an unrelated module
    procedure, contained procedure or dummy procedure that happens to share a
    name cannot flip an external procedure's ABI.

    NOTE: the address-taken exclusion is necessarily decided per translation
    unit, so an external procedure whose address is taken in one unit and
    called directly from another still disagrees with itself across that
    boundary. Making that sound needs either a single uniform ABI for all
    Fortran procedures or an ABI-adapting thunk where the address is taken.
*/

namespace {

// True when `fn` is a dummy procedure of the subprogram that owns it.
bool is_dummy_procedure_of_owner(ASR::Function_t* fn) {
    ASR::symbol_t* fn_sym = &fn->base;
    ASR::symbol_t* owner = ASRUtils::get_asr_owner(fn_sym);
    if (owner == nullptr || !ASR::is_a<ASR::Function_t>(*owner)) {
        return false;
    }
    ASR::Function_t* owner_fn = ASR::down_cast<ASR::Function_t>(owner);
    for (size_t i = 0; i < owner_fn->n_args; i++) {
        if (ASR::is_a<ASR::Var_t>(*owner_fn->m_args[i]) &&
                ASR::down_cast<ASR::Var_t>(owner_fn->m_args[i])->m_v == fn_sym) {
            return true;
        }
    }
    return false;
}

// Collects every procedure that still declares the external ABI, clearing the
// flag on the dummy procedures among them on the way.
class ExternalABICandidateCollector :
        public ASR::BaseWalkVisitor<ExternalABICandidateCollector> {
public:
    std::vector<ASR::Function_t*>& candidates;

    ExternalABICandidateCollector(std::vector<ASR::Function_t*>& candidates_)
        : candidates(candidates_) {}

    void visit_Function(const ASR::Function_t& x) {
        ASR::Function_t* fn = const_cast<ASR::Function_t*>(&x);
        ASR::FunctionType_t* ft = ASRUtils::get_FunctionType(fn);
        if (ft->m_external_abi) {
            if (is_dummy_procedure_of_owner(fn)) {
                ft->m_external_abi = false;
            } else {
                candidates.push_back(fn);
            }
        }
        ASR::BaseWalkVisitor<ExternalABICandidateCollector>::visit_Function(x);
    }
};

// Collects the names of the procedures whose address is taken. A Function
// referenced through an ASR `Var` is being used as a value rather than called
// (a direct call names the symbol in FunctionCall/SubroutineCall instead), so
// this walk finds exactly those uses. Dummy procedures are already excluded,
// because the collector above cleared their flag first.
class AddressTakenCollector :
        public ASR::BaseWalkVisitor<AddressTakenCollector> {
public:
    std::set<std::string>& names;

    AddressTakenCollector(std::set<std::string>& names_) : names(names_) {}

    void visit_Var(const ASR::Var_t& x) {
        ASR::symbol_t* s = ASRUtils::symbol_get_past_external(x.m_v);
        if (s == nullptr || !ASR::is_a<ASR::Function_t>(*s)) {
            return;
        }
        ASR::Function_t* fn = ASR::down_cast<ASR::Function_t>(s);
        if (ASRUtils::get_FunctionType(fn)->m_external_abi) {
            names.insert(std::string(ASRUtils::symbol_name(s)));
        }
    }
};

} // anonymous namespace

void pass_finalize_external_abi(Allocator& /*al*/, ASR::TranslationUnit_t& unit,
        const PassOptions& /*pass_options*/) {
    std::vector<ASR::Function_t*> candidates;
    ExternalABICandidateCollector candidate_collector(candidates);
    candidate_collector.visit_TranslationUnit(unit);
    if (candidates.empty()) {
        return;
    }

    std::set<std::string> address_taken;
    AddressTakenCollector address_taken_collector(address_taken);
    address_taken_collector.visit_TranslationUnit(unit);
    if (address_taken.empty()) {
        return;
    }

    for (ASR::Function_t* fn : candidates) {
        if (address_taken.find(std::string(fn->m_name)) != address_taken.end()) {
            ASRUtils::get_FunctionType(fn)->m_external_abi = false;
        }
    }
}

} // namespace LCompilers
