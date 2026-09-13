#include <set>

#include <libasr/asr_utils.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/pass/gpu_offload_collect.h>
#include <libasr/pass/gpu_unsupported_check.h>

namespace LCompilers {

namespace {

// The first data component, at any depth and through `extends`, of the
// derived type `struct_sym` whose type is a real the device has no floating
// point type for.
ASR::Variable_t* wide_real_component(ASR::symbol_t *struct_sym,
        const GpuDeviceCapabilities &caps,
        std::set<ASR::Struct_t*> &visited) {
    if (struct_sym == nullptr) return nullptr;
    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(struct_sym);
    if (s == nullptr || !ASR::is_a<ASR::Struct_t>(*s)) return nullptr;
    ASR::Struct_t *st = ASR::down_cast<ASR::Struct_t>(s);
    // A type that refers to itself, through a pointer component, has its
    // components looked at once.
    if (!visited.insert(st).second) return nullptr;
    if (st->m_parent != nullptr) {
        ASR::Variable_t *v = wide_real_component(st->m_parent, caps, visited);
        if (v != nullptr) return v;
    }
    for (size_t i = 0; i < st->n_members; i++) {
        ASR::symbol_t *m = st->m_symtab->get_symbol(st->m_members[i]);
        if (m == nullptr) continue;
        m = ASRUtils::symbol_get_past_external(m);
        if (m == nullptr || !ASR::is_a<ASR::Variable_t>(*m)) continue;
        ASR::Variable_t *member = ASR::down_cast<ASR::Variable_t>(m);
        ASR::ttype_t *t = ASRUtils::extract_type(member->m_type);
        if (ASR::is_a<ASR::StructType_t>(*t)) {
            ASR::Variable_t *v = wide_real_component(
                member->m_type_declaration, caps, visited);
            if (v != nullptr) return v;
        } else if (caps.lacks_real_width(t)) {
            return member;
        }
    }
    return nullptr;
}

// The derived type of a derived-type expression, for the expressions that
// can denote a derived-type value; nullptr when it is not known.
ASR::symbol_t* struct_of(const ASR::expr_t &e) {
    switch (e.type) {
        case ASR::exprType::Var: {
            ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(&e)->m_v);
            if (s == nullptr || !ASR::is_a<ASR::Variable_t>(*s)) {
                return nullptr;
            }
            return ASR::down_cast<ASR::Variable_t>(s)->m_type_declaration;
        }
        case ASR::exprType::StructInstanceMember:
        case ASR::exprType::ArrayItem:
        case ASR::exprType::ArraySection:
        case ASR::exprType::FunctionCall:
        case ASR::exprType::StructConstant:
            return ASRUtils::get_struct_sym_from_struct_expr(
                const_cast<ASR::expr_t*>(&e));
        default:
            return nullptr;
    }
}

// The name a user wrote for the variable or component `e` denotes, or an
// empty string for any other expression.
std::string name_of(const ASR::expr_t &e) {
    switch (e.type) {
        case ASR::exprType::Var:
            return ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(&e)->m_v);
        case ASR::exprType::StructInstanceMember:
            return ASRUtils::symbol_name(ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::StructInstanceMember_t>(&e)->m_m));
        case ASR::exprType::ArrayItem:
            return name_of(*ASR::down_cast<ASR::ArrayItem_t>(&e)->m_v);
        case ASR::exprType::ArraySection:
            return name_of(*ASR::down_cast<ASR::ArraySection_t>(&e)->m_v);
        default:
            return "";
    }
}

class UnsupportedConstructFinder :
        public ASRUtils::BlockBodyWalkVisitor<UnsupportedConstructFinder> {
public:
    const GpuDeviceCapabilities &caps;
    GpuUnsupportedConstruct found;
    // The procedure being walked, empty while walking the loop itself.
    std::string routine;
    // Set just before visiting the object of a component reference, so that
    // the object is not judged as a whole: only the component reaches the
    // device.
    bool object_of_component = false;

    explicit UnsupportedConstructFinder(const GpuDeviceCapabilities &caps_)
            : caps(caps_) {
        // A compile-time value is not evaluated on the device.
        visit_compile_time_value = false;
    }

    void record(GpuUnsupportedKind kind, const Location &loc) {
        found.kind = kind;
        found.loc = loc;
        found.routine = routine;
    }

    void record_real(const Location &loc, ASR::ttype_t *type,
            const std::string &name, const std::string &component,
            bool declaration) {
        record(GpuUnsupportedKind::RealWidth, loc);
        found.type = type;
        found.name = name;
        found.component = component;
        found.declaration = declaration;
    }

    // A variable declared in a procedure the loop reaches, or in a BLOCK or
    // ASSOCIATE of the loop, is storage on the device, whole.
    void check_declaration(const ASR::Variable_t &var) {
        if (found.found()) return;
        // A named constant has no storage; its uses are expressions, and
        // those are judged where they are evaluated.
        if (var.m_storage == ASR::storage_typeType::Parameter) return;
        ASR::ttype_t *t = ASRUtils::extract_type(var.m_type);
        if (ASR::is_a<ASR::StructType_t>(*t)) {
            std::set<ASR::Struct_t*> visited;
            ASR::Variable_t *member = wide_real_component(
                var.m_type_declaration, caps, visited);
            if (member != nullptr) {
                record_real(var.base.base.loc,
                    ASRUtils::extract_type(member->m_type), var.m_name,
                    member->m_name, true);
            }
        } else if (caps.lacks_real_width(t)) {
            record_real(var.base.base.loc, t, var.m_name, "", true);
        }
    }

    void check_scope(SymbolTable *symtab) {
        if (symtab == nullptr) return;
        for (auto &item : symtab->get_scope()) {
            if (found.found()) return;
            if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                check_declaration(
                    *ASR::down_cast<ASR::Variable_t>(item.second));
            }
        }
    }

    void visit_stmt(const ASR::stmt_t &s) {
        if (found.found()) return;
        switch (s.type) {
            case ASR::stmtType::Print:
            case ASR::stmtType::FileWrite:
            case ASR::stmtType::FileRead:
            case ASR::stmtType::FileOpen:
            case ASR::stmtType::FileClose:
            case ASR::stmtType::FileInquire:
            case ASR::stmtType::FileBackspace:
            case ASR::stmtType::FileRewind:
            case ASR::stmtType::FileEndfile:
            case ASR::stmtType::Flush:
                record(GpuUnsupportedKind::InputOutput, s.base.loc);
                return;
            case ASR::stmtType::ErrorStop:
                if (!caps.device_abort) {
                    record(GpuUnsupportedKind::ErrorStop, s.base.loc);
                    return;
                }
                break;
            case ASR::stmtType::Stop:
                if (!caps.device_abort) {
                    record(GpuUnsupportedKind::Stop, s.base.loc);
                    return;
                }
                break;
            default:
                break;
        }
        ASR::BaseWalkVisitor<UnsupportedConstructFinder>::visit_stmt(s);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_m);
        if (s == nullptr || !ASR::is_a<ASR::Block_t>(*s)) return;
        check_scope(ASR::down_cast<ASR::Block_t>(s)->m_symtab);
        ASRUtils::BlockBodyWalkVisitor<UnsupportedConstructFinder>
            ::visit_BlockCall(x);
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_m);
        if (s == nullptr || !ASR::is_a<ASR::AssociateBlock_t>(*s)) return;
        check_scope(ASR::down_cast<ASR::AssociateBlock_t>(s)->m_symtab);
        ASRUtils::BlockBodyWalkVisitor<UnsupportedConstructFinder>
            ::visit_AssociateBlockCall(x);
    }

    void visit_expr(const ASR::expr_t &e) {
        if (found.found()) return;
        bool as_object = object_of_component;
        object_of_component = false;
        ASR::ttype_t *type = ASRUtils::typed_expr_type(&e);
        if (type != nullptr) {
            ASR::ttype_t *t = ASRUtils::extract_type(type);
            if (ASR::is_a<ASR::StructType_t>(*t)) {
                if (!as_object) {
                    std::set<ASR::Struct_t*> visited;
                    ASR::Variable_t *member = wide_real_component(
                        struct_of(e), caps, visited);
                    if (member != nullptr) {
                        record_real(e.base.loc,
                            ASRUtils::extract_type(member->m_type),
                            name_of(e), member->m_name, false);
                        return;
                    }
                }
            } else if (caps.lacks_real_width(t)) {
                record_real(e.base.loc, t, name_of(e), "", false);
                return;
            }
        }
        // Folded at compile time: nothing below it runs on the device.
        if (ASRUtils::expr_value(const_cast<ASR::expr_t*>(&e)) != nullptr) {
            return;
        }
        switch (e.type) {
            case ASR::exprType::StructInstanceMember: {
                const ASR::StructInstanceMember_t &x =
                    *ASR::down_cast<ASR::StructInstanceMember_t>(&e);
                object_of_component = true;
                visit_expr(*x.m_v);
                return;
            }
            case ASR::exprType::ArrayItem: {
                if (!as_object) break;
                const ASR::ArrayItem_t &x =
                    *ASR::down_cast<ASR::ArrayItem_t>(&e);
                object_of_component = true;
                visit_expr(*x.m_v);
                for (size_t i = 0; i < x.n_args; i++) {
                    visit_array_index(x.m_args[i]);
                }
                return;
            }
            case ASR::exprType::ArraySection: {
                if (!as_object) break;
                const ASR::ArraySection_t &x =
                    *ASR::down_cast<ASR::ArraySection_t>(&e);
                object_of_component = true;
                visit_expr(*x.m_v);
                for (size_t i = 0; i < x.n_args; i++) {
                    visit_array_index(x.m_args[i]);
                }
                return;
            }
            default:
                break;
        }
        ASR::BaseWalkVisitor<UnsupportedConstructFinder>::visit_expr(e);
    }
};

} // namespace

GpuUnsupportedConstruct gpu_find_unsupported_construct(
        const ASR::OMPRegion_t &region, const GpuDeviceCapabilities &caps) {
    UnsupportedConstructFinder finder(caps);
    for (size_t i = 0; i < region.n_body; i++) {
        finder.visit_stmt(*region.m_body[i]);
        if (finder.found.found()) return finder.found;
    }
    // A procedure the loop calls, however deep, runs on the device too.
    for (ASR::Function_t *fn : reachable_routines(region.m_body,
            region.n_body)) {
        finder.routine = fn->m_name;
        finder.check_scope(fn->m_symtab);
        for (size_t i = 0; i < fn->n_body && !finder.found.found(); i++) {
            finder.visit_stmt(*fn->m_body[i]);
        }
        if (finder.found.found()) return finder.found;
    }
    return finder.found;
}

std::string gpu_unsupported_construct_message(
        const GpuUnsupportedConstruct &construct,
        const GpuDeviceCapabilities &caps) {
    std::string what;
    switch (construct.kind) {
        case GpuUnsupportedKind::None:
            return "";
        case GpuUnsupportedKind::RealWidth:
            what = gpu_scalar_type_name(construct.type);
            break;
        case GpuUnsupportedKind::InputOutput:
            what = "input/output statements";
            break;
        case GpuUnsupportedKind::ErrorStop:
            what = "error stop";
            break;
        case GpuUnsupportedKind::Stop:
            what = "stop";
            break;
    }
    return "the " + caps.name + " GPU does not support " + what;
}

std::string gpu_unsupported_construct_label(
        const GpuUnsupportedConstruct &construct) {
    std::string label;
    switch (construct.kind) {
        case GpuUnsupportedKind::None:
            return "";
        case GpuUnsupportedKind::RealWidth: {
            std::string type = gpu_scalar_type_name(construct.type);
            if (!construct.component.empty()) {
                label = "'" + construct.name + "' has " + type +
                    " component '" + construct.component + "'" +
                    (construct.declaration ? "" : " and is used as a whole");
            } else if (!construct.name.empty()) {
                label = "'" + construct.name + "' is " + type;
            } else {
                label = "this expression is " + type;
            }
            break;
        }
        case GpuUnsupportedKind::InputOutput:
            label = "input/output statement";
            break;
        case GpuUnsupportedKind::ErrorStop:
            label = "error stop statement";
            break;
        case GpuUnsupportedKind::Stop:
            label = "stop statement";
            break;
    }
    if (!construct.routine.empty()) {
        label += ", in '" + construct.routine +
            "', which the parallel loop calls";
    }
    return label;
}

} // namespace LCompilers
