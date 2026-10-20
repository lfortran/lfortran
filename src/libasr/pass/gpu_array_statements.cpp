#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/device_launch_expand.h>
#include <libasr/pass/gpu_array_statements.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_procedure_implementation.h>
#include <libasr/pass/gpu_unsupported_check.h>
#include <libasr/pass/intrinsic_array_function_registry.h>
#include <libasr/pass/pass_utils.h>

#include <set>
#include <string>
#include <vector>

namespace LCompilers {

/*
Whole-array assignments between kernel launches run on the device.

A whole-array assignment (`dcdw = 0.`, `c = a + 2*b`, `dcdb = sum(r, dim=3)`)
is a loop over the elements of its target whose iterations are independent,
as those of a `do concurrent` are. In a host loop that launches kernels, such
an assignment that works on the data the kernels use would make the host
wait for the device and copy that data back, every iteration, only to hand it
to the next launch again (gpu_launch_sync.cpp). So, after the
`parallel_dispatch` pass has sent the parallel loops to the device, each such
assignment becomes a parallel loop over its target, sent to the device too:

    dcdb = sum(r, dim=3)
  becomes
    do concurrent (i1 = 1:size(dcdb, 1), i2 = 1:size(dcdb, 2))
        dcdb(lbound(dcdb, 1) + i1 - 1, lbound(dcdb, 2) + i2 - 1) = &
            sum(r(lbound(r, 1) + i1 - 1, lbound(r, 2) + i2 - 1, :))
    end do

An assignment is turned into such a loop when all of this holds:

* It is in a sequential host loop (`do`, `do while`) whose body, at any
  depth, has a parallel loop the dispatch sent to the device, and its target
  or an array it reads is one that such a loop names -- or one that another
  assignment sent to the device names.
* Its target is a whole array of an intrinsic type -- a variable, a
  component, or an associate name bound to a whole array -- and the
  assignment cannot change its shape: it is not an allocatable that the
  assignment may reallocate, nor a pointer other than an associate name, and
  it is not a defined assignment.
* Its value is computed element by element: scalars, arrays of the target's
  rank, intrinsic operators, casts and elemental intrinsic functions of them,
  and `sum` or `product` with a constant `dim` of an array of rank one more.
  A scalar is evaluated by every iteration instead of once, so it may not
  call a function. An array of the value may be the target itself only
  element for element (`x = 2*x`); anything else that may overlap the target
  (`x = x(n:1:-1)`, a scalar `x(1)`) keeps the assignment on the host.
* The loop passes the unsupported-construct check, so that it is never an
  error or a warning that the source did not have: an assignment the device
  cannot run stays on the host.
*/

namespace {

typedef std::vector<ASR::symbol_t*> SPath;

// The variable and components `e` designates, an associate name replaced by
// what it is bound to. False for anything else.
bool designated(ASR::expr_t *e, SPath &path, bool &through_pointer,
        int depth = 0) {
    if (depth > 64) return false;
    switch (e->type) {
        case ASR::exprType::Var: {
            ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(e)->m_v);
            if (ASR::is_a<ASR::Variable_t>(*sym) && ASRUtils::is_pointer(
                    ASR::down_cast<ASR::Variable_t>(sym)->m_type)) {
                if (ASR::expr_t *selector = gpu_associate_selector(sym)) {
                    return designated(selector, path, through_pointer,
                        depth + 1);
                }
                through_pointer = true;
            }
            path.clear();
            path.push_back(sym);
            return true;
        }
        case ASR::exprType::StructInstanceMember: {
            ASR::StructInstanceMember_t *m =
                ASR::down_cast<ASR::StructInstanceMember_t>(e);
            if (!designated(m->m_v, path, through_pointer, depth + 1)) {
                return false;
            }
            if (ASRUtils::is_pointer(ASRUtils::symbol_type(m->m_m))) {
                through_pointer = true;
            }
            path.push_back(ASRUtils::symbol_get_past_external(m->m_m));
            return true;
        }
        case ASR::exprType::ArrayItem:
            return designated(ASR::down_cast<ASR::ArrayItem_t>(e)->m_v, path,
                through_pointer, depth + 1);
        case ASR::exprType::ArraySection:
            return designated(ASR::down_cast<ASR::ArraySection_t>(e)->m_v,
                path, through_pointer, depth + 1);
        case ASR::exprType::ArrayPhysicalCast:
            return designated(ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)
                ->m_arg, path, through_pointer, depth + 1);
        default:
            return false;
    }
}

bool overlap(const SPath &a, const SPath &b) {
    size_t n = std::min(a.size(), b.size());
    for (size_t i = 0; i < n; i++) {
        if (a[i] != b[i]) return false;
    }
    return true;
}

// Everything the parallel loops sent to the device name, at any depth of a
// statement list.
class DeviceData : public ASR::BaseWalkVisitor<DeviceData> {
public:
    std::vector<SPath> paths;
    bool in_device = false;
    std::set<ASR::symbol_t*> blocks;

    void visit_OMPRegion(const ASR::OMPRegion_t &x) {
        bool device = x.m_exec_target == ASR::exec_targetType::ExecDevice;
        bool saved = in_device;
        in_device = in_device || device;
        BaseWalkVisitor::visit_OMPRegion(x);
        in_device = saved;
    }
    void name(const ASR::expr_t &x) {
        if (!in_device) return;
        SPath p;
        bool through_pointer = false;
        if (designated(const_cast<ASR::expr_t*>(&x), p, through_pointer)) {
            paths.push_back(p);
        }
    }
    void visit_Var(const ASR::Var_t &x) {
        name(x.base);
    }
    void visit_StructInstanceMember(const ASR::StructInstanceMember_t &x) {
        name(x.base);
        BaseWalkVisitor::visit_StructInstanceMember(x);
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks.insert(b).second) return;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks.insert(b).second) return;
        ASR::AssociateBlock_t *block = ASR::down_cast<ASR::AssociateBlock_t>(
            b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

// Whether an expression calls a function, which an iteration would call
// again.
class CallFinder : public ASR::BaseWalkVisitor<CallFinder> {
public:
    bool found = false;
    void visit_FunctionCall(const ASR::FunctionCall_t &/*x*/) {
        found = true;
    }
};

// Writes one element of an assignment's value.
class ElementWriter {
public:
    ElementWriter(Allocator &al_, const Location &loc_, int rank_,
            const std::vector<ASR::expr_t*> &index_,
            const SPath &target_)
        : al(al_), loc(loc_), b(al_, loc_), rank(rank_), index(index_),
          target(target_) {}

    // The paths of the arrays and scalars the element reads.
    std::vector<SPath> read;
    // Whether the element reads the target other than element for element.
    bool aliases_target = false;

    // nullptr when the value cannot be written element by element.
    ASR::expr_t* element(ASR::expr_t *e) {
        ASR::ttype_t *type = ASRUtils::expr_type(e);
        if (!ASRUtils::is_array(type)) return scalar(e);
        ASR::ttype_t *element_type = ASRUtils::duplicate_type_without_dims(
            al, ASRUtils::type_get_past_allocatable_pointer(type), loc);
        switch (e->type) {
            case ASR::exprType::ArrayBroadcast: {
                ASR::ArrayBroadcast_t *x =
                    ASR::down_cast<ASR::ArrayBroadcast_t>(e);
                if (ASRUtils::is_array(ASRUtils::expr_type(x->m_array))) {
                    return element(x->m_array);
                }
                return scalar(x->m_array);
            }
            case ASR::exprType::ArrayPhysicalCast:
                return element(ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)
                    ->m_arg);
            case ASR::exprType::Var:
            case ASR::exprType::StructInstanceMember: {
                SPath p;
                bool through_pointer = false;
                if (!whole_array(e) || !designated(e, p, through_pointer)
                        || through_pointer) {
                    return nullptr;
                }
                if (ASRUtils::extract_n_dims_from_ttype(type) != rank) {
                    return nullptr;
                }
                if (overlap(p, target) && p != target) {
                    aliases_target = true;
                }
                read.push_back(p);
                std::vector<ASR::expr_t*> subscripts;
                for (int d = 0; d < rank; d++) {
                    subscripts.push_back(b.Sub(b.Add(
                        b.ArrayLBound(e, d + 1), index[d]), b.i32(1)));
                }
                return b.ArrayItem_01(e, subscripts);
            }
            case ASR::exprType::RealBinOp: {
                ASR::RealBinOp_t *x = ASR::down_cast<ASR::RealBinOp_t>(e);
                ASR::expr_t *l = element(x->m_left), *r = element(x->m_right);
                if (!l || !r) return nullptr;
                return ASRUtils::EXPR(ASR::make_RealBinOp_t(al, loc, l,
                    x->m_op, r, element_type, nullptr));
            }
            case ASR::exprType::IntegerBinOp: {
                ASR::IntegerBinOp_t *x =
                    ASR::down_cast<ASR::IntegerBinOp_t>(e);
                ASR::expr_t *l = element(x->m_left), *r = element(x->m_right);
                if (!l || !r) return nullptr;
                return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, l,
                    x->m_op, r, element_type, nullptr));
            }
            case ASR::exprType::LogicalBinOp: {
                ASR::LogicalBinOp_t *x =
                    ASR::down_cast<ASR::LogicalBinOp_t>(e);
                ASR::expr_t *l = element(x->m_left), *r = element(x->m_right);
                if (!l || !r) return nullptr;
                return ASRUtils::EXPR(ASR::make_LogicalBinOp_t(al, loc, l,
                    x->m_op, r, element_type, nullptr));
            }
            case ASR::exprType::RealCompare: {
                ASR::RealCompare_t *x = ASR::down_cast<ASR::RealCompare_t>(e);
                ASR::expr_t *l = element(x->m_left), *r = element(x->m_right);
                if (!l || !r) return nullptr;
                return ASRUtils::EXPR(ASR::make_RealCompare_t(al, loc, l,
                    x->m_op, r, element_type, nullptr));
            }
            case ASR::exprType::IntegerCompare: {
                ASR::IntegerCompare_t *x =
                    ASR::down_cast<ASR::IntegerCompare_t>(e);
                ASR::expr_t *l = element(x->m_left), *r = element(x->m_right);
                if (!l || !r) return nullptr;
                return ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, loc, l,
                    x->m_op, r, element_type, nullptr));
            }
            case ASR::exprType::RealUnaryMinus: {
                ASR::expr_t *a = element(
                    ASR::down_cast<ASR::RealUnaryMinus_t>(e)->m_arg);
                if (!a) return nullptr;
                return ASRUtils::EXPR(ASR::make_RealUnaryMinus_t(al, loc, a,
                    element_type, nullptr));
            }
            case ASR::exprType::IntegerUnaryMinus: {
                ASR::expr_t *a = element(
                    ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg);
                if (!a) return nullptr;
                return ASRUtils::EXPR(ASR::make_IntegerUnaryMinus_t(al, loc,
                    a, element_type, nullptr));
            }
            case ASR::exprType::LogicalNot: {
                ASR::expr_t *a = element(
                    ASR::down_cast<ASR::LogicalNot_t>(e)->m_arg);
                if (!a) return nullptr;
                return ASRUtils::EXPR(ASR::make_LogicalNot_t(al, loc, a,
                    element_type, nullptr));
            }
            case ASR::exprType::Cast: {
                ASR::Cast_t *x = ASR::down_cast<ASR::Cast_t>(e);
                ASR::expr_t *a = element(x->m_arg);
                if (!a) return nullptr;
                return ASRUtils::EXPR(ASR::make_Cast_t(al, loc, a, x->m_kind,
                    element_type, nullptr, nullptr));
            }
            case ASR::exprType::IntrinsicElementalFunction: {
                ASR::IntrinsicElementalFunction_t *x =
                    ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
                Vec<ASR::expr_t*> args;
                args.reserve(al, x->n_args);
                for (size_t i = 0; i < x->n_args; i++) {
                    ASR::expr_t *a = element(x->m_args[i]);
                    if (!a) return nullptr;
                    args.push_back(al, a);
                }
                return ASRUtils::EXPR(ASR::make_IntrinsicElementalFunction_t(
                    al, loc, x->m_intrinsic_id, args.p, args.n,
                    x->m_overload_id, element_type, nullptr));
            }
            case ASR::exprType::IntrinsicArrayFunction:
                return reduction(
                    ASR::down_cast<ASR::IntrinsicArrayFunction_t>(e),
                    element_type);
            default:
                return nullptr;
        }
    }

private:
    Allocator &al;
    Location loc;
    ASRUtils::ASRBuilder b;
    int rank;
    std::vector<ASR::expr_t*> index;
    SPath target;

    // A whole array: no subscripts anywhere along the designator.
    static bool whole_array(ASR::expr_t *e) {
        while (true) {
            if (ASR::is_a<ASR::Var_t>(*e)) return true;
            if (!ASR::is_a<ASR::StructInstanceMember_t>(*e)) return false;
            e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
            if (ASRUtils::is_array(ASRUtils::expr_type(e))) return false;
        }
    }

    // A scalar the iteration evaluates again: it may not call a function
    // nor read the target.
    ASR::expr_t* scalar(ASR::expr_t *e) {
        CallFinder calls;
        calls.visit_expr(*e);
        if (calls.found) return nullptr;
        class Reads : public ASR::BaseWalkVisitor<Reads> {
        public:
            std::vector<SPath> paths;
            void visit_Var(const ASR::Var_t &x) {
                SPath p;
                bool through_pointer = false;
                if (designated(const_cast<ASR::expr_t*>(&x.base), p,
                        through_pointer)) {
                    paths.push_back(p);
                }
            }
        } reads;
        reads.visit_expr(*e);
        for (const SPath &p : reads.paths) {
            if (overlap(p, target)) aliases_target = true;
            read.push_back(p);
        }
        ASRUtils::ExprStmtDuplicator dup(al);
        return dup.duplicate_expr(e);
    }

    // `sum(x, dim=d)` or `product(x, dim=d)` of a whole array `x` of rank
    // one more than the target: the same reduction of the section of `x`
    // along `d` at this element.
    ASR::expr_t* reduction(ASR::IntrinsicArrayFunction_t *x,
            ASR::ttype_t *element_type) {
        int64_t id = x->m_arr_intrinsic_id;
        if (id != static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::Sum)
                && id != static_cast<int64_t>(
                    ASRUtils::IntrinsicArrayFunctions::Product)) {
            return nullptr;
        }
        if (x->n_args != 2 || x->m_args[1] == nullptr) return nullptr;
        int64_t dim = 0;
        if (!ASRUtils::extract_value(ASRUtils::expr_value(x->m_args[1]),
                dim)) {
            return nullptr;
        }
        ASR::expr_t *array = x->m_args[0];
        while (ASR::is_a<ASR::ArrayPhysicalCast_t>(*array)) {
            array = ASR::down_cast<ASR::ArrayPhysicalCast_t>(array)->m_arg;
        }
        SPath p;
        bool through_pointer = false;
        if (!whole_array(array) || !designated(array, p, through_pointer)
                || through_pointer) {
            return nullptr;
        }
        int array_rank = ASRUtils::extract_n_dims_from_ttype(
            ASRUtils::expr_type(array));
        if (array_rank != rank + 1 || dim < 1 || dim > array_rank) {
            return nullptr;
        }
        if (overlap(p, target)) aliases_target = true;
        read.push_back(p);
        Vec<ASR::array_index_t> subscripts;
        subscripts.reserve(al, array_rank);
        int d_index = 0;
        for (int d = 0; d < array_rank; d++) {
            ASR::array_index_t ai;
            ai.loc = loc;
            if (d + 1 == dim) {
                ai.m_left = b.ArrayLBound(array, d + 1);
                ai.m_right = b.ArrayUBound(array, d + 1);
                ai.m_step = b.i32(1);
            } else {
                ai.m_left = nullptr;
                ai.m_right = b.Sub(b.Add(b.ArrayLBound(array, d + 1),
                    index[d_index++]), b.i32(1));
                ai.m_step = nullptr;
            }
            subscripts.push_back(al, ai);
        }
        Vec<ASR::dimension_t> section_dims;
        section_dims.reserve(al, 1);
        ASR::dimension_t sd;
        sd.loc = loc;
        sd.m_start = nullptr;
        sd.m_length = nullptr;
        section_dims.push_back(al, sd);
        ASR::ttype_t *section_type = ASRUtils::TYPE(ASR::make_Array_t(al,
            loc, ASRUtils::duplicate_type(al, element_type), section_dims.p,
            section_dims.n, ASR::array_physical_typeType::DescriptorArray,
            ASR::memory_spaceType::Global));
        ASR::expr_t *section = ASRUtils::EXPR(ASR::make_ArraySection_t(al,
            loc, array, subscripts.p, subscripts.n, section_type, nullptr));
        Vec<ASR::expr_t*> args;
        args.reserve(al, 1);
        args.push_back(al, section);
        return ASRUtils::EXPR(ASR::make_IntrinsicArrayFunction_t(al, loc,
            id, args.p, args.n, 0, element_type, nullptr));
    }
};

class ArrayStatements {
public:
    ArrayStatements(Allocator &al_, ASR::TranslationUnit_t &unit,
            const PassOptions &options)
        : al(al_), caps(gpu_device_capabilities(options)),
          implementations(al_, unit, options) {}

    void body(ASR::stmt_t **&stmts, size_t &n, SymbolTable *scope,
            const std::vector<SPath> *device) {
        Vec<ASR::stmt_t*> out;
        out.reserve(al, n);
        for (size_t i = 0; i < n; i++) {
            out.push_back(al, statement(stmts[i], scope, device));
        }
        stmts = out.p;
        n = out.n;
    }

private:
    Allocator &al;
    GpuDeviceCapabilities caps;
    GpuProcedureImplementations implementations;
    // How many assignments have been sent to the device.
    size_t offloaded = 0;

    // The data the device loops of a sequential loop name.
    std::vector<SPath> loop_device_data(ASR::stmt_t **stmts, size_t n) {
        DeviceData data;
        for (size_t i = 0; i < n; i++) data.visit_stmt(*stmts[i]);
        return data.paths;
    }

    ASR::stmt_t* statement(ASR::stmt_t *x, SymbolTable *scope,
            const std::vector<SPath> *device) {
        switch (x->type) {
            case ASR::stmtType::DoLoop: {
                ASR::DoLoop_t *d = ASR::down_cast<ASR::DoLoop_t>(x);
                loop(d->m_body, d->n_body, scope, device);
                return x;
            }
            case ASR::stmtType::WhileLoop: {
                ASR::WhileLoop_t *w = ASR::down_cast<ASR::WhileLoop_t>(x);
                loop(w->m_body, w->n_body, scope, device);
                return x;
            }
            case ASR::stmtType::If: {
                ASR::If_t *i = ASR::down_cast<ASR::If_t>(x);
                body(i->m_body, i->n_body, scope, device);
                body(i->m_orelse, i->n_orelse, scope, device);
                return x;
            }
            case ASR::stmtType::Select: {
                ASR::Select_t *s = ASR::down_cast<ASR::Select_t>(x);
                for (size_t i = 0; i < s->n_body; i++) {
                    ASR::case_stmt_t *c = s->m_body[i];
                    if (ASR::is_a<ASR::CaseStmt_t>(*c)) {
                        ASR::CaseStmt_t *cs = ASR::down_cast<ASR::CaseStmt_t>(
                            c);
                        body(cs->m_body, cs->n_body, scope, device);
                    } else {
                        ASR::CaseStmt_Range_t *cr =
                            ASR::down_cast<ASR::CaseStmt_Range_t>(c);
                        body(cr->m_body, cr->n_body, scope, device);
                    }
                }
                body(s->m_default, s->n_default, scope, device);
                return x;
            }
            case ASR::stmtType::BlockCall: {
                ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
                    ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::BlockCall_t>(x)->m_m));
                body(block->m_body, block->n_body, block->m_symtab, device);
                return x;
            }
            case ASR::stmtType::AssociateBlockCall: {
                ASR::AssociateBlock_t *block =
                    ASR::down_cast<ASR::AssociateBlock_t>(
                        ASRUtils::symbol_get_past_external(
                            ASR::down_cast<ASR::AssociateBlockCall_t>(x)
                                ->m_m));
                body(block->m_body, block->n_body, block->m_symtab, device);
                return x;
            }
            case ASR::stmtType::Assignment: {
                if (device == nullptr) return x;
                ASR::stmt_t *loop = offload(
                    *ASR::down_cast<ASR::Assignment_t>(x), scope, *device);
                return loop ? loop : x;
            }
            default:
                return x;
        }
    }

    void loop(ASR::stmt_t **&stmts, size_t &n, SymbolTable *scope,
            const std::vector<SPath> *device) {
        if (device != nullptr) {
            body(stmts, n, scope, device);
            return;
        }
        // An assignment sent to the device makes what it names device data
        // too, which can send another one: until none is left.
        while (true) {
            std::vector<SPath> data = loop_device_data(stmts, n);
            if (data.empty()) return;
            size_t before = offloaded;
            body(stmts, n, scope, &data);
            if (offloaded == before) return;
        }
    }

    // The parallel loop on the device that does what the assignment does,
    // or nullptr when the assignment stays on the host.
    ASR::stmt_t* offload(ASR::Assignment_t &a, SymbolTable *scope,
            const std::vector<SPath> &device) {
        const Location &loc = a.base.base.loc;
        if (a.m_overloaded != nullptr || a.m_move_allocation) return nullptr;
        ASR::expr_t *target = a.m_target;
        ASR::ttype_t *type = ASRUtils::expr_type(target);
        if (!ASRUtils::is_array(type)) return nullptr;
        ASR::ttype_t *element = ASRUtils::extract_type(type);
        if (!ASR::is_a<ASR::Real_t>(*element)
                && !ASR::is_a<ASR::Integer_t>(*element)
                && !ASR::is_a<ASR::Logical_t>(*element)) {
            return nullptr;
        }
        if (ASRUtils::is_allocatable(type) && a.m_realloc_lhs) return nullptr;
        if (!ASR::is_a<ASR::Var_t>(*target)
                && !ASR::is_a<ASR::StructInstanceMember_t>(*target)) {
            return nullptr;
        }
        SPath target_path;
        bool through_pointer = false;
        if (!designated(target, target_path, through_pointer)
                || through_pointer) {
            return nullptr;
        }
        // An associate name bound to a section, whose elements are not
        // the whole of an array.
        if (ASR::is_a<ASR::Var_t>(*target)
                && gpu_argument_may_be_strided(target)
                && ASRUtils::is_pointer(type)) {
            return nullptr;
        }
        int rank = ASRUtils::extract_n_dims_from_ttype(type);
        if (rank < 1) return nullptr;

        ASRUtils::ASRBuilder b(al, loc);
        std::vector<ASR::expr_t*> index;
        for (int d = 0; d < rank; d++) {
            index.push_back(b.Variable(scope, scope->get_unique_name(
                "__gpu_array_i", false), ASRUtils::TYPE(
                    ASR::make_Integer_t(al, loc, 4)),
                ASR::intentType::Local));
        }
        ElementWriter writer(al, loc, rank, index, target_path);
        ASR::expr_t *value = writer.element(a.m_value);
        if (value == nullptr || writer.aliases_target) {
            remove_indices(scope, index);
            return nullptr;
        }
        bool touches = false;
        std::vector<SPath> named = writer.read;
        named.push_back(target_path);
        for (const SPath &p : named) {
            for (const SPath &q : device) {
                touches = touches || overlap(p, q);
            }
        }
        if (!touches) {
            remove_indices(scope, index);
            return nullptr;
        }

        std::vector<ASR::expr_t*> subscripts;
        for (int d = 0; d < rank; d++) {
            subscripts.push_back(b.Sub(b.Add(b.ArrayLBound(target, d + 1),
                index[d]), b.i32(1)));
        }
        ASR::stmt_t *assignment = ASRUtils::STMT(ASR::make_Assignment_t(al,
            loc, b.ArrayItem_01(target, subscripts), value, nullptr, false,
            false, false));
        Vec<ASR::stmt_t*> nest;
        nest.reserve(al, 1);
        nest.push_back(al, assignment);
        for (int d = rank - 1; d >= 0; d--) {
            ASR::do_loop_head_t head;
            head.loc = loc;
            head.m_v = index[d];
            head.m_start = b.i32(1);
            head.m_end = b.ArraySize(target, b.i32(d + 1),
                ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)));
            head.m_increment = nullptr;
            ASR::stmt_t *loop = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc,
                s2c(al, ""), head, nest.p, nest.n, nullptr, 0));
            Vec<ASR::stmt_t*> outer;
            outer.reserve(al, 1);
            outer.push_back(al, loop);
            nest = outer;
        }
        Vec<ASR::omp_clause_t*> clauses;
        clauses.reserve(al, 2);
        clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
            ASR::make_OMPIndependent_t(al, loc)));
        if (rank > 1) {
            clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
                ASR::make_OMPCollapse_t(al, loc, b.i32(rank))));
        }
        ASR::stmt_t *region = ASRUtils::STMT(ASR::make_OMPRegion_t(al, loc,
            ASR::omp_region_typeType::ParallelDo, clauses.p, clauses.n,
            nest.p, nest.n, ASR::exec_targetType::ExecDevice));
        GpuUnsupportedConstruct construct = gpu_find_unsupported_construct(
            *ASR::down_cast<ASR::OMPRegion_t>(region), caps,
            implementations);
        if (construct.found()) {
            remove_indices(scope, index);
            return nullptr;
        }
        offloaded++;
        return region;
    }

    void remove_indices(SymbolTable *scope,
            const std::vector<ASR::expr_t*> &index) {
        for (ASR::expr_t *i : index) {
            scope->erase_symbol(ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(i)->m_v));
        }
    }
};

void visit_scope(ArrayStatements &statements, SymbolTable *scope) {
    for (auto &item : scope->get_scope()) {
        ASR::symbol_t *sym = item.second;
        if (ASR::is_a<ASR::Function_t>(*sym)) {
            ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(sym);
            statements.body(fn->m_body, fn->n_body, fn->m_symtab, nullptr);
            visit_scope(statements, fn->m_symtab);
        } else if (ASR::is_a<ASR::Program_t>(*sym)) {
            ASR::Program_t *p = ASR::down_cast<ASR::Program_t>(sym);
            statements.body(p->m_body, p->n_body, p->m_symtab, nullptr);
            visit_scope(statements, p->m_symtab);
        } else if (ASR::is_a<ASR::Module_t>(*sym)) {
            visit_scope(statements,
                ASR::down_cast<ASR::Module_t>(sym)->m_symtab);
        }
    }
}

} // namespace

void pass_gpu_array_statements(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    if (!gpu_device_capabilities(pass_options).device_selected()) return;
    ArrayStatements statements(al, unit, pass_options);
    visit_scope(statements, unit.m_symtab);
}

} // namespace LCompilers
