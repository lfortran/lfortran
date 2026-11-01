#include <iostream>
#include <chrono>
#include <functional>

#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/codegen/asr_to_x86.h>
#include <libasr/codegen/x86_assembler.h>
#include <libasr/pass/wrap_global_stmts.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>


namespace LCompilers {

namespace {

    // Local exception that is only used in this file to exit the visitor
    // pattern and caught later (not propagated outside)
    class CodeGenError
    {
    public:
        diag::Diagnostic d;
    public:
        CodeGenError(const std::string &msg)
            : d{diag::Diagnostic(msg, diag::Level::Error, diag::Stage::CodeGen)}
        { }

        CodeGenError(const std::string &msg, const Location &loc)
            : d{diag::Diagnostic(msg, diag::Level::Error, diag::Stage::CodeGen,
                {diag::Label("", {loc})})}
        { }
    };

}

using ASR::down_cast;
using ASR::is_a;

// Platform dependent fast unique hash:
uint64_t static get_hash(ASR::asr_t *node)
{
    return (uint64_t)node;
}

class ASRToX86Visitor : public ASR::BaseVisitor<ASRToX86Visitor>
{
    struct Sym {
        int32_t stack_offset; // The local variable is [ebp-stack_offset]
        std::string fn_label; // Subroutine / Function assembly label
        bool pointer; // Is variable represented as a pointer (or value)
    };
public:
    Allocator &m_al;
    X86Assembler m_a;
    std::map<std::string,std::string> m_global_strings;
    std::map<uint64_t, Sym> x86_symtab;

    // The enclosing constructs that an EXIT or a CYCLE can name: an EXIT
    // jumps to the label `exit`, a CYCLE to `cycle` (empty for a construct
    // that is not a loop).
    struct ConstructFrame {
        const char *name;
        std::string exit;
        std::string cycle;
    };
    std::vector<ConstructFrame> construct_frames;

    // The stack offset of the first of the three slots that the counted loop
    // with the head holds its end, its step and the number of iterations
    // left in.
    std::map<const ASR::do_loop_head_t*, uint32_t> m_loop_slots;
public:

    ASRToX86Visitor(Allocator &al) : m_al{al}, m_a{al, false} {}

    void visit_IfExp(const ASR::IfExp_t & /* x */) {
        throw CodeGenError("conditional expressions are not supported by the "
            "x86 backend");
    }

    // A variable in a common block has no storage of its own, and this
    // backend does not lay out the storage of common blocks.
    void reject_common_block_variable(const ASR::Variable_t &v) {
        if (ASR::CommonBlock_t *block = ASRUtils::get_common_block(&v)) {
            throw CodeGenError("COMMON blocks are not supported by the x86 backend",
                block->base.base.loc);
        }
    }

    void visit_TranslationUnit(const ASR::TranslationUnit_t &x) {
        // All loose statements must be converted to a function, so the items
        // must be empty:
        LCOMPILERS_ASSERT(x.n_items == 0);
        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::BlockData_t>(*item.second)) {
                throw CodeGenError("BLOCK DATA is not supported by the x86 backend",
                    item.second->base.loc);
            }
        }

        emit_elf32_header(m_a);

        emit_data_string(m_a, "string_neg", "-"); // - symbol for printing negative ints/floats
        // Add runtime library functions
        emit_print_int(m_a, "print_int");
        emit_exit(m_a, "my_exit", 0);
        emit_exit(m_a, "exit_error_stop", 1);


        std::vector<std::string> global_func_order = ASRUtils::determine_function_definition_order(x.m_symtab);
        for (size_t i = 0; i < global_func_order.size(); i++) {
            ASR::symbol_t* sym = x.m_symtab->get_symbol(global_func_order[i]);
            // Ignore external symbols because they are already defined by the loop above.
            if( !sym || ASR::is_a<ASR::ExternalSymbol_t>(*sym) ) {
                continue;
            }
            visit_symbol(*sym);
        }

        std::vector<std::string> build_order = ASRUtils::determine_module_dependencies(x);
        for (auto &item : build_order) {
            ASR::symbol_t *mod = x.m_symtab->get_symbol(item);
            visit_symbol(*mod);
        }

        // Then the main program:
        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Program_t>(*item.second)) {
                visit_symbol(*item.second);
            }
        }

        emit_elf32_footer(m_a);
    }

    void visit_Module(const ASR::Module_t &x) {
        std::vector<std::string> func_order
            = ASRUtils::determine_function_definition_order(x.m_symtab);
        for (size_t i = 0; i < func_order.size(); i++) {
            ASR::symbol_t* sym = x.m_symtab->get_symbol(func_order[i]);
            // Ignore external symbols because they are already defined by the loop above.
            if( !sym || ASR::is_a<ASR::ExternalSymbol_t>(*sym) ) {
                continue;
            }
            visit_symbol(*sym);
        }
    }

    void visit_Program(const ASR::Program_t &x) {



        std::vector<std::string> func_order = ASRUtils::determine_function_definition_order(x.m_symtab);
        // Generate code for nested subroutines and functions first:
        for (auto &item : func_order) {
            ASR::symbol_t* sym = x.m_symtab->get_symbol(item);
            ASR::Function_t *s = ASR::down_cast<ASR::Function_t>(sym);
            visit_Function(*s);
        }

        // Generate code for the main program
        m_a.add_label("_start");

        // Initialize the stack
        m_a.asm_push_r32(X86Reg::ebp);
        m_a.asm_mov_r32_r32(X86Reg::ebp, X86Reg::esp);

        // Allocate stack space for local variables
        uint32_t total_offset = 0;
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t *v = down_cast<ASR::Variable_t>(item.second);
                reject_common_block_variable(*v);

                if (v->m_type->type == ASR::ttypeType::Integer) {
                    total_offset += 4;
                    Sym s;
                    s.stack_offset = total_offset;
                    s.pointer = false;
                    uint64_t h = get_hash((ASR::asr_t*)v);
                    x86_symtab[h] = s;
                } else {
                    throw CodeGenError("Variable type not supported");
                }
            }
        }
        total_offset = allocate_loop_slots(x.m_body, x.n_body, total_offset);
        allocate_stack(total_offset);

        for (size_t i=0; i<x.n_body; i++) {
            this->visit_stmt(*x.m_body[i]);
        }

        m_a.asm_call_label("my_exit");

        // Restore stack
        m_a.asm_mov_r32_r32(X86Reg::esp, X86Reg::ebp);
        m_a.asm_pop_r32(X86Reg::ebp);
        //m_a.asm_ret();

        for (auto &s : m_global_strings) {
            emit_data_string(m_a, s.first, s.second);
        }

    }

    // Gives the counted loops among `body` the slots of m_loop_slots, after
    // the `total_offset` bytes of the locals, and returns the new total.
    // Loops that are not nested in each other share them, so a set of slots
    // is made for each depth of nesting.
    uint32_t allocate_loop_slots(ASR::stmt_t **body, size_t n_body,
            uint32_t total_offset) {
        class LoopHeads : public ASR::BaseWalkVisitor<LoopHeads> {
        public:
            // Each counted head with the number of counted heads around it.
            std::vector<std::pair<const ASR::do_loop_head_t*, size_t>> heads;
            size_t depth = 0;

            void visit_DoLoop(const ASR::DoLoop_t &x) {
                bool counted = x.m_head.m_v != nullptr;
                if (counted) heads.push_back({&x.m_head, depth++});
                ASR::BaseWalkVisitor<LoopHeads>::visit_DoLoop(x);
                if (counted) depth--;
            }

            void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
                for (size_t i = 0; i < x.n_head; i++) {
                    heads.push_back({&x.m_head[i], depth++});
                }
                ASR::BaseWalkVisitor<LoopHeads>::visit_DoConcurrentLoop(x);
                depth -= x.n_head;
            }

            void visit_expr(const ASR::expr_t &/*x*/) {}
            void visit_ttype(const ASR::ttype_t &/*x*/) {}
        };
        LoopHeads loops;
        for (size_t i = 0; i < n_body; i++) {
            loops.visit_stmt(*body[i]);
        }
        std::vector<uint32_t> sets;
        for (auto &[head, depth] : loops.heads) {
            while (sets.size() <= depth) {
                sets.push_back(total_offset + 4);
                total_offset += 12;
            }
            m_loop_slots[head] = sets[depth];
        }
        return total_offset;
    }

    // Reserve `size` bytes of locals below ebp.
    void allocate_stack(uint32_t size) {
        if (size < 128) {
            m_a.asm_sub_r32_imm8(X86Reg::esp, static_cast<uint8_t>(size));
        } else {
            m_a.asm_sub_r32_imm32(X86Reg::esp, size);
        }
    }

    void visit_Function(const ASR::Function_t &x) {
        uint64_t h = get_hash((ASR::asr_t*)&x);
        std::string id = std::to_string(h);

        // Generate code for the subroutine
        Sym s;
        s.stack_offset = 0;
        s.pointer = false;
        s.fn_label = x.m_name + id;
        x86_symtab[h] = s;
        m_a.add_label(s.fn_label);

        // Add arguments to x86_symtab with their correct offset
        for (size_t i=0; i<x.n_args; i++) {
            ASR::Variable_t *arg = ASRUtils::EXPR2VAR(x.m_args[i]);
            LCOMPILERS_ASSERT(ASRUtils::is_arg_dummy(arg->m_intent));
            // TODO: we are assuming integer here:
            LCOMPILERS_ASSERT(arg->m_type->type == ASR::ttypeType::Integer);
            Sym s;
            s.stack_offset = -checked_integer_cast<int32_t>(i*4+8); // TODO: reverse the sign of offset
            // We pass intent(in) as value, otherwise as pointer
            s.pointer = (arg->m_intent != ASR::intentType::In);
            uint64_t h = get_hash((ASR::asr_t*)arg);
            x86_symtab[h] = s;
        }

        // Initialize the stack
        m_a.asm_push_r32(X86Reg::ebp);
        m_a.asm_mov_r32_r32(X86Reg::ebp, X86Reg::esp);

        // Allocate stack space for local variables
        uint32_t total_offset = 0;
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t *v = down_cast<ASR::Variable_t>(item.second);
                reject_common_block_variable(*v);

                if (v->m_intent == ASRUtils::intent_local ||
                    v->m_intent == ASRUtils::intent_return_var) {
                    if (v->m_type->type == ASR::ttypeType::Integer) {
                        total_offset += 4;
                        Sym s;
                        s.stack_offset = total_offset;
                        s.pointer = false;
                        uint64_t h = get_hash((ASR::asr_t*)v);
                        x86_symtab[h] = s;
                    } else {
                        throw CodeGenError("Variable type not supported");
                    }
                }
            }
        }
        total_offset = allocate_loop_slots(x.m_body, x.n_body, total_offset);
        allocate_stack(total_offset);

        for (size_t i=0; i<x.n_body; i++) {
            this->visit_stmt(*x.m_body[i]);
        }

        // Leave return value in eax
        if (x.m_return_var) {
            ASR::Variable_t *retv = ASRUtils::EXPR2VAR(x.m_return_var);

            uint64_t h = get_hash((ASR::asr_t*)retv);
            LCOMPILERS_ASSERT(x86_symtab.find(h) != x86_symtab.end());
            Sym s = x86_symtab[h];
            X86Reg base = X86Reg::ebp;
            // mov eax, [ebp-s.stack_offset]
            m_a.asm_mov_r32_m32(X86Reg::eax, &base, nullptr, 1, -s.stack_offset);
            LCOMPILERS_ASSERT(!s.pointer);
        }

        // Restore stack
        m_a.asm_mov_r32_r32(X86Reg::esp, X86Reg::ebp);
        m_a.asm_pop_r32(X86Reg::ebp);
        m_a.asm_ret();
    }

    void visit_Return(const ASR::Return_t &/*x*/) { }

    // Expressions leave integer values in eax

    void visit_IntegerConstant(const ASR::IntegerConstant_t &x) {
        // The 32-bit backend keeps the low 32 bits of the constant.
        m_a.asm_mov_r32_imm32(X86Reg::eax, static_cast<uint32_t>(x.m_n));
    }

    void visit_LogicalConstant(const ASR::LogicalConstant_t &x) {
        int val;
        if (x.m_value == true) {
            val = 1;
        } else {
            val = 0;
        }
        m_a.asm_mov_r32_imm32(X86Reg::eax, val);
    }

    // Logical values are 1 (true) and 0 (false) in eax.
    void visit_LogicalNot(const ASR::LogicalNot_t &x) {
        this->visit_expr(*x.m_arg);
        m_a.asm_mov_r32_imm32(X86Reg::ecx, 1);
        m_a.asm_xor_r32_r32(X86Reg::eax, X86Reg::ecx);
    }

    void visit_LogicalBinOp(const ASR::LogicalBinOp_t &x) {
        this->visit_expr(*x.m_right);
        m_a.asm_push_r32(X86Reg::eax);
        this->visit_expr(*x.m_left);
        m_a.asm_pop_r32(X86Reg::ecx);
        // The left operand is in eax, the right operand is in ecx
        // Leave the result in eax.
        switch (x.m_op) {
            case ASR::logicalbinopType::And: {
                m_a.asm_and_r32_r32(X86Reg::eax, X86Reg::ecx);
                break;
            }
            case ASR::logicalbinopType::Or: {
                m_a.asm_or_r32_r32(X86Reg::eax, X86Reg::ecx);
                break;
            }
            case ASR::logicalbinopType::Xor:
            case ASR::logicalbinopType::NEqv: {
                m_a.asm_xor_r32_r32(X86Reg::eax, X86Reg::ecx);
                break;
            }
            case ASR::logicalbinopType::Eqv: {
                m_a.asm_xor_r32_r32(X86Reg::eax, X86Reg::ecx);
                m_a.asm_mov_r32_imm32(X86Reg::ecx, 1);
                m_a.asm_xor_r32_r32(X86Reg::eax, X86Reg::ecx);
                break;
            }
        }
    }

    void visit_Var(const ASR::Var_t &x) {
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(x.m_v);
        uint64_t h = get_hash((ASR::asr_t*)v);
        LCOMPILERS_ASSERT(x86_symtab.find(h) != x86_symtab.end());
        Sym s = x86_symtab[h];
        X86Reg base = X86Reg::ebp;
        // mov eax, [ebp-s.stack_offset]
        m_a.asm_mov_r32_m32(X86Reg::eax, &base, nullptr, 1, -s.stack_offset);
        if (s.pointer) {
            base = X86Reg::eax;
            // Dereference a pointer
            // mov eax, [eax]
            m_a.asm_mov_r32_m32(X86Reg::eax, &base, nullptr, 1, 0);
        }
    }

    void visit_IntegerBinOp(const ASR::IntegerBinOp_t &x) {
        this->visit_expr(*x.m_right);
        m_a.asm_push_r32(X86Reg::eax);
        this->visit_expr(*x.m_left);
        m_a.asm_pop_r32(X86Reg::ecx);
        // The left operand is in eax, the right operand is in ecx
        // Leave the result in eax.
        switch (x.m_op) {
            case ASR::binopType::Add: {
                m_a.asm_add_r32_r32(X86Reg::eax, X86Reg::ecx);
                break;
            };
            case ASR::binopType::Sub: {
                m_a.asm_sub_r32_r32(X86Reg::eax, X86Reg::ecx);
                break;
            };
            case ASR::binopType::Mul: {
                m_a.asm_mov_r32_imm32(X86Reg::edx, 0);
                m_a.asm_mul_r32(X86Reg::ecx);
                break;
            };
            case ASR::binopType::Div: {
                m_a.asm_mov_r32_imm32(X86Reg::edx, 0);
                m_a.asm_div_r32(X86Reg::ecx);
                break;
            };
            default: {
                throw CodeGenError("Binary operator '" + ASRUtils::binop_to_str_python(x.m_op) + "' not supported yet");
            }
        }
    }

    void visit_IntegerUnaryMinus(const ASR::IntegerUnaryMinus_t &x) {
        this->visit_expr(*x.m_arg);
        m_a.asm_neg_r32(X86Reg::eax);
    }

    void visit_IntegerCompare(const ASR::IntegerCompare_t &x) {
        std::string id = std::to_string(get_hash((ASR::asr_t*)&x));
        this->visit_expr(*x.m_right);
        m_a.asm_push_r32(X86Reg::eax);
        this->visit_expr(*x.m_left);
        m_a.asm_pop_r32(X86Reg::ecx);
        // The left operand is in eax, the right operand is in ecx
        // Leave the result in eax.
        m_a.asm_cmp_r32_r32(X86Reg::eax, X86Reg::ecx);
        switch (x.m_op) {
            case (ASR::cmpopType::Eq) : {
                m_a.asm_je_label(".compare1" + id);
                break;
            }
            case (ASR::cmpopType::Gt) : {
                m_a.asm_jg_label(".compare1" + id);
                break;
            }
            case (ASR::cmpopType::GtE) : {
                m_a.asm_jge_label(".compare1" + id);
                break;
            }
            case (ASR::cmpopType::Lt) : {
                m_a.asm_jl_label(".compare1" + id);
                break;
            }
            case (ASR::cmpopType::LtE) : {
                m_a.asm_jle_label(".compare1" + id);
                break;
            }
            case (ASR::cmpopType::NotEq) : {
                m_a.asm_jne_label(".compare1" + id);
                break;
            }
            default : {
                throw CodeGenError("Comparison operator not implemented");
            }
        }
        m_a.asm_mov_r32_imm32(X86Reg::eax, 0);
        m_a.asm_jmp_label(".compareend" + id);
        m_a.add_label(".compare1" + id);
        m_a.asm_mov_r32_imm32(X86Reg::eax, 1);
        m_a.add_label(".compareend" + id);
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        this->visit_expr(*x.m_value);
        // RHS is in eax
        store_eax(ASRUtils::EXPR2VAR(x.m_target));
    }

    // Store eax in the variable `v`; ecx is overwritten.
    void store_eax(ASR::Variable_t *v) {
        uint64_t h = get_hash((ASR::asr_t*)v);
        LCOMPILERS_ASSERT(x86_symtab.find(h) != x86_symtab.end());
        Sym s = x86_symtab[h];
        X86Reg base = X86Reg::ebp;
        if (s.pointer) {
            // mov ecx, [ebp-s.stack_offset]
            m_a.asm_mov_r32_m32(X86Reg::ecx, &base, nullptr, 1, -s.stack_offset);
            // mov [ecx], eax
            base = X86Reg::ecx;
            m_a.asm_mov_m32_r32(&base, nullptr, 1, 0, X86Reg::eax);
        } else {
            // mov [ebp-s.stack_offset], eax
            m_a.asm_mov_m32_r32(&base, nullptr, 1, -s.stack_offset, X86Reg::eax);
        }
    }

    void visit_Print(const ASR::Print_t &x) {
        LCOMPILERS_ASSERT(x.m_text != nullptr);
        ASR::expr_t *e = x.m_text;
        //HACKISH way to handle print refactoring (always using stringformat).
        // TODO : Implement stringformat visitor.
        if(e && ASR::is_a<ASR::StringFormat_t>(*e)){
            if (ASR::down_cast<ASR::StringFormat_t>(e)->n_dtio_bindings > 0) {
                throw CodeGenError("user-defined derived-type output is not "
                    "supported by the x86 backend yet", x.base.base.loc);
            }
            e = ASR::down_cast<ASR::StringFormat_t>(e)->m_args[0];
        }
        if (e->type == ASR::exprType::StringConstant) {
            ASR::StringConstant_t *s = down_cast<ASR::StringConstant_t>(e);
            std::string msg = s->m_s;
            msg += "\n";
            std::string id = "string" + std::to_string(get_hash((ASR::asr_t*)e));
            emit_print(m_a, id, checked_integer_cast<uint32_t>(msg.size()));
            m_global_strings[id] = msg;
        } else {
            this->visit_expr(*e);
            ASR::ttype_t *t = ASRUtils::expr_type(e);
            if (t->type == ASR::ttypeType::Integer) {
                m_a.asm_push_r32(X86Reg::eax);
                m_a.asm_call_label("print_int");
                m_a.asm_add_r32_imm8(X86Reg::esp, 4);
            } else if (t->type == ASR::ttypeType::Real) {
                throw LCompilersException("Type not implemented");
            } else if (t->type == ASR::ttypeType::String) {
                throw LCompilersException("Type not implemented");
            } else {
                throw LCompilersException("Type not implemented");
            }


            std::string msg = "\n";
            std::string id = "string" + std::to_string(get_hash((ASR::asr_t*)e));
            emit_print(m_a, id, checked_integer_cast<uint32_t>(msg.size()));
            m_global_strings[id] = msg;
        }
    }

    void visit_ErrorStop(const ASR::ErrorStop_t &x) {
        std::string id = "err" + std::to_string(get_hash((ASR::asr_t*)&x));
        std::string msg = "ERROR STOP\n";
        emit_print(m_a, id, checked_integer_cast<uint32_t>(msg.size()));
        m_global_strings[id] = msg;

        m_a.asm_call_label("exit_error_stop");
    }

    void visit_If(const ASR::If_t &x) {
        std::string id = std::to_string(get_hash((ASR::asr_t*)&x));
        this->visit_expr(*x.m_test);
        // eax contains the logical value (true=1, false=0) of the if condition
        m_a.asm_cmp_r32_imm8(X86Reg::eax, 1);
        m_a.asm_je_label(".then" + id);
        m_a.asm_jmp_label(".else" + id);
        m_a.add_label(".then" + id);
        if (x.m_name) {
            construct_frames.push_back({x.m_name, ".endif" + id, ""});
        }
        for (size_t i=0; i<x.n_body; i++) {
            this->visit_stmt(*x.m_body[i]);
        }
        m_a.asm_jmp_label(".endif" +id);
        m_a.add_label(".else" + id);
        for (size_t i=0; i<x.n_orelse; i++) {
            this->visit_stmt(*x.m_orelse[i]);
        }
        if (x.m_name) {
            construct_frames.pop_back();
        }
        m_a.add_label(".endif" + id);
    }

    void visit_WhileLoop(const ASR::WhileLoop_t &x) {
        std::string id = std::to_string(get_hash((ASR::asr_t*)&x));

        // head
        m_a.add_label(".loop.head" + id);
        this->visit_expr(*x.m_test);
        // eax contains the logical value (true=1, false=0) of the while condition
        m_a.asm_cmp_r32_imm8(X86Reg::eax, 1);
        m_a.asm_je_label(".loop.body" + id);
        m_a.asm_jmp_label(".loop.end" + id);

        // body
        m_a.add_label(".loop.body" + id);
        construct_frames.push_back({x.m_name, ".loop.end" + id,
            ".loop.head" + id});
        for (size_t i=0; i<x.n_body; i++) {
            this->visit_stmt(*x.m_body[i]);
        }
        construct_frames.pop_back();
        m_a.asm_jmp_label(".loop.head" + id);

        // end
        m_a.add_label(".loop.end" + id);
    }

    // A counted DO loop, lowered by the rules for native CFG backends
    // (#16398). The start a, end b and step c are evaluated once; b and a
    // step that is not constant are kept in stack slots:
    //
    //     i = a
    //     [c not constant: c == 0 -> "Runtime error: zero step in a DO loop"]
    //     if (c > 0 ? a > b : a < b) goto orelse
    //     [other steps: r = |b - a| / |c|]
    //   body:               // EXIT -> end, CYCLE -> latch
    //     ...
    //   latch:
    //     i = i + c
    //     step 1 or -1: if (i - c != b) goto body
    //     other steps:  if (r != 0) { r = r - 1; goto body }
    //   orelse:
    //     loop-else
    //   end:
    //
    // The remaining trip count r is unsigned, so it is right even where
    // b - a overflows.
    //
    // `body` writes the loop body.
    void emit_counted_loop(const ASR::do_loop_head_t &head, const char *name,
            const std::function<void()> &body, ASR::stmt_t **orelse,
            size_t n_orelse) {
        std::string id = std::to_string(get_hash((ASR::asr_t*)&head));
        std::string body_label = ".do.body" + id;
        std::string latch_label = ".do.latch" + id;
        std::string orelse_label = ".do.orelse" + id;
        std::string end_label = ".do.end" + id;
        if (!head.m_v) {
            // A DO without loop control ends only by an EXIT, which skips
            // the loop-else.
            m_a.add_label(body_label);
            construct_frames.push_back({name, end_label, body_label});
            body();
            construct_frames.pop_back();
            m_a.asm_jmp_label(body_label);
            m_a.add_label(end_label);
            return;
        }
        if (!ASR::is_a<ASR::Var_t>(*head.m_v)
                || !ASRUtils::is_integer(*ASRUtils::expr_type(head.m_v))) {
            throw CodeGenError("the x86 backend supports only an integer "
                "variable as the index of a DO loop", head.m_v->base.loc);
        }
        ASR::Variable_t *v = ASRUtils::EXPR2VAR(head.m_v);
        int64_t step = 0;
        bool step_known = ASRUtils::extract_value(
            ASRUtils::expr_value(head.m_increment), step);
        if (step_known && step == 0) {
            throw CodeGenError("a DO loop with a zero step reached the x86 "
                "backend", head.m_increment->base.loc);
        }
        bool unit = step_known && (step == 1 || step == -1);
        X86Reg base = X86Reg::ebp;
        int32_t end_slot = -checked_integer_cast<int32_t>(m_loop_slots.at(&head));
        int32_t step_slot = end_slot - 4;
        int32_t count_slot = end_slot - 8;
        // Load the step into `r`.
        auto load_step = [&](X86Reg r) {
            if (step_known) {
                m_a.asm_mov_r32_imm32(r, static_cast<uint32_t>(step));
            } else {
                m_a.asm_mov_r32_m32(r, &base, nullptr, 1, step_slot);
            }
        };

        if (!step_known) {
            this->visit_expr(*head.m_increment);
            m_a.asm_mov_m32_r32(&base, nullptr, 1, step_slot, X86Reg::eax);
        }
        this->visit_expr(*head.m_end);
        m_a.asm_mov_m32_r32(&base, nullptr, 1, end_slot, X86Reg::eax);
        this->visit_expr(*head.m_start);
        m_a.asm_push_r32(X86Reg::eax);
        store_eax(v);
        if (!step_known) {
            m_a.asm_mov_r32_m32(X86Reg::eax, &base, nullptr, 1, step_slot);
            m_a.asm_cmp_r32_imm8(X86Reg::eax, 0);
            m_a.asm_jne_label(".do.step" + id);
            std::string msg = "Runtime error: zero step in a DO loop\n";
            std::string msg_id = "zerostep" + id;
            emit_print(m_a, msg_id, checked_integer_cast<uint32_t>(msg.size()));
            m_global_strings[msg_id] = msg;
            m_a.asm_call_label("exit_error_stop");
            m_a.add_label(".do.step" + id);
        }
        m_a.asm_pop_r32(X86Reg::eax);

        // With a in eax and b in ecx, skip the loop if it has no iterations
        // and otherwise store the remaining trip count.
        auto enter = [&](bool positive) {
            m_a.asm_cmp_r32_r32(X86Reg::eax, X86Reg::ecx);
            if (positive) {
                m_a.asm_jg_label(orelse_label);
            } else {
                m_a.asm_jl_label(orelse_label);
            }
            if (unit) return;
            if (positive) {
                m_a.asm_sub_r32_r32(X86Reg::ecx, X86Reg::eax);
                m_a.asm_mov_r32_r32(X86Reg::eax, X86Reg::ecx);
                load_step(X86Reg::ecx);
            } else {
                m_a.asm_sub_r32_r32(X86Reg::eax, X86Reg::ecx);
                load_step(X86Reg::ecx);
                m_a.asm_neg_r32(X86Reg::ecx);
            }
            m_a.asm_mov_r32_imm32(X86Reg::edx, 0);
            m_a.asm_div_r32(X86Reg::ecx);
            m_a.asm_mov_m32_r32(&base, nullptr, 1, count_slot, X86Reg::eax);
        };
        m_a.asm_mov_r32_m32(X86Reg::ecx, &base, nullptr, 1, end_slot);
        if (step_known) {
            enter(step > 0);
        } else {
            m_a.asm_mov_r32_m32(X86Reg::edx, &base, nullptr, 1, step_slot);
            m_a.asm_cmp_r32_imm8(X86Reg::edx, 0);
            m_a.asm_jl_label(".do.negative" + id);
            enter(true);
            m_a.asm_jmp_label(body_label);
            m_a.add_label(".do.negative" + id);
            enter(false);
        }

        m_a.add_label(body_label);
        construct_frames.push_back({name, end_label, latch_label});
        body();
        construct_frames.pop_back();

        m_a.add_label(latch_label);
        this->visit_expr(*head.m_v);
        if (unit) {
            m_a.asm_push_r32(X86Reg::eax);
        }
        load_step(X86Reg::ecx);
        m_a.asm_add_r32_r32(X86Reg::eax, X86Reg::ecx);
        store_eax(v);
        if (unit) {
            m_a.asm_pop_r32(X86Reg::eax);
            m_a.asm_mov_r32_m32(X86Reg::ecx, &base, nullptr, 1, end_slot);
            m_a.asm_cmp_r32_r32(X86Reg::eax, X86Reg::ecx);
            m_a.asm_jne_label(body_label);
        } else {
            m_a.asm_mov_r32_m32(X86Reg::eax, &base, nullptr, 1, count_slot);
            m_a.asm_cmp_r32_imm8(X86Reg::eax, 0);
            m_a.asm_je_label(orelse_label);
            m_a.asm_dec_r32(X86Reg::eax);
            m_a.asm_mov_m32_r32(&base, nullptr, 1, count_slot, X86Reg::eax);
            m_a.asm_jmp_label(body_label);
        }

        m_a.add_label(orelse_label);
        for (size_t i = 0; i < n_orelse; i++) {
            this->visit_stmt(*orelse[i]);
        }
        m_a.add_label(end_label);
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        emit_counted_loop(x.m_head, x.m_name,
            [&]() {
                for (size_t i = 0; i < x.n_body; i++) {
                    this->visit_stmt(*x.m_body[i]);
                }
            }, x.m_orelse, x.n_orelse);
    }

    // DO CONCURRENT runs its iterations in order, as nested counted loops
    // with the first index outermost. A CYCLE goes to the next iteration of
    // the innermost one, which is the next iteration of the construct.
    void emit_concurrent_loops(const ASR::DoConcurrentLoop_t &x, size_t i) {
        if (i == x.n_head) {
            for (size_t j = 0; j < x.n_body; j++) {
                this->visit_stmt(*x.m_body[j]);
            }
            return;
        }
        emit_counted_loop(x.m_head[i], nullptr,
            [&]() { emit_concurrent_loops(x, i + 1); }, nullptr, 0);
    }

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        emit_concurrent_loops(x, 0);
    }

    // An EXIT or a CYCLE without a name refers to the innermost loop.
    const ConstructFrame &construct_frame(const char *name,
            const Location &loc) {
        for (auto f = construct_frames.rbegin();
                f != construct_frames.rend(); ++f) {
            if (name ? f->name && std::string(f->name) == name
                    : !f->cycle.empty()) {
                return *f;
            }
        }
        throw CodeGenError("the construct that EXIT or CYCLE refers to was "
            "not found", loc);
    }

    void visit_Exit(const ASR::Exit_t &x) {
        m_a.asm_jmp_label(construct_frame(x.m_stmt_name, x.base.base.loc).exit);
    }

    void visit_Cycle(const ASR::Cycle_t &x) {
        m_a.asm_jmp_label(construct_frame(x.m_stmt_name, x.base.base.loc).cycle);
    }

    // Push arguments to stack (last argument first)
    template <typename T, typename T2>
    uint8_t push_call_args(const T &x, const T2 &sub) {
        LCOMPILERS_ASSERT(sub.n_args == x.n_args);
        // Note: when counting down in a loop with an unsigned `i`, test
        // before decrementing so that the loop stops after i == 0.
        for (size_t i=x.n_args; i-- > 0;) {
            bool pass_as_pointer;
            {
                ASR::Variable_t *arg = ASRUtils::EXPR2VAR(sub.m_args[i]);
                LCOMPILERS_ASSERT(ASRUtils::is_arg_dummy(arg->m_intent));
                // TODO: we are assuming integer here:
                LCOMPILERS_ASSERT(arg->m_type->type == ASR::ttypeType::Integer);
                uint64_t h = get_hash((ASR::asr_t*)arg);
                Sym &s = x86_symtab[h];
                pass_as_pointer = s.pointer;
            }
            if (x.m_args[i].m_value->type == ASR::exprType::Var) {
                ASR::Variable_t *arg = ASRUtils::EXPR2VAR(x.m_args[i].m_value);
                uint64_t h = get_hash((ASR::asr_t*)arg);
                LCOMPILERS_ASSERT(x86_symtab.find(h) != x86_symtab.end());
                Sym s = x86_symtab[h];
                X86Reg base = X86Reg::ebp;
                if (s.pointer) {
                    if (pass_as_pointer) {
                        // Copy over the stack variable (already a pointer)
                        // mov eax, [ebp-s.stack_offset]
                        m_a.asm_mov_r32_m32(X86Reg::eax, &base, nullptr, 1, -s.stack_offset);
                    } else {
                        // Copy and dereference the stack variable

                        // Copy
                        // mov eax, [ebp-s.stack_offset]
                        m_a.asm_mov_r32_m32(X86Reg::eax, &base, nullptr, 1, -s.stack_offset);

                        // Dereference a pointer
                        // mov eax, [eax]
                        m_a.asm_mov_r32_m32(X86Reg::eax, &base, nullptr, 1, 0);
                    }
                    m_a.asm_push_r32(X86Reg::eax);
                } else {
                    if (pass_as_pointer) {
                        // Get a pointer to the stack variable
                        // lea eax, [ebp-s.stack_offset]
                        m_a.asm_lea_r32_m32(X86Reg::eax, &base, nullptr, 1, -s.stack_offset);
                    } else {
                        // Copy over the stack variable
                        // mov eax, [ebp-s.stack_offset]
                        m_a.asm_mov_r32_m32(X86Reg::eax, &base, nullptr, 1, -s.stack_offset);
                    }
                    m_a.asm_push_r32(X86Reg::eax);
                }
            } else {
                LCOMPILERS_ASSERT(!pass_as_pointer);
                this->visit_expr(*(x.m_args[i].m_value));
                // The value of the argument is in eax, push it onto the stack
                m_a.asm_push_r32(X86Reg::eax);
            }
        }
        // Preserves the existing imm8 truncation for more than 63 arguments.
        return static_cast<uint8_t>(x.n_args*4);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        ASR::Function_t *s = ASR::down_cast<ASR::Function_t>(
            ASRUtils::symbol_get_past_external(x.m_name));

        uint64_t h = get_hash((ASR::asr_t*)s);
        if (x86_symtab.find(h) == x86_symtab.end()) {
            throw CodeGenError("Subroutine code not generated for '"
                + std::string(s->m_name) + "'");
        }
        Sym &sym = x86_symtab[h];
        // Push arguments to stack (last argument first)
        uint8_t arg_offset = push_call_args(x, *s);
        // Call the subroutine
        m_a.asm_call_label(sym.fn_label);
        // Remove arguments from stack
        m_a.asm_add_r32_imm8(X86Reg::esp, arg_offset);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::Function_t *s = ASR::down_cast<ASR::Function_t>(x.m_name);

        uint64_t h = get_hash((ASR::asr_t*)s);
        if (x86_symtab.find(h) == x86_symtab.end()) {
            throw CodeGenError("Function code not generated for '"
                + std::string(s->m_name) + "'");
        }
        Sym &sym = x86_symtab[h];
        // Push arguments to stack (last argument first)
        uint8_t arg_offset = push_call_args(x, *s);
        // Call the function (the result is in eax, we leave it there)
        m_a.asm_call_label(sym.fn_label);
        // Remove arguments from stack
        m_a.asm_add_r32_imm8(X86Reg::esp, arg_offset);
    }

};


Result<int> asr_to_x86(ASR::TranslationUnit_t &asr, Allocator &al,
        const std::string &filename, bool time_report,
        diag::Diagnostics &diagnostics)
{
    int64_t time_pass_global=0;
    int64_t time_visit_asr=0;
    int64_t time_verify=0;
    int64_t time_save=0;

    ASRToX86Visitor v(al);

    LCompilers::PassOptions pass_options;
    pass_options.run_fun = "f";

    {
        auto t1 = std::chrono::high_resolution_clock::now();
        pass_wrap_global_stmts(al, asr, pass_options);
        auto t2 = std::chrono::high_resolution_clock::now();
        time_pass_global = std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count();
    }

    {
        auto t1 = std::chrono::high_resolution_clock::now();
        try {
            v.visit_asr((ASR::asr_t &)asr);
        } catch (const CodeGenError &e) {
            diagnostics.diagnostics.push_back(e.d);
            return Error();
        }
        auto t2 = std::chrono::high_resolution_clock::now();
        time_visit_asr = std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count();
    }

    {
        auto t1 = std::chrono::high_resolution_clock::now();
        v.m_a.verify();
        auto t2 = std::chrono::high_resolution_clock::now();
        time_verify = std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count();
    }

    {
        auto t1 = std::chrono::high_resolution_clock::now();
        if (!v.m_a.save_binary(filename)) {
            diagnostics.add(diag::Diagnostic(
                "cannot write the output file '" + filename + "'",
                diag::Level::Error, diag::Stage::CodeGen));
            return Error();
        }
        auto t2 = std::chrono::high_resolution_clock::now();
        time_save = std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count();
    }

    //! Helpful for debugging
    // std::cout << v.m_a.get_asm() << std::endl;

    if (time_report) {
        std::cout << "Codegen Time report:" << std::endl;
        std::cout << "Global:     " << std::setw(5) << time_pass_global << std::endl;
        std::cout << "ASR -> x86: " << std::setw(5) << time_visit_asr << std::endl;
        std::cout << "Verify:     " << std::setw(5) << time_verify << std::endl;
        std::cout << "Save:       " << std::setw(5) << time_save << std::endl;
        int64_t total = time_pass_global + time_visit_asr + time_verify + time_verify + time_save;
        std::cout << "Total:      " << std::setw(5) << total << std::endl;
    }
    return 0;
}

} // namespace LCompilers
