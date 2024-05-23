#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/replace_array_op.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/intrinsic_array_function_registry.h>

#include <vector>
#include <utility>

namespace LCompilers {

class ArrayVarAddressReplacer: public ASR::BaseExprReplacer<ArrayVarAddressReplacer> {

    public:

    Allocator& al;
    Vec<ASR::expr_t**>& vars;

    ArrayVarAddressReplacer(Allocator& al_, Vec<ASR::expr_t**>& vars_):
        al(al_), vars(vars_) {
        call_replacer_on_value = false;
    }

    void replace_Var(ASR::Var_t* x) {
        if( ASRUtils::is_array(ASRUtils::symbol_type(x->m_v)) ) {
            vars.push_back(al, current_expr);
        }
    }

    void replace_StructInstanceMember(ASR::StructInstanceMember_t* x) {
        if( ASRUtils::is_array(ASRUtils::symbol_type(x->m_m)) ) {
            vars.push_back(al, current_expr);
        }
    }

};

class ArrayVarAddressCollector: public ASR::CallReplacerOnExpressionsVisitor<ArrayVarAddressCollector> {

    private:

    ArrayVarAddressReplacer replacer;

    public:

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }

    ArrayVarAddressCollector(Allocator& al_, Vec<ASR::expr_t**>& vars_):
        replacer(al_, vars_) {
    }

};

class ReplaceArrayOp: public ASR::BaseExprReplacer<ReplaceArrayOp> {

    private:

    Allocator& al;
    Vec<ASR::stmt_t*>& pass_result;

    public:

    ASR::expr_t* result_expr;

    ReplaceArrayOp(Allocator& al_, Vec<ASR::stmt_t*>& pass_result_):
        al(al_), pass_result(pass_result_) {}

    void replace_ArrayConstant(ASR::ArrayConstant_t* x) {
        pass_result.reserve(al, x->m_n_data);
        const Location& loc = x->base.base.loc;
        LCOMPILERS_ASSERT(result_expr != nullptr);
        ASR::ttype_t* result_type = ASRUtils::expr_type(result_expr);
        ASR::ttype_t* result_element_type = ASRUtils::type_get_past_array_pointer_allocatable(result_type);
        for( int64_t i = 0; i < ASRUtils::get_fixed_size_of_array(x->m_type); i++ ) {
            ASR::expr_t* x_i = ASRUtils::fetch_ArrayConstant_value(al, x, i);
            Vec<ASR::array_index_t> array_index_args;
            array_index_args.reserve(al, 1);
            ASR::array_index_t array_index_arg;
            array_index_arg.loc = loc;
            array_index_arg.m_left = nullptr;
            array_index_arg.m_right = make_ConstantWithKind(
                make_IntegerConstant_t, make_Integer_t, i + 1, 4, loc);
            array_index_arg.m_step = nullptr;
            array_index_args.push_back(al, array_index_arg);
            ASR::expr_t* y_i = ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
                result_expr, array_index_args.p, array_index_args.size(),
                result_element_type, ASR::arraystorageType::ColMajor, nullptr));
            pass_result.push_back(al, ASRUtils::STMT(ASR::make_Assignment_t(al, loc, y_i, x_i, nullptr)));
        }
    }

    void replace_ArrayConstructor(ASR::ArrayConstructor_t* x) {
        if( !ASRUtils::is_fixed_size_array(x->m_type) ) {
            LCOMPILERS_ASSERT(false);
        }

        pass_result.reserve(al, x->n_args);
        const Location& loc = x->base.base.loc;
        LCOMPILERS_ASSERT(result_expr != nullptr);
        ASR::ttype_t* result_type = ASRUtils::expr_type(result_expr);
        ASR::ttype_t* result_element_type = ASRUtils::type_get_past_array_pointer_allocatable(result_type);
        for( int64_t i = 0; i < ASRUtils::get_fixed_size_of_array(x->m_type); i++ ) {
            ASR::expr_t* x_i = x->m_args[i];
            LCOMPILERS_ASSERT(!ASRUtils::is_array(ASRUtils::expr_type(x_i)));
            Vec<ASR::array_index_t> array_index_args;
            array_index_args.reserve(al, 1);
            ASR::array_index_t array_index_arg;
            array_index_arg.loc = loc;
            array_index_arg.m_left = nullptr;
            array_index_arg.m_right = make_ConstantWithKind(
                make_IntegerConstant_t, make_Integer_t, i + 1, 4, loc);
            array_index_arg.m_step = nullptr;
            array_index_args.push_back(al, array_index_arg);
            ASR::expr_t* y_i = ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
                result_expr, array_index_args.p, array_index_args.size(),
                result_element_type, ASR::arraystorageType::ColMajor, nullptr));
            pass_result.push_back(al, ASRUtils::STMT(ASR::make_Assignment_t(al, loc, y_i, x_i, nullptr)));
        }
    }

};

class FixTypeVisitor: public ASR::BaseWalkVisitor<FixTypeVisitor> {
    private:

    Allocator& al;

    public:

    FixTypeVisitor(Allocator& al_): al(al_) {}

    void visit_Cast(const ASR::Cast_t& x) {
        ASR::Cast_t& xx = const_cast<ASR::Cast_t&>(x);
        if( !ASRUtils::is_array(ASRUtils::expr_type(x.m_arg)) &&
             ASRUtils::is_array(x.m_type) ) {
            xx.m_type = ASRUtils::type_get_past_array(xx.m_type);
            xx.m_value = nullptr;
        }
    }
};

ASR::expr_t* at(Vec<ASR::expr_t*>& vec, int64_t index) {
    index = index + vec.size();
    if( index < 0 ) {
        return nullptr;
    }
    return vec[index];
}

class ArrayOpVisitor: public ASR::CallReplacerOnExpressionsVisitor<ArrayOpVisitor> {
    private:

    Allocator& al;
    ReplaceArrayOp replacer;
    Vec<ASR::stmt_t*> pass_result;

    public:

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }

    ArrayOpVisitor(Allocator& al_): al(al_), replacer(al, pass_result) {
        pass_result.n = 0;
        pass_result.reserve(al, 0);
    }

    void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
        Vec<ASR::stmt_t*> body;
        body.reserve(al, n_body);
        for (size_t i = 0; i < n_body; i++) {
            pass_result.n = 0;
            pass_result.reserve(al, 1);
            visit_stmt(*m_body[i]);
            if( pass_result.size() > 0 ) {
                for (size_t j=0; j < pass_result.size(); j++) {
                    body.push_back(al, pass_result[j]);
                }
            } else {
                body.push_back(al, m_body[i]);
            }
        }
        m_body = body.p;
        n_body = body.size();
    }

    bool call_replace_on_expr(ASR::exprType expr_type) {
        switch( expr_type ) {
            case ASR::exprType::ArrayConstant:
            case ASR::exprType::ArrayConstructor: {
                return true;
            }
            default: {
                return false;
            }
        }
    }

    void increment_index_variables(std::unordered_map<size_t, Vec<ASR::expr_t*>>& var2indices,
                                   size_t var_with_maxrank, int64_t loop_depth,
                                   Vec<ASR::stmt_t*>& do_loop_body, const Location& loc) {
        ASR::expr_t* step = make_ConstantWithKind(make_IntegerConstant_t, make_Integer_t, 1, 4, loc);
        for( size_t i = 0; i < var2indices.size(); i++ ) {
            if( i == var_with_maxrank ) {
                continue;
            }
            ASR::expr_t* index_var = at(var2indices[i], loop_depth);
            if( index_var == nullptr ) {
                continue;
            }
            ASR::expr_t* plus_one = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, index_var,
                ASR::binopType::Add, step, ASRUtils::expr_type(index_var), nullptr));
            ASR::stmt_t* increment = ASRUtils::STMT(ASR::make_Assignment_t(
                al, loc, index_var, plus_one, nullptr));
            do_loop_body.push_back(al, increment);
        }
    }

    void set_index_variables(std::unordered_map<size_t, Vec<ASR::expr_t*>>& var2indices,
                             Vec<ASR::expr_t*>& vars_expr, size_t var_with_maxrank, size_t max_rank,
                             int64_t loop_depth, Vec<ASR::stmt_t*>& dest_vec, const Location& loc) {
        for( size_t i = 0; i < var2indices.size(); i++ ) {
            if( i == var_with_maxrank ) {
                continue;
            }
            ASR::expr_t* index_var = at(var2indices[i], loop_depth);
            if( index_var == nullptr ) {
                continue;
            }
            ASR::expr_t* lbound = PassUtils::get_bound(vars_expr[i],
                loop_depth + max_rank + 1, "lbound", al);
            ASR::stmt_t* set_index_var = ASRUtils::STMT(ASR::make_Assignment_t(
                al, loc, index_var, lbound, nullptr));
            dest_vec.push_back(al, set_index_var);
        }
    }

    template <typename T>
    void generate_loop(const T& x, Vec<ASR::expr_t**>& vars,
                       Vec<const ASR::expr_t*>& fix_types_args,
                       const Location& loc) {
        Vec<size_t> var_ranks;
        Vec<ASR::expr_t*> vars_expr;
        var_ranks.reserve(al, vars.size()); vars_expr.reserve(al, vars.size());
        for( size_t i = 0; i < vars.size(); i++ ) {
            ASR::expr_t* expr = *vars[i];
            ASR::ttype_t* type = ASRUtils::expr_type(expr);
            var_ranks.push_back(al, ASRUtils::extract_n_dims_from_ttype(type));
            vars_expr.push_back(al, expr);
        }

        std::unordered_map<size_t, Vec<ASR::expr_t*>> var2indices;
        ASR::ttype_t* int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
        for( size_t i = 0; i < vars.size(); i++ ) {
            Vec<ASR::expr_t*> indices;
            indices.reserve(al, var_ranks[i]);
            for( size_t j = 0; j < var_ranks[i]; j++ ) {
                std::string index_var_name = current_scope->get_unique_name(
                    "__libasr_index_" + std::to_string(j) + "_");
                ASR::symbol_t* index = ASR::down_cast<ASR::symbol_t>(ASR::make_Variable_t(
                    al, loc, current_scope, s2c(al, index_var_name), nullptr, 0, ASR::intentType::Local,
                    nullptr, nullptr, ASR::storage_typeType::Default, int32_type, nullptr,
                    ASR::abiType::Source, ASR::accessType::Public, ASR::presenceType::Required, false));
                current_scope->add_symbol(index_var_name, index);
                ASR::expr_t* index_expr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, index));
                indices.push_back(al, index_expr);
            }
            var2indices[i] = indices;
        }

        for( size_t i = 0; i < vars.size(); i++ ) {
            Vec<ASR::array_index_t> indices;
            indices.reserve(al, var_ranks[i]);
            for( size_t j = 0; j < var_ranks[i]; j++ ) {
                ASR::array_index_t array_index;
                array_index.loc = loc;
                array_index.m_left = nullptr;
                array_index.m_right = var2indices[i][j];
                array_index.m_step = nullptr;
                indices.push_back(al, array_index);
            }
            ASR::ttype_t* var_i_type = ASRUtils::type_get_past_array_pointer_allocatable(
                ASRUtils::expr_type(*vars[i]));
            *vars[i] = ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc, *vars[i], indices.p,
                indices.size(), var_i_type, ASR::arraystorageType::ColMajor, nullptr));
        }

        FixTypeVisitor fix_types(al);
        for( size_t i = 0; i < fix_types_args.size(); i++ ) {
            fix_types.visit_expr(*fix_types_args[i]);
        }

        size_t var_with_maxrank = 0;
        for( size_t i = 0; i < var_ranks.size(); i++ ) {
            if( var_ranks[i] > var_ranks[var_with_maxrank] ) {
                var_with_maxrank = i;
            }
        }

        ASR::do_loop_head_t do_loop_head;
        do_loop_head.loc = loc;
        do_loop_head.m_v = at(var2indices[var_with_maxrank], -1);
        do_loop_head.m_start = PassUtils::get_bound(vars_expr[var_with_maxrank],
            var_ranks[var_with_maxrank], "lbound", al);
        do_loop_head.m_end = PassUtils::get_bound(vars_expr[var_with_maxrank],
            var_ranks[var_with_maxrank], "ubound", al);
        do_loop_head.m_increment = nullptr;
        Vec<ASR::stmt_t*> parent_do_loop_body; parent_do_loop_body.reserve(al, 1);
        Vec<ASR::stmt_t*> do_loop_body; do_loop_body.reserve(al, 1);
        set_index_variables(var2indices, vars_expr, var_with_maxrank,
                            var_ranks[var_with_maxrank], -1, parent_do_loop_body, loc);
        do_loop_body.push_back(al, const_cast<ASR::stmt_t*>(&(x.base)));
        increment_index_variables(var2indices, var_with_maxrank, -1,
                                  do_loop_body, loc);
        ASR::stmt_t* do_loop = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc, nullptr,
            do_loop_head, do_loop_body.p, do_loop_body.size(), nullptr, 0));
        parent_do_loop_body.push_back(al, do_loop);
        do_loop_body.from_pointer_n_copy(al, parent_do_loop_body.p, parent_do_loop_body.size());
        parent_do_loop_body.reserve(al, 1);

        for( int64_t i = -2; i >= -static_cast<int64_t>(var_ranks[var_with_maxrank]); i-- ) {
            set_index_variables(var2indices, vars_expr, var_with_maxrank,
                                var_ranks[var_with_maxrank], i, parent_do_loop_body, loc);
            increment_index_variables(var2indices, var_with_maxrank, i,
                                      do_loop_body, loc);
            ASR::do_loop_head_t do_loop_head;
            do_loop_head.loc = loc;
            do_loop_head.m_v = at(var2indices[var_with_maxrank], i);
            do_loop_head.m_start = PassUtils::get_bound(vars_expr[var_with_maxrank],
                var_ranks[var_with_maxrank] + i + 1, "lbound", al);
            do_loop_head.m_end = PassUtils::get_bound(vars_expr[var_with_maxrank],
                var_ranks[var_with_maxrank] + i + 1, "ubound", al);
            do_loop_head.m_increment = nullptr;
            ASR::stmt_t* do_loop = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc, nullptr,
                do_loop_head, do_loop_body.p, do_loop_body.size(), nullptr, 0));
            parent_do_loop_body.push_back(al, do_loop);
            do_loop_body.from_pointer_n_copy(al, parent_do_loop_body.p, parent_do_loop_body.size());
            parent_do_loop_body.reserve(al, 1);
        }

        for( size_t i = 0; i < do_loop_body.size(); i++ ) {
            pass_result.push_back(al, do_loop_body[i]);
        }
    }

    void visit_Assignment(const ASR::Assignment_t& x) {
        if( !ASRUtils::is_array(ASRUtils::expr_type(x.m_target)) ) {
            return ;
        }
        ASR::Assignment_t& xx = const_cast<ASR::Assignment_t&>(x);
        xx.m_value = ASRUtils::get_past_array_broadcast(x.m_value);
        const Location loc = x.base.base.loc;
        if( call_replace_on_expr(x.m_value->type) ) {
            replacer.result_expr = x.m_target;
            ASR::expr_t** current_expr_copy = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&x.m_value);
            this->call_replacer();
            current_expr = current_expr_copy;
            replacer.result_expr = nullptr;
            return ;
        }

        Vec<ASR::expr_t**> vars;
        Vec<ASR::expr_t*> vars_expr;
        vars.reserve(al, 1); vars_expr.reserve(al, 1);
        vars.push_back(al, const_cast<ASR::expr_t**>(&(x.m_target)));
        ArrayVarAddressCollector var_collector(al, vars);
        var_collector.current_expr = const_cast<ASR::expr_t**>(&(x.m_value));
        var_collector.call_replacer();

        Vec<const ASR::expr_t*> fix_type_args;
        fix_type_args.reserve(al, 1);
        fix_type_args.push_back(al, x.m_value);

        generate_loop(x, vars, fix_type_args, loc);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t& x) {
        if( !PassUtils::is_elemental(x.m_name) ) {
            return ;
        }
        const Location loc = x.base.base.loc;

        Vec<ASR::expr_t**> vars;
        vars.reserve(al, 1);
        for( size_t i = 0; i < x.n_args; i++ ) {
            if( x.m_args[i].m_value != nullptr &&
                ASRUtils::is_array(ASRUtils::expr_type(x.m_args[i].m_value)) ) {
                vars.push_back(al, &(x.m_args[i].m_value));
            }
        }

        Vec<const ASR::expr_t*> fix_type_args;
        fix_type_args.reserve(al, 1);

        generate_loop(x, vars, fix_type_args, loc);
    }

};

void pass_replace_array_op(Allocator &al, ASR::TranslationUnit_t &unit,
                           const LCompilers::PassOptions& /*pass_options*/) {
    ArrayOpVisitor v(al);
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}


} // namespace LCompilers
