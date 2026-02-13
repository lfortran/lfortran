#include <string>
#include <cmath>
#include <cstring>
#include <set>
#include <unordered_set>

#include <lfortran/ast.h>
#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/exception.h>
#include <lfortran/semantics/asr_implicit_cast_rules.h>
#include <lfortran/semantics/ast_common_visitor.h>
#include <lfortran/semantics/ast_to_asr.h>
#include <lfortran/parser/parser_stype.h>
#include <libasr/string_utils.h>
#include <lfortran/utils.h>
#include <libasr/pass/instantiate_template.h>

namespace LCompilers::LFortran {

class BodyVisitor : public CommonVisitor<BodyVisitor> {
private:

    // The Fortran standard allows the stop code to be a default integer,
    // but the runtime ABI expects a 32-bit (kind=4) integer. When
    // -fdefault-integer-8 is active, integer literals become kind=8.
    // This helper inserts an explicit Cast to kind=4 when needed.
    ASR::expr_t* cast_stop_code_to_int32(ASR::expr_t* code,
                                         const Location& loc) {
        if (code && ASR::is_a<ASR::Integer_t>(
                *ASRUtils::expr_type(code))) {
            int kind = ASRUtils::extract_kind_from_ttype_t(
                ASRUtils::expr_type(code));
            if (kind != 4) {
                ASR::ttype_t* int32_type = ASRUtils::TYPE(
                    ASR::make_Integer_t(al, loc, 4));
                code = CastingUtil::perform_casting(
                    code, int32_type, al, loc);
            }
        }
        return code;
    }

public:
    ASR::asr_t *asr;
    bool from_block;
    bool needs_implicit_interface_postprocessing = false;
    std::set<std::string> labels;
    size_t starting_n_body = 0;
    int loop_nesting = 0;
    int all_loops_blocks_nesting = 0;
    int all_blocks_nesting = 0;
    int pragma_nesting_level = 0;
    int pragma_nesting_level_2 = 0;
    bool openmp_collapse = false;
    bool pragma_in_block=false;
    bool do_in_pragma=false;
    int nesting_lvl_inside_pragma=0;
    int collapse_value=0;
    Vec<ASR::do_loop_head_t> do_loop_heads_for_collapse;
    Vec<ASR::stmt_t*> do_loop_bodies_for_collapse;
    AST::stmt_t **starting_m_body = nullptr;
    std::vector<ASR::symbol_t*> do_loop_variables;
    std::map<ASR::asr_t*, std::pair<const AST::stmt_t*,int64_t>> print_statements;
    std::vector<ASR::DoConcurrentLoop_t *> omp_constructs;
    std::vector<ASR::stmt_t*> omp_region_body={};
    bool is_first_section=false;
    int program_count = 0;

    BodyVisitor(Allocator &al, ASR::asr_t *unit, diag::Diagnostics &diagnostics,
        CompilerOptions &compiler_options,
        std::map<uint64_t, std::map<std::string, ASR::ttype_t*>> &implicit_mapping,
        std::map<uint64_t, ASR::symbol_t*>& common_variables_hash,
        std::map<uint64_t, size_t>& common_variables_byte_offset,
        std::map<uint64_t, std::vector<std::string>>& external_procedures_mapping,
        std::map<uint64_t, std::vector<std::string>>& explicit_intrinsic_procedures_mapping,
        std::map<uint32_t, std::map<std::string, std::pair<ASR::ttype_t*, ASR::symbol_t*>>> &instantiate_types,
        std::map<uint32_t, std::map<std::string, ASR::symbol_t*>> &instantiate_symbols,
        std::map<std::string, std::map<std::string, std::vector<AST::stmt_t*>>> &entry_functions,
        std::map<std::string, std::vector<int>> &entry_function_arguments_mapping,
        std::map<uint32_t, std::vector<ASR::stmt_t*>> &data_structure,
        LCompilers::LocationManager &lm
    ) : CommonVisitor(
            al, nullptr, diagnostics, compiler_options, implicit_mapping,
            common_variables_hash, common_variables_byte_offset,
            external_procedures_mapping,
            explicit_intrinsic_procedures_mapping, instantiate_types,
            instantiate_symbols, entry_functions, entry_function_arguments_mapping,
            data_structure, lm
        ), asr{unit}, from_block{false} {}

    void visit_Declaration(const AST::Declaration_t& x) {
        if( from_block ) {
            visit_DeclarationUtil(x);
        }
    }

    void visit_Block(const AST::Block_t &x) {
        all_loops_blocks_nesting++;
        from_block = true;
        SymbolTable *parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);
        ASR::asr_t* block;
        std::string name;
        if (x.m_stmt_name) {
            name = std::string(x.m_stmt_name);
            block = ASR::make_Block_t(al, x.base.base.loc,
                current_scope, x.m_stmt_name, nullptr, 0);
        } else {
            // TODO: Understand tests/block1.f90 to know if this is needed, otherwise
            // it might be possible to allow x.m_stmt_name to be nullptr
            name = parent_scope->get_unique_name("block");
            block = ASR::make_Block_t(al, x.base.base.loc,
                current_scope, s2c(al, name), nullptr, 0);
        }
        ASR::Block_t* block_t = ASR::down_cast<ASR::Block_t>(
            ASR::down_cast<ASR::symbol_t>(block));

        for (size_t i=0; i<x.n_use; i++) {
            visit_unit_decl1(*x.m_use[i]);
        }
        for (size_t i=0; i<x.n_decl; i++) {
            visit_unit_decl2(*x.m_decl[i]);
        }

        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body);
        transform_stmts(body, x.n_body, x.m_body);
        block_t->m_body = body.p;
        block_t->n_body = body.size();
        current_scope = parent_scope;
        current_scope->add_symbol(name, ASR::down_cast<ASR::symbol_t>(block));
        tmp = ASR::make_BlockCall_t(al, x.base.base.loc,  -1,
                                    ASR::down_cast<ASR::symbol_t>(block));
        from_block = false;
        all_loops_blocks_nesting--;
    }

    // Transforms statements to a list of ASR statements
    // In addition, it also inserts the following nodes if needed:
    //   * ImplicitDeallocate
    //   * GoToTarget
    // The `body` Vec must already be reserved
    void transform_stmts(Vec<ASR::stmt_t*> &body, size_t n_body, AST::stmt_t **m_body) {
        tmp = nullptr;
        Vec<ASR::stmt_t*>* current_body_copy = current_body;
        current_body = &body;
        for (size_t i=0; i<n_body; i++) {
            // If there is a label, create a GoToTarget node first
            int64_t label = stmt_label(m_body[i]);
            if (label != 0) {
                ASR::asr_t *l = ASR::make_GoToTarget_t(al, m_body[i]->base.loc, label,
                                    s2c(al, std::to_string(label)));
                body.push_back(al, ASR::down_cast<ASR::stmt_t>(l));
            }
            // Visit the statement
            LCOMPILERS_ASSERT(current_body != nullptr)
            try {
                this->visit_stmt(*m_body[i]);
            } catch (const SemanticAbort &a) {
                if (!compiler_options.continue_compilation) {
                    throw a;
                } else {
                    tmp = nullptr;
                    tmp_vec.clear();
                }
            }
            if((all_blocks_nesting ==0 || pragma_in_block) && !do_in_pragma && !omp_region_body.empty() && !(m_body[i]->type == AST::stmtType::Pragma && AST::down_cast<AST::Pragma_t>(m_body[i])->m_type == AST::OMPPragma)) {
                ASR::stmt_t* tmp_stmt = ASRUtils::STMT(tmp);
                omp_region_body.push_back(tmp_stmt);
            } else {
                if (tmp) {
                    ASR::stmt_t* tmp_stmt = ASRUtils::STMT(tmp);
                    if (tmp_stmt->type == ASR::stmtType::SubroutineCall) {
                        ASR::stmt_t* impl_decl = create_implicit_deallocate_subrout_call(tmp_stmt);
                        if (impl_decl) body.push_back(al, impl_decl);
                    }
                    body.push_back(al, tmp_stmt);
                } else if (!tmp_vec.empty()) {
                    for (auto &x : tmp_vec) body.push_back(al, ASRUtils::STMT(x));
                    tmp_vec.clear();
                }
                tmp = nullptr;
            }
            // To avoid last statement to be entered twice once we exit this node
            tmp = nullptr;
        }
        current_body = current_body_copy;
    }

    void visit_TranslationUnit(const AST::TranslationUnit_t &x) {
        program_count = 0;
        ASR::TranslationUnit_t *unit = ASR::down_cast2<ASR::TranslationUnit_t>(asr);
        current_scope = unit->m_symtab;
        Vec<ASR::asr_t*> items;
        items.reserve(al, x.n_items);
        for (size_t i=0; i<x.n_items; i++) {
            tmp = nullptr;
            try {
                visit_ast(*x.m_items[i]);
            } catch (const SemanticAbort &a) {
                if (!compiler_options.continue_compilation) {
                    throw a;
                } else {
                    tmp = nullptr;
                    tmp_vec.clear();
                }
            }
            if (tmp) {
                items.push_back(al, tmp);
            } else if (!tmp_vec.empty()) {
                for (auto &t: tmp_vec) {
                    items.push_back(al, t);
                }
                tmp_vec.clear();
            }
        }
        unit->m_items = items.p;
        unit->n_items = items.size();
    }

    template <typename T>
    void process_format_statement(ASR::asr_t *old_tmp, int &label, Allocator &al, std::map<int64_t, std::string> &format_statements) {
        T *old_stmt = ASR::down_cast<T>(ASRUtils::STMT(old_tmp));  
        ASR::ttype_t *fmt_type = ASRUtils::TYPE(ASR::make_String_t(
            al, old_stmt->base.base.loc, 1,
            ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, old_stmt->base.base.loc, format_statements[label].size(),
                ASRUtils::TYPE(ASR::make_Integer_t(al, old_stmt->base.base.loc, 4)))),
                ASR::string_length_kindType::ExpressionLength,
            ASR::string_physical_typeType::DescriptorString));
        ASR::expr_t *fmt_constant = ASRUtils::EXPR(ASR::make_StringConstant_t(
            al, old_stmt->base.base.loc, s2c(al, format_statements[label]), fmt_type));
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Allocatable_t(al, old_stmt->base.base.loc,
                ASRUtils::TYPE(ASR::make_String_t(
                    al, old_stmt->base.base.loc, 1, nullptr,
                    ASR::string_length_kindType::DeferredLength,
                    ASR::string_physical_typeType::DescriptorString))));
        ASR::expr_t *string_format = ASRUtils::EXPR(ASRUtils::make_StringFormat_t_util(al, old_stmt->base.base.loc,
            fmt_constant, old_stmt->m_values, old_stmt->n_values, ASR::string_format_kindType::FormatFortran,
            type, nullptr));
        Vec<ASR::expr_t *> print_args;
        print_args.reserve(al, 1);
        print_args.push_back(al, string_format);
        old_stmt->m_values = print_args.p;
        old_stmt->n_values = print_args.size();
    }

    void handle_format() {
        for(auto it = print_statements.begin(); it != print_statements.end(); it++) {
            ASR::asr_t* old_tmp = it->first;
            const AST::stmt_t* x = it->second.first;
            int label = it->second.second;
            if (format_statements.find(label) == format_statements.end()) {
                diag.semantic_error_label("The label " + std::to_string(label) + " does not point to any format statement",
                 {x->base.loc},"Invalid label in this statement");
            } else {
                // To Do :: after refactoring write, change process_format_statement() to tackle stringformat instead.
                if (AST::is_a<AST::Print_t>(*x)) {
                    ASR::Print_t* print_stmt = ASR::down_cast2<ASR::Print_t>(old_tmp);
                    if(ASR::is_a<ASR::StringFormat_t>(*print_stmt->m_text)){
                        ASR::StringFormat_t* string_fmt_arg = ASR::down_cast<ASR::StringFormat_t>(
                            print_stmt->m_text);
                        ASR::ttype_t *fmt_type = ASRUtils::TYPE(ASR::make_String_t(
                            al, print_stmt->base.base.loc, 1,
                            ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, print_stmt->base.base.loc, format_statements[label].size(),
                            ASRUtils::TYPE(ASR::make_Integer_t(al, print_stmt->base.base.loc, 4)))),
                            ASR::string_length_kindType::ExpressionLength,
                            ASR::string_physical_typeType::DescriptorString));
                        ASR::expr_t *fmt_constant = ASRUtils::EXPR(ASR::make_StringConstant_t(
                            al, print_stmt->base.base.loc, s2c(al, format_statements[label]), fmt_type));
                        string_fmt_arg->m_fmt = fmt_constant;
                    }
                } else if (AST::is_a<AST::Write_t>(*x)) {
                    process_format_statement<ASR::FileWrite_t>(old_tmp, label, al, format_statements);
                } else if (AST::is_a<AST::Read_t>(*x)) {
                    process_format_statement<ASR::FileRead_t>(old_tmp, label, al, format_statements);
                }
            }
        }
        format_statements.clear();
        print_statements.clear();
    }

    void visit_Open(const AST::Open_t& x) {
        ASR::expr_t *a_newunit = nullptr, *a_filename = nullptr, *a_status = nullptr, *a_form = nullptr,
            *a_access = nullptr, *a_iostat = nullptr, *a_iomsg = nullptr, *a_action = nullptr, *a_delim = nullptr,
            *a_recl = nullptr, *a_position = nullptr, *a_blank = nullptr, *a_encoding = nullptr, *a_sign = nullptr;
        if( x.n_args > 1 ) {
            diag.add(Diagnostic(
                "Number of arguments cannot be more than 1 in Open statement.",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }
        if( x.n_args == 1 ) {
            this->visit_expr(*x.m_args[0]);
            a_newunit = ASRUtils::EXPR(tmp);
        }
        for( std::uint32_t i = 0; i < x.n_kwargs; i++ ) {
            AST::keyword_t kwarg = x.m_kwargs[i];
            std::string m_arg_str(kwarg.m_arg);
            m_arg_str = to_lower(m_arg_str);
            if( m_arg_str == std::string("newunit") ||
                m_arg_str == std::string("unit") ) {
                if( a_newunit != nullptr ) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `unit` found, `unit` has already been specified via argument or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                this->visit_expr(*kwarg.m_value);
                a_newunit = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_newunit_type = ASRUtils::expr_type(a_newunit);
                if( ( m_arg_str == std::string("newunit") &&
                     !ASRUtils::is_variable(a_newunit) ) ||
                    ( !ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(a_newunit_type))
                    ) ) {
                        diag.add(Diagnostic(
                            "`newunit`/`unit` must be a mutable variable of type, Integer or IntegerPointer",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.base.base.loc})
                            }));
                        throw SemanticAbort();
                }

                if ( m_arg_str == std::string("newunit") ) {
                    Vec<AST::fnarg_t> args;
                    args.reserve(al, 0);
                    AST::fnarg_t arg;
                    arg.loc = x.base.base.loc;
                    arg.m_label = 0;
                    arg.m_start = nullptr;
                    arg.m_step = nullptr;
                    arg.m_end = kwarg.m_value;
                    args.push_back(al, arg);
                    AST::SubroutineCall_t* subrout_call = AST::down_cast2<AST::SubroutineCall_t>(
                        AST::make_SubroutineCall_t(al, x.base.base.loc, 0, s2c(al, "newunit"), nullptr, 0, args.p, args.size(), nullptr, 0, nullptr, 0, nullptr));
                    visit_SubroutineCall(*subrout_call);
                    tmp_vec.push_back(tmp);
                }
            } else if( m_arg_str == std::string("file") ) {
                if( a_filename != nullptr ) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `file` found, unit has already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                this->visit_expr(*kwarg.m_value);
                a_filename = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_filename_type = ASRUtils::expr_type(a_filename);
                if (!ASRUtils::is_character(*a_filename_type)) {
                        diag.add(Diagnostic(
                            "`file` must be of type, String or StringPointer",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.base.base.loc})
                            }));
                        throw SemanticAbort();
                }

                if (ASR::is_a<ASR::StringConstant_t>(*a_filename)) {
                    std::string str = std::string(ASR::down_cast<ASR::StringConstant_t>(a_filename)->m_s);
                    rtrim(str);
                    a_filename = ASRUtils::EXPR(ASR::make_StringConstant_t(
                                al, x.base.base.loc, s2c(al, str),
                                ASRUtils::TYPE(ASR::make_String_t(
                                    al, a_filename->base.loc, 1,
                                    ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, a_filename->base.loc, str.length(),
                                        ASRUtils::TYPE(ASR::make_Integer_t(al, a_filename->base.loc, 4)))),
                                    ASR::string_length_kindType::ExpressionLength,
                                    ASR::string_physical_typeType::DescriptorString))));
                }
            } else if( m_arg_str == std::string("status") ) {
                if( a_status != nullptr ) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `status` found, unit has already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                this->visit_expr(*kwarg.m_value);
                a_status = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_status_type = ASRUtils::expr_type(a_status);
                if (!ASRUtils::is_character(*a_status_type)) {
                        diag.add(Diagnostic(
                            "`status` must be of type, String or StringPointer",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.base.base.loc})
                            }));
                        throw SemanticAbort();
                }
                if (ASR::is_a<ASR::StringConstant_t>(*a_status)) {
                    std::string str = std::string(ASR::down_cast<ASR::StringConstant_t>(a_status)->m_s);
                    rtrim(str);
                    a_status = ASRUtils::EXPR(ASR::make_StringConstant_t(
                                al, x.base.base.loc, s2c(al, str),
                                ASRUtils::TYPE(ASR::make_String_t(
                                    al, a_status->base.loc, 1,
                                    ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, a_status->base.loc, str.length(),
                                        ASRUtils::TYPE(ASR::make_Integer_t(al, a_status->base.loc, 4)))),
                                    ASR::string_length_kindType::ExpressionLength,
                                    ASR::string_physical_typeType::DescriptorString))));
                }
            } else if( m_arg_str == std::string("form") ) {
                if ( a_form != nullptr ) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `form` found, unit has already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();

                }
                this->visit_expr(*kwarg.m_value);
                a_form = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_form_type = ASRUtils::expr_type(a_form);
                if (!ASRUtils::is_character(*a_form_type)) {
                    diag.add(Diagnostic(
                        "`form` must be of type, String or StringPointer",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                if (ASR::is_a<ASR::StringConstant_t>(*a_form)) {
                    std::string str = std::string(ASR::down_cast<ASR::StringConstant_t>(a_form)->m_s);
                    rtrim(str);
                    a_form = ASRUtils::EXPR(ASR::make_StringConstant_t(
                                al, x.base.base.loc, s2c(al, str),
                                ASRUtils::TYPE(ASR::make_String_t(
                                    al, a_form->base.loc, 1,
                                    ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, a_form->base.loc, str.length(),
                                        ASRUtils::TYPE(ASR::make_Integer_t(al, a_form->base.loc, 4)))),
                                    ASR::string_length_kindType::ExpressionLength,
                                    ASR::string_physical_typeType::DescriptorString))));
                }
            } else if( m_arg_str == std::string("access") ) {  //TODO: Handle 'direct' as access argument
                if ( a_access != nullptr ) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `access` found, unit has already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();

                }
                this->visit_expr(*kwarg.m_value);
                a_access = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_access_type = ASRUtils::expr_type(a_access);
                if (!ASRUtils::is_character(*a_access_type)) {
                    diag.add(Diagnostic(
                        "`access` must be of type, String or StringPointer",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
            } else if( m_arg_str == std::string("iostat") ) {
                if ( a_iostat != nullptr ) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `iostat` found, unit has already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                this->visit_expr(*kwarg.m_value);
                a_iostat = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_iostat_type = ASRUtils::expr_type(a_iostat);
                if (!ASRUtils::is_variable(a_iostat)) {
                    diag.add(Diagnostic(
                        "Non-variable expression for `iostat`",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                if (!ASRUtils::is_integer(*a_iostat_type)) {
                    diag.add(Diagnostic(
                        "`iostat` must be of type, integer or integer*",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
            } else if (m_arg_str == std::string("iomsg")) {
                if( a_iomsg != nullptr ) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `iomsg` found, it has already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                this->visit_expr(*kwarg.m_value);
                a_iomsg = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_iomsg_type = ASRUtils::expr_type(a_iomsg);
                if (!ASRUtils::is_variable(a_iomsg)) {
                    diag.add(Diagnostic(
                        "Non-variable expression for `iomsg`",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                if (!ASR::is_a<ASR::String_t>(*ASRUtils::type_get_past_allocatable_pointer(a_iomsg_type))) {
                    diag.add(Diagnostic(
                        "`iomsg` must be of type, String",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
            } else if (m_arg_str == std::string("action")) {
                if (a_action != nullptr) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `action` found, it has already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                this->visit_expr(*kwarg.m_value);
                a_action = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_action_type = ASRUtils::expr_type(a_action);
                if (!ASRUtils::is_character(*a_action_type)) {
                    diag.add(Diagnostic(
                        "`action` must be of type, String or StringPointer",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
            } else if (m_arg_str == std::string("delim")) {
                if (a_delim != nullptr) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `delim` found, it has already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                this->visit_expr(*kwarg.m_value);
                a_delim = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_delim_type = ASRUtils::expr_type(a_delim);
                if (!ASRUtils::is_character(*a_delim_type)) {
                    diag.add(Diagnostic(
                        "`delim` must be of type, String or StringPointer",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
            } else if (m_arg_str == std::string("position")) {
                if (a_position != nullptr) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `position` found, it has already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                this->visit_expr(*kwarg.m_value);
                a_position = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_position_type = ASRUtils::expr_type(a_position);
                if (!ASRUtils::is_character(*a_position_type)) {
                    diag.add(Diagnostic(
                        "`position` must be of type, String or StringPointer",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
            } else if (m_arg_str == std::string("recl")) {
                if (a_recl != nullptr) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `recl` found, it has already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                this->visit_expr(*kwarg.m_value);
                a_recl = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_recl_type = ASRUtils::expr_type(a_recl);
                if (!ASRUtils::is_integer(*a_recl_type)) {
                    diag.add(Diagnostic(
                        "`recl` must be of type, Integer",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
            } else if (m_arg_str == std::string("blank")) {
                if  (a_blank != nullptr) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `blank` found, it has already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                this->visit_expr(*kwarg.m_value);
                a_blank = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_blank_type = ASRUtils::expr_type(a_blank);
                if (!ASRUtils::is_character(*a_blank_type)) {
                    diag.add(Diagnostic(
                        "`blank` must be of type, String or StringPointer",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
            }
            else if (m_arg_str == std::string("encoding")) {
                if (a_encoding != nullptr) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `encoding` found)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }

                this->visit_expr(*kwarg.m_value);
                a_encoding = ASRUtils::EXPR(tmp);

                ASR::ttype_t *a_encoding_type = ASRUtils::expr_type(a_encoding);
                if (!ASRUtils::is_character(*a_encoding_type)) {
                    diag.add(Diagnostic(
                        "`encoding` must be of type, String or StringPointer",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                if (ASR::is_a<ASR::StringConstant_t>(*a_encoding)) {
                    std::string str = std::string(ASR::down_cast<ASR::StringConstant_t>(a_encoding)->m_s);
                    rtrim(str);
                    a_encoding = ASRUtils::EXPR(
                        ASR::make_StringConstant_t(
                            al, x.base.base.loc, s2c(al, str),
                            ASRUtils::TYPE(
                                ASR::make_String_t(
                                    al, a_encoding->base.loc, 1,
                                    ASRUtils::EXPR(
                                        ASR::make_IntegerConstant_t(
                                            al, a_encoding->base.loc, str.length(),
                                            ASRUtils::TYPE(
                                                ASR::make_Integer_t(al, a_encoding->base.loc, 4)
                                            )
                                        )
                                    ),
                                    ASR::string_length_kindType::ExpressionLength,
                                    ASR::string_physical_typeType::DescriptorString
                                )
                            )
                        )
                    );
                }
            } else if (m_arg_str == std::string("sign")) {
                if (a_sign != nullptr) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `sign` found)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }

                this->visit_expr(*kwarg.m_value);
                a_sign = ASRUtils::EXPR(tmp);

                ASR::ttype_t *a_sign_type = ASRUtils::expr_type(a_sign);
                if (!ASRUtils::is_character(*a_sign_type)) {
                    diag.add(Diagnostic(
                        "`sign` must be of type, String or StringPointer",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                if (ASR::is_a<ASR::StringConstant_t>(*a_sign)) {
                    std::string str = std::string(ASR::down_cast<ASR::StringConstant_t>(a_sign)->m_s);
                    rtrim(str);
                    a_sign = ASRUtils::EXPR(
                        ASR::make_StringConstant_t(
                            al, x.base.base.loc, s2c(al, str),
                            ASRUtils::TYPE(
                                ASR::make_String_t(
                                    al, a_sign->base.loc, 1,
                                    ASRUtils::EXPR(
                                        ASR::make_IntegerConstant_t(
                                            al, a_sign->base.loc, str.length(),
                                            ASRUtils::TYPE(
                                                ASR::make_Integer_t(al, a_sign->base.loc, 4)
                                            )
                                        )
                                    ),
                                    ASR::string_length_kindType::ExpressionLength,
                                    ASR::string_physical_typeType::DescriptorString
                                )
                            )
                        )
                    );
                }
            }
            else {
                const std::unordered_set<std::string> unsupported_args {"err", "fileopt", "pad"};
                if (unsupported_args.find(m_arg_str) == unsupported_args.end()) {
                    diag.add(diag::Diagnostic("Invalid argument `" + m_arg_str + "` supplied",
                        diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("", {x.base.base.loc})}));
                    throw SemanticAbort();
                } else {
                    diag.semantic_warning_label(
                        "Argument `" + m_arg_str + "` isn't supported yet",
                        {x.base.base.loc},
                        "ignored for now"
                    );
                }
            }
        }
        if (a_recl != nullptr &&
            a_access != nullptr &&
            ASR::is_a<ASR::StringConstant_t>(*a_access)) {

            std::string acc =
                to_lower(ASR::down_cast<ASR::StringConstant_t>(a_access)->m_s);

            if (acc != "direct") {
                diag.add(Diagnostic(
                    "`recl` is only permitted with access='direct'",
                    Level::Error, Stage::Semantic,
                    { Label("", {x.base.base.loc}) }
                ));
                throw SemanticAbort();
            }
        }
        if( a_newunit == nullptr ) {
            diag.add(Diagnostic(
                "`newunit` or `unit` must be specified either in argument or keyword arguments.",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }
        tmp = ASR::make_FileOpen_t(
            al, x.base.base.loc, x.m_label, a_newunit, a_filename, a_status, a_form, a_access, a_iostat, a_iomsg, a_action, a_delim, a_recl, a_position, a_blank, a_encoding, a_sign);
        tmp_vec.push_back(tmp);
        tmp = nullptr;
    }

    void visit_Close(const AST::Close_t& x) {
        ASR::expr_t *a_unit = nullptr, *a_iostat = nullptr, *a_iomsg = nullptr;
        ASR::expr_t *a_err = nullptr, *a_status = nullptr;
        if( x.n_args > 1 ) {
            diag.add(Diagnostic(
                "Number of arguments cannot be more than 1 in Close statement.",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }
        if( x.n_args == 1 ) {
            this->visit_expr(*x.m_args[0]);
            a_unit = ASRUtils::EXPR(tmp);
        }
        for( std::uint32_t i = 0; i < x.n_kwargs; i++ ) {
            AST::keyword_t kwarg = x.m_kwargs[i];
            std::string m_arg_str(kwarg.m_arg);
            m_arg_str = to_lower(m_arg_str);
            if( m_arg_str == std::string("unit") ) {
                if( a_unit != nullptr ) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `unit` found, `unit` has already been specified via argument or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                this->visit_expr(*kwarg.m_value);
                a_unit = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_newunit_type = ASRUtils::expr_type(a_unit);
                if (!ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(a_newunit_type))) {
                        diag.add(Diagnostic(
                            "`unit` must be of type, Integer or IntegerPointer",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.base.base.loc})
                            }));
                        throw SemanticAbort();
                }
            } else if( m_arg_str == std::string("iostat") ) {
                if( a_iostat != nullptr ) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `iostat` found, unit has already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                this->visit_expr(*kwarg.m_value);
                a_iostat = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_iostat_type = ASRUtils::expr_type(a_iostat);
                if( a_iostat->type != ASR::exprType::Var ||
                    (!ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(a_iostat_type))) ) {
                        diag.add(Diagnostic(
                            "`iostat` must be a variable of type, Integer or IntegerPointer",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.base.base.loc})
                            }));
                        throw SemanticAbort();
                }
            } else if( m_arg_str == std::string("iomsg") ) {
                if( a_iomsg != nullptr ) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `iomsg` found, unit has already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                this->visit_expr(*kwarg.m_value);
                a_iomsg = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_iomsg_type = ASRUtils::expr_type(a_iomsg);
                if( a_iomsg->type != ASR::exprType::Var ||
                    (!ASRUtils::is_character(*a_iomsg_type)) ) {
                        diag.add(Diagnostic(
                            "`iomsg` must be of type, String or StringPointer",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.base.base.loc})
                            }));
                        throw SemanticAbort();
                    }
            } else if( m_arg_str == std::string("status") ) {
                if( a_status != nullptr ) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `status` found, unit has already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                this->visit_expr(*kwarg.m_value);
                a_status = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_status_type = ASRUtils::expr_type(a_status);
                if (!ASRUtils::is_character(*a_status_type)) {
                    diag.add(Diagnostic(
                        "`status` must be of type, String or StringPointer",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
            } else if( m_arg_str == std::string("err") ) {
                if( a_err != nullptr ) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `err` found, `err` has already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                if( kwarg.m_value->type != AST::exprType::Num ) {
                    diag.add(Diagnostic(
                        "`err` must be a literal integer",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                this->visit_expr(*kwarg.m_value);
                a_err = ASRUtils::EXPR(tmp);
            } else {
                diag.add(diag::Diagnostic("Invalid argument `" + m_arg_str + "` supplied",
                        diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("", {x.base.base.loc})}));
                throw SemanticAbort();
            }
        }
        if( a_unit == nullptr ) {
            diag.add(Diagnostic(
                "`newunit` or `unit` must be specified either in argument or keyword arguments.",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }
        tmp = ASR::make_FileClose_t(al, x.base.base.loc, x.m_label, a_unit, a_iostat, a_iomsg, a_err, a_status);
    }

    void visit_Backspace(const AST::Backspace_t& x) {
        ASR::expr_t *a_unit = nullptr, *a_iostat = nullptr;
        ASR::expr_t *a_err = nullptr;
        if( x.n_args > 1 ) {
            diag.add(Diagnostic(
                "Number of arguments cannot be more than 1 in Backspace statement.",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }
        if( x.n_args == 1 ) {
            this->visit_expr(*x.m_args[0]);
            a_unit = ASRUtils::EXPR(tmp);
        }
        for( std::uint32_t i = 0; i < x.n_kwargs; i++ ) {
            AST::keyword_t kwarg = x.m_kwargs[i];
            std::string m_arg_str(kwarg.m_arg);
            m_arg_str = to_lower(m_arg_str);
            if( m_arg_str == std::string("unit") ) {
                if( a_unit != nullptr ) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `unit` found, `unit` "
                        " has already been specified via argument or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                this->visit_expr(*kwarg.m_value);
                a_unit = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_newunit_type = ASRUtils::expr_type(a_unit);
                if (!ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(a_newunit_type))) {
                        diag.add(Diagnostic(
                            "`unit` must be of type, Integer or IntegerPointer",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.base.base.loc})
                            }));
                        throw SemanticAbort();
                }
            } else if( m_arg_str == std::string("iostat") ) {
                if( a_iostat != nullptr ) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `iostat` found, unit has "
                        " already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                this->visit_expr(*kwarg.m_value);
                a_iostat = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_iostat_type = ASRUtils::expr_type(a_iostat);
                if( a_iostat->type != ASR::exprType::Var ||
                    (!ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(a_iostat_type))) ) {
                        diag.add(Diagnostic(
                            "`iostat` must be a variable of type, Integer "
                            " or IntegerPointer",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.base.base.loc})
                            }));
                        throw SemanticAbort();
                }
            } else if( m_arg_str == std::string("err") ) {
                if( a_err != nullptr ) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `err` found, `err` has "
                        " already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                if( kwarg.m_value->type != AST::exprType::Num ) {
                    diag.add(Diagnostic(
                        "`err` must be a literal integer",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                this->visit_expr(*kwarg.m_value);
                a_err = ASRUtils::EXPR(tmp);
            } else {
                diag.add(diag::Diagnostic("Invalid argument `" + m_arg_str + "` supplied",
                        diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("", {x.base.base.loc})}));
                throw SemanticAbort();
            }
        }
        if( a_unit == nullptr ) {
            diag.add(Diagnostic(
                "`unit` must be specified either in argument or keyword arguments.",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }
        tmp = ASR::make_FileBackspace_t(al, x.base.base.loc, x.m_label, a_unit, a_iostat, a_err);
    }

    // Expand ImpliedDoLoop for READ statements into individual elements or array section.
    // For constant bounds: (vals(i), i=1,3) -> vals(1), vals(2), vals(3)
    // For variable bounds: (vals(i), i=1,n) -> vals(1:n)
    void expand_implied_do_for_read(ASR::ImpliedDoLoop_t* idl, Vec<ASR::expr_t*>& out) {
        int64_t start_val, end_val, inc_val = 1;
        bool constant_bounds = true;

        // Check if bounds are constant
        if (!ASRUtils::is_value_constant(idl->m_start) ||
            !ASRUtils::is_value_constant(idl->m_end)) {
            constant_bounds = false;
        } else if (!ASRUtils::extract_value(idl->m_start, start_val) ||
                   !ASRUtils::extract_value(idl->m_end, end_val)) {
            constant_bounds = false;
        }
        if (idl->m_increment) {
            if (!ASRUtils::is_value_constant(idl->m_increment) ||
                !ASRUtils::extract_value(idl->m_increment, inc_val)) {
                constant_bounds = false;
            }
        }

        if (constant_bounds && inc_val != 0) {
            // Expand to individual elements
            ASR::Var_t* loop_var = ASR::down_cast<ASR::Var_t>(idl->m_var);
            for (int64_t idx = start_val;
                 (inc_val > 0) ? (idx <= end_val) : (idx >= end_val);
                 idx += inc_val) {
                ASR::ttype_t* int_type = ASRUtils::TYPE(
                    ASR::make_Integer_t(al, idl->m_var->base.loc, 4));
                ASR::expr_t* idx_const = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                    al, idl->m_var->base.loc, idx, int_type));
                for (size_t j = 0; j < idl->n_values; j++) {
                    ASR::expr_t* value = idl->m_values[j];
                    if (ASR::is_a<ASR::ImpliedDoLoop_t>(*value)) {
                        ASR::ImpliedDoLoop_t* inner_idl =
                            ASR::down_cast<ASR::ImpliedDoLoop_t>(value);
                        ASR::ImpliedDoLoop_t* substituted_idl =
                            substitute_loop_var_in_idl(inner_idl, loop_var, idx_const);
                        expand_implied_do_for_read(substituted_idl, out);
                    } else if (ASR::is_a<ASR::ArrayItem_t>(*value)) {
                        // Replace loop variable with constant index
                        ASR::ArrayItem_t* arr_item = ASR::down_cast<ASR::ArrayItem_t>(value);
                        ASR::expr_t* new_item = substitute_loop_var_in_array_item(
                            arr_item, loop_var, idx_const);
                        out.push_back(al, new_item);
                    } else {
                        out.push_back(al, value);
                    }
                }
            }
        } else {
            out.push_back(al, ASRUtils::EXPR((ASR::asr_t*)idl));
        }
    }

    ASR::expr_t* substitute_loop_var_in_array_item(
            ASR::ArrayItem_t* arr_item, ASR::Var_t* loop_var,
            ASR::expr_t* idx_const) {
        Vec<ASR::array_index_t> new_args;
        new_args.reserve(al, arr_item->n_args);
        for (size_t k = 0; k < arr_item->n_args; k++) {
            ASR::array_index_t arg = arr_item->m_args[k];
            if (arg.m_right && ASR::is_a<ASR::Var_t>(*arg.m_right)) {
                ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(arg.m_right);
                if (var->m_v == loop_var->m_v) {
                    arg.m_right = idx_const;
                }
            }
            new_args.push_back(al, arg);
        }
        return ASRUtils::EXPR(ASR::make_ArrayItem_t(
            al, arr_item->base.base.loc, arr_item->m_v,
            new_args.p, new_args.size(), arr_item->m_type,
            arr_item->m_storage_format, arr_item->m_value));
    }

    ASR::ImpliedDoLoop_t* substitute_loop_var_in_idl(
            ASR::ImpliedDoLoop_t* idl, ASR::Var_t* loop_var,
            ASR::expr_t* idx_const) {
        Vec<ASR::expr_t*> new_values;
        new_values.reserve(al, idl->n_values);
        for (size_t i = 0; i < idl->n_values; i++) {
            ASR::expr_t* value = idl->m_values[i];
            if (ASR::is_a<ASR::ArrayItem_t>(*value)) {
                ASR::ArrayItem_t* arr_item = ASR::down_cast<ASR::ArrayItem_t>(value);
                new_values.push_back(al, substitute_loop_var_in_array_item(
                    arr_item, loop_var, idx_const));
            } else if (ASR::is_a<ASR::ImpliedDoLoop_t>(*value)) {
                ASR::ImpliedDoLoop_t* inner_idl =
                    ASR::down_cast<ASR::ImpliedDoLoop_t>(value);
                ASR::ImpliedDoLoop_t* substituted =
                    substitute_loop_var_in_idl(inner_idl, loop_var, idx_const);
                new_values.push_back(al, ASRUtils::EXPR((ASR::asr_t*)substituted));
            } else {
                new_values.push_back(al, value);
            }
        }
        return ASR::down_cast2<ASR::ImpliedDoLoop_t>(ASR::make_ImpliedDoLoop_t(
            al, idl->base.base.loc, new_values.p, new_values.size(),
            idl->m_var, idl->m_start, idl->m_end, idl->m_increment,
            idl->m_type, idl->m_value));
    }

    void emit_read_end_err_label_jumps(int64_t end_label, int64_t err_label,
            ASR::expr_t* a_iostat, const Location& loc,
            std::vector<ASR::asr_t*>& tmp_vec) {
        ASR::ttype_t* iostat_type = ASRUtils::type_get_past_pointer(
            ASRUtils::expr_type(a_iostat));
        ASR::expr_t* zero = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
            al, loc, 0, iostat_type));
        ASR::ttype_t* cmp_type = ASRUtils::TYPE(ASR::make_Logical_t(
            al, loc, compiler_options.po.default_integer_kind));
        if (end_label != -1) {
            ASR::expr_t* eof_test = ASRUtils::EXPR(ASR::make_IntegerCompare_t(
                al, loc, a_iostat, ASR::cmpopType::Lt, zero, cmp_type, nullptr));
            Vec<ASR::stmt_t*> body; body.reserve(al, 1);
            body.push_back(al, ASRUtils::STMT(ASR::make_GoTo_t(
                al, loc, end_label, s2c(al, std::to_string(end_label)))));
            tmp_vec.push_back(ASR::make_If_t(al, loc, nullptr, eof_test,
                body.p, body.size(), nullptr, 0));
        }
        if (err_label != -1) {
            ASR::expr_t* err_test = ASRUtils::EXPR(ASR::make_IntegerCompare_t(
                al, loc, a_iostat, ASR::cmpopType::Gt, zero, cmp_type, nullptr));
            Vec<ASR::stmt_t*> body; body.reserve(al, 1);
            body.push_back(al, ASRUtils::STMT(ASR::make_GoTo_t(
                al, loc, err_label, s2c(al, std::to_string(err_label)))));
            tmp_vec.push_back(ASR::make_If_t(al, loc, nullptr, err_test,
                body.p, body.size(), nullptr, 0));
        }
    }

    void collect_labels() {
        labels.clear();
        if (starting_m_body == nullptr) return;
        for (size_t i = 0; i < starting_n_body; ++i) {
            int64_t label = stmt_label(starting_m_body[i]);
            if (label != 0) {
                labels.insert(std::to_string(label));
            }
        }
    }

    // Returns true if parsing succeeded, false if should continue to next kwarg
    bool parse_read_label_kwarg(const char* kwarg_name, int64_t& label,
            AST::stmtType _type, const AST::kw_argstar_t& kwarg,
            const Location& loc) {
        // END= is only valid for READ; ERR= is valid for READ/WRITE
        bool is_end = (strcmp(kwarg_name, "end") == 0);
        bool valid_stmt = (_type == AST::stmtType::Read) ||
                          (!is_end && _type == AST::stmtType::Write);
        if (!valid_stmt) {
            Location diag_loc = kwarg.loc;
            if (diag_loc.first == 0 && diag_loc.last == 0) {
                diag_loc = loc;
            }
            diag.add(Diagnostic(
                std::string("`") + kwarg_name + "` is only supported for READ statements",
                Level::Error, Stage::Semantic, {Label("", {diag_loc})}));
            if (!compiler_options.continue_compilation) throw SemanticAbort();
            return false;
        }
        if (label != -1) {
            diag.add(Diagnostic(
                std::string("Duplicate value of `") + kwarg_name +
                    "` found, `" + kwarg_name + "` has already been specified via keyword arguments",
                Level::Error, Stage::Semantic, {Label("", {loc})}));
            if (!compiler_options.continue_compilation) throw SemanticAbort();
            return false;
        }
        this->visit_expr(*kwarg.m_value);
        if (tmp == nullptr) {
            if (!compiler_options.continue_compilation) throw SemanticAbort();
            return false;
        }
        ASR::expr_t* label_expr = ASRUtils::EXPR(tmp);
        if (!ASR::is_a<ASR::IntegerConstant_t>(*label_expr)) {
            Location diag_loc = kwarg.loc;
            if (diag_loc.first == 0 && diag_loc.last == 0) {
                diag_loc = kwarg.m_value->base.loc;
            }
            if (diag_loc.first == 0 && diag_loc.last == 0) {
                diag_loc = loc;
            }
            diag.add(Diagnostic(
                std::string("`") + kwarg_name + "` must be a literal integer label",
                Level::Error, Stage::Semantic, {Label("", {diag_loc})}));
            if (!compiler_options.continue_compilation) throw SemanticAbort();
            return false;
        }
        label = ASR::down_cast<ASR::IntegerConstant_t>(label_expr)->m_n;
        if (labels.find(std::to_string(label)) == labels.end()) {
            Location diag_loc = kwarg.loc;
            if (diag_loc.first == 0 && diag_loc.last == 0) {
                diag_loc = kwarg.m_value->base.loc;
            }
            if (diag_loc.first == 0 && diag_loc.last == 0) {
                diag_loc = loc;
            }

            diag.add(Diagnostic(
                std::string(kwarg_name) + "= label " + std::to_string(label) + " is not defined",
                Level::Error, Stage::Semantic, {Label("", {diag_loc})}));
            if (!compiler_options.continue_compilation) throw SemanticAbort();
            return false;
        }
        return true;
    }

    void create_read_write_ASR_node(const AST::stmt_t& read_write_stmt, AST::stmtType _type) {
        int64_t m_label = -1;
        int64_t end_label = -1;
        int64_t err_label = -1;
        AST::argstar_t* m_args = nullptr; size_t n_args = 0;
        AST::kw_argstar_t* m_kwargs = nullptr; size_t n_kwargs = 0;
        AST::expr_t** m_values = nullptr; size_t n_values = 0;
        const Location& loc = read_write_stmt.base.loc;
        AST::Write_t* w = nullptr;
        AST::Read_t* r = nullptr;
        if( _type == AST::stmtType::Write ) {
            w = (AST::Write_t*)(&read_write_stmt);
            m_label = w->m_label;
            m_args = w->m_args; n_args = w->n_args;
            m_kwargs = w->m_kwargs; n_kwargs = w->n_kwargs;
            m_values = w->m_values; n_values = w->n_values;
        } else if( _type == AST::stmtType::Read ) {
            r = (AST::Read_t*)(&read_write_stmt);
            m_label = r->m_label;
            m_args = r->m_args; n_args = r->n_args;
            m_kwargs = r->m_kwargs; n_kwargs = r->n_kwargs;
            m_values = r->m_values; n_values = r->n_values;
        }

        ASR::expr_t *a_unit, *a_fmt, *a_iomsg, *a_iostat, *a_size, *a_id, *a_separator, *a_end, *a_fmt_constant, *a_advance, *a_pos, *a_rec;
        a_unit = a_fmt = a_iomsg = a_iostat = a_size = a_id = a_separator = a_end = a_fmt_constant = a_advance = a_pos = a_rec = nullptr;
        ASR::stmt_t *overloaded_stmt = nullptr;
        ASR::symbol_t *a_nml = nullptr;
        std::string read_write = "";
        bool needs_internal_iostat = false;
        bool formatted = (n_args == 2);
        Vec<ASR::expr_t*> a_values_vec;
        a_values_vec.reserve(al, n_values);

        // check if format as a keyword arg
        for (size_t i = 0; i < n_kwargs; i++) {
            if (strcmp(m_kwargs[i].m_arg, "fmt") == 0) {
                formatted = true;
                break;
            }
        }

        if( n_args > 2 ) {
            diag.add(Diagnostic(
                "Number of arguments cannot be more than 2 in Read/Write statement.",
                Level::Error, Stage::Semantic, {
                    Label("",{loc})
                }));
            throw SemanticAbort();
        }
        bool nml_in_positional_args = false;
        if (n_args == 2 && m_args[1].m_value &&
                AST::is_a<AST::Name_t>(*m_args[1].m_value)) {
            AST::Name_t* name_expr = AST::down_cast<AST::Name_t>(m_args[1].m_value);
            std::string nml_name = to_lower(std::string(name_expr->m_id));
            ASR::symbol_t* nml_sym = current_scope->resolve_symbol(nml_name);
            if (nml_sym) {
                ASR::symbol_t* nml_sym_past = ASRUtils::symbol_get_past_external(nml_sym);
                if (ASR::is_a<ASR::Namelist_t>(*nml_sym_past)) {
                    if (n_values != 0) {
                        diag.add(Diagnostic(
                            "Namelist group cannot be combined with an I/O list",
                            Level::Error, Stage::Semantic, {
                                Label("", {m_args[1].m_value->base.loc})
                            }));
                        throw SemanticAbort();
                    }
                    a_nml = nml_sym;
                    formatted = false;
                    nml_in_positional_args = true;
                }
            }
        }
        std::vector<ASR::expr_t**> args = {&a_unit, &a_fmt};
        for( std::uint32_t i = 0; i < n_args; i++ ) {
            if (i == 1 && nml_in_positional_args) {
                continue;
            }
            if( m_args[i].m_value != nullptr ) {
                this->visit_expr(*m_args[i].m_value);
                *args[i] = ASRUtils::EXPR(tmp);
            }
        }
        bool unit_explicit = false;
        bool iostat_explicit = false;
        bool iomsg_explicit = false;
        if (_type == AST::stmtType::Write && a_unit == nullptr) {
            ASR::ttype_t *int_type = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));
            a_unit = ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, loc, 6, int_type)); //default output/input unit is 6
        }
        // Ensure iomsg is always present for WRITE
        if (_type == AST::stmtType::Write && a_iomsg == nullptr) {
            // Create empty string iomsg
            ASR::ttype_t *str_type = ASRUtils::TYPE(
                ASR::make_String_t(
                    al, loc, 1,
                    ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                        al, loc, 0,
                        ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)))),
                    ASR::string_length_kindType::ExpressionLength,
                    ASR::string_physical_typeType::DescriptorString));

            std::string iomsg_name =
                current_scope->get_unique_name("lfortran_iomsg");

            ASR::symbol_t *iomsg_sym = declare_implicit_variable2(
                loc, iomsg_name, ASRUtils::intent_local, str_type);

            a_iomsg = ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, iomsg_sym));
        }

        //check positional fmt argument (second arg)
        if (a_fmt != nullptr) {
            ASR::ttype_t* fmt_type = ASRUtils::expr_type(a_fmt);
            bool is_array = ASRUtils::is_array(fmt_type);
            if (is_array && ASRUtils::is_character(*fmt_type)) {
                // Create temporary string variable to hold format string
                // by concatenating all elements of the array
                std::string fmt_string_name = current_scope->get_unique_name("_fmt_string");
                ASR::ttype_t* str_type = ASRUtils::TYPE(ASR::make_Allocatable_t(
                    al, a_fmt->base.loc,
                    ASRUtils::TYPE(ASR::make_String_t(
                    al, a_fmt->base.loc, 1, nullptr,
                    ASR::string_length_kindType::DeferredLength,
                    ASR::string_physical_typeType::DescriptorString))));
                ASR::symbol_t* fmt_string_sym = ASR::down_cast<ASR::symbol_t>(ASR::make_Variable_t(al, a_fmt->base.loc,
                    current_scope, s2c(al, fmt_string_name),
                nullptr, 0, ASR::intentType::Local, nullptr,
                nullptr, ASR::storage_typeType::Default, str_type,
                nullptr, ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Required, false, 
                false, false, nullptr, false, false));
                current_scope->add_symbol(fmt_string_name, fmt_string_sym);
                ASR::expr_t* fmt_string_var = ASRUtils::EXPR(ASR::make_Var_t(
                    al, a_fmt->base.loc, fmt_string_sym));
                
                ASRUtils::ASRBuilder b(al, a_fmt->base.loc);

                ASR::ttype_t* empty_str_type = ASRUtils::TYPE(ASR::make_String_t(
                    al, a_fmt->base.loc, 1, b.i32(0),
                    ASR::string_length_kindType::ExpressionLength,
                    ASR::string_physical_typeType::DescriptorString));
                ASR::expr_t* empty_str = b.StringConstant("", empty_str_type);
                ASR::stmt_t* assign_stmt = ASRUtils::STMT(ASR::make_Assignment_t(
                    al, a_fmt->base.loc, fmt_string_var, empty_str, nullptr, true, false));
                tmp_vec.push_back((ASR::asr_t*)assign_stmt);

                std::string loop_var = current_scope->get_unique_name("i");
                ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, a_fmt->base.loc, 4));
                ASR::symbol_t* loop_var_sym = ASR::down_cast<ASR::symbol_t>(ASR::make_Variable_t(al, a_fmt->base.loc,
                    current_scope, s2c(al, loop_var), nullptr, 0, ASR::intentType::Local, nullptr, nullptr,
                    ASR::storage_typeType::Default, int_type, nullptr, ASR::abiType::Source,
                    ASR::accessType::Public, ASR::presenceType::Required, false, false, false, nullptr,
                    false, false));
                current_scope->add_symbol(loop_var, loop_var_sym);
                ASR::expr_t* loop_var_expr = ASRUtils::EXPR(ASR::make_Var_t(al, a_fmt->base.loc, loop_var_sym));

                ASR::expr_t* lb = b.ArrayLBound(a_fmt, 1);
                ASR::expr_t* ub = b.ArrayUBound(a_fmt, 1);

                ASR::do_loop_head_t head;
                head.m_v = loop_var_expr;
                head.m_start = lb;
                head.m_end = ub;
                head.m_increment = b.i32(1);
                head.loc = a_fmt->base.loc;

                Vec<ASR::array_index_t> args; args.reserve(al, 1);
                ASR::array_index_t idx; 
                idx.loc = a_fmt->base.loc;
                idx.m_left = nullptr; idx.m_right = loop_var_expr; idx.m_step = nullptr;
                args.push_back(al, idx);

                ASR::expr_t* array_elem = ASRUtils::EXPR(ASR::make_ArrayItem_t(
                    al, a_fmt->base.loc, a_fmt, args.p, args.n, 
                    ASRUtils::type_get_past_array(fmt_type),
                    ASR::arraystorageType::ColMajor, nullptr));

                ASR::expr_t* concat = b.StringConcat(fmt_string_var, array_elem, ASRUtils::type_get_past_allocatable(str_type));
                ASR::stmt_t* body_assign = ASRUtils::STMT(ASR::make_Assignment_t(
                    al, a_fmt->base.loc, fmt_string_var, concat, nullptr, true, false));
                
                Vec<ASR::stmt_t*> body; body.reserve(al, 1);
                body.push_back(al, body_assign);

                ASR::stmt_t* do_loop = ASRUtils::STMT(ASR::make_DoLoop_t(
                    al, a_fmt->base.loc, nullptr, head, body.p, body.n, nullptr, 0));
                
                tmp_vec.push_back((ASR::asr_t*)do_loop);

                a_fmt = fmt_string_var;
            }
            if (ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(fmt_type)) && ASR::is_a<ASR::Var_t>(*a_fmt)) {
                diag.add(Diagnostic(
                    "Assigned format (using integer variable as format specifier) is not supported. "
                    "Use character variables instead.",
                    Level::Error, Stage::Semantic, {
                        Label("", {loc})
                    }));
                throw SemanticAbort();
            }
            if (ASR::is_a<ASR::StringConstant_t>(*a_fmt)) {
                ASR::StringConstant_t *fmt_const = ASR::down_cast<ASR::StringConstant_t>(a_fmt);
                std::string fmt_str = std::string(fmt_const->m_s);
                validate_format_string(fmt_str, a_fmt->base.loc, diag);
            }
            else if (ASR::is_a<ASR::Var_t>(*a_fmt)) {
                ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(a_fmt);
                ASR::symbol_t* sym = var->m_v;
                if (ASR::is_a<ASR::Variable_t>(*sym)) {
                    ASR::Variable_t* var_sym = ASR::down_cast<ASR::Variable_t>(sym);
                    if (var_sym->m_storage == ASR::storage_typeType::Parameter && 
                        var_sym->m_value != nullptr &&
                        ASR::is_a<ASR::StringConstant_t>(*var_sym->m_value)) {
                        ASR::StringConstant_t *fmt_const = ASR::down_cast<ASR::StringConstant_t>(var_sym->m_value);
                        std::string fmt_str = std::string(fmt_const->m_s);
                        validate_format_string(fmt_str, a_fmt->base.loc, diag);
                    }
                    else if (var_sym->m_symbolic_value != nullptr &&
                             ASR::is_a<ASR::StringConstant_t>(*var_sym->m_symbolic_value)) {
                        ASR::StringConstant_t *fmt_const = ASR::down_cast<ASR::StringConstant_t>(var_sym->m_symbolic_value);
                        std::string fmt_str = std::string(fmt_const->m_s);
                        validate_format_string(fmt_str, a_fmt->base.loc, diag);
                    }
                }
            }
        }
        std::vector<ASR::asr_t*> newline_for_advance;
        for( std::uint32_t i = 0; i < n_kwargs; i++ ) {
            AST::kw_argstar_t kwarg = m_kwargs[i];
            std::string m_arg_str(kwarg.m_arg);
            m_arg_str = to_lower(m_arg_str);
            if( m_arg_str == std::string("unit") ) {
                if( unit_explicit ) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `unit` found, `unit` has already been specified via argument or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{loc})
                        }));
                    throw SemanticAbort();
                }
                unit_explicit = true;
                if (kwarg.m_value != nullptr) {
                    this->visit_expr(*kwarg.m_value);
                    a_unit = ASRUtils::EXPR(tmp);
                    ASR::ttype_t* a_unit_type = ASRUtils::expr_type(a_unit);
                    if  (!ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(a_unit_type))) {
                            diag.add(Diagnostic(
                                "`unit` must be of type, Integer",
                                Level::Error, Stage::Semantic, {
                                    Label("",{loc})
                                }));
                            throw SemanticAbort();
                    }
                }
            } else if( m_arg_str == std::string("iostat") ) {
                if( iostat_explicit ) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `iostat` found, unit has already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{loc})
                        }));
                    throw SemanticAbort();
                }
                iostat_explicit = true;
                this->visit_expr(*kwarg.m_value);
                a_iostat = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_iostat_type = ASRUtils::expr_type(a_iostat);
                if (!ASRUtils::is_variable(a_iostat)) {
                    diag.add(Diagnostic(
                        "Non-variable expression for `iostat`",
                        Level::Error, Stage::Semantic, {
                            Label("",{loc})
                        }));
                    throw SemanticAbort();
                }
                if (ASRUtils::is_array(a_iostat_type)) {
                    diag.add(Diagnostic(
                        "`iostat` must be scalar",
                        Level::Error, Stage::Semantic, {
                            Label("",{loc})
                        }));
                    throw SemanticAbort();
                }
                if (!ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(a_iostat_type))) {
                        diag.add(Diagnostic(
                            "`iostat` must be of type, Integer",
                            Level::Error, Stage::Semantic, {
                                Label("",{loc})
                            }));
                        throw SemanticAbort();
                }
            } else if( m_arg_str == std::string("iomsg") ) {
                if( iomsg_explicit ) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `iomsg` found, it has already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{loc})
                        }));
                    throw SemanticAbort();
                }
                iomsg_explicit = true;
                this->visit_expr(*kwarg.m_value);
                a_iomsg = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_iomsg_type = ASRUtils::expr_type(a_iomsg);
                if (!ASRUtils::is_variable(a_iomsg)) {
                    diag.add(Diagnostic(
                        "Non-variable expression for `iomsg`",
                        Level::Error, Stage::Semantic, {
                            Label("",{loc})
                        }));
                    throw SemanticAbort();
                }
                if (ASRUtils::is_array(a_iomsg_type)) {
                    diag.add(Diagnostic(
                        "`iomsg` must be scalar",
                        Level::Error, Stage::Semantic, {
                            Label("",{loc})
                        }));
                    throw SemanticAbort();
                }
                if (!ASR::is_a<ASR::String_t>(*ASRUtils::type_get_past_pointer(a_iomsg_type))) {
                    diag.add(Diagnostic(
                        "`iomsg` must be of type, String",
                        Level::Error, Stage::Semantic, {
                            Label("",{loc})
                        }));
                    throw SemanticAbort();
                }
            } else if( m_arg_str == std::string("size") ) {
                if( a_size != nullptr ) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `size` found, `size` has already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{loc})
                        }));
                    throw SemanticAbort();
                }
                this->visit_expr(*kwarg.m_value);
                a_size = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_size_type = ASRUtils::expr_type(a_size);
                if( a_size->type != ASR::exprType::Var ||
                    (!ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(a_size_type))) ) {
                        diag.add(Diagnostic(
                            "`size` must be of type, Integer",
                            Level::Error, Stage::Semantic, {
                                Label("",{loc})
                            }));
                        throw SemanticAbort();
                }
            } else if( m_arg_str == std::string("id") ) {
                if( a_id != nullptr ) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `id` found, it has already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{loc})
                        }));
                    throw SemanticAbort();
                }
                this->visit_expr(*kwarg.m_value);
                a_id = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_status_type = ASRUtils::expr_type(a_id);
                if (!ASR::is_a<ASR::String_t>(*ASRUtils::type_get_past_pointer(a_status_type))) {
                        diag.add(Diagnostic(
                            "`status` must be of type String",
                            Level::Error, Stage::Semantic, {
                                Label("",{loc})
                            }));
                        throw SemanticAbort();
                }
            } else if( m_arg_str == std::string("fmt")  ) {
                if( a_fmt != nullptr ) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `fmt` found, it has already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{loc})
                        }));
                    throw SemanticAbort();
                }
                if (kwarg.m_value != nullptr) {
                    tmp = nullptr;
                    this->visit_expr(*kwarg.m_value);
                    if (tmp == nullptr) {
                        diag.add(Diagnostic(
                            "?",
                            Level::Error, Stage::Semantic, {
                                Label("",{loc})
                            }));
                        throw SemanticAbort();
                    }
                    a_fmt = ASRUtils::EXPR(tmp); 
                    // check that fmt is not integer
                    ASR::ttype_t* fmt_type = ASRUtils::expr_type(a_fmt);
                    if (ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(fmt_type)) && ASR::is_a<ASR::Var_t>(*a_fmt)) {
                        diag.add(Diagnostic(
                            "Assigned format (using integer variable as format specifier) is not supported. "
                            "Use character variables instead.",
                            Level::Error, Stage::Semantic, {
                                Label("", {loc})
                            }));
                        throw SemanticAbort();
                    }
                    if (ASR::is_a<ASR::StringConstant_t>(*a_fmt)) {
                        ASR::StringConstant_t *fmt_const = ASR::down_cast<ASR::StringConstant_t>(a_fmt);
                        std::string fmt_str = std::string(fmt_const->m_s);
                        validate_format_string(fmt_str, a_fmt->base.loc, diag);
                    }
                    else if (ASR::is_a<ASR::Var_t>(*a_fmt)) {
                        ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(a_fmt);
                        ASR::symbol_t* sym = var->m_v;
                        if (ASR::is_a<ASR::Variable_t>(*sym)) {
                            ASR::Variable_t* var_sym = ASR::down_cast<ASR::Variable_t>(sym);
                            if (var_sym->m_storage == ASR::storage_typeType::Parameter && 
                                var_sym->m_value != nullptr &&
                                ASR::is_a<ASR::StringConstant_t>(*var_sym->m_value)) {
                                ASR::StringConstant_t *fmt_const = ASR::down_cast<ASR::StringConstant_t>(var_sym->m_value);
                                std::string fmt_str = std::string(fmt_const->m_s);
                                validate_format_string(fmt_str, a_fmt->base.loc, diag);
                            }
                            else if (var_sym->m_symbolic_value != nullptr &&
                                     ASR::is_a<ASR::StringConstant_t>(*var_sym->m_symbolic_value)) {
                                ASR::StringConstant_t *fmt_const = ASR::down_cast<ASR::StringConstant_t>(var_sym->m_symbolic_value);
                                std::string fmt_str = std::string(fmt_const->m_s);
                                validate_format_string(fmt_str, a_fmt->base.loc, diag);
                            }
                        }
                    }
                }
            } else if( m_arg_str == std::string("advance") ) {
                if( a_end != nullptr ) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `advance` found, it has already been specified via arguments or keyword arguments)""",
                    Level::Error, Stage::Semantic, {
                            Label("",{loc})
                        }));
                    throw SemanticAbort();
                }
                this->visit_expr(*kwarg.m_value);
                ASR::expr_t* adv_val_expr = ASRUtils::EXPR(tmp);
                a_advance = adv_val_expr;
                ASR::ttype_t *str_type_len_0 = ASRUtils::TYPE(ASR::make_String_t(
                    al, loc, 1,
                    ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 0,
                        ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)))),
                    ASR::string_length_kindType::ExpressionLength,
                    ASR::string_physical_typeType::DescriptorString));
                ASR::expr_t *empty = ASRUtils::EXPR(ASR::make_StringConstant_t(
                    al, loc, s2c(al, ""), str_type_len_0));
                ASR::ttype_t *str_type_len_1 = ASRUtils::TYPE(ASR::make_String_t(
                    al, loc, 1,
                    ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1,
                        ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)))),
                    ASR::string_length_kindType::ExpressionLength, 
                    ASR::string_physical_typeType::DescriptorString));
                ASR::expr_t *newline = ASRUtils::EXPR(ASR::make_StringConstant_t(
                    al, loc, s2c(al, "\n"), str_type_len_1));
                if (ASR::is_a<ASR::StringConstant_t>(*adv_val_expr)) {
                    std::string adv_val = to_lower(ASR::down_cast<ASR::StringConstant_t>(adv_val_expr)->m_s);
                    if (adv_val == "yes") {
                        a_end = newline;
                    } else if (adv_val == "no") {
                        a_end = empty;
                    } else {
                        diag.add(Diagnostic(
                            "ADVANCE= specifier must have value = YES or NO",
                            Level::Error, Stage::Semantic, {
                                Label("",{kwarg.loc})
                            }));
                        throw SemanticAbort();
                    }
                } else {
                    Vec<ASR::expr_t*> trim_arg; trim_arg.reserve(al, 1);
                    trim_arg.push_back(al, a_advance);
                    a_advance = ASRUtils::EXPR(ASR::make_IntrinsicElementalFunction_t(al, a_advance->base.loc,
                        static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::StringTrim),
                        trim_arg.p, trim_arg.n, 0, ASRUtils::expr_type(a_advance), nullptr));
                    ASR::ttype_t *str_type_len_3 = ASRUtils::TYPE(ASR::make_String_t(
                        al, loc, 1,
                        ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 3,
                            ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)))),
                        ASR::string_length_kindType::ExpressionLength,
                        ASR::string_physical_typeType::DescriptorString));
                    ASR::expr_t *yes = ASRUtils::EXPR(ASR::make_StringConstant_t(
                        al, loc, s2c(al, "yes"), str_type_len_3));
                    // TODO: Support case insensitive compare
                    ASR::ttype_t *cmp_type = ASRUtils::TYPE(ASR::make_Logical_t(
                        al, loc, compiler_options.po.default_integer_kind));
                    ASR::expr_t *test = ASRUtils::EXPR(ASR::make_StringCompare_t(al,
                        loc, adv_val_expr, ASR::cmpopType::Eq, yes, cmp_type, nullptr));
                    Vec<ASR::stmt_t*> body;
                    body.reserve(al, 1);
                    body.push_back(al, ASRUtils::STMT(
                        ASR::make_FileWrite_t(al, loc, 0, a_unit,
                        nullptr, nullptr, nullptr,
                        nullptr, 0, nullptr, newline, nullptr, formatted, a_nml, a_rec)));
                    // TODO: Compare with "no" (case-insensitive) in else part
                    // Throw runtime error if advance expression does not match "no"
                    newline_for_advance.push_back(ASR::make_If_t(al, loc, nullptr, test, body.p,
                            body.size(), nullptr, 0));
                    a_end = empty;
                }
            } else if (m_arg_str == "end") {
                if (!parse_read_label_kwarg("end", end_label, _type, kwarg, loc)) {
                    continue;
                }
            } else if (m_arg_str == "err") {
                if (!parse_read_label_kwarg("err", err_label, _type, kwarg, loc)) {
                    continue;
                }
            } else if (m_arg_str == "nml") {
                if (a_nml != nullptr) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `nml` found, it has already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{loc})
                        }));
                    throw SemanticAbort();
                }
                if (kwarg.m_value == nullptr || kwarg.m_value->type != AST::exprType::Name) {
                    diag.add(Diagnostic(
                        "`nml` must be a namelist group name",
                        Level::Error, Stage::Semantic, {
                            Label("",{loc})
                        }));
                    throw SemanticAbort();
                }
                AST::Name_t* name_expr = AST::down_cast<AST::Name_t>(kwarg.m_value);
                std::string nml_name = to_lower(std::string(name_expr->m_id));
                a_nml = current_scope->resolve_symbol(nml_name);
                if (!a_nml) {
                    diag.add(Diagnostic(
                        "Namelist group '" + nml_name + "' not found",
                        Level::Error, Stage::Semantic, {
                            Label("",{loc})
                        }));
                    throw SemanticAbort();
                }
                // Unwrap external symbol to get the actual namelist
                ASR::symbol_t* nml_sym = ASRUtils::symbol_get_past_external(a_nml);
                if (!ASR::is_a<ASR::Namelist_t>(*nml_sym)) {
                    diag.add(Diagnostic(
                      "'" + nml_name + "' is not a namelist group",
                        Level::Error, Stage::Semantic, {
                            Label("",{loc})
                        }));
                    throw SemanticAbort();
                }
            } else if (m_arg_str == std::string("rec")) {
                if (a_rec != nullptr) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `rec` found, it has already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("",{loc})
                        }));
                    throw SemanticAbort();
                }
                this->visit_expr(*kwarg.m_value);
                a_rec = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_rec_type = ASRUtils::expr_type(a_rec);
                if (!ASRUtils::is_integer(*a_rec_type)) {
                    diag.add(Diagnostic(
                        "`rec` must be of type, Integer",
                        Level::Error, Stage::Semantic, {
                            Label("",{loc})
                        }));
                    throw SemanticAbort();
                }
            } else if (m_arg_str == "pos") {
                if (a_pos != nullptr) {
                    diag.add(Diagnostic(
                        R"""(Duplicate value of `pos` found, it has already been specified via arguments or keyword arguments)""",
                        Level::Error, Stage::Semantic, {
                            Label("", {loc})
                        }));
                    throw SemanticAbort();
                }
                this->visit_expr(*kwarg.m_value);
                a_pos = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_pos_type = ASRUtils::expr_type(a_pos);
                if (!ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(a_pos_type))) {
                    diag.add(Diagnostic(
                        "`pos` must be of type Integer",
                        Level::Error, Stage::Semantic, {
                            Label("", {loc})
                        }));
                    throw SemanticAbort();
                }
            }
        }
        if (a_rec && a_unit && ASRUtils::is_character(*ASRUtils::expr_type(a_unit))) {
            diag.add(Diagnostic(
                "`rec` specifier is not allowed for internal file I/O",
                Level::Error, Stage::Semantic, {
                    Label("", {loc})
                }));
            throw SemanticAbort();
        }
        if( a_fmt == nullptr && a_end != nullptr ) {
            diag.add(Diagnostic(
                R"""(List directed format(*) is not allowed with a ADVANCE= specifier)""",
                Level::Error, Stage::Semantic, {
                    Label("",{loc})
                }));
            throw SemanticAbort();
        }
        if (_type == AST::stmtType::Write && a_fmt == nullptr
                && compiler_options.print_leading_space && formatted) {
            ASR::asr_t* file_write_asr_t = construct_leading_space(loc);
            ASR::FileWrite_t* file_write = ASR::down_cast<ASR::FileWrite_t>(ASRUtils::STMT(file_write_asr_t));
            file_write->m_id = a_id;
            file_write->m_iomsg = a_iomsg;
            file_write->m_iostat = a_iostat;
            file_write->m_unit = a_unit;
            file_write->m_label = m_label;
            tmp_vec.push_back(file_write_asr_t);
        } else if (_type == AST::stmtType::Write) {
            a_fmt_constant = a_fmt;
        }
        for( std::uint32_t i = 0; i < n_values; i++ ) {
            if (AST::is_a<AST::Name_t>(*m_values[i])) {
                AST::Name_t* name_expr = AST::down_cast<AST::Name_t>(m_values[i]);
                std::string nml_name = to_lower(std::string(name_expr->m_id));
                ASR::symbol_t* nml_sym = current_scope->resolve_symbol(nml_name);
                if (nml_sym) {
                    ASR::symbol_t* nml_sym_past = ASRUtils::symbol_get_past_external(nml_sym);
                    if (ASR::is_a<ASR::Namelist_t>(*nml_sym_past)) {
                        if (a_nml != nullptr) {
                            diag.add(Diagnostic(
                                "Namelist group already specified",
                                Level::Error, Stage::Semantic, {
                                    Label("", {m_values[i]->base.loc})
                                }));
                            throw SemanticAbort();
                        }
                        if (n_values != 1) {
                            diag.add(Diagnostic(
                                "Namelist group must be the only item in the I/O list",
                                Level::Error, Stage::Semantic, {
                                    Label("", {m_values[i]->base.loc})
                                }));
                            throw SemanticAbort();
                        }
                        a_nml = nml_sym;
                        continue;
                    }
                }
            }
            this->visit_expr(*m_values[i]);
            ASR::expr_t* expr = ASRUtils::EXPR(tmp);
            // For READ: expand implied-do loops to individual elements or array section
            if (_type == AST::stmtType::Read && ASR::is_a<ASR::ImpliedDoLoop_t>(*expr)) {
                expand_implied_do_for_read(
                    ASR::down_cast<ASR::ImpliedDoLoop_t>(expr), a_values_vec);
            } else {
                a_values_vec.push_back(al, expr);
            }
        }

        read_write = (_type == AST::stmtType::Write) ? "~write" : "~read";
        read_write += (formatted) ? "_formatted" : "_unformatted";
        if (n_values > 0) {
            Vec<ASR::expr_t*> overload_args;
            overload_args.reserve(al, 2);
            overload_args.push_back(al, a_values_vec[0]);
            overload_args.push_back(al, a_unit);
            if (formatted) {
                if (a_fmt) {
                    overload_args.push_back(al, a_fmt);
                } else {
                    ASR::ttype_t* char_type = ASRUtils::TYPE(
                        ASR::make_String_t(
                            al, loc, 1,
                            ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 12,
                                ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)))),
                            ASR::string_length_kindType::ExpressionLength,
                            ASR::string_physical_typeType::DescriptorString));
                    ASR::expr_t* list_directed = ASRUtils::EXPR(
                        ASR::make_StringConstant_t(al, loc, s2c(al, "LISTDIRECTED"), char_type));
                    overload_args.push_back(al, list_directed);
                }
                const Location& loc = read_write_stmt.base.loc;
                ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
                Vec<ASR::dimension_t> dims;
                dims.reserve(al, 1);
                ASR::dimension_t dim;
                dim.loc = loc;
                dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1, int_type));
                dim.m_length = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 0, int_type));
                dims.push_back(al, dim);
                ASR::ttype_t* arr_type = ASRUtils::TYPE(ASR::make_Array_t(al, loc, int_type,
                    dims.p, dims.n, ASR::array_physical_typeType::FixedSizeArray));
                Vec<ASR::expr_t*> arr_args;
                arr_args.reserve(al, 0);
                overload_args.push_back(al, ASRUtils::EXPR(ASRUtils::make_ArrayConstructor_t_util(
                    al, loc, arr_args.p, arr_args.n, arr_type, ASR::arraystorageType::ColMajor)));
            }
            ASR::expr_t *a_tmp_iostat = a_iostat;
            if (_type == AST::stmtType::Write && a_tmp_iostat == nullptr) {
                ASR::ttype_t* int_type =
                    ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));

                std::string iostat_name =
                    current_scope->get_unique_name("lfortran_tmp_iostat");

                ASR::symbol_t* iostat_sym =
                    declare_implicit_variable2(
                        loc, iostat_name,
                        ASRUtils::intent_local,
                        int_type);

                a_tmp_iostat =
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc, iostat_sym));
            }
            overload_args.push_back(al, a_tmp_iostat);
            overload_args.push_back(al, a_iomsg);
            if (ASRUtils::use_overloaded_file_read_write(read_write, overload_args,
                    current_scope, asr, al, read_write_stmt.base.loc,
                    current_function_dependencies, current_module_dependencies,
                    [&](const std::string &msg, const Location &loc) {
                        diag.add(Diagnostic(msg, Level::Error, Stage::Semantic,
                            {Label("", {loc})}));
                        throw SemanticAbort();
                    })) {
                overloaded_stmt = ASRUtils::STMT(asr);
            }
            if (overloaded_stmt != nullptr) {
                needs_internal_iostat = true;
            }
        }
        if (_type == AST::stmtType::Write &&
            needs_internal_iostat &&
            a_iostat == nullptr) {

            ASR::ttype_t* int_type =
                ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));

            std::string iostat_name =
                current_scope->get_unique_name("__lfortran_iostat");

            ASR::symbol_t* iostat_sym =
                declare_implicit_variable2(
                    loc, iostat_name,
                    ASRUtils::intent_local,
                    int_type);

            a_iostat = ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, iostat_sym));
        }

        bool inserted_iostat = false;
        if (_type == AST::stmtType::Read && a_iostat == nullptr
                && (end_label != -1 || err_label != -1)) {
            ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
            std::string iostat_name = current_scope->get_unique_name("__lfortran_iostat");
            ASR::symbol_t* iostat_sym = declare_implicit_variable2(
                loc, iostat_name, ASRUtils::intent_local, int_type);
            a_iostat = ASRUtils::EXPR(ASR::make_Var_t(al, loc, iostat_sym));
            inserted_iostat = true;
        }

        if (a_fmt && ASR::is_a<ASR::IntegerConstant_t>(*a_fmt)) {
            ASR::IntegerConstant_t* a_fmt_int = ASR::down_cast<ASR::IntegerConstant_t>(a_fmt);
            int64_t label = a_fmt_int->m_n;
            if (format_statements.find(label) == format_statements.end()) {
                if (_type == AST::stmtType::Write) {
                    tmp = ASR::make_FileWrite_t(al, loc, m_label, a_unit, a_iomsg, a_iostat,
                        a_id, a_values_vec.p, a_values_vec.size(), a_separator, a_end, nullptr, true, a_nml, a_rec);
                    print_statements[tmp] = std::make_pair(&w->base, label);
                } else if (_type == AST::stmtType::Read) {
                    tmp = ASR::make_FileRead_t(al, loc, m_label, a_unit, a_fmt, a_iomsg,
                        a_iostat, a_advance, a_size, a_id, a_pos, a_values_vec.p, a_values_vec.size(),
                        nullptr, formatted, a_nml, a_rec);
                    print_statements[tmp] = std::make_pair(&r->base, label);
                }
                if (_type == AST::stmtType::Read && (end_label != -1 || err_label != -1)) {
                    tmp_vec.push_back(tmp);
                    tmp = nullptr;
                    emit_read_end_err_label_jumps(end_label, err_label, a_iostat, loc, tmp_vec);
                    tmp_vec.insert(tmp_vec.end(), newline_for_advance.begin(), newline_for_advance.end());
                }
                return;
            }
            ASR::ttype_t* a_fmt_type = ASRUtils::TYPE(ASR::make_String_t(
                al, a_fmt->base.loc, 1,
                ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                    al, a_fmt->base.loc, format_statements[label].size(),
                    ASRUtils::TYPE(ASR::make_Integer_t(al, a_fmt->base.loc, 4)))),
                ASR::string_length_kindType::ExpressionLength,
                ASR::string_physical_typeType::DescriptorString));
            a_fmt_constant = ASRUtils::EXPR(ASR::make_StringConstant_t(
                al, a_fmt->base.loc, s2c(al, format_statements[label]), a_fmt_type));
        }
        // Don't use stringFormat with single character argument
        if (!a_fmt
            && _type == AST::stmtType::Write
            && a_values_vec.size() == 1
            && ASR::is_a<ASR::String_t>(*ASRUtils::expr_type(a_values_vec[0]))){
            tmp = ASR::make_FileWrite_t(al, loc, m_label, a_unit,
            a_iomsg, a_iostat, a_id, a_values_vec.p,
            a_values_vec.size(), a_separator, a_end, overloaded_stmt, formatted, a_nml, nullptr);
        } else if ( _type == AST::stmtType::Write ) { // If not the previous case, Wrap everything in stringFormat.
            if (formatted) {
                ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Allocatable_t(al, loc,
                    ASRUtils::TYPE(ASR::make_String_t(
                        al, loc, 1, nullptr,
                        ASR::string_length_kindType::DeferredLength,
                        ASR::string_physical_typeType::DescriptorString))));
                ASR::expr_t* string_format = ASRUtils::EXPR(ASRUtils::make_StringFormat_t_util(al, a_fmt? a_fmt->base.loc : read_write_stmt.base.loc,
                    a_fmt_constant, a_values_vec.p, a_values_vec.size(), ASR::string_format_kindType::FormatFortran,
                    type, nullptr));
                a_values_vec.reserve(al, 1);
                a_values_vec.push_back(al, string_format);
            }
            tmp = ASR::make_FileWrite_t(al, loc, m_label, a_unit,
                a_iomsg, a_iostat, a_id, a_values_vec.p,
                a_values_vec.size(), a_separator, a_end, overloaded_stmt, formatted, a_nml, a_rec);
        } else if( _type == AST::stmtType::Read ) {
            if (formatted && a_fmt_constant) {
                ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Allocatable_t(al, loc,
                    ASRUtils::TYPE(ASR::make_String_t(
                        al, loc, 1, nullptr,
                        ASR::string_length_kindType::DeferredLength,
                        ASR::string_physical_typeType::DescriptorString))));
                ASR::expr_t* string_format = ASRUtils::EXPR(ASRUtils::make_StringFormat_t_util(al, a_fmt->base.loc,
                    a_fmt_constant, a_values_vec.p, a_values_vec.size(), ASR::string_format_kindType::FormatFortran,
                    type, nullptr));
                a_values_vec.reserve(al, 1);
                a_values_vec.push_back(al, string_format);
            }
            tmp = ASR::make_FileRead_t(al, loc, m_label, a_unit, a_fmt, a_iomsg,
               a_iostat, a_advance, a_size, a_id, a_pos, a_values_vec.p, a_values_vec.size(), overloaded_stmt, formatted, a_nml, a_rec);
        }

        tmp_vec.push_back(tmp);
        if (_type == AST::stmtType::Read && (end_label != -1 || err_label != -1)) {
            emit_read_end_err_label_jumps(end_label, err_label, a_iostat, loc, tmp_vec);
            if (inserted_iostat) {
                ASR::ttype_t* iostat_type = ASRUtils::type_get_past_pointer(
                    ASRUtils::expr_type(a_iostat));
                ASR::expr_t* zero = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                    al, loc, 0, iostat_type));
                ASR::ttype_t* cmp_type = ASRUtils::TYPE(ASR::make_Logical_t(
                    al, loc, compiler_options.po.default_integer_kind));
                if (end_label == -1) {
                    ASR::expr_t* eof_test = ASRUtils::EXPR(ASR::make_IntegerCompare_t(
                        al, loc, a_iostat, ASR::cmpopType::Lt, zero, cmp_type, nullptr));
                    Vec<ASR::stmt_t*> body; body.reserve(al, 1);
                    body.push_back(al, ASRUtils::STMT(ASR::make_FileRead_t(al, loc, m_label,
                        a_unit, a_fmt, a_iomsg, nullptr, a_advance, a_size, a_id, a_pos,
                        a_values_vec.p, a_values_vec.size(), overloaded_stmt, formatted, a_nml, a_rec)));
                    tmp_vec.push_back(ASR::make_If_t(al, loc, nullptr, eof_test,
                        body.p, body.size(), nullptr, 0));
                }
                if (err_label == -1) {
                    ASR::expr_t* err_test = ASRUtils::EXPR(ASR::make_IntegerCompare_t(
                        al, loc, a_iostat, ASR::cmpopType::Gt, zero, cmp_type, nullptr));
                    Vec<ASR::stmt_t*> body; body.reserve(al, 1);
                    body.push_back(al, ASRUtils::STMT(ASR::make_FileRead_t(al, loc, m_label,
                        a_unit, a_fmt, a_iomsg, nullptr, a_advance, a_size, a_id, a_pos,
                        a_values_vec.p, a_values_vec.size(), overloaded_stmt, formatted, a_nml, a_rec)));
                    tmp_vec.push_back(ASR::make_If_t(al, loc, nullptr, err_test,
                        body.p, body.size(), nullptr, 0));
                }
            }
        }
        tmp_vec.insert(tmp_vec.end(), newline_for_advance.begin(), newline_for_advance.end());
        tmp = nullptr;
    }

    void visit_Write(const AST::Write_t& x) {
        create_read_write_ASR_node(x.base, x.class_type);
    }

    void visit_Read(const AST::Read_t& x) {
        create_read_write_ASR_node(x.base, x.class_type);
    }

    template <typename T>
    void fill_args_for_rewind_inquire_flush(const T& x, const size_t max_args,
        std::vector<ASR::expr_t*>& args, const size_t args_size,
        std::map<std::string, size_t>& argname2idx, std::string& stmt_name) {
        if( x.n_args + x.n_kwargs > max_args ) {
            diag.add(Diagnostic(
                "Incorrect number of arguments passed to " + stmt_name + "."
                " It accepts a total of 3 arguments namely unit, iostat and err.",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }

        for( size_t i = 0; i < args_size; i++ ) {
            args.push_back(nullptr);
        }
        for( size_t i = 0; i < x.n_args; i++ ) {
            visit_expr(*x.m_args[i]);
            args[i] = ASRUtils::EXPR(tmp);
        }

        for( size_t i = 0; i < x.n_kwargs; i++ ) {
            if( x.m_kwargs[i].m_value ) {
                std::string m_arg_str = to_lower(std::string(x.m_kwargs[i].m_arg));
                if (argname2idx.find(m_arg_str) == argname2idx.end()) {
                    diag.add(diag::Diagnostic("Invalid argument `" + m_arg_str + "` supplied",
                        diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("", {x.base.base.loc})}));
                    throw SemanticAbort();
                } else if (args[argname2idx[m_arg_str]]) {
                    diag.add(Diagnostic(
                        m_arg_str + " has already been specified.",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                visit_expr(*x.m_kwargs[i].m_value);
                args[argname2idx[m_arg_str]] = ASRUtils::EXPR(tmp);
            }
        }
    }

    void visit_Rewind(const AST::Rewind_t& x) {
        std::map<std::string, size_t> argname2idx = {{"unit", 0}, {"iostat", 1}, {"err", 2 }};
        std::vector<ASR::expr_t*> args;
        std::string node_name = "Rewind";
        fill_args_for_rewind_inquire_flush(x, 3, args, 3, argname2idx, node_name);
        ASR::expr_t *unit = args[0], *iostat = args[1], *err = args[2];
        tmp = ASR::make_FileRewind_t(al, x.base.base.loc, x.m_label, unit, iostat, err);
    }

    void visit_Instantiate(const AST::Instantiate_t &x) {
        ASR::symbol_t *sym = current_scope->resolve_symbol(x.m_name);
        ASR::Template_t* temp = ASR::down_cast<ASR::Template_t>(ASRUtils::symbol_get_past_external(sym));

        std::map<std::string, std::pair<ASR::ttype_t*, ASR::symbol_t*>> type_subs = instantiate_types[x.base.base.loc.first];
        std::map<std::string, ASR::symbol_t*> symbol_subs = instantiate_symbols[x.base.base.loc.first];

        if (x.n_symbols == 0) {
            for (auto const &sym_pair: temp->m_symtab->get_scope()) {
                ASR::symbol_t *s = sym_pair.second;
                std::string s_name = ASRUtils::symbol_name(s);
                if (ASR::is_a<ASR::Function_t>(*s) && !ASRUtils::is_template_arg(sym, s_name)) {
                    instantiate_body(al, type_subs, symbol_subs, current_scope->resolve_symbol(s_name), s);
                }
            }
        } else {
            for (size_t i = 0; i < x.n_symbols; i++){
                AST::UseSymbol_t* use_symbol = AST::down_cast<AST::UseSymbol_t>(x.m_symbols[i]);
                ASR::symbol_t *s = temp->m_symtab->get_symbol(to_lower(use_symbol->m_remote_sym));
                std::string new_s_name = use_symbol->m_remote_sym;
                if (use_symbol->m_local_rename) {
                    new_s_name = to_lower(use_symbol->m_local_rename);
                }
                instantiate_body(al, type_subs, symbol_subs, current_scope->resolve_symbol(new_s_name), s);
            }
        }

    }

    void visit_Inquire(const AST::Inquire_t& x) {
        std::map<std::string, size_t> argname2idx = {
            {"unit", 0}, {"file", 1}, {"iostat", 2}, {"err", 3},
            {"exist", 4}, {"opened", 5}, {"number", 6}, {"named", 7},
            {"name", 8}, {"access", 9}, {"sequential", 10}, {"direct", 11},
            {"form", 12}, {"formatted", 13}, {"unformatted", 14}, {"recl", 15},
            {"nextrec", 16}, {"blank", 17}, {"position", 18}, {"action", 19},
            {"read", 20}, {"write", 21}, {"readwrite", 22}, {"delim", 23},
            {"pad", 24}, {"flen", 25}, {"blocksize", 26}, {"convert", 27},
            {"carriagecontrol", 28}, {"size", 29}, {"pos", 30}, {"iolength", 31}};
        std::vector<ASR::expr_t*> args;
        Vec<ASR::expr_t*> iolength_args; iolength_args.reserve(al, 0);
        std::string node_name = "Inquire";
        fill_args_for_rewind_inquire_flush(x, 31, args, 32, argname2idx, node_name);
        ASR::expr_t *unit = args[0], *file = args[1], *iostat = args[2], *err = args[3];
        ASR::expr_t *exist = args[4], *opened = args[5], *number = args[6], *named = args[7];
        ASR::expr_t *name = args[8], *access = args[9], *sequential = args[10], *direct = args[11];
        ASR::expr_t *form = args[12], *formatted = args[13], *unformatted = args[14], *recl = args[15];
        ASR::expr_t *nextrec = args[16], *blank = args[17], *position = args[18], *action = args[19];
        ASR::expr_t *read = args[20], *write = args[21], *readwrite = args[22], *delim = args[23];
        ASR::expr_t *pad = args[24], *flen = args[25], *blocksize = args[26], *convert = args[27];
        ASR::expr_t *carriagecontrol = args[28], *size = args[29], *pos = args[30], *iolength = args[31];
        bool is_iolength_present = iolength != nullptr;
        for( size_t i = 0; i < args.size() - 1; i++ ) {
            if( is_iolength_present && args[i] ) {
                diag.add(Diagnostic(
                    "No argument should be specified when iolength is already present.",
                    Level::Error, Stage::Semantic, {
                        Label("",{x.base.base.loc})
                    }));
                throw SemanticAbort();
            }
        }

        if (x.n_values > 0) {
            if (!is_iolength_present) {
                diag.add(Diagnostic(
                    "iolength must be present when positional arguments are used.",
                    Level::Error, Stage::Semantic, {
                        Label("",{x.base.base.loc})
                    }));
                throw SemanticAbort();
            }
            iolength_args.reserve(al, x.n_values);
            for (size_t i = 0; i < x.n_values; i++) {
                this->visit_expr(*x.m_values[i]);
                ASR::expr_t* expr = ASRUtils::EXPR(tmp);
                iolength_args.push_back(al, expr);
            }
        }

        tmp = ASR::make_FileInquire_t(al, x.base.base.loc, x.m_label,
                                  unit, file, iostat, err,
                                  exist, opened, number, named,
                                  name, access, sequential, direct,
                                  form, formatted, unformatted, recl,
                                  nextrec, blank, position, action,
                                  read, write, readwrite, delim,
                                  pad, flen, blocksize, convert,
                                  carriagecontrol, size, pos, iolength, iolength_args.p, iolength_args.n);
    }

    void visit_Flush(const AST::Flush_t& x) {
        std::map<std::string, size_t> argname2idx = {{"unit", 0}, {"err", 1}, {"iomsg", 2}, {"iostat", 3}};
        std::vector<ASR::expr_t*> args;
        std::string node_name = "Flush";
        fill_args_for_rewind_inquire_flush(x, 4, args, 4, argname2idx, node_name);
        if( !args[0] ) {
            diag.add(Diagnostic(
                "unit must be present in flush statement arguments",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }
        ASR::expr_t *unit = args[0], *err = args[1], *iomsg = args[2], *iostat = args[3];
        tmp = ASR::make_Flush_t(al, x.base.base.loc, x.m_label, unit, err, iomsg, iostat);
    }

    void visit_Associate(const AST::Associate_t& x) {
        this->visit_expr(*(x.m_target));
        ASR::expr_t* target = ASRUtils::EXPR(tmp);
        ASR::ttype_t* target_type = ASRUtils::expr_type(target);
        current_variable_type_ = target_type;
        current_struct_type_var_expr = target;
        this->visit_expr(*(x.m_value));
        ASR::expr_t* value = ASRUtils::EXPR(tmp);
        ASR::ttype_t* value_type = ASRUtils::expr_type(value);
        bool is_target_pointer = ASRUtils::is_pointer(target_type);
        if (ASR::is_a<ASR::ArraySection_t>(*target)) {
            ASR::ArraySection_t* array_section = ASR::down_cast<ASR::ArraySection_t>(target);
            ASR::ttype_t* var_type = ASRUtils::expr_type(array_section->m_v);
            if (ASRUtils::is_pointer(var_type)) {
                is_target_pointer = true;
            }
        }
        if ( !is_target_pointer && !ASR::is_a<ASR::FunctionType_t>(*target_type) ) {
            diag.add(Diagnostic(
                "Only a pointer variable can be associated with another variable.",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }

        if (ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(target_type))
            && ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(value_type))) {
            ASR::Struct_t* target_struct = ASR::down_cast<ASR::Struct_t>(
                ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(target)));
            // Set value of `value_struct` to `target_struct` to
            // handle the case of a pointer null constant assignment.
            ASR::Struct_t* value_struct = target_struct;

            if (!ASR::is_a<ASR::PointerNullConstant_t>(*value)) {
                value_struct = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(value)));
            }

            if (ASRUtils::is_derived_type_similar(target_struct, value_struct)) {
                tmp = ASRUtils::make_Associate_t_util(al, x.base.base.loc, target, value);
            }
        } else if (ASRUtils::types_equal(target_type, value_type, target, value)) {
            tmp = ASRUtils::make_Associate_t_util(al, x.base.base.loc, target, value);
        }
    }

    void visit_AssociateBlock(const AST::AssociateBlock_t& x) {
        SymbolTable* new_scope = al.make_new<SymbolTable>(current_scope);
        std::string name = current_scope->get_unique_name("associate_block");
        ASR::asr_t* associate_block = ASR::make_AssociateBlock_t(al, x.base.base.loc,
                                        new_scope, s2c(al, name), nullptr, 0);
        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body);
        for( size_t i = 0; i < x.n_syms; i++ ) {
            this->visit_expr(*x.m_syms[i].m_initializer);
            ASR::expr_t* tmp_expr = ASRUtils::EXPR(tmp);
            ASR::ttype_t* tmp_type = ASRUtils::expr_type(tmp_expr);
            ASR::storage_typeType tmp_storage = ASR::storage_typeType::Default;
            bool create_associate_stmt = false;

            if( ASR::is_a<ASR::Var_t>(*tmp_expr) ) {
                create_associate_stmt = true;
                ASR::Variable_t* variable = ASRUtils::EXPR2VAR(tmp_expr);
                tmp_storage = variable->m_storage;
                tmp_type = variable->m_type;
            } else if (ASR::is_a<ASR::StructInstanceMember_t>(*tmp_expr)) {
                create_associate_stmt = true;
                ASR::StructInstanceMember_t* sim = ASR::down_cast<ASR::StructInstanceMember_t>(tmp_expr);
                tmp_type = sim->m_type;
            } else if( ASR::is_a<ASR::ArraySection_t>(*tmp_expr) ) {
                create_associate_stmt = true;
                ASR::ArraySection_t* tmp_array_section = ASR::down_cast<ASR::ArraySection_t>(tmp_expr);
                ASR::Variable_t* variable = ASRUtils::EXPR2VAR(tmp_array_section->m_v);
                tmp_storage = variable->m_storage;
                ASR::dimension_t *var_dims;
                ASRUtils::extract_dimensions_from_ttype(variable->m_type, var_dims);
                Vec<ASR::dimension_t> tmp_dims; tmp_dims.reserve(al, 1);
                for ( size_t i = 0; i < tmp_array_section->n_args; i++ ) {
                    if (tmp_array_section->m_args[i].m_left) {
                        tmp_dims.push_back(al, var_dims[i]);
                    }
                }
                tmp_type = ASRUtils::duplicate_type(al, variable->m_type, &tmp_dims);
            } else if (ASR::is_a<ASR::ArrayItem_t>(*tmp_expr)) {
                create_associate_stmt = true;
            }

            if ( create_associate_stmt && !ASR::is_a<ASR::Pointer_t>(*tmp_type) ) {
                tmp_type = ASRUtils::duplicate_type_with_empty_dims(al, tmp_type);
                tmp_type = ASRUtils::TYPE(ASR::make_Pointer_t(al, tmp_type->base.loc,
                    ASRUtils::type_get_past_allocatable(tmp_type)));
            }

            std::string name = to_lower(x.m_syms[i].m_name);
            char *name_c = s2c(al, name);
            SetChar variable_dependencies_vec;
            variable_dependencies_vec.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, tmp_type, nullptr, nullptr, name);
            ASR::symbol_t* struct_sym = ASRUtils::import_struct_sym_as_external(al, x.base.base.loc, tmp_expr, current_scope);
            ASR::asr_t *v = ASRUtils::make_Variable_t_util(al, x.base.base.loc, new_scope,
                                                 name_c, variable_dependencies_vec.p, variable_dependencies_vec.size(),
                                                 ASR::intentType::Local, nullptr, nullptr, tmp_storage, tmp_type, struct_sym,
                                                 ASR::abiType::Source, ASR::accessType::Private, ASR::presenceType::Required,
                                                 false);
            new_scope->add_symbol(name, ASR::down_cast<ASR::symbol_t>(v));
            ASR::expr_t* target_var = ASRUtils::EXPR(ASR::make_Var_t(al, v->loc, ASR::down_cast<ASR::symbol_t>(v)));
            if( create_associate_stmt ) {
                ASR::stmt_t* associate_stmt = ASRUtils::STMT(ASRUtils::make_Associate_t_util(al, tmp_expr->base.loc, target_var, tmp_expr));
                body.push_back(al, associate_stmt);
            } else {
                ASRUtils::make_ArrayBroadcast_t_util(al, tmp_expr->base.loc, target_var, tmp_expr);
                ASR::stmt_t* assign_stmt = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, tmp_expr->base.loc, target_var, tmp_expr, nullptr, compiler_options.po.realloc_lhs_arrays, false));
                body.push_back(al, assign_stmt);
            }
        }
        SymbolTable* current_scope_copy = current_scope;
        current_scope = new_scope;
        transform_stmts(body, x.n_body, x.m_body);
        current_scope = current_scope_copy;
        ASR::AssociateBlock_t* associate_block_t = ASR::down_cast<ASR::AssociateBlock_t>(
            ASR::down_cast<ASR::symbol_t>(associate_block));
        associate_block_t->m_body = body.p;
        associate_block_t->n_body = body.size();
        current_scope->add_symbol(name, ASR::down_cast<ASR::symbol_t>(associate_block));
        tmp = ASR::make_AssociateBlockCall_t(al, x.base.base.loc, ASR::down_cast<ASR::symbol_t>(associate_block));
    }

    void set_string_len_if_needed(const AST::Allocate_t& x, Vec<ASR::alloc_arg_t> &alloc_args_vec, bool cond, ASR::expr_t* expr) {
            if (cond && ASRUtils::is_character(*ASRUtils::expr_type(expr))) {
                ASR::expr_t* string_len = ASRUtils::EXPR(
                    ASR::make_StringLen_t(al, x.base.base.loc, expr,
                        ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc,
                            compiler_options.po.default_integer_kind)),
                        nullptr
                    )
                );
                for (size_t i = 0; i < alloc_args_vec.size(); i++) {
                    LCOMPILERS_ASSERT(
                        ASRUtils::is_character(*ASRUtils::expr_type(alloc_args_vec[i].m_a)) ||
                        ASRUtils::is_class_type(ASRUtils::extract_type(ASRUtils::expr_type(alloc_args_vec[i].m_a)))
                    );
                    alloc_args_vec.p[i].m_len_expr = string_len;
                }
            }
        }

    void visit_Allocate(const AST::Allocate_t& x) {
        Vec<ASR::alloc_arg_t> alloc_args_vec;
        alloc_args_vec.reserve(al, x.n_args);
        ASR::ttype_t *int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, compiler_options.po.default_integer_kind));
        ASR::expr_t* const_1 = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, 1, int_type));
        for( size_t i = 0; i < x.n_args; i++ ) {
            ASR::alloc_arg_t new_arg;
            new_arg.m_len_expr = nullptr;
            new_arg.m_type = nullptr;
            new_arg.m_sym_subclass = nullptr;
            ASR::expr_t* tmp_stmt = nullptr;
            new_arg.loc = x.base.base.loc;
            if( x.m_args[i].m_end && !x.m_args[i].m_start && !x.m_args[i].m_step ) {
                this->visit_expr(*(x.m_args[i].m_end));
                tmp_stmt = ASRUtils::EXPR(tmp);

                if (ASR::is_a<ASR::StringItem_t>(*tmp_stmt)) {
                    diag.add(Diagnostic(
                        "Deferred-length CHARACTER variable must be allocated with a character length `character(..)`",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.m_args[i].m_end->base.loc})
                        }));
                    throw SemanticAbort();
                }
            } else if( x.m_args[i].m_start && !x.m_args[i].m_end && x.m_args[i].m_step ) {
                this->visit_expr(*(x.m_args[i].m_step));
                tmp_stmt = ASRUtils::EXPR(tmp);
                if( AST::is_a<AST::FuncCallOrArray_t>(*x.m_args[i].m_start) ) {
                    AST::FuncCallOrArray_t* func_call_t =
                        AST::down_cast<AST::FuncCallOrArray_t>(x.m_args[i].m_start);
                    if( to_lower(std::string(func_call_t->m_func)) == "character" ) {
                        if (func_call_t->n_args > 0 && func_call_t->n_args <= 2
                            && func_call_t->m_args[0].m_end) {
                            visit_expr(*func_call_t->m_args[0].m_end);
                            new_arg.m_len_expr = ASRUtils::EXPR(tmp);
                        } else {
                            LCOMPILERS_ASSERT(func_call_t->n_keywords <= 2);
                            for( size_t i = 0; i < func_call_t->n_keywords; i++ ) {
                                if( to_lower(std::string(func_call_t->m_keywords[i].m_arg)) == "len" ) {
                                    visit_expr(*func_call_t->m_keywords[i].m_value);
                                    new_arg.m_len_expr = ASRUtils::EXPR(tmp);
                                }
                            }
                        }
                    } else {
                        diag.add(Diagnostic(
                            "The type-spec: `" + std::string(func_call_t->m_func)
                            + "` is not supported yet",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.m_args[i].m_start->base.loc})
                            }));
                        throw SemanticAbort();
                    }
                } else if( AST::is_a<AST::Name_t>(*x.m_args[i].m_start) ) {
                    AST::Name_t* name_t = AST::down_cast<AST::Name_t>(x.m_args[i].m_start);
                    ASR::symbol_t *v = current_scope->resolve_symbol(to_lower(name_t->m_id));
                    if (v) {
                        ASR::ttype_t* struct_t = ASRUtils::make_StructType_t_util(al, x.base.base.loc, v, true);
                        new_arg.m_type = struct_t;
                        new_arg.m_sym_subclass = v;
                    } else {
                        diag.add(Diagnostic(
                            "`The type-spec: " + std::string(name_t->m_id)
                            + "` is not supported yet",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.m_args[i].m_start->base.loc})
                            }));
                        throw SemanticAbort();
                    }
                } else {
                    LCOMPILERS_ASSERT_MSG(false, std::to_string(x.m_args[i].m_start->type));
                }
            }
            // Assume that tmp is an `ArraySection` or `ArrayItem`
            if( ASR::is_a<ASR::ArraySection_t>(*tmp_stmt) ) {
                ASR::ArraySection_t* array_ref = ASR::down_cast<ASR::ArraySection_t>(tmp_stmt);
                new_arg.m_a = array_ref->m_v;
                Vec<ASR::dimension_t> dims_vec;
                dims_vec.reserve(al, array_ref->n_args);
                for( size_t j = 0; j < array_ref->n_args; j++ ) {
                    ASR::dimension_t new_dim; new_dim.m_length = nullptr;
                    new_dim.m_start = nullptr;
                    new_dim.loc = array_ref->m_args[j].loc;
                    ASR::expr_t* m_left = array_ref->m_args[j].m_left;
                    if( m_left != nullptr ) {
                        new_dim.m_start = m_left;
                    } else {
                        new_dim.m_start = const_1;
                    }
                    ASR::expr_t* m_right = array_ref->m_args[j].m_right;
                    new_dim.m_length = ASRUtils::compute_length_from_start_end(al, new_dim.m_start, m_right);
                    dims_vec.push_back(al, new_dim);
                }
                new_arg.m_dims = dims_vec.p;
                new_arg.n_dims = dims_vec.size();
                alloc_args_vec.push_back(al, new_arg);
            } else if( ASR::is_a<ASR::ArrayItem_t>(*tmp_stmt) ) {
                ASR::ArrayItem_t* array_ref = ASR::down_cast<ASR::ArrayItem_t>(tmp_stmt);
                new_arg.m_a = array_ref->m_v;
                Vec<ASR::dimension_t> dims_vec;
                dims_vec.reserve(al, array_ref->n_args);
                for( size_t j = 0; j < array_ref->n_args; j++ ) {
                    ASR::dimension_t new_dim; new_dim.m_length = nullptr;
                    new_dim.m_start = nullptr;
                    new_dim.loc = array_ref->m_args[j].loc;
                    new_dim.m_start = const_1;
                    new_dim.m_length = ASRUtils::compute_length_from_start_end(al, new_dim.m_start,
                                            array_ref->m_args[j].m_right);
                    dims_vec.push_back(al, new_dim);
                }
                new_arg.m_dims = dims_vec.p;
                new_arg.n_dims = dims_vec.size();
                alloc_args_vec.push_back(al, new_arg);
            } else if( ASR::is_a<ASR::Var_t>(*tmp_stmt) ||
                       ASR::is_a<ASR::StructInstanceMember_t>(*tmp_stmt) ) {
                new_arg.m_a = tmp_stmt;
                new_arg.m_dims = nullptr;
                new_arg.n_dims = 0;
                alloc_args_vec.push_back(al, new_arg);
            }
        }

        bool cond = x.n_keywords == 0;
        bool stat_cond = false, errmsg_cond = false, source_cond = false, mold_cond = false;
        ASR::expr_t *stat = nullptr, *errmsg = nullptr, *source = nullptr, *mold = nullptr;
        for ( size_t i=0; i<x.n_keywords; i++ ) {
            stat_cond = !stat_cond && (to_lower(x.m_keywords[i].m_arg) == "stat");
            errmsg_cond = !errmsg_cond && (to_lower(x.m_keywords[i].m_arg) == "errmsg");
            source_cond = !source_cond && (to_lower(x.m_keywords[i].m_arg) == "source");
            mold_cond = !mold_cond && (to_lower(x.m_keywords[i].m_arg) == "mold");
            cond = cond || (stat_cond || errmsg_cond || source_cond || mold_cond);
            if( stat_cond ) {
                this->visit_expr(*(x.m_keywords[i].m_value));
                stat = ASRUtils::EXPR(tmp);
            } else if( errmsg_cond ) {
                this->visit_expr(*(x.m_keywords[i].m_value));
                errmsg = ASRUtils::EXPR(tmp);
            } else if( source_cond ) {
                this->visit_expr(*(x.m_keywords[i].m_value));
                source = ASRUtils::EXPR(tmp);
                set_string_len_if_needed(x, alloc_args_vec, source_cond, source);
            } else if ( mold_cond ) {
                this->visit_expr(*(x.m_keywords[i].m_value));
                mold = ASRUtils::EXPR(tmp);
                set_string_len_if_needed(x, alloc_args_vec, mold_cond, mold);
            }
        }

        if ( mold_cond || source_cond ) {
            Vec<ASR::alloc_arg_t> new_alloc_args_vec;
            new_alloc_args_vec.reserve(al, alloc_args_vec.size());
            for (size_t i = 0; i < alloc_args_vec.size(); i++) {
                if ( alloc_args_vec[i].n_dims == 0 ) {
                    if (mold_cond ) {
                        ASR::ttype_t* mold_type = ASRUtils::type_get_past_allocatable_pointer(ASRUtils::expr_type(mold));
                        ASR::ttype_t* a_type = ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(alloc_args_vec[i].m_a));
                        if ( ASRUtils::check_equal_type(mold_type, a_type, mold, alloc_args_vec[i].m_a) ) {
                            if (ASRUtils::is_array(mold_type)) {
                                // Compute m_len_expr for character arrays
                                ASR::expr_t* len_expr = alloc_args_vec[i].m_len_expr;
                                if (len_expr == nullptr && ASRUtils::is_character(*ASRUtils::type_get_past_array(mold_type))) {
                                    len_expr = ASRUtils::EXPR(
                                        ASR::make_StringLen_t(al, x.base.base.loc, mold,
                                            ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc,
                                                compiler_options.po.default_integer_kind)),
                                            nullptr
                                        )
                                    );
                                }
                                if (ASR::is_a<ASR::Array_t>(*mold_type) && ASR::down_cast<ASR::Array_t>(mold_type)->m_dims[0].m_length != nullptr) {
                                    ASR::Array_t* mold_array_type = ASR::down_cast<ASR::Array_t>(mold_type);
                                    ASR::alloc_arg_t new_arg;
                                    new_arg.loc = alloc_args_vec[i].loc;
                                    new_arg.m_a = alloc_args_vec[i].m_a;
                                    new_arg.m_len_expr = len_expr;
                                    new_arg.m_type = nullptr;
                                    new_arg.m_sym_subclass = nullptr;
                                    new_arg.m_dims = mold_array_type->m_dims;
                                    new_arg.n_dims = mold_array_type->n_dims;
                                    new_alloc_args_vec.push_back(al, new_arg);
                                } else {
                                    int n_dims = ASRUtils::extract_n_dims_from_ttype(mold_type);
                                    Vec<ASR::dimension_t> mold_dims_vec; mold_dims_vec.reserve(al, n_dims);
                                    ASR::ttype_t* integer_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, 4));
                                    for(int i=0; i<n_dims; i++) {
                                        ASR::dimension_t dim;
                                        dim.loc = x.base.base.loc;
                                        dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                                            al, x.base.base.loc, 1, integer_type));
                                        dim.m_length = ASRUtils::EXPR(ASR::make_ArraySize_t(
                                al, x.base.base.loc, mold, ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, i+1, integer_type)), integer_type, nullptr));
                                        mold_dims_vec.push_back(al, dim);
                                    }
                                    ASR::alloc_arg_t new_arg;
                                    new_arg.loc = alloc_args_vec[i].loc;
                                    new_arg.m_a = alloc_args_vec[i].m_a;
                                    new_arg.m_len_expr = len_expr;
                                    new_arg.m_type = nullptr;
                                    new_arg.m_sym_subclass = nullptr;
                                    new_arg.m_dims = mold_dims_vec.p;
                                    new_arg.n_dims = mold_dims_vec.size();
                                    new_alloc_args_vec.push_back(al, new_arg);
                                }
                            } else if ( ASR::is_a<ASR::StructType_t>(*mold_type) ) {
                                ASR::alloc_arg_t new_arg;
                                new_arg.loc = alloc_args_vec[i].loc;
                                new_arg.m_a = alloc_args_vec[i].m_a;
                                new_arg.m_len_expr = nullptr;
                                new_arg.m_type = mold_type;
                                new_arg.m_sym_subclass = nullptr;
                                new_arg.m_dims = nullptr;
                                new_arg.n_dims = 0;
                                new_alloc_args_vec.push_back(al, new_arg);
                            } else {
                                diag.add(Diagnostic("The type of the argument is not supported yet for mold.",
                                    Level::Error, Stage::Semantic, {
                                        Label("",{mold->base.loc})
                                    }));
                                throw SemanticAbort();
                            }
                        } else {
                            diag.add(Diagnostic(
                                "The type of the variable to be allocated does not match the type of the mold.",
                                Level::Error, Stage::Semantic, {
                                    Label("",{x.base.base.loc})
                                }));
                            throw SemanticAbort();
                        }
                    } else if ( source_cond && ASRUtils::is_array(ASRUtils::type_get_past_allocatable_pointer(ASRUtils::expr_type(source))) ) {
                        ASR::ttype_t* source_type = ASRUtils::type_get_past_allocatable_pointer(ASRUtils::expr_type(source));
                        ASR::alloc_arg_t new_arg;
                        // Compute m_len_expr for character arrays
                        ASR::expr_t* len_expr = alloc_args_vec[i].m_len_expr;
                        if (len_expr == nullptr && ASRUtils::is_character(*ASRUtils::type_get_past_array(source_type))) {
                            len_expr = ASRUtils::EXPR(
                                ASR::make_StringLen_t(al, x.base.base.loc, source,
                                    ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc,
                                        compiler_options.po.default_integer_kind)),
                                    nullptr
                                )
                            );
                        }
                        if (ASR::down_cast<ASR::Array_t>(source_type)->m_dims[0].m_length != nullptr) {
                            ASR::Array_t* source_array_type = ASR::down_cast<ASR::Array_t>(source_type);
                            new_arg.loc = alloc_args_vec[i].loc;
                            new_arg.m_a = alloc_args_vec[i].m_a;
                            new_arg.m_len_expr = len_expr;
                            new_arg.m_type = nullptr;
                            new_arg.m_sym_subclass = nullptr;
                            new_arg.m_dims = source_array_type->m_dims;
                            new_arg.n_dims = source_array_type->n_dims;
                        } else {
                            int n_dims = ASRUtils::extract_n_dims_from_ttype(source_type);
                            Vec<ASR::dimension_t> source_dims_vec; source_dims_vec.reserve(al, n_dims);
                            ASR::ttype_t* integer_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, 4));
                            for(int i=0; i<n_dims; i++) {
                                ASR::dimension_t dim;
                                dim.loc = x.base.base.loc;
                                dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                                    al, x.base.base.loc, 1, integer_type));
                                dim.m_length = ASRUtils::EXPR(ASR::make_ArraySize_t(
                                    al, x.base.base.loc, source, ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                                        al, x.base.base.loc, i+1, integer_type)), integer_type, nullptr));
                                source_dims_vec.push_back(al, dim);
                            }
                            new_arg.loc = alloc_args_vec[i].loc;
                            new_arg.m_a = alloc_args_vec[i].m_a;
                            new_arg.m_len_expr = len_expr;
                            new_arg.m_type = nullptr;
                            new_arg.m_sym_subclass = nullptr;
                            new_arg.m_dims = source_dims_vec.p;
                            new_arg.n_dims = source_dims_vec.size();
                        }
                        new_alloc_args_vec.push_back(al, new_arg);
                    } else {
                        new_alloc_args_vec.push_back(al, alloc_args_vec[i]);
                    }
                } else {
                    new_alloc_args_vec.push_back(al, alloc_args_vec[i]);
                }
            }
            alloc_args_vec = new_alloc_args_vec;
            if (!source_cond) source = mold;
        }

        if( !cond ) {
            diag.add(Diagnostic(
                "`allocate` statement only "
                "accepts four keyword arguments: "
                "`stat`, `errmsg`, `source` and `mold`",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }

        // Perform all validation checks BEFORE creating any ASR nodes
        // to avoid creating malformed ASR when continuing compilation after errors
        for (size_t i = 0; i < alloc_args_vec.n; i++) {
            ASR::expr_t* alloc_expr = alloc_args_vec.p[i].m_a;
            ASR::ttype_t* alloc_type = ASRUtils::expr_type(alloc_expr);
            
            if (!ASRUtils::is_allocatable(alloc_type) && !ASRUtils::is_pointer(alloc_type)) {
                std::string type_str = ASRUtils::type_to_str_python_expr(alloc_type, alloc_expr);
                ASR::symbol_t* sym = get_allocate_expr_sym(alloc_expr);
                std::string var_name = sym ? ASRUtils::symbol_name(sym) : "variable";
                
                diag.add(Diagnostic(
                    "Allocate should only be called with Allocatable or Pointer type inputs, found " + type_str,
                    Level::Error, Stage::Semantic, {
                        Label("'" + var_name + "' is not allocatable or pointer", {alloc_expr->base.loc})
                    }));
                throw SemanticAbort();
            }
        }
        
        if (source) {
            for (size_t i = 0; i < alloc_args_vec.n ; i++) {
                ASR::ttype_t* source_type = ASRUtils::expr_type(source);
                ASR::ttype_t* var_type = ASRUtils::expr_type(alloc_args_vec.p[i].m_a);

                ASR::dimension_t* source_m_dims = nullptr;
                ASR::dimension_t* var_m_dims = alloc_args_vec.p[i].m_dims;
                ASR::dimension_t* var_m_dims_decl = nullptr;
                size_t source_n_dims = ASRUtils::extract_dimensions_from_ttype(source_type, source_m_dims);
                size_t var_n_dims = alloc_args_vec.p[i].n_dims;
                size_t var_n_dims_decl = ASRUtils::extract_dimensions_from_ttype(var_type, var_m_dims_decl);

                if (!ASRUtils::check_equal_type(source_type, var_type, source, alloc_args_vec.p[i].m_a)) {
                    std::string source_type_str = ASRUtils::type_to_str_fortran_expr(source_type, source);
                    std::string var_type_str = ASRUtils::type_to_str_fortran_expr(var_type, alloc_args_vec.p[i].m_a);
                    diag.add(Diagnostic(
                        "Type mismatch: The `source` argument in `allocate` must have the same type as the allocated variable.\n"
                        "Expected type: " + var_type_str + ", but got: " + source_type_str + ".",
                        Level::Error, Stage::Semantic, {
                            Label("incompatible types in `allocate` statement", {alloc_args_vec.p[i].m_a->base.loc, source->base.loc})
                        }));
                    throw SemanticAbort();
                }

                if (source_n_dims != 0 && ((var_n_dims != 0 && var_n_dims != source_n_dims) || (var_n_dims == 0 && source_n_dims != var_n_dims_decl))) {
                    diag.add(Diagnostic(
                        "Dimension mismatch in `allocate` statement.",
                        Level::Error, Stage::Semantic, {
                            Label("mismatch in dimensions between allocated variable and `source`", {alloc_args_vec.p[i].m_a->base.loc, source->base.loc})
                        }));
                    throw SemanticAbort();
                } else if (source_n_dims == 0 && (var_n_dims == 0 && var_n_dims_decl != 0)) {
                    diag.add(Diagnostic(
                        "Cannot allocate an array from a scalar source.",
                        Level::Error, Stage::Semantic, {
                            Label("allocated variable is an array, but `source` is a scalar",
                                {alloc_args_vec.p[i].m_a->base.loc})
                        }));
                    throw SemanticAbort();
                }

                // Check individual dimension shapes
                if (source_m_dims && var_m_dims) {
                    for (size_t j = 0; j < var_n_dims; j++) {
                        int source_dim_shape = ASRUtils::extract_dim_value_int(source_m_dims[j].m_length);
                        int var_dim_shape = ASRUtils::extract_dim_value_int(var_m_dims[j].m_length);

                        if (source_dim_shape != -1 && var_dim_shape != -1 && source_dim_shape != var_dim_shape) {
                            diag.add(Diagnostic(
                                        "Shape mismatch in `allocate` statement.",
                                        Level::Error, Stage::Semantic, {
                                            Label("shape mismatch in dimension " + std::to_string(j+1),
                                                {alloc_args_vec.p[i].m_a->base.loc, source->base.loc})
                                        }));
                                    throw SemanticAbort();
                        }
                    }
                }
            }
        }

        tmp = ASR::make_Allocate_t(al, x.base.base.loc,
                                    alloc_args_vec.p, alloc_args_vec.size(),
                                    stat, errmsg, source);

        if (source) {
            current_body->push_back(al, ASRUtils::STMT(ASR::make_Allocate_t(al, x.base.base.loc,
                                        alloc_args_vec.p, alloc_args_vec.size(),
                                        stat, errmsg, source)));
            // Pushing assignment statements to source
            for (size_t i = 0; i < alloc_args_vec.n ; i++) {
                // Create assignment statement only for non-struct types
                // All validation was already done above before creating the Allocate ASR node
                if (!ASR::is_a<ASR::StructType_t>(*ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(alloc_args_vec[i].m_a)))) {
                    ASR::stmt_t* assign_stmt = ASRUtils::STMT(
                        ASRUtils::make_Assignment_t_util(
                            al, x.base.base.loc, alloc_args_vec[i].m_a, source, nullptr, compiler_options.po.realloc_lhs_arrays, false
                        )
                    );
                    current_body->push_back(al, assign_stmt);
                }
            }
            tmp = nullptr;
        } else {
            for( size_t i = 0; i < x.n_args; i++ ) {
                if(alloc_args_vec.p[i].m_a && ASRUtils::is_array(ASRUtils::expr_type(alloc_args_vec.p[i].m_a)) &&
                    alloc_args_vec.p[i].n_dims == 0) {
                    diag.add(Diagnostic(
                        "Allocate for arrays should have dimensions specified, "
                        "found only array variable with no dimensions",
                        Level::Error, Stage::Semantic, {
                            Label("Array specification required in allocate statement", {alloc_args_vec.p[i].m_a->base.loc})
                        }));
                    throw SemanticAbort();
                }
            }
        }
    }

    ASR::symbol_t* get_allocate_expr_sym(ASR::expr_t* v) {
        if (ASR::is_a<ASR::Var_t>(*v)) {
            ASR::Var_t *var = ASR::down_cast<ASR::Var_t>(v);
            return var->m_v;
        }
        if (ASR::is_a<ASR::StructInstanceMember_t>(*v)) {
            ASR::StructInstanceMember_t *sim = ASR::down_cast<ASR::StructInstanceMember_t>(v);
            return get_allocate_expr_sym(sim->m_v);
        }
        if (ASR::is_a<ASR::ArrayItem_t>(*v)) {
            ASR::ArrayItem_t *arri = ASR::down_cast<ASR::ArrayItem_t>(v);
            return get_allocate_expr_sym(arri->m_v);
        }
        if (ASR::is_a<ASR::ArraySection_t>(*v)) {
            ASR::ArraySection_t *arrs = ASR::down_cast<ASR::ArraySection_t>(v);
            return get_allocate_expr_sym(arrs->m_v);
        }
        LCOMPILERS_ASSERT(false);
        return nullptr;
    }

    inline void check_for_deallocation(ASR::symbol_t* tmp_sym, const Location& loc) {
        tmp_sym = ASRUtils::symbol_get_past_external(tmp_sym);
        if( !ASR::is_a<ASR::Variable_t>(*tmp_sym) ) {
            diag.add(Diagnostic(
                "Only an allocatable variable symbol "
                "can be deallocated.",
                Level::Error, Stage::Semantic, {
                    Label("",{loc})
                }));
            throw SemanticAbort();
        }

        ASR::Variable_t* tmp_v = ASR::down_cast<ASR::Variable_t>(tmp_sym);
        if( ASR::is_a<ASR::Allocatable_t>(*tmp_v->m_type) &&
            tmp_v->m_storage != ASR::storage_typeType::Save ) {
            // If it is not allocatable, it can also be a pointer
            if (ASR::is_a<ASR::Pointer_t>(*tmp_v->m_type) ||
                ASR::is_a<ASR::Allocatable_t>(*tmp_v->m_type)) {
                // OK
            } else {
                diag.add(Diagnostic(
                    "Only an allocatable or a pointer variable "
                                    "can be deallocated.",
                    Level::Error, Stage::Semantic, {
                        Label("",{loc})
                    }));
                throw SemanticAbort();
            }
        }
    }

    void visit_Deallocate(const AST::Deallocate_t& x) {
        Vec<ASR::expr_t*> arg_vec;
        arg_vec.reserve(al, x.n_args);
        for( size_t i = 0; i < x.n_args; i++ ) {
            this->visit_expr(*(x.m_args[i].m_end));
            ASR::expr_t* tmp_expr = ASRUtils::EXPR(tmp);
            if( ASR::is_a<ASR::Var_t>(*tmp_expr) ) {
                const ASR::Var_t* tmp_var = ASR::down_cast<ASR::Var_t>(tmp_expr);
                ASR::symbol_t* tmp_sym = tmp_var->m_v;
                check_for_deallocation(tmp_sym, tmp_expr->base.loc);
            } else if( ASR::is_a<ASR::StructInstanceMember_t>(*tmp_expr) ) {
                const ASR::StructInstanceMember_t* tmp_struct_ref = ASR::down_cast<ASR::StructInstanceMember_t>(tmp_expr);
                ASR::symbol_t* tmp_member = tmp_struct_ref->m_m;
                check_for_deallocation(tmp_member, tmp_expr->base.loc);
            } else {
                diag.add(Diagnostic(
                    "Cannot deallocate variables in expression " +
                    ASRUtils::type_to_str_python_expr(ASRUtils::expr_type((tmp_expr)), tmp_expr),
                    Level::Error, Stage::Semantic, {
                        Label("",{tmp_expr->base.loc})
                    }));
                throw SemanticAbort();
            }
            arg_vec.push_back(al, tmp_expr);
        }
        tmp = ASR::make_ExplicitDeallocate_t(al, x.base.base.loc,
                                            arg_vec.p, arg_vec.size());
    }

    void visit_Return(const AST::Return_t& x) {
        // TODO
        tmp = ASR::make_Return_t(al, x.base.base.loc);
    }

    void visit_case_stmt(const AST::case_stmt_t& x) {
        Vec<ASR::case_stmt_t*> case_stmts;
        case_stmts.reserve(al, 1);
        visit_case_stmt(x, case_stmts);
        tmp = nullptr;
        tmp_vec.clear();
        for (size_t i = 0; i < case_stmts.size(); i++) {
            tmp_vec.push_back(reinterpret_cast<ASR::asr_t*>(case_stmts[i]));
        }
    }

    void visit_case_stmt(const AST::case_stmt_t& x, Vec<ASR::case_stmt_t*>& case_stmts) {
        AST::CaseStmt_t* Case_Stmt = (AST::CaseStmt_t*)(&(x.base));
        std::vector<ASR::expr_t*> expr_accum;
        auto emit_expr_group = [&](const Location &loc) {
            if (expr_accum.empty()) return;
            Vec<ASR::expr_t*> expr_vec;
            expr_vec.reserve(al, expr_accum.size());
            for (ASR::expr_t* item: expr_accum) {
                expr_vec.push_back(al, item);
            }
            Vec<ASR::stmt_t*> case_body_vec;
            case_body_vec.reserve(al, Case_Stmt->n_body);
            transform_stmts(case_body_vec, Case_Stmt->n_body, Case_Stmt->m_body);
            ASR::case_stmt_t *new_case = ASR::down_cast<ASR::case_stmt_t>(
                ASR::make_CaseStmt_t(al, loc, expr_vec.p, expr_vec.size(),
                                        case_body_vec.p, case_body_vec.size(), false));
            case_stmts.push_back(al, new_case);
            expr_accum.clear();
        };

        for( std::uint32_t i = 0; i < Case_Stmt->n_test; i++ ) {
            if (AST::is_a<AST::CaseCondExpr_t>(*(Case_Stmt->m_test[i]))) {
                AST::CaseCondExpr_t *condexpr
                    = AST::down_cast<AST::CaseCondExpr_t>(Case_Stmt->m_test[i]);
                this->visit_expr(*condexpr->m_cond);
                expr_accum.push_back(ASRUtils::EXPR(tmp));
                continue;
            } else if (AST::is_a<AST::CaseCondRange_t>(*(Case_Stmt->m_test[i]))) {
                emit_expr_group(x.base.loc);
                AST::CaseCondRange_t *condrange
                    = AST::down_cast<AST::CaseCondRange_t>(Case_Stmt->m_test[i]);
                ASR::expr_t *m_start = nullptr, *m_end = nullptr;
                if( condrange->m_start != nullptr ) {
                    this->visit_expr(*(condrange->m_start));
                    m_start = ASRUtils::EXPR(tmp);
                }
                if( condrange->m_end != nullptr ) {
                    this->visit_expr(*(condrange->m_end));
                    m_end = ASRUtils::EXPR(tmp);
                }
                Vec<ASR::stmt_t*> case_body_vec;
                case_body_vec.reserve(al, Case_Stmt->n_body);
                transform_stmts(case_body_vec, Case_Stmt->n_body, Case_Stmt->m_body);
                ASR::case_stmt_t *new_case = ASR::down_cast<ASR::case_stmt_t>(
                    ASR::make_CaseStmt_Range_t(al, x.base.loc, m_start, m_end,
                                                case_body_vec.p, case_body_vec.size()));
                case_stmts.push_back(al, new_case);
                continue;
            } else {
                LCOMPILERS_ASSERT(false);
            }
        }
        emit_expr_group(x.base.loc);
    }

    void visit_Select(const AST::Select_t& x) {
        this->visit_expr(*(x.m_test));
        ASR::expr_t* a_test = ASRUtils::EXPR(tmp);
        Vec<ASR::case_stmt_t*> a_body_vec;
        a_body_vec.reserve(al, x.n_body);
        Vec<ASR::stmt_t*> def_body;
        def_body.reserve(al, 1);
        for( std::uint32_t i = 0; i < x.n_body; i++ ) {
            AST::case_stmt_t *body = x.m_body[i];
            if (AST::is_a<AST::CaseStmt_Default_t>(*body)) {
                if (def_body.size() != 0) {
                    diag.add(Diagnostic(
                        "Default case present more than once",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                AST::CaseStmt_Default_t *d =
                        AST::down_cast<AST::CaseStmt_Default_t>(body);
                transform_stmts(def_body, d->n_body, d->m_body);
            } else {
                visit_case_stmt(*body, a_body_vec);
            }
        }

        tmp = ASR::make_Select_t(al, x.base.base.loc, x.m_stmt_name, a_test, a_body_vec.p,
                           a_body_vec.size(), def_body.p, def_body.size(), false);
    }

    void visit_SelectRank(const AST::SelectRank_t& x) {
        if ( !x.m_selector ) {
            diag.add(Diagnostic(
                "Selector expression is missing in select rank statement.",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }
        visit_expr(*x.m_selector);
        ASR::expr_t* m_selector = ASRUtils::EXPR(tmp);
        std::string array_var_name = "";
        if (ASR::is_a<ASR::Var_t>(*m_selector)) {
            ASR::Var_t* selector_var = ASR::down_cast<ASR::Var_t>(m_selector);
            if (ASR::is_a<ASR::Variable_t>(*selector_var->m_v)) {
                ASR::Variable_t* selector_variable = ASR::down_cast<ASR::Variable_t>(selector_var->m_v);
                array_var_name = selector_variable->m_name;
            }
        }
        ASR::ttype_t* selector_type = ASRUtils::expr_type(m_selector);
        if( !ASR::is_a<ASR::Array_t>(*selector_type)) {
            // Throw error if selector is not an array
        }
        Vec<ASR::rank_stmt_t*> select_rank_body;
        Vec<ASR::stmt_t*> select_rank_default;
        select_rank_body.reserve(al, x.n_body);
        select_rank_default.reserve(al, 1);
        for(size_t i=0; i<x.n_body; i++) {
            SymbolTable* parent_scope = current_scope;
            current_scope = al.make_new<SymbolTable>(parent_scope);
            switch (x.m_body[i]->type) {
                case AST::rank_stmtType::RankExpr: {
                    // rank(1), rank(2), rank(3) etc. : Need to be an IntegerConstant
                    AST::RankExpr_t* rank_expr = AST::down_cast<AST::RankExpr_t>(x.m_body[i]);
                    visit_expr(*rank_expr->m_value);
                    ASR::expr_t* rank_expr_value = ASRUtils::EXPR(tmp);
                    if( !ASR::is_a<ASR::IntegerConstant_t>(*rank_expr_value) ) {
                        diag.add(Diagnostic(
                            "Rank expression must be an integer constant.",
                            Level::Error, Stage::Semantic, {
                                Label("",{rank_expr->m_value->base.loc})
                            }));
                        throw SemanticAbort();
                    }
                    int rank = ASR::down_cast<ASR::IntegerConstant_t>(rank_expr_value)->m_n;
                    assumed_rank_arrays[array_var_name] = rank;
                    Vec<ASR::stmt_t*> rank_body; rank_body.reserve(al, rank_expr->n_body);
                    if (x.m_assoc_name) {
                        ASR::ttype_t* variable_type = ASRUtils::extract_type(selector_type);
                        ASR::ttype_t* desc_type = ASRUtils::create_array_type_with_empty_dims(al, rank, variable_type);
                        ASR::ttype_t* target_type = ASRUtils::make_Pointer_t_util(al, m_selector->base.loc, desc_type);
                        ASR::symbol_t* type_decl = nullptr;
                        if (ASR::is_a<ASR::StructType_t>(*variable_type)) {
                            type_decl = ASRUtils::get_struct_sym_from_struct_expr(m_selector);
                        }
                        ASR::symbol_t* assoc_sym = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Variable_t_util(al, x.base.base.loc, 
                            current_scope, x.m_assoc_name, nullptr, 0, ASR::intentType::Local, 
                            nullptr, nullptr, ASR::storage_typeType::Default, target_type, type_decl, 
                            ASR::abiType::Source, ASR::accessType::Public, ASR::presenceType::Required, false));
                        current_scope->add_symbol(x.m_assoc_name, assoc_sym);
                        ASR::expr_t* assoc_var = ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc, assoc_sym));
                        ASR::expr_t* cast_expr = nullptr;
                        if (ASR::is_a<ASR::StructType_t>(*variable_type) && rank == 0) {
                            cast_expr = m_selector;
                        } else {
                            cast_expr = ASRUtils::EXPR(ASRUtils::make_ArrayPhysicalCast_t_util(al, m_selector->base.loc, m_selector,
                                ASR::array_physical_typeType::AssumedRankArray, ASR::array_physical_typeType::DescriptorArray, desc_type, nullptr));
                        }
                        ASR::stmt_t* assoc_stmt = ASRUtils::STMT(ASRUtils::make_Associate_t_util(al, x.base.base.loc, assoc_var, cast_expr));
                        rank_body.push_back(al, assoc_stmt);
                    }
                    transform_stmts(rank_body, rank_expr->n_body, rank_expr->m_body);
                    std::string block_name = parent_scope->get_unique_name("~select_rank_block_");
                    ASR::symbol_t* block_sym = ASR::down_cast<ASR::symbol_t>(ASR::make_Block_t(al, 
                        rank_expr->base.base.loc, current_scope, s2c(al, block_name),
                        nullptr, 0));
                    ASR::Block_t* block_t = ASR::down_cast<ASR::Block_t>(block_sym);
                    block_t->m_body = rank_body.p;
                    block_t->n_body = rank_body.size();
                    parent_scope->add_symbol(block_name, block_sym);
                    Vec<ASR::stmt_t*> block_call_stmt; block_call_stmt.reserve(al, 1);
                    block_call_stmt.push_back(al, ASRUtils::STMT(ASR::make_BlockCall_t(al, rank_expr->base.base.loc, -1, block_sym)));
                    select_rank_body.push_back(al, ASR::down_cast<ASR::rank_stmt_t>(ASR::make_RankExpr_t(al, rank_expr->base.base.loc,
                        rank_expr_value, block_call_stmt.p, block_call_stmt.size())));
                    break;
                }

                case AST::rank_stmtType::RankDefault: {
                    // rank default
                    // TODO: throw error if more than one default case statement is present
                    SymbolTable* current_scope_copy = current_scope;
                    current_scope = parent_scope;
                    AST::RankDefault_t* rank_default = AST::down_cast<AST::RankDefault_t>(x.m_body[i]);
                    transform_stmts(select_rank_default, rank_default->n_body, rank_default->m_body);
                    current_scope = current_scope_copy;
                    break;
                }

                case AST::rank_stmtType::RankStar: {
                    // rank (*)
                }
            }
            current_scope = parent_scope;
        }

        tmp = ASR::make_SelectRank_t(al, x.base.base.loc, m_selector, select_rank_body.p, 
                    select_rank_body.size(), select_rank_default.p, select_rank_default.size());
    }

    void visit_SelectType(const AST::SelectType_t& x) {
        // TODO: We might need to re-order all ASR::TypeStmtName
        // before ASR::ClassStmt as per GFortran's semantics
        if( !x.m_selector ) {
            diag.add(Diagnostic(
                "Selector expression is missing in select type statement.",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }
        visit_expr(*x.m_selector);
        ASR::expr_t* m_selector = ASRUtils::EXPR(tmp);
        Vec<ASR::stmt_t*> select_type_default;
        select_type_default.reserve(al, 1);
        Vec<ASR::type_stmt_t*> select_type_body;
        select_type_body.reserve(al, x.n_body);
        ASR::Variable_t* selector_variable = nullptr;
        ASR::ttype_t* selector_variable_type = nullptr;
        ASR::symbol_t* select_variable_m_type_declaration = nullptr;
        char** selector_variable_dependencies = nullptr;
        size_t selector_variable_n_dependencies = 0;
        if( ASR::is_a<ASR::Var_t>(*m_selector) ) {
            ASR::symbol_t* selector_sym = ASR::down_cast<ASR::Var_t>(m_selector)->m_v;
            LCOMPILERS_ASSERT(ASR::is_a<ASR::Variable_t>(*selector_sym));
            selector_variable = ASR::down_cast<ASR::Variable_t>(selector_sym);
            selector_variable_type = selector_variable->m_type;
            select_variable_m_type_declaration = selector_variable->m_type_declaration;
            selector_variable_dependencies = selector_variable->m_dependencies;
            selector_variable_n_dependencies = selector_variable->n_dependencies;
        } else if( ASR::is_a<ASR::StructInstanceMember_t>(*m_selector) ) {
            ASR::symbol_t* selector_sym = ASR::down_cast<ASR::StructInstanceMember_t>(m_selector)->m_m;
            LCOMPILERS_ASSERT(ASR::is_a<ASR::ExternalSymbol_t>(*selector_sym));
            ASR::symbol_t* selector_ext = ASR::down_cast<ASR::ExternalSymbol_t>(selector_sym)->m_external;
            LCOMPILERS_ASSERT(ASR::is_a<ASR::Variable_t>(*selector_ext));
            selector_variable = ASR::down_cast<ASR::Variable_t>(selector_ext);
            selector_variable_type = selector_variable->m_type;
            select_variable_m_type_declaration = ASRUtils::get_struct_sym_from_struct_expr(m_selector);
            selector_variable_dependencies = selector_variable->m_dependencies;
            selector_variable_n_dependencies = selector_variable->n_dependencies;
        }

        auto make_typed_selector_view_type = [&](const Location& loc,
                ASR::ttype_t* selector_type, ASR::ttype_t* guard_type) -> ASR::ttype_t* {
            bool is_ptr = ASR::is_a<ASR::Pointer_t>(*selector_type);
            bool is_allocatable = ASR::is_a<ASR::Allocatable_t>(*selector_type);
            ASR::ttype_t* base_type = ASRUtils::type_get_past_allocatable(
                ASRUtils::type_get_past_pointer(selector_type));

            ASR::ttype_t* result = guard_type;
            if (ASR::is_a<ASR::Array_t>(*base_type)) {
                if (!is_allocatable) is_ptr = true;

                ASR::Array_t* arr = ASR::down_cast<ASR::Array_t>(base_type);
                bool is_assumed_rank = arr->m_physical_type == ASR::array_physical_typeType::AssumedRankArray;

                if (is_assumed_rank) {
                    int known_rank = selector_variable ? [&]() {
                        auto it = assumed_rank_arrays.find(selector_variable->m_name);
                        return it != assumed_rank_arrays.end() ? it->second : 0;
                    }() : 0;
                    if (known_rank > 0) {
                        result = ASRUtils::create_array_type_with_empty_dims(al, known_rank, guard_type);
                    } else {
                        result = ASRUtils::TYPE(ASR::make_Array_t(
                            al, loc, guard_type, arr->m_dims, arr->n_dims,
                            ASR::array_physical_typeType::AssumedRankArray));
                    }
                } else {
                    result = ASRUtils::make_Array_t_util(
                        al, loc, guard_type, arr->m_dims, arr->n_dims,
                        ASR::abiType::Source, false, arr->m_physical_type, true);
                }
            }
            if (is_ptr) result = ASRUtils::make_Pointer_t_util(al, loc, result);
            if (is_allocatable) result = ASRUtils::TYPE(ASRUtils::make_Allocatable_t_util(al, loc, result));
            return result;
        };

        auto element_types_differ = [&](ASR::ttype_t* from, ASR::ttype_t* to) -> bool {
            ASR::ttype_t* from_el = ASRUtils::extract_type(from);
            ASR::ttype_t* to_el = ASRUtils::extract_type(to);
            if (from_el->type != to_el->type) return true;
            if (ASR::is_a<ASR::StructType_t>(*from_el)) {
                return ASR::down_cast<ASR::StructType_t>(from_el)->m_is_unlimited_polymorphic !=
                       ASR::down_cast<ASR::StructType_t>(to_el)->m_is_unlimited_polymorphic;
            }
            return ASRUtils::extract_kind_from_ttype_t(from_el) !=
                   ASRUtils::extract_kind_from_ttype_t(to_el);
        };

        for( size_t i = 0; i < x.n_body; i++ ) {
            SymbolTable* parent_scope = current_scope;
            current_scope = al.make_new<SymbolTable>(parent_scope);
            ASR::Variable_t* assoc_variable = nullptr;
            ASR::symbol_t* assoc_sym = nullptr;
            bool need_shadow_assoc = (x.m_assoc_name != nullptr);
            if (!need_shadow_assoc && selector_variable) {
                ASR::ttype_t* sel_base = ASRUtils::type_get_past_allocatable(
                    ASRUtils::type_get_past_pointer(selector_variable->m_type));
                need_shadow_assoc = ASR::is_a<ASR::Array_t>(*sel_base);
            }
            char* assoc_name = nullptr;
            if (need_shadow_assoc) {
                assoc_name = x.m_assoc_name ? x.m_assoc_name :
                    (selector_variable ? selector_variable->m_name : nullptr);
            }
            if (assoc_name) {
                assoc_sym = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Variable_t_util(
                    al, x.base.base.loc, current_scope, assoc_name,
                    nullptr, 0, ASR::intentType::Local, nullptr, nullptr,
                    ASR::storage_typeType::Default, nullptr, nullptr, ASR::abiType::Source,
                    ASR::accessType::Public, ASR::presenceType::Required, false));
                current_scope->add_symbol(std::string(assoc_name), assoc_sym);
                assoc_variable = ASR::down_cast<ASR::Variable_t>(assoc_sym);
            } else if (selector_variable) {
                assoc_variable = selector_variable;
            }
            switch( x.m_body[i]->type ) {
                case AST::type_stmtType::ClassStmt: {
                    AST::ClassStmt_t* class_stmt = AST::down_cast<AST::ClassStmt_t>(x.m_body[i]);
                    ASR::symbol_t* sym = current_scope->resolve_symbol(to_lower(std::string(class_stmt->m_id)));
                    if( assoc_variable ) {
                        ASR::ttype_t* selector_type = nullptr;
                        ASR::symbol_t* selector_m_type_declaration = nullptr;
                        ASR::symbol_t* sym_underlying = ASRUtils::symbol_get_past_external(sym);
                        if( ASR::is_a<ASR::Struct_t>(*sym_underlying) ) {
                            selector_type = ASRUtils::make_StructType_t_util(al, sym->base.loc, sym, !compiler_options.new_classes);
                            // Apply array wrapper if selector is an array
                            if (selector_variable_type && ASRUtils::is_array(selector_variable_type)) {
                                // Use the guard type (selector_type) as element type, preserving array structure from selector
                                selector_type = make_typed_selector_view_type(
                                    class_stmt->base.base.loc, ASRUtils::type_get_past_allocatable(selector_variable_type), selector_type);
                            } else {
                                selector_type = ASRUtils::make_Pointer_t_util(al, sym->base.loc, ASRUtils::extract_type(selector_type));
                            }
                            selector_m_type_declaration = sym;
                        } else {
                            diag.add(Diagnostic(
                                "Only class and derived type in select type test expressions.",
                                Level::Error, Stage::Semantic, {
                                    Label("",{class_stmt->base.base.loc})
                                }));
                            throw SemanticAbort();
                        }
                        SetChar assoc_deps;
                        assoc_deps.reserve(al, 1);
                        ASRUtils::collect_variable_dependencies(al, assoc_deps, selector_type, nullptr, nullptr, ASRUtils::symbol_name(sym_underlying));
                        assoc_variable->m_dependencies = assoc_deps.p;
                        assoc_variable->n_dependencies = assoc_deps.size();
                        assoc_variable->m_type = selector_type;
                        assoc_variable->m_type_declaration = selector_m_type_declaration;
                    }
                    Vec<ASR::stmt_t*> class_stmt_body;
                    class_stmt_body.reserve(al, class_stmt->n_body);
                    if( assoc_sym ) {
                        class_stmt_body.push_back(al, ASRUtils::STMT(ASRUtils::make_Associate_t_util(al,
                            class_stmt->base.base.loc, ASRUtils::EXPR(ASR::make_Var_t(al,
                            class_stmt->base.base.loc, assoc_sym)), m_selector)) );
                    }
                    std::string block_name = parent_scope->get_unique_name("~select_type_block_");
                    ASR::symbol_t* block_sym = ASR::down_cast<ASR::symbol_t>(ASR::make_Block_t(
                                                    al, class_stmt->base.base.loc,
                                                    current_scope, s2c(al, block_name),
                                                    nullptr, 0));
                    transform_stmts(class_stmt_body, class_stmt->n_body, class_stmt->m_body);
                    ASR::Block_t* block_t_ = ASR::down_cast<ASR::Block_t>(block_sym);
                    block_t_->m_body = class_stmt_body.p;
                    block_t_->n_body = class_stmt_body.size();
                    parent_scope->add_symbol(block_name, block_sym);
                    Vec<ASR::stmt_t*> block_call_stmt;
                    block_call_stmt.reserve(al, 1);
                    block_call_stmt.push_back(al, ASRUtils::STMT(ASR::make_BlockCall_t(al, class_stmt->base.base.loc, -1, block_sym)));
                    select_type_body.push_back(al, ASR::down_cast<ASR::type_stmt_t>(ASR::make_ClassStmt_t(al,
                        class_stmt->base.base.loc, sym, block_call_stmt.p, block_call_stmt.size())));
                    break;
                }
                case AST::type_stmtType::TypeStmtName: {
                    AST::TypeStmtName_t* type_stmt_name = AST::down_cast<AST::TypeStmtName_t>(x.m_body[i]);
                    ASR::symbol_t* sym = current_scope->resolve_symbol(to_lower(std::string(type_stmt_name->m_name)));
                    if( assoc_variable ) {
                        ASR::ttype_t* selector_type = nullptr;
                        ASR::symbol_t* selector_m_type_declaration = nullptr;
                        ASR::symbol_t* sym_underlying = ASRUtils::symbol_get_past_external(sym);
                        if( ASR::is_a<ASR::Struct_t>(*sym_underlying) ) {
                            selector_type = ASRUtils::make_StructType_t_util(al, sym->base.loc, sym, true);
                            // Apply array wrapper if selector is an array
                            if (selector_variable_type && ASRUtils::is_array(selector_variable_type)) {
                                // Use the guard type (selector_type) as element type, preserving array structure from selector
                                selector_type = make_typed_selector_view_type(
                                    type_stmt_name->base.base.loc, ASRUtils::type_get_past_allocatable(selector_variable_type), selector_type);
                            } else {
                                selector_type = ASRUtils::make_Pointer_t_util(al, sym->base.loc, ASRUtils::extract_type(selector_type));
                            }
                            selector_m_type_declaration = sym;
                        } else {
                            diag.add(Diagnostic(
                                "Only class and derived type in select type test expressions.",
                                Level::Error, Stage::Semantic, {
                                    Label("",{type_stmt_name->base.base.loc})
                                }));
                            throw SemanticAbort();
                        }
                        SetChar assoc_deps;
                        assoc_deps.reserve(al, 1);
                        ASRUtils::collect_variable_dependencies(al, assoc_deps, selector_type, nullptr, nullptr, ASRUtils::symbol_name(sym_underlying));
                        assoc_variable->m_dependencies = assoc_deps.p;
                        assoc_variable->n_dependencies = assoc_deps.size();
                        assoc_variable->m_type = selector_type;
                        assoc_variable->m_type_declaration = selector_m_type_declaration;
                    }
                    Vec<ASR::stmt_t*> type_stmt_name_body;
                    type_stmt_name_body.reserve(al, type_stmt_name->n_body);
                    if( assoc_sym ) {
                        type_stmt_name_body.push_back(al, ASRUtils::STMT(ASRUtils::make_Associate_t_util(al,
                            type_stmt_name->base.base.loc, ASRUtils::EXPR(ASR::make_Var_t(al,
                            type_stmt_name->base.base.loc, assoc_sym)), m_selector)) );
                    }
                    std::string block_name = parent_scope->get_unique_name("~select_type_block_");
                    ASR::symbol_t* block_sym = ASR::down_cast<ASR::symbol_t>(ASR::make_Block_t(
                                                    al, type_stmt_name->base.base.loc,
                                                    current_scope, s2c(al, block_name),
                                                    nullptr, 0));
                    transform_stmts(type_stmt_name_body, type_stmt_name->n_body, type_stmt_name->m_body);
                    ASR::Block_t* block_t_ = ASR::down_cast<ASR::Block_t>(block_sym);
                    block_t_->m_body = type_stmt_name_body.p;
                    block_t_->n_body = type_stmt_name_body.size();
                    parent_scope->add_symbol(block_name, block_sym);
                    Vec<ASR::stmt_t*> block_call_stmt;
                    block_call_stmt.reserve(al, 1);
                    block_call_stmt.push_back(al, ASRUtils::STMT(ASR::make_BlockCall_t(al, type_stmt_name->base.base.loc, -1, block_sym)));
                    select_type_body.push_back(al, ASR::down_cast<ASR::type_stmt_t>(ASR::make_TypeStmtName_t(al,
                        type_stmt_name->base.base.loc, sym, block_call_stmt.p, block_call_stmt.size())));
                    break;
                }
                case AST::type_stmtType::TypeStmtType: {
                    AST::TypeStmtType_t* type_stmt_type = AST::down_cast<AST::TypeStmtType_t>(x.m_body[i]);
                    ASR::ttype_t* selector_type = nullptr;
                    ASR::ttype_t* selector_ttype = nullptr;
                    if( assoc_variable ) {
                        Vec<ASR::dimension_t> m_dims;
                        m_dims.reserve(al, 1);
                        std::string assoc_variable_name = std::string(assoc_variable->m_name);
                        ASR::symbol_t *type_declaration;
                        selector_type = determine_type(type_stmt_type->base.base.loc,
                                                       assoc_variable_name,
                                                       type_stmt_type->m_vartype, false, false, m_dims,
                                                       nullptr,
                                                       type_declaration, ASR::abiType::Source);
                        // Convert AssumedLength to DeferredLength for select type associate statements
                        if (ASRUtils::is_character(*selector_type)) {
                            ASR::String_t* str_type = ASR::down_cast<ASR::String_t>(
                                ASRUtils::extract_type(selector_type));
                            if (str_type->m_len_kind == ASR::string_length_kindType::AssumedLength) {
                                str_type->m_len_kind = ASR::string_length_kindType::DeferredLength;
                            }
                        }
                        selector_ttype = selector_variable ? selector_variable->m_type : nullptr;
                        if (selector_ttype) {
                            ASR::ttype_t* view_type = make_typed_selector_view_type(
                                type_stmt_type->base.base.loc, selector_ttype, selector_type);
                            assoc_variable->m_type = view_type;
                        } else {
                            assoc_variable->m_type = selector_type;
                        }
                        SetChar assoc_deps;
                        assoc_deps.reserve(al, 1);
                        ASRUtils::collect_variable_dependencies(al, assoc_deps, assoc_variable->m_type, nullptr, nullptr, assoc_variable_name);
                        assoc_variable->m_dependencies = assoc_deps.p;
                        assoc_variable->n_dependencies = assoc_deps.size();
                        assoc_variable->m_type_declaration = type_declaration;
                    }
                    Vec<ASR::stmt_t*> type_stmt_type_body;
                    type_stmt_type_body.reserve(al, type_stmt_type->n_body);
                    if( assoc_sym ) {
                        ASR::expr_t* assoc_value = m_selector;
                        bool is_array_selector = selector_variable && selector_ttype &&
                            ASRUtils::is_array(ASRUtils::type_get_past_allocatable(
                                ASRUtils::type_get_past_pointer(selector_ttype)));
                        bool needs_cast = is_array_selector &&
                            element_types_differ(ASRUtils::expr_type(m_selector), assoc_variable->m_type);
                        if (needs_cast) {
                            ASR::ttype_t* assoc_base = ASRUtils::type_get_past_allocatable(
                                ASRUtils::type_get_past_pointer(assoc_variable->m_type));
                            ASR::ttype_t* cast_type = assoc_variable->m_type;
                            if (ASR::is_a<ASR::Array_t>(*assoc_base) &&
                                !ASRUtils::is_assumed_rank_array(ASRUtils::expr_type(m_selector))) {
                                ASR::Array_t* assoc_arr = ASR::down_cast<ASR::Array_t>(assoc_base);
                                Vec<ASR::dimension_t> dims;
                                dims.reserve(al, assoc_arr->n_dims);
                                for (size_t d = 0; d < assoc_arr->n_dims; d++) {
                                    ASR::dimension_t dim;
                                    dim.loc = type_stmt_type->base.base.loc;
                                    dim.m_start = ASRUtils::get_bound<SemanticAbort>(
                                        m_selector, (int)d + 1, "lbound", al, diag);
                                    dim.m_length = ASRUtils::get_size(
                                        m_selector, (int)d + 1, al);
                                    dims.push_back(al, dim);
                                }
                                cast_type = ASRUtils::make_Array_t_util(
                                    al, type_stmt_type->base.base.loc,
                                    assoc_arr->m_type, dims.p, dims.size(),
                                    ASR::abiType::Source, false, assoc_arr->m_physical_type, true);
                            }
                            ASR::array_physical_typeType old_ptype =
                                ASRUtils::extract_physical_type(ASRUtils::expr_type(m_selector));
                            ASR::array_physical_typeType new_ptype =
                                ASRUtils::extract_physical_type(assoc_variable->m_type);
                            assoc_value = ASRUtils::EXPR(ASRUtils::make_ArrayPhysicalCast_t_util(
                                al, type_stmt_type->base.base.loc, m_selector,
                                old_ptype, new_ptype, cast_type, nullptr));
                        }
                        type_stmt_type_body.push_back(al, ASRUtils::STMT(ASRUtils::make_Associate_t_util(al,
                            type_stmt_type->base.base.loc, ASRUtils::EXPR(ASR::make_Var_t(al,
                            type_stmt_type->base.base.loc, assoc_sym)), assoc_value)) );
                    }
                    std::string block_name = parent_scope->get_unique_name("~select_type_block_");
                    ASR::symbol_t* block_sym = ASR::down_cast<ASR::symbol_t>(ASR::make_Block_t(
                                                    al, type_stmt_type->base.base.loc,
                                                    current_scope, s2c(al, block_name),
                                                    nullptr, 0));
                    // Track type guard context for polymorphic intrinsic validation
                    ASR::ttype_t* prev_select_type_guard = current_select_type_guard;
                    current_select_type_guard = selector_type;
                    transform_stmts(type_stmt_type_body, type_stmt_type->n_body, type_stmt_type->m_body);
                    current_select_type_guard = prev_select_type_guard;
                    ASR::Block_t* block_t_ = ASR::down_cast<ASR::Block_t>(block_sym);
                    block_t_->m_body = type_stmt_type_body.p;
                    block_t_->n_body = type_stmt_type_body.size();
                    parent_scope->add_symbol(block_name, block_sym);
                    Vec<ASR::stmt_t*> block_call_stmt;
                    block_call_stmt.reserve(al, 1);
                    block_call_stmt.push_back(al, ASRUtils::STMT(ASR::make_BlockCall_t(al, type_stmt_type->base.base.loc, -1, block_sym)));
                    select_type_body.push_back(al, ASR::down_cast<ASR::type_stmt_t>(ASR::make_TypeStmtType_t(al,
                        type_stmt_type->base.base.loc, selector_type, block_call_stmt.p, block_call_stmt.size())));
                    break;
                }
                case AST::type_stmtType::ClassDefault: {
                    AST::ClassDefault_t* class_default = AST::down_cast<AST::ClassDefault_t>(x.m_body[i]);
                    if( assoc_variable ) {
                        ASR::ttype_t* target_type = ASRUtils::type_get_past_allocatable(selector_variable_type);
                        if (!ASR::is_a<ASR::Pointer_t>(*target_type)) {
                            target_type = ASRUtils::make_Pointer_t_util(al, class_default->base.base.loc, target_type);
                        }
                        assoc_variable->m_type = target_type;
                        ASR::symbol_t* type_decl = select_variable_m_type_declaration;
                        if (type_decl) {
                            type_decl = ASRUtils::import_struct_type(al, type_decl, current_scope);
                        }
                        assoc_variable->m_type_declaration = type_decl;
                        assoc_variable->m_dependencies = selector_variable_dependencies;
                        assoc_variable->n_dependencies = selector_variable_n_dependencies;
                    }
                    Vec<ASR::stmt_t*> class_default_body;
                    class_default_body.reserve(al, class_default->n_body);
                    if( assoc_sym && class_default->n_body > 0 ) {
                        class_default_body.push_back(al, ASRUtils::STMT(ASRUtils::make_Associate_t_util(al,
                            class_default->base.base.loc, ASRUtils::EXPR(ASR::make_Var_t(al,
                            class_default->base.base.loc, assoc_sym)), m_selector)) );
                    }
                    std::string block_name = parent_scope->get_unique_name("~select_type_block_");
                    ASR::symbol_t* block_sym = ASR::down_cast<ASR::symbol_t>(ASR::make_Block_t(
                                                    al, class_default->base.base.loc,
                                                    current_scope, s2c(al, block_name),
                                                    nullptr, 0));
                    transform_stmts(class_default_body, class_default->n_body, class_default->m_body);
                    ASR::Block_t* block_t_ = ASR::down_cast<ASR::Block_t>(block_sym);
                    block_t_->m_body = class_default_body.p;
                    block_t_->n_body = class_default_body.size();
                    parent_scope->add_symbol(block_name, block_sym);
                    Vec<ASR::stmt_t*> block_call_stmt;
                    block_call_stmt.reserve(al, 1);
                    block_call_stmt.push_back(al, ASRUtils::STMT(ASR::make_BlockCall_t(al, class_default->base.base.loc, -1, block_sym)));
                    for( auto& stmt: block_call_stmt ) {
                        select_type_default.push_back(al, stmt);
                    }
                    break;
                }
                default: {
                    diag.add(Diagnostic(
                        std::to_string(x.m_body[i]->type) + " statement not supported yet in select type",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.m_body[i]->base.loc})
                        }));
                    throw SemanticAbort();
                }
            }
            current_scope = parent_scope;
        }

        if (selector_variable) {
            selector_variable->m_type = selector_variable_type;
            selector_variable->m_type_declaration = select_variable_m_type_declaration;
            selector_variable->m_dependencies = selector_variable_dependencies;
            selector_variable->n_dependencies = selector_variable_n_dependencies;
        }

        tmp = ASR::make_SelectType_t(al, x.base.base.loc, m_selector, x.m_assoc_name,
                                     select_type_body.p, select_type_body.size(),
                                     select_type_default.p, select_type_default.size());
    }

    template <typename T>
    void visit_SubmoduleModuleCommon(const T& x) {
        SymbolTable *old_scope = current_scope;
        ASR::symbol_t *t = current_scope->get_symbol(to_lower(x.m_name));
        ASR::Module_t *v = ASR::down_cast<ASR::Module_t>(t);
        current_module_dependencies.clear(al);
        current_scope = v->m_symtab;
        current_module = v;

        for (size_t i=0; i<x.n_decl; i++) {
            if(x.m_decl[i]->type == AST::unit_decl2Type::Template){
                visit_unit_decl2(*x.m_decl[i]);
            }
        }

        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body);
        auto& scope_data = data_structure[current_scope->counter];
        if (scope_data.size()>0) {
            for(auto it: scope_data) {
                body.push_back(al, it);
            }
        }
        scope_data.clear();

        transform_stmts(body, x.n_body, x.m_body);
        // We have to visit unit_decl_2 because in the example, the Template is directly inside the module and
        // Template is a unit_decl_2

        for (size_t i=0; i<x.n_contains; i++) {
            visit_program_unit(*x.m_contains[i]);
        }

        if( current_module_dependencies.size() > 0 ) {
            SetChar module_dependencies;
            module_dependencies.from_pointer_n_copy(al, v->m_dependencies, v->n_dependencies);
            for( size_t i = 0; i < current_module_dependencies.size(); i++ ) {
                module_dependencies.push_back(al, current_module_dependencies[i]);
            }
            v->m_dependencies = module_dependencies.p;
            v->n_dependencies = module_dependencies.size();
        }

        current_scope = old_scope;
        current_module = nullptr;
        tmp = nullptr;
    }

    void visit_Submodule(const AST::Submodule_t &x) {
        visit_SubmoduleModuleCommon(x);
    }

    void visit_Module(const AST::Module_t &x) {
        visit_SubmoduleModuleCommon(x);
    }

    void visit_Use(const AST::Use_t& /* x */) {
        // handled in symbol table visitor
    }
    void remove_common_variable_declarations(SymbolTable* current_scope) {
        // iterate over all symbols in symbol table and check if any of them is present in common_variables_hash
        // if yes, then remove it from scope
        std::map<std::string, ASR::symbol_t*> syms = current_scope->get_scope();
        for (auto it = syms.begin(); it != syms.end(); ++it) {
            if (ASR::is_a<ASR::Variable_t>(*(it->second))) {
                ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(it->second);
                uint64_t hash = get_hash((ASR::asr_t*) var);
                if (common_variables_hash.find(hash) != common_variables_hash.end()) {
                    current_scope->erase_symbol(it->first);
                }
            }
        }
    }


    void visit_Program(const AST::Program_t &x) {
        program_count++;
        if (program_count >= 2) {
        return;  // Just skip 
        }
        SymbolTable *old_scope = current_scope;
        ASR::symbol_t *t = current_scope->get_symbol(to_lower(x.m_name));
        ASR::Program_t *v = ASR::down_cast<ASR::Program_t>(t);
        current_scope = v->m_symtab;
        starting_m_body = x.m_body;
        starting_n_body = x.n_body;
        collect_labels();

        for (size_t i=0; i<x.n_decl; i++) {
            visit_unit_decl2(*x.m_decl[i]);
        }

        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body);
        auto& scope_data_prog = data_structure[current_scope->counter];
        if (scope_data_prog.size()>0) {
            for(auto it: scope_data_prog) {
                body.push_back(al, it);
            }
        }
        scope_data_prog.clear();

        transform_stmts(body, x.n_body, x.m_body);
        handle_format();
        v->m_body = body.p;
        v->n_body = body.size();

        replace_ArrayItem_in_SubroutineCall(al, compiler_options.legacy_array_sections, current_scope, compiler_options.po.default_integer_kind);

        for (size_t i=0; i<x.n_contains; i++) {
            visit_program_unit(*x.m_contains[i]);
        }

        ASRUtils::update_call_args(al, current_scope, compiler_options.implicit_interface, changed_external_function_symbol);

        starting_m_body = nullptr;
        starting_n_body =  0;
        remove_common_variable_declarations(current_scope);
        current_scope = old_scope;
        tmp = nullptr;
    }

    ASR::stmt_t* create_implicit_deallocate_subrout_call(ASR::stmt_t* x) {
        ASR::SubroutineCall_t* subrout_call = ASR::down_cast<ASR::SubroutineCall_t>(x);
        const ASR::symbol_t* subrout_sym = ASRUtils::symbol_get_past_external(subrout_call->m_name);
        if( ! ASR::is_a<ASR::Function_t>(*subrout_sym)
            || ASR::down_cast<ASR::Function_t>(subrout_sym)->m_return_var != nullptr ) {
            return nullptr;
        }
        ASR::Function_t* subrout = ASR::down_cast<ASR::Function_t>(subrout_sym);
        Vec<ASR::expr_t*> del_syms;
        del_syms.reserve(al, 1);
        for( size_t i = 0; i < subrout_call->n_args; i++ ) {
            if (!subrout_call->m_args[i].m_value) continue;
            // Bounds check: call may have more args than definition (optional args)
            if (i >= subrout->n_args) continue;
            if (!subrout->m_args[i] || subrout->m_args[i]->type != ASR::exprType::Var) continue;

            const ASR::Var_t* orig_arg_var = ASR::down_cast<ASR::Var_t>(subrout->m_args[i]);
            const ASR::symbol_t* orig_sym = ASRUtils::symbol_get_past_external(orig_arg_var->m_v);
            if (orig_sym->type != ASR::symbolType::Variable) continue;
            ASR::Variable_t* orig_var = ASR::down_cast<ASR::Variable_t>(orig_sym);

            if (orig_var->m_intent != ASR::intentType::Out) continue;
            if (!ASRUtils::is_allocatable(orig_var->m_type) &&
                !ASR::is_a<ASR::StructType_t>(*orig_var->m_type)) continue;

            if( subrout_call->m_args[i].m_value &&
                subrout_call->m_args[i].m_value->type == ASR::exprType::Var ) {
                const ASR::Var_t* arg_var = ASR::down_cast<ASR::Var_t>(subrout_call->m_args[i].m_value);
                const ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(arg_var->m_v);
                if( sym->type == ASR::symbolType::Variable ) {
                    ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(sym);
                    if( ASR::is_a<ASR::Allocatable_t>(*var->m_type) &&
                        ASR::is_a<ASR::Allocatable_t>(*orig_var->m_type) &&
                        orig_var->m_intent == ASR::intentType::Out ) {
                        del_syms.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, x->base.loc, arg_var->m_v)));
                    }
                    // Check struct-type members
                    if (ASR::is_a<ASR::StructType_t>(*ASRUtils::symbol_type(sym))
                        && !ASRUtils::is_class_type(ASRUtils::symbol_type(sym))
                        && ASR::down_cast<ASR::Variable_t>(orig_sym)->m_intent == ASR::intentType::Out) {
                        ASR::Struct_t* struct_type = ASR::down_cast<ASR::Struct_t>(
                            ASRUtils::symbol_get_past_external(var->m_type_declaration));
                        while (struct_type) {
                            SymbolTable* sym_table_of_struct = struct_type->m_symtab;
                            for(auto struct_member : sym_table_of_struct->get_scope()){
                                if(ASR::is_a<ASR::Variable_t>(*struct_member.second) &&
                                    ASRUtils::is_allocatable(ASRUtils::symbol_type(struct_member.second))){
                                    del_syms.push_back(al, ASRUtils::EXPR(
                                        ASRUtils::getStructInstanceMember_t(al,subrout_call->m_args[i].m_value->base.loc,
                                        (ASR::asr_t*)subrout_call->m_args[i].m_value,
                                        const_cast<ASR::symbol_t*>(sym), struct_member.second, current_scope)));
                                }
                            }
                            if (struct_type->m_parent) {
                                struct_type = ASR::down_cast<ASR::Struct_t>(
                                    ASRUtils::symbol_get_past_external(struct_type->m_parent));
                            } else {
                                struct_type = nullptr;
                            }
                        }
                    }
                }
            }
            // NOTE: StructInstanceMember actuals are NOT handled here because
            // the pass_insert_deallocate pass adds deallocation at function entry
            // for allocatable intent(out) dummies. Adding ImplicitDeallocate at
            // the call site would cause double-free.
        }
        if( del_syms.size() == 0 ) {
            return nullptr;
        }
        return ASRUtils::STMT(ASR::make_ImplicitDeallocate_t(al, x->base.loc,
                    del_syms.p, del_syms.size()));
    }

    void visit_Entry(const AST::Entry_t& /*x*/) {
        tmp = nullptr;
    }

    void add_subroutine_call(const Location& loc, std::string entry_function_name, std::string parent_function_name,
                            int label, bool is_function = false) {
        ASR::symbol_t* entry_function_sym = current_scope->resolve_symbol(entry_function_name);
        if (entry_function_sym == nullptr) {
            diag.add(Diagnostic(
                "Entry function '" + entry_function_name + "' not defined",
                Level::Error, Stage::Semantic, {
                    Label("",{loc})
                }));
            throw SemanticAbort();
        }
        if (ASR::is_a<ASR::Variable_t>(*entry_function_sym)) {
            // we have to find the function symbol
            entry_function_sym = current_scope->parent->resolve_symbol(entry_function_name);
        }
        ASR::Function_t* entry_function = ASR::down_cast<ASR::Function_t>(entry_function_sym);

        std::string master_function_name = parent_function_name + "_main__lcompilers";
        ASR::symbol_t* master_function_sym = current_scope->resolve_symbol(master_function_name);
        if (master_function_sym == nullptr) {
            diag.add(Diagnostic(
                "Master function '" + master_function_name + "' not defined",
                Level::Error, Stage::Semantic, {
                    Label("",{loc})
                }));
            throw SemanticAbort();
        }
        ASR::Function_t* master_function = ASR::down_cast<ASR::Function_t>(master_function_sym);

        // iterate over argument mapping of entry function, keep only those arguments
        // which are present at vector<int> index, rest all are 0
        std::vector<int> indices = entry_function_arguments_mapping[entry_function_name];
        Vec<ASR::call_arg_t> args; args.reserve(al, master_function->n_args);

        for (size_t i = 0; i < master_function->n_args; i++) {
            ASR::expr_t* arg = nullptr;
            if (i == 0) {
                ASR::ttype_t *int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, compiler_options.po.default_integer_kind));
                arg = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, label, int_type));
            } else if (std::find(indices.begin(), indices.end(), i) != indices.end()) {
                ASR::expr_t* master_function_arg = master_function->m_args[i];
                ASR::symbol_t* sym = nullptr;
                if (ASR::is_a<ASR::Var_t>(*master_function_arg)) {
                    ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(master_function_arg);
                    std::string sym_name = ASRUtils::symbol_name(var->m_v);
                    sym = entry_function->m_symtab->resolve_symbol(sym_name);
                }
                LCOMPILERS_ASSERT(sym != nullptr);
                arg = ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
            } else {
                ASR::expr_t* master_function_arg = master_function->m_args[i];
                ASR::ttype_t* type = ASRUtils::expr_type(master_function_arg);
                ASR::ttype_t* raw_type = ASRUtils::type_get_past_array(type);

                if (ASR::is_a<ASR::Real_t>(*raw_type)) {
                    arg = ASRUtils::EXPR(ASR::make_RealConstant_t(al, loc, 0.0, raw_type));
                } else if (ASR::is_a<ASR::Integer_t>(*raw_type)) {
                    arg = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 0, raw_type));
                } else if (ASR::is_a<ASR::Logical_t>(*raw_type)) {
                    arg = ASRUtils::EXPR(ASR::make_LogicalConstant_t(al, loc, false, raw_type));
                } else if (ASR::is_a<ASR::String_t>(*raw_type)) {
                    ASR::ttype_t* character_type = ASRUtils::TYPE(ASR::make_String_t(al, loc, 1,
                        ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 0,
                            ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)))),
                        ASR::string_length_kindType::ExpressionLength,
                        ASR::string_physical_typeType::DescriptorString));
                    arg = ASRUtils::EXPR(ASR::make_StringConstant_t(al, loc, s2c(al, ""), character_type));
                } else {
                    diag.add(Diagnostic(
                        "Argument type not supported yet",
                        Level::Error, Stage::Semantic, {
                            Label("",{loc})
                        }));
                    throw SemanticAbort();
                }
                if (ASRUtils::is_array(type)) {
                    Vec<ASR::dimension_t> dims; dims.reserve(al, 1);
                    ASR::dimension_t dim; dim.loc = loc; dim.m_start = nullptr; dim.m_length = nullptr;
                    dims.push_back(al, dim);
                    ASR::ttype_t* arr_type = ASRUtils::TYPE(ASR::make_Array_t(al, type->base.loc,
                                            raw_type, dims.p, dims.n, ASR::array_physical_typeType::DescriptorArray));
                    type = ASRUtils::TYPE(ASR::make_Pointer_t(al, type->base.loc, arr_type));
                    // create a variable of type: type
                    std::string sym_name = "";
                    if (ASR::is_a<ASR::Var_t>(*master_function_arg)) {
                        ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(master_function_arg);
                        sym_name = ASRUtils::symbol_name(var->m_v);
                    }
                    ASR::asr_t* var_asr = ASRUtils::make_Variable_t_util(al, master_function_arg->base.loc,
                                            entry_function->m_symtab, s2c(al,sym_name), nullptr, 0, ASR::intentType::Local,
                                            nullptr, nullptr, ASR::storage_typeType::Default, type, ASRUtils::get_struct_sym_from_struct_expr(master_function_arg), ASR::abiType::Source,
                                            ASR::accessType::Public, ASR::presenceType::Required, false);
                    ASR::symbol_t* var_sym = ASR::down_cast<ASR::symbol_t>(var_asr);
                    entry_function->m_symtab->add_or_overwrite_symbol(sym_name, var_sym);
                    arg = ASRUtils::EXPR(ASR::make_Var_t(al, loc, var_sym));
                }
            }
            ASR::call_arg_t call_arg; call_arg.loc = loc; call_arg.m_value = arg; args.push_back(al, call_arg);
        }

        ASR::stmt_t* stmt = nullptr;
        if (is_function) {
            // make an assignment to the return variable of entry function
            ASR::expr_t* lhs = entry_function->m_return_var;
            ASR::expr_t* rhs = ASRUtils::EXPR(ASR::make_FunctionCall_t(al, loc, master_function_sym,
                                master_function_sym, args.p, args.n, ASRUtils::expr_type(lhs), nullptr, nullptr));

            stmt = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, loc, lhs, rhs, nullptr, compiler_options.po.realloc_lhs_arrays, false));

        } else {
            stmt = ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(al,loc, master_function_sym,
                                        master_function_sym, args.p, args.n, nullptr, nullptr, compiler_options.implicit_argument_casting, current_scope, current_function_dependencies));
        }
        LCOMPILERS_ASSERT(stmt != nullptr);

        Vec<ASR::stmt_t*> body; body.reserve(al, entry_function->n_body + 1);
        for (size_t i = 0; i < entry_function->n_body; i++) {
            body.push_back(al, entry_function->m_body[i]);
        }

        body.push_back(al, stmt);
        entry_function->m_body = body.p;
        entry_function->n_body = body.size();

        // add master function to dependencies of entry function
        SetChar entry_function_dependencies;
        entry_function_dependencies.from_pointer_n_copy(al, entry_function->m_dependencies, entry_function->n_dependencies);
        entry_function_dependencies.push_back(al, s2c(al, master_function_name));
        entry_function->m_dependencies = entry_function_dependencies.p;
        entry_function->n_dependencies = entry_function_dependencies.size();
    }

    void visit_stmts_helper(std::vector<AST::stmt_t*> ast_stmt_vector, std::vector<ASR::stmt_t*> &stmt_vector,
                            std::vector<ASR::asr_t*> &tmp_vec, std::string original_function_name, ASR::expr_t* return_var,
                            std::vector<ASR::stmt_t*> &after_return_stmt_entry_function, bool is_last = false, bool is_main_function = false) {
        bool return_encountered = false;
        bool entry_encountered = false;
        for (auto &ast_stmt: ast_stmt_vector) {
            bool is_pushed = false;
            int64_t label = stmt_label(ast_stmt);
            if (label != 0) {
                ASR::asr_t *l = ASR::make_GoToTarget_t(al, ast_stmt->base.loc, label,
                                    s2c(al, std::to_string(label)));
                // body.push_back(al, ASR::down_cast<ASR::stmt_t>(l));
                if (is_main_function && return_encountered && !entry_encountered) {
                    after_return_stmt_entry_function.push_back(ASRUtils::STMT(l));
                    is_pushed = true;
                } else if (is_main_function && entry_encountered && return_encountered) {
                    break;
                } else {
                    stmt_vector.push_back(ASRUtils::STMT(l));
                    is_pushed = true;
                }
                if (!is_main_function && !is_pushed) {
                    if (return_encountered && is_last) {
                        after_return_stmt_entry_function.push_back(ASRUtils::STMT(l));
                        is_pushed = true;
                    } else {
                        stmt_vector.push_back(ASRUtils::STMT(l));
                        is_pushed = true;
                    }
                }
            }
            if (ast_stmt->type == AST::stmtType::Entry) {
                entry_encountered = true;
            }
            this->visit_stmt(*ast_stmt);
            ASR::stmt_t* tmp_stmt = nullptr;
            if (tmp != nullptr) {
                tmp_stmt = ASRUtils::STMT(tmp);
                if (tmp_stmt->type == ASR::stmtType::SubroutineCall) {
                    ASR::stmt_t* impl_decl = create_implicit_deallocate_subrout_call(tmp_stmt);
                    if (impl_decl != nullptr) {
                        stmt_vector.push_back(impl_decl);
                    }
                }
                if (tmp_stmt->type == ASR::stmtType::Assignment) {
                    // if it is an assignment to any of the entry function return variables, then
                    // make an assignment to return variable of master function
                    ASR::Assignment_t* assignment = ASR::down_cast<ASR::Assignment_t>(tmp_stmt);
                    ASR::expr_t* target = assignment->m_target;
                    if (ASR::is_a<ASR::Var_t>(*target)) {
                        ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(target);
                        std::string var_name = ASRUtils::symbol_name(var->m_v);
                        if (original_function_name == var_name || (entry_functions[original_function_name].find(var_name) != entry_functions[original_function_name].end())) {
                            // this is an assignment to entry function return variable
                            // make an assignment to return variable of master function
                            if (return_var != nullptr) {
                                ASR::expr_t* lhs = return_var; ASR::expr_t* rhs = assignment->m_value;
                                ASR::stmt_t* stmt = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, tmp_stmt->base.loc, lhs, rhs, nullptr, compiler_options.po.realloc_lhs_arrays, false));
                                tmp_stmt = stmt;
                            }
                        }
                    }
                }
                if (is_main_function && return_encountered && !entry_encountered) {
                    after_return_stmt_entry_function.push_back(tmp_stmt);
                    is_pushed = true;
                } else if (is_main_function && entry_encountered && return_encountered) {
                    break;
                } else {
                    stmt_vector.push_back(tmp_stmt);
                    is_pushed = true;
                }
                if (!is_main_function && !is_pushed) {
                    if (return_encountered && is_last) {
                        after_return_stmt_entry_function.push_back(tmp_stmt);
                    } else {
                        stmt_vector.push_back(tmp_stmt);
                    }
                }
                if (tmp_stmt->type == ASR::stmtType::Return) {
                    return_encountered = true;
                }
            } else if (!tmp_vec.empty()) {
                for(auto &x: tmp_vec) {
                    stmt_vector.push_back(ASRUtils::STMT(x));
                }
                tmp_vec.clear();
            }
            tmp = nullptr;
        }
    }

    template <typename T>
    void populate_master_function(const T& x, const Location &loc, std::string master_function_name) {
        // populate master function
        std::string original_function_name = master_function_name.substr(0, master_function_name.find("_main__lcompilers"));
        ASR::symbol_t* master_function_sym = current_scope->resolve_symbol(master_function_name);
        ASR::Function_t* master_function = ASR::down_cast<ASR::Function_t>(master_function_sym);

        std::vector<ASR::asr_t*> tmp_vector; std::vector<ASR::stmt_t*> stmt_vector; std::vector<ASR::stmt_t*> after_return_stmt_entry_function;
        SetChar current_function_dependencies_copy = current_function_dependencies;
        current_function_dependencies.clear(al);

        // create if else statements for entry functions
        ASR::expr_t* left = nullptr;
        ASR::symbol_t* left_sym = master_function->m_symtab->resolve_symbol("entry__lcompilers");
        LCOMPILERS_ASSERT(left_sym != nullptr);
        left = ASRUtils::EXPR(ASR::make_Var_t(al, loc, left_sym));
        ASR::ttype_t *int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, compiler_options.po.default_integer_kind));
        ASR::ttype_t* logical_type = ASRUtils::TYPE(ASR::make_Logical_t(al, loc, compiler_options.po.default_integer_kind));
        int num_entry_functions = entry_functions[original_function_name].size();
        for(int i = 0; i < num_entry_functions + 1; i++) {
            ASR::stmt_t* if_stmt = nullptr;
            ASR::expr_t* right = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, i+1, int_type));
            ASR::expr_t* cmp = ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, loc, left, ASR::cmpopType::Eq, right,
                                logical_type, nullptr));

            Vec<ASR::stmt_t*> if_body; if_body.reserve(al, 1);
            ASR::stmt_t* go_to_label = ASRUtils::STMT(ASR::make_GoTo_t(al, loc, i+1, s2c(al, std::to_string(i+1))));
            if_body.push_back(al, go_to_label);

            if_stmt = ASRUtils::STMT(ASR::make_If_t(al, loc, nullptr, cmp, if_body.p, if_body.size(), nullptr, 0));
            stmt_vector.push_back(if_stmt);
        }

        // handle subroutine
        int go_to_target = 1;
        ASR::stmt_t* go_to_target_stmt = ASRUtils::STMT(ASR::make_GoToTarget_t(al, loc, go_to_target, s2c(al, std::to_string(go_to_target))));
        stmt_vector.push_back(go_to_target_stmt); go_to_target++;

        std::vector<AST::stmt_t*> subroutine_stmt_vector;
        for (size_t i = 0; i < x.n_body; i++) {
            subroutine_stmt_vector.push_back(x.m_body[i]);
        }
        Vec<ASR::stmt_t*> master_function_body; master_function_body.reserve(al, stmt_vector.size());
        current_body = &master_function_body;
        SymbolTable* old_scope = current_scope;
        current_scope = master_function->m_symtab;
        visit_stmts_helper(subroutine_stmt_vector, stmt_vector, tmp_vector, original_function_name, master_function->m_return_var, after_return_stmt_entry_function, false, true);

        // handle entry functions
        for (auto &it: entry_functions[original_function_name]) {
            go_to_target_stmt = ASRUtils::STMT(ASR::make_GoToTarget_t(al, loc, go_to_target, s2c(al, std::to_string(go_to_target))));
            stmt_vector.push_back(go_to_target_stmt); go_to_target++;
            // check if it is last entry function
            bool is_last = it.first == entry_functions[original_function_name].rbegin()->first;
            visit_stmts_helper(it.second, stmt_vector, tmp_vector, original_function_name, master_function->m_return_var, after_return_stmt_entry_function, is_last);
        }
        for (auto &it: stmt_vector) {
            master_function_body.push_back(al, it);
        }
        for (auto &it: after_return_stmt_entry_function) {
            master_function_body.push_back(al, it);
        }

        master_function->m_body = master_function_body.p;
        master_function->n_body = master_function_body.size();

        master_function->m_dependencies = current_function_dependencies.p;
        master_function->n_dependencies = current_function_dependencies.size();
        current_function_dependencies = current_function_dependencies_copy;

        current_scope = old_scope;
    }

    void visit_Procedure(const AST::Procedure_t &x) {
        SymbolTable* old_scope = current_scope;
        ASR::symbol_t* t = current_scope->get_symbol(to_lower(x.m_name));
        starting_m_body = x.m_body;
        starting_n_body = x.n_body;
        collect_labels();

        ASR::Function_t* v = ASR::down_cast<ASR::Function_t>(t);
        current_scope = v->m_symtab;

        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body);
        auto& scope_data_sub = data_structure[current_scope->counter];
        if (scope_data_sub.size()>0) {
            for(auto it: scope_data_sub) {
                body.push_back(al, it);
            }
        }
        scope_data_sub.clear();
        SetChar current_function_dependencies_copy = current_function_dependencies;
        current_function_dependencies.clear(al);
        transform_stmts(body, x.n_body, x.m_body);
        handle_format();
        SetChar func_deps;
        func_deps.from_pointer_n_copy(al, v->m_dependencies, v->n_dependencies);
        for( auto& itr: current_function_dependencies ) {
            func_deps.push_back(al, s2c(al, itr));
        }
        current_function_dependencies = current_function_dependencies_copy;
        v->m_body = body.p;
        v->n_body = body.size();
        v->m_dependencies = func_deps.p;
        v->n_dependencies = func_deps.size();
        for (size_t i=0; i<x.n_contains; i++) {
            visit_program_unit(*x.m_contains[i]);
        }

        starting_m_body = nullptr;
        starting_n_body = 0;
        remove_common_variable_declarations(current_scope);
        current_scope = old_scope;
        tmp = nullptr;
    }

    void visit_Subroutine(const AST::Subroutine_t &x) {
    // TODO: add SymbolTable::lookup_symbol(), which will automatically return
    // an error
    // TODO: add SymbolTable::get_symbol(), which will only check in Debug mode
        SymbolTable *old_scope = current_scope;
        ASR::symbol_t *t = current_scope->get_symbol(to_lower(x.m_name));
        starting_m_body = x.m_body;
        starting_n_body = x.n_body;
        collect_labels();
        if( t->type == ASR::symbolType::GenericProcedure ) {
            std::string subrout_name = to_lower(x.m_name) + "~genericprocedure";
            t = current_scope->get_symbol(subrout_name);
        }

        if (x.n_temp_args > 0) {
            t = ASRUtils::symbol_symtab(t)->get_symbol(to_lower(x.m_name));
        }

        ASR::Function_t *v = ASR::down_cast<ASR::Function_t>(t);
        current_scope = v->m_symtab;
        for (size_t i=0; i<x.n_decl; i++) {
            is_Function = true;
            if(x.m_decl[i]->type == AST::unit_decl2Type::Instantiate)
                visit_unit_decl2(*x.m_decl[i]);
            is_Function = false;
        }
        if (entry_functions.find(to_lower(v->m_name)) != entry_functions.end()) {
            /*
                Subroutine is parent of entry function.
                For all template functions, create a subroutine call to master function and add it to the body
                of the subroutine.
            */
            int label = 1;
            add_subroutine_call(x.base.base.loc, v->m_name, v->m_name, label++);
            for (auto &it: entry_functions[v->m_name]) {
                add_subroutine_call(x.base.base.loc, it.first, v->m_name, label++);
            }
            // populate master function
            std::string master_function_name = to_lower(v->m_name) + "_main__lcompilers";
            populate_master_function(x, x.base.base.loc, master_function_name);

            current_scope = old_scope;
            tmp = nullptr;
            return;
        }

        Vec<ASR::stmt_t*> body;
        SetChar current_function_dependencies_copy = current_function_dependencies;
        current_function_dependencies.clear(al);
        body.reserve(al, x.n_body);
        auto& scope_data_func = data_structure[current_scope->counter];
        if (scope_data_func.size()>0) {
            for(auto it: scope_data_func) {
                body.push_back(al, it);
            }
        }
        scope_data_func.clear();
        transform_stmts(body, x.n_body, x.m_body);
        handle_format();
        SetChar func_deps;
        func_deps.from_pointer_n_copy(al, v->m_dependencies, v->n_dependencies);
        for( auto& itr: current_function_dependencies ) {
            func_deps.push_back(al, s2c(al, itr));
        }
        current_function_dependencies = current_function_dependencies_copy;
        v->m_body = body.p;
        v->n_body = body.size();
        v->m_dependencies = func_deps.p;
        v->n_dependencies = func_deps.size();

        replace_ArrayItem_in_SubroutineCall(al, compiler_options.legacy_array_sections, current_scope, compiler_options.po.default_integer_kind);

        for (size_t i=0; i<x.n_contains; i++) {
            visit_program_unit(*x.m_contains[i]);
        }

        ASRUtils::update_call_args(al, current_scope, compiler_options.implicit_interface, changed_external_function_symbol);

        starting_m_body = nullptr;
        starting_n_body = 0;
        remove_common_variable_declarations(current_scope);
        current_scope = old_scope;
        tmp = nullptr;
    }

    void visit_Function(const AST::Function_t &x) {
        starting_m_body = x.m_body;
        starting_n_body = x.n_body;
        collect_labels();
        SymbolTable *old_scope = current_scope;
        ASR::symbol_t *t = current_scope->get_symbol(to_lower(x.m_name));
        if( t->type == ASR::symbolType::GenericProcedure ) {
            t = current_scope->get_symbol(to_lower(x.m_name) + "~genericprocedure");
        }

        if (x.n_temp_args > 0) {
            t = ASRUtils::symbol_symtab(t)->get_symbol(to_lower(x.m_name));
        }

        ASR::Function_t *v = ASR::down_cast<ASR::Function_t>(t);
        current_scope = v->m_symtab;
        if (entry_functions.find(to_lower(v->m_name)) != entry_functions.end()) {
            /*
                Subroutine is parent of entry function.
                For all template functions, create a subroutine call to master function and add it to the body
                of the subroutine.
            */
            int label = 1;
            add_subroutine_call(x.base.base.loc, v->m_name, v->m_name, label++, true);
            for (auto &it: entry_functions[v->m_name]) {
                add_subroutine_call(x.base.base.loc, it.first, v->m_name, label++, true);
            }
            // populate master function
            std::string master_function_name = to_lower(v->m_name) + "_main__lcompilers";
            populate_master_function(x, x.base.base.loc, master_function_name);

            current_scope = old_scope;
            tmp = nullptr;
            return;
        }
        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body);
        auto& scope_data_func2 = data_structure[current_scope->counter];
        if (scope_data_func2.size()>0) {
            for(auto it: scope_data_func2) {
                body.push_back(al, it);
            }
        }
        scope_data_func2.clear();
        SetChar current_function_dependencies_copy = current_function_dependencies;
        current_function_dependencies.clear(al);
        transform_stmts(body, x.n_body, x.m_body);
        handle_format();
        SetChar func_deps;
        func_deps.from_pointer_n_copy(al, v->m_dependencies, v->n_dependencies);
        for( auto& itr: current_function_dependencies ) {
            func_deps.push_back(al, s2c(al, itr));
        }
        current_function_dependencies = current_function_dependencies_copy;
        v->m_body = body.p;
        v->n_body = body.size();
        v->m_dependencies = func_deps.p;
        v->n_dependencies = func_deps.size();

        replace_ArrayItem_in_SubroutineCall(al, compiler_options.legacy_array_sections, current_scope, compiler_options.po.default_integer_kind);

        for (size_t i=0; i<x.n_contains; i++) {
            visit_program_unit(*x.m_contains[i]);
        }

        for (size_t i=0; i<x.n_decl; i++) {
            is_Function = true;
            if(x.m_decl[i]->type == AST::unit_decl2Type::Instantiate)
                visit_unit_decl2(*x.m_decl[i]);
            is_Function = false;
        }

        ASRUtils::update_call_args(al, current_scope, compiler_options.implicit_interface, changed_external_function_symbol);

        starting_m_body = nullptr;
        starting_n_body = 0;
        remove_common_variable_declarations(current_scope);
        current_scope = old_scope;
        tmp = nullptr;
    }

    void visit_Assign(const AST::Assign_t &x) {
        std::string var_name = to_lower(std::string{x.m_variable});
        ASR::symbol_t *sym = current_scope->resolve_symbol(var_name);
        ASR::ttype_t *int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, compiler_options.po.default_integer_kind));
        if (!sym) {
            labels.insert(var_name);
            Str a_var_name_f;
            a_var_name_f.from_str(al, var_name);
            SetChar variable_dependencies_vec;
            variable_dependencies_vec.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, int_type, nullptr, nullptr, var_name);
            ASR::asr_t* a_variable = ASRUtils::make_Variable_t_util(al, x.base.base.loc, current_scope, a_var_name_f.c_str(al),
                                                          variable_dependencies_vec.p, variable_dependencies_vec.size(),
                                                          ASR::intentType::Local, nullptr, nullptr,
                                                          ASR::storage_typeType::Default, int_type, nullptr,
                                                          ASR::abiType::Source, ASR::Public, ASR::presenceType::Optional, false);
            current_scope->add_symbol(var_name, ASR::down_cast<ASR::symbol_t>(a_variable));
            sym = ASR::down_cast<ASR::symbol_t>(a_variable);
        } else {
            // symbol found but we need to have consistent types
            if (!ASR::is_a<ASR::Variable_t>(*sym)) {
                diag.add(Diagnostic(
                        "Assign target needs to be a variable.",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
            }

            if (std::find(labels.begin(), labels.end(), var_name) == labels.end()) {
                labels.insert(var_name);
            }
            // ensure the precision is consistent
            auto v = ASR::down_cast<ASR::Variable_t>(sym);
            auto t = ASR::down_cast<ASR::Integer_t>(v->m_type);
            t->m_kind = 4;
        }

        // ASSIGN XXX TO k -- XXX can only be integer for now.
        ASR::expr_t* target_var = ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc, sym));
        ASR::expr_t* tmp_expr = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, x.m_assign_label, int_type));
        ASRUtils::make_ArrayBroadcast_t_util(al, x.base.base.loc, target_var, tmp_expr);
        tmp = (ASR::asr_t*)ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, x.base.base.loc, target_var, tmp_expr, nullptr, compiler_options.po.realloc_lhs_arrays, false));
    }

    /* Returns true if `x` is a statement function, false otherwise.
    Example of statement functions:
    integer :: A
    A(i,j) = i*j
    // implicit typing on
    A(i,j) = i*j
    Examples of non statement functions:
    integer :: A(3, 5)
    A(i,j) = i*j
    */
    bool is_statement_function( const AST::Assignment_t &x ) {
        if (AST::is_a<AST::FuncCallOrArray_t>(*x.m_target)) {
            // Look for the type of *x.m_target in symbol table, if it is integer, real, logical, or nullptr then it is a statement function
            // unless it is being indexed as an array
            AST::FuncCallOrArray_t *func_call_or_array = AST::down_cast<AST::FuncCallOrArray_t>(x.m_target);
            if (func_call_or_array->n_member > 0) {
                // This is part of a derived type, so it is not a statement function
                return false;
            }
            std::string var_name = func_call_or_array->m_func;
            var_name = to_lower(var_name);
            ASR::symbol_t *sym = current_scope->resolve_symbol(var_name);
            if (sym==nullptr) {
                if (compiler_options.implicit_typing) {
                    return true;
                } else {
                    return false;
                }
            } else {
                if (ASR::is_a<ASR::Variable_t>(*sym)) {
                    auto v = ASR::down_cast<ASR::Variable_t>(sym);
                    if (ASR::is_a<ASR::Integer_t>(*v->m_type) || ASR::is_a<ASR::Real_t>(*v->m_type) || ASR::is_a<ASR::Logical_t>(*v->m_type)
                        || ASR::is_a<ASR::Complex_t>(*v->m_type)) {
                        if (ASRUtils::is_array(v->m_type)) {
                            return false;
                        } else {
                            bool no_array_sections = true;
                            bool constant_args = false;
                            for (size_t i = 0; i < func_call_or_array->n_args; i++) {
                                if (func_call_or_array->m_args[i].m_step != nullptr) {
                                    no_array_sections = false;
                                    break;
                                }
                                if ( func_call_or_array->m_args[i].m_end != nullptr &&
                                     AST::is_a<AST::Num_t>(*func_call_or_array->m_args[i].m_end) ) {
                                    constant_args = true;
                                    break;
                                }
                            }
                            if (constant_args) {
                                return false;
                            }
                            if (no_array_sections) {
                                return true;
                            } else {
                                return false;
                            }
                        }
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }
            }
        } else {
            return false;
        }
    }

    void create_statement_function(const AST::Assignment_t &x) {
        current_function_dependencies.clear(al);
        SymbolTable *parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);

        if (compiler_options.implicit_typing && implicit_mapping.size() != 0) {
            implicit_dictionary = implicit_mapping[get_hash(parent_scope->asr_owner)];
        }

        //create a new function, and add it to the symbol table
        std::string var_name = to_lower(AST::down_cast<AST::FuncCallOrArray_t>(x.m_target)->m_func);
        auto v = AST::down_cast<AST::FuncCallOrArray_t>(x.m_target);

        Vec<ASR::expr_t*> args;
        args.reserve(al, v->n_args);

        for (size_t i=0; i<v->n_args; i++) {
            visit_expr(*(v->m_args[i]).m_end);
            ASR::expr_t *end = ASRUtils::EXPR(tmp);
            ASR::Var_t* tmp_var;
            if (ASR::is_a<ASR::Var_t>(*end)) {
                tmp_var = ASR::down_cast<ASR::Var_t>(end);
            } else {
                diag.add(Diagnostic(
                    "Statement function can only contain variables as arguments.",
                    Level::Error, Stage::Semantic, {
                        Label("",{x.base.base.loc})
                    }));
                throw SemanticAbort();
            }

            ASR::Variable_t* variable = ASR::down_cast<ASR::Variable_t>(tmp_var->m_v);
            std::string arg_name = variable->m_name;
            arg_name = to_lower(arg_name);
            SetChar variable_dependencies_vec;
            variable_dependencies_vec.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, ASRUtils::expr_type(end), nullptr, nullptr, arg_name);
            ASR::asr_t *arg_var = ASRUtils::make_Variable_t_util(al, x.base.base.loc,
                current_scope, s2c(al, arg_name),
                variable_dependencies_vec.p, variable_dependencies_vec.size(),
                ASRUtils::intent_in, nullptr, nullptr,
                ASR::storage_typeType::Default, ASRUtils::expr_type(end), ASRUtils::get_struct_sym_from_struct_expr(end),
                ASR::abiType::Source, ASR::Public, ASR::presenceType::Required,
                false);
            if (compiler_options.implicit_typing) {
                current_scope->add_or_overwrite_symbol(arg_name, ASR::down_cast<ASR::symbol_t>(arg_var));
            } else {
                current_scope->add_symbol(arg_name, ASR::down_cast<ASR::symbol_t>(arg_var));
            }
            args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc,
                current_scope->get_symbol(arg_name))));
        }

        // extract the type of var_name from symbol table
        ASR::symbol_t *sym = current_scope->resolve_symbol(var_name);
        ASR::ttype_t *type = nullptr;

        if (sym==nullptr) {
            if (compiler_options.implicit_typing) {
                implicit_dictionary = implicit_mapping[get_hash(parent_scope->asr_owner)];
                type = implicit_dictionary[std::string(1, to_lower(var_name)[0])];
            } else {
                diag.add(Diagnostic(
                    "Statement function needs to be declared.",
                    Level::Error, Stage::Semantic, {
                        Label("",{x.base.base.loc})
                    }));
                throw SemanticAbort();
            }
        } else {
            type = ASRUtils::symbol_type(sym);
        }

        // Assign where to_return
        std::string return_var_name = var_name + "_return_var_name";
        SetChar variable_dependencies_vec;
        variable_dependencies_vec.reserve(al, 1);
        ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, type, nullptr, nullptr, return_var_name);
        ASR::asr_t *return_var = ASRUtils::make_Variable_t_util(al, x.base.base.loc,
            current_scope, s2c(al, return_var_name),
            variable_dependencies_vec.p, variable_dependencies_vec.size(),
            ASRUtils::intent_return_var, nullptr, nullptr,
            ASR::storage_typeType::Default, type, sym,
            ASR::abiType::Source, ASR::Public, ASR::presenceType::Required,
            false);
        current_scope->add_symbol(return_var_name, ASR::down_cast<ASR::symbol_t>(return_var));
        ASR::expr_t* to_return = ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc,
            ASR::down_cast<ASR::symbol_t>(return_var)));

        tmp = ASRUtils::make_Function_t_util(
            al, x.base.base.loc,
            /* a_symtab */ current_scope,
            /* a_name */ s2c(al, var_name),
            /* m_dependency */ nullptr,
            /* n_dependency */ 0,
            /* a_args */ args.p,
            /* n_args */ args.size(),
            /* a_body */ nullptr,
            /* n_body */ 0,
            /* a_return_var */ to_return,
            ASR::abiType::Source, ASR::accessType::Public, ASR::deftypeType::Implementation,
            nullptr, false, false, false, false, false, nullptr, 0,
            false, false, false);
        ASR::symbol_t* fn_sym = ASR::down_cast<ASR::symbol_t>(tmp);
        parent_scope->add_or_overwrite_symbol(var_name, fn_sym);

        Vec<ASR::stmt_t*> body;
        body.reserve(al, 1);
        this->visit_expr(*x.m_value);
        ASR::expr_t *value = ASRUtils::EXPR(tmp);
        ImplicitCastRules::set_converted_value(al, x.base.base.loc, &value,
                                        ASRUtils::expr_type(value),ASRUtils::expr_type(to_return), diag);
        if (!ASRUtils::check_equal_type(ASRUtils::expr_type(to_return),
                                    ASRUtils::expr_type(value), to_return, value)) {
            std::string ltype = ASRUtils::type_to_str_fortran_expr(ASRUtils::expr_type(to_return), to_return);
            std::string rtype = ASRUtils::type_to_str_fortran_expr(ASRUtils::expr_type(value), value);
            diag.semantic_error_label(
                "Type mismatch in statement function, the types must be compatible",
                {to_return->base.loc, value->base.loc},
                "type mismatch (" + ltype + " and " + rtype + ")"
            );
            throw SemanticAbort();
        }
        ASRUtils::make_ArrayBroadcast_t_util(al, x.base.base.loc, to_return, value);
        body.push_back(al, ASR::down_cast<ASR::stmt_t>(ASRUtils::make_Assignment_t_util(al, x.base.base.loc, to_return, value, nullptr, compiler_options.po.realloc_lhs_arrays, false)));

        ASR::Function_t* fn = ASR::down_cast<ASR::Function_t>(fn_sym);
        fn->m_body = body.p;
        fn->n_body = body.n;
        fn->m_dependencies = current_function_dependencies.p;
        fn->n_dependencies = current_function_dependencies.n;
        current_function_dependencies.clear(al);
        current_scope = parent_scope;
    }

    /**
     * Checks compatibility between target and value arrays for assignment operations.
     *
     * This function verifies that the target and value arrays are compatible for assignment by ensuring
     * - they have the same rank and
     * - (if applicable) dimensions.
     * and throws a semantic error if otherwise
     */
    void check_ArrayAssignmentCompatibility(ASR::expr_t* target, ASR::expr_t* value, const AST::Assignment_t &x) {
        ASR::ttype_t *target_type = ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(target));
        ASR::ttype_t *value_type = ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(value));
        // we don't want to check by "check_equal_type" because we want to allow
        // real :: x(4); x = [1, 2, 3, 4] to be a valid assignment (as RHS is "integer array")
        // TODO: the only reason to not do this check for "reshape" is because
        // incorrect 'n_dims' and 'shape' returned for "reshape" currently
        if ( target_type != nullptr && value_type != nullptr && ASRUtils::is_array(target_type) && ASRUtils::is_array(value_type) ) {
            ASR::dimension_t* target_dims = nullptr;
            ASR::dimension_t* value_dims = nullptr;
            size_t target_rank = ASRUtils::extract_dimensions_from_ttype(target_type, target_dims);
            size_t value_rank = ASRUtils::extract_dimensions_from_ttype(value_type, value_dims);
            // ranks of LHS and RHS for an array assignment should
            // be same (including allocatable arrays)
            if (target_rank != value_rank) {
                diag.add(Diagnostic(
                    "Incompatible ranks " + std::to_string(target_rank) +
                   " and " + std::to_string(value_rank) + " in assignment",
                    Level::Error, Stage::Semantic, {
                        Label("",{x.base.base.loc})
                    }));
                throw SemanticAbort();
            } else if (ASRUtils::expr_type(target)->type == ASR::ttypeType::Array &&
                       target_dims->m_length != nullptr &&
                       !ASR::is_a<ASR::ArraySize_t>(*target_dims->m_length))
            {
                // if in any of the dimension, arrays have different size
                // raise an error
                bool is_array_concat = false;
                int flat_size = 0;
                if( AST::is_a<AST::ArrayInitializer_t>(*x.m_value)){
                     AST::ArrayInitializer_t *temp_array =
                            AST::down_cast<AST::ArrayInitializer_t>(x.m_value);
                    for(size_t i=0; i < temp_array->n_args; i++){
                        this->visit_expr(*temp_array->m_args[i]);
                        ASR::expr_t *temp = ASRUtils::EXPR(tmp);
                        if( ASRUtils::expr_type(temp)->type == ASR::ttypeType::Array ) {
                            flat_size += ASRUtils::get_fixed_size_of_array(ASRUtils::expr_type(temp));
                            is_array_concat = true;
                        } else {
                            flat_size += 1;
                        }
                    }
                }
                if(!is_array_concat){
                    for (size_t i = 0; i < target_rank; i++) {
                        ASR::dimension_t dim_a = target_dims[i];
                        ASR::dimension_t dim_b = value_dims[i];
                        int dim_a_int {-1};
                        int dim_b_int {-1};
                        if (!dim_a.m_length) {
                            continue;
                        }
                        ASRUtils::extract_value(ASRUtils::expr_value(dim_a.m_length), dim_a_int);
                        if (dim_b.m_length) {
                            ASRUtils::extract_value(ASRUtils::expr_value(dim_b.m_length), dim_b_int);
                        } else if (ASR::is_a<ASR::ArraySection_t>(*value)) {
                            // Try to compute array section dimension size
                            ASR::ArraySection_t* arr_sec = ASR::down_cast<ASR::ArraySection_t>(value);
                            if (i < arr_sec->n_args) {
                                ASR::expr_t* start = arr_sec->m_args[i].m_left;
                                ASR::expr_t* end = arr_sec->m_args[i].m_right;
                                ASR::expr_t* step = arr_sec->m_args[i].m_step;
                                if (start && end && step) {
                                    int64_t step_val = 0;
                                    if (ASRUtils::extract_value(ASRUtils::expr_value(step), step_val) && step_val != 0) {
                                        int64_t start_val = 0, end_val = 0;
                                        bool start_const = ASRUtils::extract_value(ASRUtils::expr_value(start), start_val);
                                        bool end_const = ASRUtils::extract_value(ASRUtils::expr_value(end), end_val);
                                        if (start_const && end_const) {
                                            // Both are constants: size = (end - start) / step + 1
                                            dim_b_int = (end_val - start_val) / step_val + 1;
                                        } else if ((ASR::is_a<ASR::Var_t>(*start) ||
                                                    ASR::is_a<ASR::IntegerConstant_t>(*start) ||
                                                    ASR::is_a<ASR::IntegerBinOp_t>(*start) ||
                                                    ASR::is_a<ASR::Cast_t>(*start)) &&
                                                   (ASR::is_a<ASR::Var_t>(*end) ||
                                                    ASR::is_a<ASR::IntegerConstant_t>(*end) ||
                                                    ASR::is_a<ASR::IntegerBinOp_t>(*end) ||
                                                    ASR::is_a<ASR::Cast_t>(*end)) &&
                                                   ASRUtils::expr_equal(start, end)) {
                                            // Same scalar index expression (e.g., x(i:i), x(i+1:i+1)):
                                            // the resulting section has extent 1.
                                            dim_b_int = 1;
                                        }
                                    }
                                }
                            }
                        }
                        if (dim_a_int > 0 && dim_b_int > 0 && dim_a_int != dim_b_int) {
                            diag.add(diag::Diagnostic(
                                "Different shape for array assignment on dimension "
                                + std::to_string(i + 1) + "(" + std::to_string(dim_a_int)
                                + " and " + std::to_string(dim_b_int) + ")",
                                diag::Level::Error, diag::Stage::Semantic, {
                                    diag::Label("", {x.base.base.loc})}));
                            throw SemanticAbort();
                        }
                    }
                } else {
                    //Case : C = [B, A] where B or A is Array
                    ASR::dimension_t dim_a = target_dims[0];
                    int dim_a_int {-1};
                    ASRUtils::extract_value(ASRUtils::expr_value(dim_a.m_length), dim_a_int);
                    if (dim_a_int > 0 && flat_size > 0 && dim_a_int != flat_size) {
                        diag.add(Diagnostic(
                            "Different shape for array assignment on "
                            "dimension 1 (" + std::to_string(dim_a_int) + " and " +
                            std::to_string(flat_size) + ")",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.base.base.loc})
                            }));
                        throw SemanticAbort();
                    }
                }
            }
        }
    }

    void visit_Assignment(const AST::Assignment_t &x) {
        if (is_statement_function(x)) {
            create_statement_function(x);
            tmp = nullptr;
            return;
        }
        this->visit_expr(*x.m_target);
        ASR::expr_t *target = ASRUtils::EXPR(tmp);
        if (ASRUtils::is_assumed_rank_array(ASRUtils::expr_type(target))) {
            std::string array_name = ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(target)->m_v);
            if (assumed_rank_arrays.find(array_name) == assumed_rank_arrays.end()) {
                diag.add(Diagnostic(
                    "Assumed-rank array '" + array_name + "' must be a dummy argument",
                    Level::Error, Stage::Semantic, {
                        Label("", {target->base.loc})
                    }
                ));
                throw SemanticAbort();
            } else {
                size_t rank = assumed_rank_arrays[array_name];
                ASR::ttype_t* target_type = ASRUtils::create_array_type_with_empty_dims(
                    al, rank, ASRUtils::extract_type(ASRUtils::expr_type(target)));
                ASR::expr_t* cast_expr = ASRUtils::EXPR(ASRUtils::make_ArrayPhysicalCast_t_util(
                    al, target->base.loc, target, ASR::array_physical_typeType::AssumedRankArray, 
                    ASR::array_physical_typeType::DescriptorArray, target_type, nullptr));
                target = cast_expr;
            }
        }
        if (auto* v = ASRUtils::extract_ExternalSymbol_Variable(target)) {
            if (v->m_is_protected) {
                diag.add(Diagnostic(
                    "Variable " + std::string(v->m_name) + " is PROTECTED and cannot appear in LHS of assignment",
                    Level::Error, Stage::Semantic, {
                        Label("", {target->base.loc})
                    }
                ));
                throw SemanticAbort();
            }
        }
        // Handle Conversion from BOZ to Real Type for assign statements
        ASR::ttype_t* temp_current_variable_type_ = current_variable_type_;
        if (AST::is_a<AST::BOZ_t>(*x.m_value)){
            //For assigning Scalar Variable
            if (ASR::is_a<ASR::Var_t>(*target)){
                ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(target);
                ASR::symbol_t* sym = var->m_v;
                if (ASR::is_a<ASR::Variable_t>(*sym)){
                    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
                    if (ASR::is_a<ASR::Real_t>(*v->m_type)){
                        current_variable_type_ = v->m_type;
                    }
                }
            }
            // For assigning Array Variable    
            else if (ASR::is_a<ASR::ArrayItem_t>(*target)) {
                ASR::ArrayItem_t *array_item = ASR::down_cast<ASR::ArrayItem_t>(target);
                if (ASR::is_a<ASR::Real_t>(*array_item->m_type)){
                    current_variable_type_ = array_item->m_type;
                }
            }
        }
        try {
            this->visit_expr(*x.m_value);
        } catch (const SemanticAbort &e) {
            if (!compiler_options.continue_compilation) throw e;
        }
        current_variable_type_ = temp_current_variable_type_;
        ASR::expr_t *value = ASRUtils::EXPR(tmp);
        ASR::stmt_t *overloaded_stmt = nullptr;
        if (ASR::is_a<ASR::Var_t>(*target)) {
            ASR::Var_t *var = ASR::down_cast<ASR::Var_t>(target);
            ASR::symbol_t *sym = var->m_v;
            if (do_loop_variables.size() > 0 && std::find(do_loop_variables.begin(), do_loop_variables.end(), sym) != do_loop_variables.end()) {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
                std::string var_name = std::string(v->m_name);
                diag.add(Diagnostic(
                    "Assignment to loop variable `" + std::string(to_lower(var_name)) +"` is not allowed",
                    Level::Error, Stage::Semantic, {
                        Label("",{target->base.loc})
                    }));
                throw SemanticAbort();
            }
            if (ASR::is_a<ASR::Variable_t>(*sym)){
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
                ASR::intentType intent = v->m_intent;
                if (intent == ASR::intentType::In) {
                    diag.add(Diagnostic(
                        "Cannot assign to an intent(in) variable `" + std::string(v->m_name) + "`",
                        Level::Error, Stage::Semantic, {
                            Label("",{target->base.loc})
                        }));
                    throw SemanticAbort();
                }
                if (v->m_storage == ASR::storage_typeType::Parameter) {
                    diag.add(diag::Diagnostic(
                        "Cannot assign to a constant variable",
                        diag::Level::Error, diag::Stage::Semantic, {
                            diag::Label("assignment here", {x.base.base.loc}),
                            diag::Label("declared as constant", {v->base.base.loc}, false),
                        }));
                    if (!compiler_options.continue_compilation) throw SemanticAbort();
                }
            }
            if (ASR::is_a<ASR::Function_t>(*sym)){
                diag.add(Diagnostic(
                    "Assignment to subroutine is not allowed",
                    Level::Error, Stage::Semantic, {
                        Label("",{target->base.loc})
                    }));
                throw SemanticAbort();
            }
        }
        if( ASRUtils::use_overloaded_assignment(target, value,
            current_scope, asr, al, x.base.base.loc, current_function_dependencies,
            current_module_dependencies,
            [&](const std::string &msg, const Location &loc) {
                diag.add(Diagnostic(
                    msg,
                    Level::Error, Stage::Semantic, {
                        Label("",{loc})
                    }));
                throw SemanticAbort();
                }) ) {
            overloaded_stmt = ASRUtils::STMT(asr);
        }
        if (ASR::is_a<ASR::Cast_t>(*target)) {
            ASR::Cast_t* cast = ASR::down_cast<ASR::Cast_t>(target);
            if (cast->m_kind == ASR::cast_kindType::ComplexToReal) {
                /*
                    Case: x%re = y
                    we do: x = cmplx(y, x%im)
                    i.e. target = x, value = cmplx(y, x%im)
                */
                target = cast->m_arg;
                ASR::expr_t* y = value;
                const Location& loc = x.base.base.loc;
                ASR::expr_t *val = target;

                ASR::ttype_t *real_type = ASRUtils::TYPE(ASR::make_Real_t(al, loc,
                    ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(val))));
                ASR::expr_t *im = ASRUtils::EXPR(ASR::make_ComplexIm_t(al, loc,
                    val, real_type, nullptr));
                ASR::expr_t* cmplx = ASRUtils::EXPR(ASR::make_ComplexConstructor_t(
                    al, loc, y, im, ASRUtils::expr_type(target), nullptr));
                value = cmplx;
            }
        } else if ( ASR::is_a<ASR::ComplexRe_t>(*target) ) {
            ASR::ComplexRe_t* re = ASR::down_cast<ASR::ComplexRe_t>(target);
            /*
                Case: x % re = y
                we do: x = cmplx(y, x%im)
                i.e. target = x, value = cmplx(y, x%im)
            */
            target = re->m_arg;
            ASR::expr_t* y = value;
            const Location& loc = x.base.base.loc;
            ASR::expr_t *val = target;

            ASR::ttype_t *real_type = ASRUtils::TYPE(ASR::make_Real_t(al, loc,
                ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(val))));
            ASR::expr_t *im = ASRUtils::EXPR(ASR::make_ComplexIm_t(al, loc,
                val, real_type, nullptr));
            ASR::expr_t* cmplx = ASRUtils::EXPR(ASR::make_ComplexConstructor_t(
                al, loc, y, im, ASRUtils::expr_type(target), nullptr));
            value = cmplx;
        } else if ( ASR::is_a<ASR::ComplexIm_t>(*target) ) {
            ASR::ComplexIm_t* im = ASR::down_cast<ASR::ComplexIm_t>(target);
            /*
                Case: x % im = y
                we do: x = cmplx(x%re, y)
                i.e. target = x, value = cmplx(x%re, y)
            */
            target = im->m_arg;
            ASR::expr_t* y = value;
            const Location& loc = x.base.base.loc;
            ASR::expr_t* re = ASRUtils::EXPR(ASR::make_Cast_t(al, loc, target,
                ASR::cast_kindType::ComplexToReal, ASRUtils::expr_type(y), nullptr));
            ASR::expr_t* cmplx = ASRUtils::EXPR(ASR::make_ComplexConstructor_t(al,
                loc, re, y, ASRUtils::expr_type(target), nullptr));
            value = cmplx;
        }
        bool is_valid_lhs = (
            target->type == ASR::exprType::Var ||
            target->type == ASR::exprType::ArrayItem ||
            target->type == ASR::exprType::ArraySection ||
            target->type == ASR::exprType::ArrayPhysicalCast ||
            target->type == ASR::exprType::StringSection ||
            target->type == ASR::exprType::StringItem ||
            target->type == ASR::exprType::StructInstanceMember ||
            target->type == ASR::exprType::UnionInstanceMember ||
            target->type == ASR::exprType::ComplexRe ||
            target->type == ASR::exprType::ComplexIm
        );

        // Allow function calls on LHS if they return a POINTER
        if (!is_valid_lhs && target->type == ASR::exprType::FunctionCall) {
            ASR::ttype_t* target_type = ASRUtils::expr_type(target);
            if (ASRUtils::is_pointer(target_type)) {
                is_valid_lhs = true;
            }
        }

        if (!is_valid_lhs) {
            diag.add(Diagnostic(
                "The LHS of assignment can only be a variable or an array reference",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }

        ASR::ttype_t *target_type = ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(target));
        ASR::ttype_t *value_type = ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(value));
        if (target->type == ASR::exprType::Var && !ASRUtils::is_array(target_type) &&
            value->type == ASR::exprType::ArrayConstant ) {
            diag.add(Diagnostic(
                "ArrayInitalizer expressions can only be assigned array references",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }
        check_ArrayAssignmentCompatibility(target, value, x);

        if( overloaded_stmt == nullptr ) {
            if ((target->type == ASR::exprType::Var ||
                target->type == ASR::exprType::ArrayItem ||
                target->type == ASR::exprType::ArraySection ||
                target->type == ASR::exprType::StructInstanceMember ||
                target->type == ASR::exprType::UnionInstanceMember) &&
                !ASRUtils::check_equal_type(target_type, value_type, target, value)) {
                if (value->type == ASR::exprType::ArrayConstant) {
                    ASR::ArrayConstant_t *ac = ASR::down_cast<ASR::ArrayConstant_t>(value);
                    int size = ASRUtils::get_fixed_size_of_array(ac->m_type);
                    Vec<ASR::expr_t*> args; args.reserve(al, size);
                    for (size_t i = 0; i < (size_t) size; i++) {
                        ASR::expr_t* arg = ASRUtils::fetch_ArrayConstant_value(al, ac, i);
                        ImplicitCastRules::set_converted_value(al, x.base.base.loc, &arg,
                                                ASRUtils::expr_type(arg),
                                                ASRUtils::type_get_past_allocatable(target_type), diag);
                        // ASRUtils::set_ArrayConstant_value(ac, arg, i);
                        args.push_back(al, arg);
                    }
                    ac = ASR::down_cast<ASR::ArrayConstant_t>(ASRUtils::expr_value(ASRUtils::EXPR(ASRUtils::make_ArrayConstructor_t_util(al, ac->base.base.loc,
                            args.p, args.n, ASRUtils::type_get_past_allocatable(ASRUtils::type_get_past_pointer(target_type)), ac->m_storage_format))));
                    value = ASRUtils::EXPR((ASR::asr_t*) ac);
                    LCOMPILERS_ASSERT(ASRUtils::is_array(ac->m_type));
                    if( ASR::is_a<ASR::Array_t>(*ASRUtils::type_get_past_pointer(
                            ASRUtils::type_get_past_allocatable(ac->m_type))) ) {
                        ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(
                            ASRUtils::type_get_past_pointer(
                                ASRUtils::type_get_past_allocatable(ac->m_type)));
                        array_t->m_type = ASRUtils::expr_type(ASRUtils::fetch_ArrayConstant_value(al, ac, 0));
                    }
                } else {
                    if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*value) &&
                          ASR::down_cast<ASR::IntrinsicElementalFunction_t>(value)->m_intrinsic_id == static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::Maskl) &&
                            ASRUtils::extract_kind_from_ttype_t(target_type) == 8) {
                        // Do return_type = kind(8)
                        ASR::ttype_t* int_64 = ASRUtils::TYPE(ASR::make_Integer_t(al, value->base.loc, 8));
                        ASR::IntrinsicElementalFunction_t* int_func = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(value);
                        value = ASRUtils::EXPR(ASR::make_IntrinsicElementalFunction_t(al, value->base.loc, int_func->m_intrinsic_id,
                            int_func->m_args, int_func->n_args, int_func->m_overload_id, int_64, int_func->m_value));
                    } else {
                    if (ASR::is_a<ASR::ArrayReshape_t>(*value)) {
                        ASR::ArrayReshape_t* array_reshape = ASR::down_cast<ASR::ArrayReshape_t>(value);
                        if (ASR::is_a<ASR::ArrayConstructor_t>(*array_reshape->m_array) && ASR::is_a<ASR::ImpliedDoLoop_t>(**ASR::down_cast<ASR::ArrayConstructor_t>(array_reshape->m_array)->m_args)) {
                            ASR::Array_t* array_reshape_array_type = ASR::down_cast<ASR::Array_t>(array_reshape->m_type);
                            Vec<ASR::dimension_t> array_reshape_dims;
                            array_reshape_dims.reserve(al, array_reshape_array_type->n_dims);
                            for (size_t i=0;i<array_reshape_array_type->n_dims;i++) {
                                array_reshape_dims.push_back(al, array_reshape_array_type->m_dims[i]);
                            }
                            array_reshape->m_type = ASRUtils::duplicate_type(al, array_reshape->m_type, &array_reshape_dims, ASR::array_physical_typeType::DescriptorArray,true);
                        }
                    }
                    ImplicitCastRules::set_converted_value(al, x.base.base.loc, &value,
                                        value_type, target_type, diag);
                    }
                }
            }
            std::string ltype = ASRUtils::type_to_str_fortran_expr(ASRUtils::expr_type(target), target);
            std::string rtype = ASRUtils::type_to_str_fortran_expr(ASRUtils::expr_type(value), value);
            if (!ASRUtils::check_equal_type(ASRUtils::expr_type(target),
                                        ASRUtils::expr_type(value), target, value)) {
                if(value->type == ASR::exprType::ArrayConstant) {
                    ASR::ArrayConstant_t *ac = ASR::down_cast<ASR::ArrayConstant_t>(value);
                    for (size_t i = 0; i < (size_t) ASRUtils::get_fixed_size_of_array(ac->m_type); i++) {
                        ASR::expr_t* arr_const_val = ASRUtils::fetch_ArrayConstant_value(al, ac, i);
                        if(!ASRUtils::check_equal_type(ASRUtils::expr_type(arr_const_val), ASRUtils::expr_type(target), arr_const_val, target)) {
                            diag.semantic_error_label(
                                "Type mismatch in assignment, the types must be compatible",
                                {target->base.loc, value->base.loc},
                                "type mismatch (" + ltype + " and " + rtype + ")"
                            );
                            throw SemanticAbort();
                        }
                    }
                    LCOMPILERS_ASSERT(ASRUtils::is_array(ac->m_type));
                    if(!ASRUtils::check_equal_type(ac->m_type, ASRUtils::expr_type(target), value, target)) {
                        diag.semantic_error_label(
                            "Type mismatch in assignment, the types must be compatible",
                            {target->base.loc, value->base.loc},
                            "type mismatch (" + ltype + " and " + rtype + ")"
                        );
                        throw SemanticAbort();
                    }
                } else if(value->type == ASR::exprType::ArrayConstructor) {
                    ASR::ArrayConstructor_t *ac = ASR::down_cast<ASR::ArrayConstructor_t>(value);
                    for (size_t i = 0; i < ac->n_args; i++) {
                        if(!ASRUtils::check_equal_type(ASRUtils::expr_type(ac->m_args[i]), ASRUtils::expr_type(target), ac->m_args[i], target)) {
                            diag.semantic_error_label(
                                "Type mismatch in assignment, the types must be compatible",
                                {target->base.loc, value->base.loc},
                                "type mismatch (" + ltype + " and " + rtype + ")"
                            );
                            throw SemanticAbort();
                        }
                    }
                    LCOMPILERS_ASSERT(ASRUtils::is_array(ac->m_type));
                    if(!ASRUtils::check_equal_type(ac->m_type, ASRUtils::expr_type(target), value, target)) {
                        diag.semantic_error_label(
                            "Type mismatch in assignment, the types must be compatible",
                            {target->base.loc, value->base.loc},
                            "type mismatch (" + ltype + " and " + rtype + ")"
                        );
                        throw SemanticAbort();
                    }
                } else {
                        diag.semantic_error_label(
                            "Type mismatch in assignment, the types must be compatible",
                            {target->base.loc, value->base.loc},
                            "type mismatch (" + ltype + " and " + rtype + ")"
                        );
                        throw SemanticAbort();
                }
            }
            if (ASRUtils::is_array(ASRUtils::expr_type(value)) &&
                !ASRUtils::is_array(ASRUtils::expr_type(target))) {
                    diag.semantic_error_label(
                        "Type mismatch in assignment, the types must be compatible",
                        {target->base.loc, value->base.loc},
                        "type mismatch (" + ltype + " and " + rtype + ")"
                    );
                    throw SemanticAbort();
            }
            if (!ASRUtils::is_array(ASRUtils::expr_type(target)) && ASRUtils::is_struct(*ASRUtils::expr_type(target)) && ASRUtils::is_allocatable(ASRUtils::expr_type(target)) && ASR::is_a<ASR::FunctionCall_t>(*value)) {
                // Allocate the target if the value is a function call returning an allocatable
                // array and the target is allocatable
                ASR::alloc_arg_t alloc_arg;
                alloc_arg.loc = x.base.base.loc;
                alloc_arg.m_a = target;
                alloc_arg.m_dims = nullptr;
                alloc_arg.n_dims = 0;
                alloc_arg.m_len_expr = nullptr;
                alloc_arg.m_sym_subclass = nullptr;
                alloc_arg.m_type = nullptr;
                Vec<ASR::alloc_arg_t> alloc_args;
                alloc_args.reserve(al, 1);
                alloc_args.push_back(al, alloc_arg);
                ASR::stmt_t* alloc_stmt = ASRUtils::STMT(ASR::make_Allocate_t(al, target->base.loc,
                                            alloc_args.p, alloc_args.n,
                                            nullptr, nullptr, nullptr));
                Vec<ASR::stmt_t*> if_body;
                if_body.reserve(al, 1);
                if_body.push_back(al, alloc_stmt);
                Vec<ASR::expr_t*> allocated_args;
                allocated_args.reserve(al, 1);
                allocated_args.push_back(al, target);
                ASR::expr_t* allocated_expr = ASRUtils::EXPR(ASR::make_IntrinsicImpureFunction_t(al, target->base.loc, static_cast<int64_t>(ASRUtils::IntrinsicImpureFunctions::Allocated), allocated_args.p, allocated_args.n, 0, ASRUtils::TYPE(ASR::make_Logical_t(al, target->base.loc, 4)), nullptr));
                ASR::expr_t* if_test_expr = ASRUtils::EXPR(ASR::make_LogicalNot_t(al, target->base.loc, allocated_expr, ASRUtils::TYPE(ASR::make_Logical_t(al, target->base.loc, 4)), nullptr));
                ASR::stmt_t* if_stmt = ASRUtils::STMT(ASR::make_If_t(al, target->base.loc, nullptr, if_test_expr, if_body.p, if_body.n, nullptr, 0));
                current_body->push_back(al, if_stmt);
            }
        }

        ASRUtils::make_ArrayBroadcast_t_util(al, x.base.base.loc, target, value);

        tmp = ASRUtils::make_Assignment_t_util(al, x.base.base.loc, target, value,
                            overloaded_stmt, compiler_options.po.realloc_lhs_arrays, false);
    }

    ASR::asr_t* create_CFPointer(const AST::SubroutineCall_t& x) {
        Vec<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"cptr", "fptr", "shape"};
        handle_intrinsic_node_args<AST::SubroutineCall_t>(
            x, args, kwarg_names, 2, 3, std::string("c_f_ptr"));
        ASR::expr_t *cptr = args[0], *fptr = args[1], *shape = args[2];
        if( shape && ASR::is_a<ASR::ArrayConstant_t>(*shape) ) {
            ASR::ttype_t* array_constant_type = ASRUtils::expr_type(shape);
            ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(array_constant_type);
            array_t->m_physical_type = ASR::array_physical_typeType::PointerArray;
        }
        ASR::ttype_t* fptr_type = ASRUtils::expr_type(fptr);
        bool is_fptr_array = ASRUtils::is_array(fptr_type);
        bool is_ptr = ASR::is_a<ASR::Pointer_t>(*fptr_type);
        if( !is_ptr ) {
            diag.add(Diagnostic(
                "fptr is not a pointer.",
                Level::Error, Stage::Semantic, {
                    Label("",{fptr->base.loc})
                }));
            throw SemanticAbort();
        }
        if(!is_fptr_array && shape) {
            diag.add(diag::Diagnostic(
                                "shape argument specified in c_f_pointer "
                                "even though fptr is not an array.",
                                diag::Level::Error, diag::Stage::Semantic, {
                                    diag::Label("", {shape->base.loc})}));
            throw SemanticAbort();
        }
        if(is_fptr_array && !shape) {
            diag.add(Diagnostic(
                "shape argument not specified in c_f_pointer "
                "even though fptr is an array.",
                Level::Error, Stage::Semantic, {
                    Label("",{shape->base.loc})
                }));
            throw SemanticAbort();
        }
        ASR::dimension_t* shape_dims;
        ASR::expr_t* lower_bounds = nullptr;
        if( shape ) {
            int shape_rank = ASRUtils::extract_dimensions_from_ttype(
                                ASRUtils::expr_type(shape),
                                shape_dims);
            if( shape_rank != 1 ) {
                diag.add(diag::Diagnostic(
                                "shape array passed to c_f_pointer "
                                "must be of rank 1 but given rank is " +
                                std::to_string(shape_rank),
                                diag::Level::Error, diag::Stage::Semantic, {
                                    diag::Label("", {shape->base.loc})}));
            throw SemanticAbort();
            }

            ASR::dimension_t* target_dims;
            int target_n_dims = ASRUtils::extract_dimensions_from_ttype(fptr_type, target_dims);
            if( target_n_dims > 0 ) {
                Vec<ASR::expr_t*> lbs;
                lbs.reserve(al, target_n_dims);
                bool success = true;
                for( int i = 0; i < target_n_dims; i++ ) {
                    if( target_dims->m_length == nullptr ) {
                        success = false;
                        break;
                    }
                    lbs.push_back(al, ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                        al, x.base.base.loc, 1, ASRUtils::TYPE(
                            ASR::make_Integer_t(al, x.base.base.loc, compiler_options.po.default_integer_kind)))));
                }
                if( success ) {
                    Vec<ASR::dimension_t> dims;
                    dims.reserve(al, 1);
                    ASR::dimension_t dim;  dim.m_length = nullptr; dim.m_start = nullptr;
                    dim.loc = x.base.base.loc;
                    dim.m_length = make_ConstantWithKind(make_IntegerConstant_t,
                        make_Integer_t, target_n_dims, compiler_options.po.default_integer_kind, dim.loc);
                    dim.m_start = make_ConstantWithKind(make_IntegerConstant_t,
                        make_Integer_t, 0, compiler_options.po.default_integer_kind, dim.loc);
                    dims.push_back(al, dim);
                    ASR::ttype_t* type = ASRUtils::make_Array_t_util(al, dim.loc,
                        ASRUtils::expr_type(lbs[0]), dims.p, dims.size(), ASR::abiType::Source,
                        false, ASR::array_physical_typeType::PointerArray, true);
                    lower_bounds = ASRUtils::EXPR(ASRUtils::make_ArrayConstructor_t_util(al,
                        x.base.base.loc, lbs.p, lbs.size(), type,
                        ASR::arraystorageType::RowMajor));
                }
            }
        }
        return ASR::make_CPtrToPointer_t(al, x.base.base.loc, cptr, fptr, shape, lower_bounds);
    }

    ASR::asr_t* create_CFProcPointer(const AST::SubroutineCall_t& x) {
        Vec<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"cptr", "fptr"};
        handle_intrinsic_node_args<AST::SubroutineCall_t>(x, args, kwarg_names, 2, 2, std::string("c_f_procpointer"));
        ASR::expr_t *cptr = args[0], *fptr = args[1];
        // Reusing CPtrToPointer for procedure pointers
        return ASR::make_CPtrToPointer_t(al, x.base.base.loc, cptr, fptr, nullptr, nullptr);
    }

    ASR::asr_t* intrinsic_subroutine_as_node(const AST::SubroutineCall_t &x, std::string var_name) {
        if (is_intrinsic_registry_subroutine(var_name)) {
            if ( ASRUtils::IntrinsicImpureSubroutineRegistry::is_intrinsic_subroutine(var_name)) {
                IntrinsicSignature signature = get_intrinsic_signature(var_name);
                Vec<ASR::expr_t*> args;
                bool signature_matched = false;
                signature_matched = handle_intrinsic_node_args(
                    x, args, signature.kwarg_names,
                    signature.positional_args, signature.max_args,
                    var_name, true);

                if( !signature_matched ) {
                    diag.add(Diagnostic(
                        "No matching signature found for intrinsic " + var_name,
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                if( ASRUtils::IntrinsicImpureSubroutineRegistry::is_intrinsic_subroutine(var_name) ) {
                    fill_optional_kind_arg(var_name, args);

                    ASRUtils::create_intrinsic_subroutine create_func =
                        ASRUtils::IntrinsicImpureSubroutineRegistry::get_create_subroutine(var_name);
                    tmp = create_func(al, x.base.base.loc, args, diag);
                    return tmp;
                }
            }
        } else if (startswith(var_name, "_lfortran_")) {
            // LFortran specific intrinsics
            
            IntrinsicSignature signature = get_intrinsic_signature(var_name);
            Vec<ASR::expr_t*> args;
            bool signature_matched = false;
            signature_matched = handle_intrinsic_node_args(
                x, args, signature.kwarg_names,
                signature.positional_args, signature.max_args,
                var_name, true);

            if( !signature_matched ) {
                diag.add(Diagnostic(
                    "No matching signature found for intrinsic " + var_name,
                    Level::Error, Stage::Semantic, {
                        Label("",{x.base.base.loc})
                    }));
                throw SemanticAbort();
            }

            if (var_name == "_lfortran_list_append") {
                if (!ASR::is_a<ASR::List_t>(*ASRUtils::expr_type(args[0]))) {
                    diag.add(Diagnostic(
                        "First argument of " + var_name + " must be of list type",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }

                ASR::List_t *list_type = ASR::down_cast<ASR::List_t>(ASRUtils::expr_type(args[0]));
                ASR::ttype_t *arg_type = ASRUtils::expr_type(args[1]);
                ASR::ttype_t *contained_type = ASRUtils::get_contained_type((ASR::ttype_t *)list_type);
                if (!ASRUtils::check_equal_type(contained_type, arg_type, nullptr, nullptr)) {
                    std::string contained_type_str = ASRUtils::type_to_str_fortran_expr(contained_type, nullptr);
                    std::string arg_type_str = ASRUtils::type_to_str_fortran_expr(arg_type, nullptr);
                    diag.add(Diagnostic(
                        "Type mismatch in " + var_name + ", the types must be compatible",
                        Level::Error, Stage::Semantic, {
                            Label("Types mismatch (found '" + 
                        arg_type_str + "', expected '" + contained_type_str +  "')",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }

                return ASR::make_ListAppend_t(al, x.base.base.loc, args[0], args[1]);

            } else if (var_name == "_lfortran_list_insert") {
                if (!ASR::is_a<ASR::List_t>(*ASRUtils::expr_type(args[0]))) {
                    diag.add(Diagnostic(
                        "First argument of " + var_name + " must be of list type",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                
                ASR::ttype_t *contained_type = ASRUtils::get_contained_type(ASRUtils::expr_type(args[0]));

                if (!ASRUtils::check_equal_type(contained_type, ASRUtils::expr_type(args[2]), nullptr, nullptr)) {
                    std::string contained_type_str = ASRUtils::type_to_str_fortran_expr(contained_type, nullptr);
                    std::string arg_type_str = ASRUtils::type_to_str_fortran_expr(ASRUtils::expr_type(args[2]), nullptr);
                    diag.add(Diagnostic(
                        "Type mismatch in " + var_name + ", the types must be compatible",
                        Level::Error, Stage::Semantic, {
                            Label("Types mismatch (found '" + 
                        arg_type_str + "', expected '" + contained_type_str +  "')",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                
                if (!ASR::is_a<ASR::Integer_t>(*ASRUtils::expr_type(args[1]))) {
                    std::string arg_type_str = ASRUtils::type_to_str_fortran_expr(ASRUtils::expr_type(args[1]), nullptr);
                    diag.add(Diagnostic("Index of a list must be an integer not '" + arg_type_str + "'",
                                    Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                    throw SemanticAbort();
                } 

                return ASR::make_ListInsert_t(al, x.base.base.loc, args[0], args[1], args[2]);
            } else if (var_name == "_lfortran_list_remove") {
                if (!ASR::is_a<ASR::List_t>(*ASRUtils::expr_type(args[0]))) {
                    diag.add(Diagnostic(
                        "First argument of " + var_name + " must be of list type",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                
                ASR::ttype_t *contained_type = ASRUtils::get_contained_type(ASRUtils::expr_type(args[0]));

                if (!ASRUtils::check_equal_type(contained_type, ASRUtils::expr_type(args[1]), nullptr, nullptr)) {
                    std::string contained_type_str = ASRUtils::type_to_str_fortran_expr(contained_type, nullptr);
                    std::string arg_type_str = ASRUtils::type_to_str_fortran_expr(ASRUtils::expr_type(args[2]), nullptr);
                    diag.add(Diagnostic(
                        "Type mismatch in " + var_name + ", the types must be compatible",
                        Level::Error, Stage::Semantic, {
                            Label("Types mismatch (found '" + 
                        arg_type_str + "', expected '" + contained_type_str +  "')",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }

                return ASR::make_ListRemove_t(al, x.base.base.loc, args[0], args[1]);
            } else if (var_name == "_lfortran_list_reverse") {
                if (!ASR::is_a<ASR::List_t>(*ASRUtils::expr_type(args[0]))) {
                    diag.add(Diagnostic(
                        "First argument of " + var_name + " must be of list type",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }

                ASRUtils::create_intrinsic_function create_function =
                    ASRUtils::IntrinsicElementalFunctionRegistry::get_create_function("list.reverse");
                return create_function(al, x.base.base.loc, args, diag);
            } else if (var_name == "_lfortran_set_add") {
                if (!ASR::is_a<ASR::Set_t>(*ASRUtils::expr_type(args[0]))) {
                    diag.add(Diagnostic(
                        "First argument of " + var_name + " must be of set type",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }

                ASR::Set_t *set_type = ASR::down_cast<ASR::Set_t>(ASRUtils::expr_type(args[0]));
                ASR::ttype_t *arg_type = ASRUtils::expr_type(args[1]);
                ASR::ttype_t *contained_type = ASRUtils::get_contained_type((ASR::ttype_t *)set_type);
                if (!ASRUtils::check_equal_type(contained_type, arg_type, nullptr, nullptr)) {
                    std::string contained_type_str = ASRUtils::type_to_str_fortran_expr(contained_type, nullptr);
                    std::string arg_type_str = ASRUtils::type_to_str_fortran_expr(arg_type, nullptr);
                    diag.add(Diagnostic(
                        "Type mismatch in " + var_name + ", the types must be compatible",
                        Level::Error, Stage::Semantic, {
                            Label("Types mismatch (found '" + 
                        arg_type_str + "', expected '" + contained_type_str +  "')",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }

                ASRUtils::create_intrinsic_function create_function =
                    ASRUtils::IntrinsicElementalFunctionRegistry::get_create_function("set.add");
                return create_function(al, x.base.base.loc, args, diag);
            } else if (var_name == "_lfortran_set_item") {
                if (ASR::is_a<ASR::List_t>(*ASRUtils::expr_type(args[0]))) {
                    ASR::List_t *list_type = ASR::down_cast<ASR::List_t>(ASRUtils::expr_type(args[0]));
                    ASR::ttype_t *index_type = ASRUtils::expr_type(args[1]);
                    ASR::ttype_t *arg_type = ASRUtils::expr_type(args[2]);
                    ASR::ttype_t *contained_type = ASRUtils::get_contained_type((ASR::ttype_t *)list_type);

                    if (!ASRUtils::check_equal_type(contained_type, arg_type, nullptr, nullptr)) {
                        std::string contained_type_str = ASRUtils::type_to_str_fortran_expr(contained_type, nullptr);
                        std::string arg_type_str = ASRUtils::type_to_str_fortran_expr(arg_type, nullptr);
                        diag.add(Diagnostic(
                            "Type mismatch in " + var_name + ", the types must be compatible",
                            Level::Error, Stage::Semantic, {
                                Label("Types mismatch (found '" + 
                            arg_type_str + "', expected '" + contained_type_str +  "')",{x.base.base.loc})
                            }));
                        throw SemanticAbort();
                    }

                    if (!ASR::is_a<ASR::Integer_t>(*index_type)) {
                        std::string index_type_str = ASRUtils::type_to_str_fortran_expr(index_type, nullptr);
                        diag.add(Diagnostic("Index of a list must be an integer not '" + index_type_str + "'",
                                    Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                        throw SemanticAbort();
                    }

                    return ASRUtils::make_Assignment_t_util(al, x.base.base.loc, 
                                        ASRUtils::EXPR(ASR::make_ListItem_t(al, x.base.base.loc, args[0], args[1],
                                                             contained_type, nullptr)), 
                                                            args[2], nullptr, false, false);
                } else if (ASR::is_a<ASR::Dict_t>(*ASRUtils::expr_type(args[0]))) {
                    ASR::Dict_t *dict_type = ASR::down_cast<ASR::Dict_t>(ASRUtils::expr_type(args[0]));
                    ASR::ttype_t *key_type = ASRUtils::expr_type(args[1]);
                    ASR::ttype_t *value_type = ASRUtils::expr_type(args[2]);

                    ASR::ttype_t *dict_key_type = dict_type->m_key_type;
                    ASR::ttype_t *dict_value_type = dict_type->m_value_type;
                    if (!ASRUtils::check_equal_type(dict_key_type, key_type, nullptr, nullptr)) {
                        std::string contained_type_str = ASRUtils::type_to_str_fortran_expr(dict_key_type, nullptr);
                        std::string arg_type_str = ASRUtils::type_to_str_fortran_expr(key_type, nullptr);
                        diag.add(Diagnostic(
                            "Type mismatch in " + var_name + ", the key types must be compatible",
                            Level::Error, Stage::Semantic, {
                                Label("Types mismatch (found '" + 
                            arg_type_str + "', expected '" + contained_type_str +  "')",{x.base.base.loc})
                            }));
                        throw SemanticAbort();
                    }

                    if (!ASRUtils::check_equal_type(dict_value_type, value_type, nullptr, nullptr)) {
                        std::string contained_type_str = ASRUtils::type_to_str_fortran_expr(dict_value_type, nullptr);
                        std::string arg_type_str = ASRUtils::type_to_str_fortran_expr(value_type, nullptr);
                        diag.add(Diagnostic(
                            "Type mismatch in " + var_name + ", the value types must be compatible",
                            Level::Error, Stage::Semantic, {
                                Label("Types mismatch (found '" + 
                            arg_type_str + "', expected '" + contained_type_str +  "')",{x.base.base.loc})
                            }));
                        throw SemanticAbort();
                    }

                    return ASR::make_DictInsert_t(al, x.base.base.loc, args[0], args[1], args[2]);
                } else {
                    std::string type_string = ASRUtils::type_to_str_fortran_expr(ASRUtils::expr_type(args[0]), nullptr);
                    diag.add(Diagnostic(
                        "First argument of type '"  + type_string + "' has not been implemented for " + var_name + " yet",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                return nullptr;
            } else if (var_name == "_lfortran_clear") {
                if (ASR::is_a<ASR::List_t>(*ASRUtils::expr_type(args[0]))) {
                    return ASR::make_ListClear_t(al, x.base.base.loc, args[0]);
                } /*else if (ASR::is_a<ASR::Dict_t>(*ASRUtils::expr_type(args[0]))) {
                    return ASR::make_DictClear_t(al, x.base.base.loc, args[0]);
                } else if (ASR::is_a<ASR::Set_t>(*ASRUtils::expr_type(args[0]))) { TODO: Set, Dict Clear is not implemented
                    return ASR::make_SetClear_t(al, x.base.base.loc, args[0]);
                } */ 
                else {
                    std::string type_string = ASRUtils::type_to_str_fortran_expr(ASRUtils::expr_type(args[0]), nullptr);
                    diag.add(Diagnostic(
                        "First argument of type '"  + type_string + "' has not been implemented for " + var_name + " yet",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                return nullptr;
            }
        }
        return nullptr;
    }

    /*
        Function to convert 'FLUSH' subroutine call to 'FLUSH' ASR node
    */
    void flush_subroutine_to_flush_statement(const AST::SubroutineCall_t &x) {
        LCOMPILERS_ASSERT(to_lower(x.m_name) == "flush")

        // for FLUSH subroutine call, the only argument 'unit' is optional
        ASR::expr_t* unit_flush = nullptr;
        if (x.n_args == 1 && x.n_keywords == 0) {
            visit_expr(*x.m_args[0].m_end);
            unit_flush = ASRUtils::EXPR(tmp);
        } else if (x.n_args == 0 && x.n_keywords == 1) {
            visit_expr(*x.m_keywords[0].m_value);
            unit_flush = ASRUtils::EXPR(tmp);
        } else if (x.n_args == 0 && x.n_keywords == 0) {
            // when 'FLUSH' intrinsic is called with no argument,
            // then all open units need to be 'FLUSH'ed
            ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, 4));
            ASR::expr_t* one = ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(
                    al, x.base.base.loc, 1, int_type
                )
            );
            ASR::expr_t* minus_one = ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(
                    al, x.base.base.loc, -1, int_type
                )
            );
            // we assign an integer -1 unit to that
            unit_flush = ASRUtils::EXPR(
                ASR::make_IntegerUnaryMinus_t(
                    al, x.base.base.loc, one, int_type, minus_one
                )
            );
        } else {
            diag.add(Diagnostic(
                "Expected 0 or 1 arguments, got " + std::to_string(x.n_args + x.n_keywords) +
                " arguments instead.",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                })
            );
            throw SemanticAbort();
        }
        tmp = ASR::make_Flush_t(
            al, x.base.base.loc, 0, unit_flush, nullptr,
            nullptr, nullptr
        );
        // encourage users to use `FLUSH` statement instead,
        // as 'FLUSH' subroutine call isn't in Fortran standard
        diag.semantic_warning_label(
            "The 'flush' intrinsic function is an LFortran extension",
            {x.base.base.loc},
            "help: use the 'flush' statement instead"
        );
    }

    /*
        visit passed args, and visit kwargs and sets "nopass" only for GenericProcedure
    */
    void process_call_args_and_kwargs(
        const AST::SubroutineCall_t &x,
        Vec<ASR::call_arg_t>& args,
        ASR::symbol_t* original_sym,
        diag::Diagnostics& diag,
        ASR::expr_t* v_expr,
        Allocator& al,
        bool& nopass
    ) {
        visit_expr_list(x.m_args, x.n_args, args);
        ASR::symbol_t* f2 = ASRUtils::symbol_get_past_external(original_sym);

        // we handle kwargs here
        if (x.n_keywords > 0) {
            if (ASR::is_a<ASR::Variable_t>(*f2)) {
                ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(f2);
                LCOMPILERS_ASSERT(ASR::is_a<ASR::FunctionType_t>(*v->m_type));
                f2 = ASRUtils::symbol_get_past_external(v->m_type_declaration);
            }
            if (ASR::is_a<ASR::Function_t>(*f2)) {
                ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(f2);
                diag::Diagnostics diags;
                visit_kwargs(args, x.m_keywords, x.n_keywords,
                             f->m_args, f->n_args, x.base.base.loc, f,
                             diags, x.n_member);
                if (diags.has_error()) {
                    diag.diagnostics.insert(diag.diagnostics.end(),
                                            diags.diagnostics.begin(), diags.diagnostics.end());
                    throw SemanticAbort();
                }
            } else if (ASR::is_a<ASR::StructMethodDeclaration_t>(*f2)) {
                ASR::StructMethodDeclaration_t* f3 = ASR::down_cast<ASR::StructMethodDeclaration_t>(f2);
                ASR::symbol_t* f4 = f3->m_proc;
                bool is_nopass = f3->m_is_nopass;
                if (!ASR::is_a<ASR::Function_t>(*f4)) {
                    diag.add(Diagnostic(
                        std::string(f3->m_proc_name) + " is not a subroutine.",
                        Level::Error, Stage::Semantic, {
                            Label("", {x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(f4);
                diag::Diagnostics diags;
                visit_kwargs(args, x.m_keywords, x.n_keywords,
                             f->m_args, f->n_args, x.base.base.loc, f,
                             diags, x.n_member, is_nopass);
                if (diags.has_error()) {
                    diag.diagnostics.insert(diag.diagnostics.end(),
                                            diags.diagnostics.begin(), diags.diagnostics.end());
                    throw SemanticAbort();
                }
            } else if (ASR::is_a<ASR::GenericProcedure_t>(*f2)) {
                // pass, this is handled below
            } else {
                diag.add(Diagnostic(
                    "Keyword arguments are not implemented for generic subroutines yet, symbol type: " + std::to_string(f2->type),
                    Level::Error, Stage::Semantic, {
                        Label("", {x.base.base.loc})
                    }));
                throw SemanticAbort();
            }
        }

        // handle GenericProcedure
        if (ASR::is_a<ASR::GenericProcedure_t>(*f2)) {
            ASR::GenericProcedure_t* f3 = ASR::down_cast<ASR::GenericProcedure_t>(f2);
            bool function_found = false;
            bool is_nopass = false;
            bool is_class_procedure = false;
            for (size_t i = 0; i < f3->n_procs && !function_found; i++) {
                ASR::symbol_t* f4 = ASRUtils::symbol_get_past_external(f3->m_procs[i]);
                if (ASR::is_a<ASR::StructMethodDeclaration_t>(*f4)) {
                    ASR::StructMethodDeclaration_t* f5 = ASR::down_cast<ASR::StructMethodDeclaration_t>(f4);
                    f4 = f5->m_proc;
                    is_nopass = f5->m_is_nopass;
                    is_class_procedure = true;
                }
                if (!ASR::is_a<ASR::Function_t>(*f4)) {
                    diag.add(Diagnostic(
                        std::string(ASRUtils::symbol_name(f4)) + " is not a function/subroutine.",
                        Level::Error, Stage::Semantic, {
                            Label("", {x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }

                if (x.n_keywords > 0) {
                    ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(f4);
                    diag::Diagnostics diags;
                    Vec<ASR::call_arg_t> args_;
                    args_.from_pointer_n_copy(al, args.p, args.size());
                    visit_kwargs(args_, x.m_keywords, x.n_keywords,
                                 f->m_args, f->n_args, x.base.base.loc, f,
                                 diags, x.n_member, is_nopass);
                    if (is_class_procedure && !is_nopass) {
                        ASR::call_arg_t this_arg;
                        this_arg.loc = x.m_member[0].loc;
                        this_arg.m_value = v_expr;
                        args_.push_front(al, this_arg);
                    }
                    if (!diags.has_error()) {
                        if (static_cast<size_t>(ASRUtils::select_generic_procedure(args_, *f3, x.base.base.loc,
                            [&](const std::string& msg, const Location& loc) {
                                diag.add(Diagnostic(
                                    msg,
                                    Level::Error, Stage::Semantic, {
                                        Label("", {loc})
                                    }));
                                throw SemanticAbort();
                            }, false)) == i) {
                            function_found = true;
                            args.n = 0;
                            if (is_class_procedure && !is_nopass) {
                                args.from_pointer_n_copy(al, args_.p + 1, args_.size() - 1);
                            } else {
                                args.from_pointer_n_copy(al, args_.p, args_.size());
                            }
                        }
                    }
                }
            }
            if (!function_found && x.n_keywords > 0) {
                diag.add(Diagnostic(
                    "No matching function found for the call to generic procedure, " + std::string(f3->m_name),
                    Level::Error, Stage::Semantic, {
                        Label("", {x.base.base.loc})
                    }));
                throw SemanticAbort();
            }
            nopass = is_nopass;
        }
    }

    void visit_SubroutineCall(const AST::SubroutineCall_t &x) {
        std::string sub_name = to_lower(x.m_name);
        ASR::asr_t* intrinsic_subroutine = intrinsic_subroutine_as_node(x, sub_name);
        if( intrinsic_subroutine ) {
            tmp = intrinsic_subroutine;
            return;
        }
        if (x.n_temp_args > 0) {
            ASR::symbol_t *owner_sym = ASR::down_cast<ASR::symbol_t>(current_scope->asr_owner);
            sub_name = handle_templated(x.m_name, ASR::is_a<ASR::Template_t>(*ASRUtils::get_asr_owner(owner_sym)),
                x.m_temp_args, x.n_temp_args, x.base.base.loc);
        }
        SymbolTable* scope = current_scope;
        ASR::symbol_t *original_sym;
        ASR::expr_t *v_expr = nullptr;
        bool is_external = check_is_external(sub_name);
        bool sub_contain_entry_function = entry_functions.find(sub_name) != entry_functions.end();
        if (!is_external && sub_contain_entry_function) {
            // there can be a chance that scope is new scope of master entry function
            // Then check if it is external procedure by checking all the external_procedures_mapping

            for(auto it: external_procedures_mapping) {
                std::vector<std::string> external_procedures_copy = it.second;
                if (std::find(external_procedures_copy.begin(), external_procedures_copy.end(), sub_name) != external_procedures_copy.end()) {
                    is_external = true;
                    break;
                }
            }
        }

        // If this is a type bound procedure (in a class) it won't be in the
        // main symbol table. Need to check n_member.
        if (x.n_member >= 1) {
            visit_NameUtil(x.m_member, x.n_member - 1,
                x.m_member[x.n_member - 1].m_name, x.base.base.loc, x.n_member);
            v_expr = ASRUtils::EXPR(tmp);
            original_sym = resolve_deriv_type_proc(x.base.base.loc, sub_name,
                            to_lower(x.m_member[x.n_member - 1].m_name), v_expr,
                            ASRUtils::type_get_past_pointer(ASRUtils::expr_type(v_expr)), scope);
            original_sym = ASRUtils::import_class_procedure(al, x.base.base.loc,
                original_sym, current_scope);
        } else {
            original_sym = current_scope->resolve_symbol(sub_name);
        }
        if (original_sym && is_external) {
            ASR::symbol_t* sym_past_external = ASRUtils::symbol_get_past_external(original_sym);
            if (ASRUtils::is_symbol_procedure_variable(sym_past_external)) {
                is_external = false;
            }
        }
        if (!original_sym || (original_sym && is_external)) {
            if (to_lower(sub_name) == "exit") {
                diag.semantic_warning_label(
                    "Routine `" + sub_name + "` is a non-standard function",
                    {x.base.base.loc},
                    ""
                );
                ASR::expr_t* arg = nullptr;
                if ( x.n_args >= 1 ) {
                    visit_expr(*x.m_args[0].m_end);
                    arg = ASRUtils::EXPR(tmp);
                }
                tmp = ASR::make_Stop_t( al, x.base.base.loc, arg );
                return;
            }
            if (to_lower(sub_name) == "flush") {
                // assigns 'tmp' to an ASR node
                flush_subroutine_to_flush_statement(x);
                return;
            }
            ASR::symbol_t* external_sym = is_external ? original_sym : nullptr;
            original_sym = resolve_intrinsic_function(x.base.base.loc, sub_name);
            if (!original_sym && compiler_options.implicit_interface) {
                ASR::ttype_t* type = ASRUtils::TYPE(ASR::make_Real_t(al, x.base.base.loc, 8));
                create_implicit_interface_function(x, sub_name, false, type);
                original_sym = current_scope->resolve_symbol(sub_name);
                LCOMPILERS_ASSERT(original_sym!=nullptr);
            }
            // check if external sym is updated, or: say if signature of external_sym and original_sym are different
            if (original_sym && external_sym && is_external && ASRUtils::is_external_sym_changed(original_sym, external_sym)) {
                changed_external_function_symbol[ASRUtils::symbol_name(original_sym)] = original_sym;
            }
            // remove from external_procedures_mapping
            if (original_sym && is_external) {
                erase_from_external_mapping(sub_name);
            }

            // Update arguments if the symbol belonged to a function
            ASR::symbol_t* asr_owner_sym = ASR::down_cast<ASR::symbol_t>(current_scope->asr_owner);
            if (ASR::is_a<ASR::Function_t>(*asr_owner_sym)) {
                ASR::Function_t *current_function = ASR::down_cast<ASR::Function_t>(asr_owner_sym);
                for (size_t i = 0; i < current_function->n_args; i++) {
                    if (ASR::is_a<ASR::Var_t>(*current_function->m_args[i])) {
                        ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(current_function->m_args[i]);
                        if (std::string(ASRUtils::symbol_name(var->m_v)) == sub_name) {
                            var->m_v = original_sym;
                        }
                    }
                }
            }
        }
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(original_sym);
        ASR::Function_t *f = nullptr;
        if (ASR::is_a<ASR::Function_t>(*sym)) {
            f = ASR::down_cast<ASR::Function_t>(sym);
            if (ASRUtils::is_intrinsic_procedure(f)) {
                if (intrinsic_module_procedures_as_asr_nodes.find(sub_name) !=
                    intrinsic_module_procedures_as_asr_nodes.end()) {
                    if (sub_name == "c_f_pointer") {
                        tmp = create_CFPointer(x);
                    } else if (sub_name == "c_f_procpointer") {
                        tmp = create_CFProcPointer(x);
                    } else {
                        LCOMPILERS_ASSERT(false)
                    }
                    return;
                }
            }
        }

        Vec<ASR::call_arg_t> args;
        bool nopass = false;
        process_call_args_and_kwargs(x, args, original_sym, diag, v_expr, al, nopass);

        // checking for intent mismatch   
        if (f) { 
            ASRUtils::check_simple_intent_mismatch<SemanticAbort>(this->diag, f, args);
            for (size_t i = 0; i < args.size() && i < f->n_args; i++) {
                if (args[i].m_value == nullptr) continue;
                ASR::symbol_t* dummy_sym = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(f->m_args[i])->m_v);
                if (ASR::is_a<ASR::Variable_t>(*dummy_sym)) {
                    ASR::Variable_t* dummy_var = ASR::down_cast<ASR::Variable_t>(dummy_sym);
                    ASR::ttype_t* dummy_type = dummy_var->m_type;
                    ASR::ttype_t* actual_type = ASRUtils::expr_type(args[i].m_value);
                    if (ASRUtils::is_allocatable(dummy_type)) {
                        ASR::ttype_t* dummy_type_unwrapped = ASRUtils::type_get_past_array(
                            ASRUtils::type_get_past_allocatable(dummy_type));
                        ASR::ttype_t* actual_type_unwrapped = ASRUtils::type_get_past_array(
                            ASRUtils::type_get_past_allocatable(actual_type));
                        if (ASR::is_a<ASR::String_t>(*dummy_type_unwrapped) && 
                            ASR::is_a<ASR::String_t>(*actual_type_unwrapped)) {
                            ASR::String_t* dummy_str = ASR::down_cast<ASR::String_t>(dummy_type_unwrapped);
                            ASR::String_t* actual_str = ASR::down_cast<ASR::String_t>(actual_type_unwrapped);
                            
                            if (dummy_str->m_len_kind == ASR::string_length_kindType::ExpressionLength &&
                                actual_str->m_len_kind == ASR::string_length_kindType::ExpressionLength &&
                                dummy_str->m_len != nullptr && actual_str->m_len != nullptr) {
                                
                                int64_t dummy_len = -1;
                                int64_t actual_len = -1;
                                
                                if (ASR::is_a<ASR::IntegerConstant_t>(*dummy_str->m_len)) {
                                    dummy_len = ASR::down_cast<ASR::IntegerConstant_t>(dummy_str->m_len)->m_n;
                                }
                                if (ASR::is_a<ASR::IntegerConstant_t>(*actual_str->m_len)) {
                                    actual_len = ASR::down_cast<ASR::IntegerConstant_t>(actual_str->m_len)->m_n;
                                }
                                
                                if (dummy_len > 0 && actual_len > 0 && dummy_len != actual_len) {
                                    std::string dummy_name = dummy_var->m_name;
                                    std::string error_msg = "Type mismatch: allocatable character dummy argument '" + 
                                        dummy_name + "' has length " + std::to_string(dummy_len) + 
                                        " but actual argument has length " + std::to_string(actual_len) + ".";
                                    diag.semantic_error_label(
                                        error_msg, {args[i].m_value->base.loc},
                                        "dummy argument '" + dummy_name + "' declared here");
                                    throw SemanticAbort();
                                }
                            }
                            
                            // Check deferred length consistency: dummy and actual must both
                            // have deferred length or both have non-deferred length
                            bool dummy_is_deferred = dummy_str->m_len_kind == ASR::string_length_kindType::DeferredLength;
                            bool actual_is_deferred = actual_str->m_len_kind == ASR::string_length_kindType::DeferredLength;
                            
                            if (dummy_is_deferred != actual_is_deferred) {
                                // std::string dummy_name = dummy_var->m_name;
                                std::string error_msg = "cannot pass unallocatable string to allocatable argument";
                                diag.semantic_error_label(
                                    error_msg, {args[i].m_value->base.loc},
                                    "make this expression allocatable");
                                throw SemanticAbort();
                            }
                        }
                    }
                }
            }
        }

        ASR::symbol_t *final_sym=nullptr;
        switch (original_sym->type) {
            case (ASR::symbolType::Function) : {
                final_sym = original_sym;
                original_sym = nullptr;
                legacy_array_sections_helper(final_sym, args, x.base.base.loc);
                break;
            }
            case (ASR::symbolType::GenericProcedure) : {
                ASR::GenericProcedure_t *p = ASR::down_cast<ASR::GenericProcedure_t>(original_sym);
                ASR::symbol_t* original_sym_owner = ASRUtils::get_asr_owner(original_sym);
                Vec<ASR::call_arg_t> args_with_mdt;
                if( x.n_member >= 1 ) {
                    // we append "this/self" (i.e. ClassObject) as first argument
                    // only when nopass is not used in the associated subroutine
                    if (!nopass) {
                        // this assigns n_args + 1
                        args_with_mdt.reserve(al, x.n_args + 1);
                        ASR::call_arg_t v_expr_call_arg;
                        v_expr_call_arg.loc = v_expr->base.loc, v_expr_call_arg.m_value = v_expr;
                        args_with_mdt.push_back(al, v_expr_call_arg);
                    } else {
                        // we *don't* append "this/self", hence assign
                        // size as `n_args`
                        args_with_mdt.reserve(al, x.n_args);
                    }
                    for( size_t i = 0; i < args.size(); i++ ) {
                        args_with_mdt.push_back(al, args[i]);
                    }
                }
                if( !ASR::is_a<ASR::Module_t>(*original_sym_owner) &&
                    !ASR::is_a<ASR::Program_t>(*original_sym_owner) ) {
                    std::string s_name = "1_" + std::string(p->m_name);
                    std::string original_sym_owner_name = ASRUtils::symbol_name(original_sym_owner);
                    if( current_scope->resolve_symbol(original_sym_owner_name) == nullptr ) {
                        std::string original_sym_owner_name_ = "1_" + original_sym_owner_name;
                        if( current_scope->resolve_symbol(original_sym_owner_name) == nullptr ) {
                            ASR::symbol_t* module_ = ASRUtils::get_asr_owner(original_sym_owner);
                            LCOMPILERS_ASSERT(ASR::is_a<ASR::Module_t>(*module_));
                            original_sym_owner = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(al,
                                                    x.base.base.loc, current_scope, s2c(al, original_sym_owner_name_),
                                                    original_sym_owner, ASRUtils::symbol_name(module_), nullptr, 0,
                                                    s2c(al, original_sym_owner_name), ASR::accessType::Private));
                            current_scope->add_or_overwrite_symbol(original_sym_owner_name_, original_sym_owner);
                            original_sym_owner_name = original_sym_owner_name_;
                        }
                    }
                    original_sym = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(al,
                        p->base.base.loc, current_scope, s2c(al, s_name), original_sym,
                        s2c(al, original_sym_owner_name), nullptr, 0, p->m_name, ASR::accessType::Private));
                    current_scope->add_or_overwrite_symbol(s_name, original_sym);
                }

                // If GenericProcedure resolves to a parent struct symbol, resolve the procedure names again with the original struct symbol
                if (v_expr &&
                    x.n_member >= 1 &&
                    ASR::is_a<ASR::StructType_t>(*ASRUtils::expr_type(v_expr)) && !ASRUtils::is_class_type(ASRUtils::expr_type(v_expr)) &&
                    (ASRUtils::symbol_get_past_external(ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(v_expr)))) !=
                        ASRUtils::get_asr_owner(ASRUtils::symbol_get_past_external(original_sym))) {
                    for (size_t i = 0; i < p->n_procs; i++) {
                        final_sym = resolve_deriv_type_proc(x.base.base.loc, ASRUtils::symbol_name(p->m_procs[i]),
                                        to_lower(x.m_member[x.n_member - 1].m_name), v_expr,
                                        ASRUtils::type_get_past_pointer(ASRUtils::expr_type(v_expr)), scope);
                        final_sym = ASRUtils::import_class_procedure(al, x.base.base.loc,
                            final_sym, current_scope);
                        ASR::StructMethodDeclaration_t* cp = ASR::down_cast<ASR::StructMethodDeclaration_t>(ASRUtils::symbol_get_past_external(final_sym));
                        Location l = x.base.base.loc;
                        // TODO: Add error message here
                        if (ASRUtils::select_func_subrout(cp->m_proc, args_with_mdt, l,
                            [&](const std::string &msg, const Location &loc) {
                                diag.add(Diagnostic(
                                    msg,
                                    Level::Error, Stage::Semantic, {
                                        Label("",{loc})
                                    }));
                                throw SemanticAbort();
                                })) {
                                    break;
                                }
                    }
                    break;
                }

                int idx;
                if( x.n_member >= 1 ) {
                    idx = ASRUtils::select_generic_procedure(args_with_mdt, *p, x.base.base.loc,
                            [&](const std::string &msg, const Location &loc) {
                                diag.add(Diagnostic(
                                    msg,
                                    Level::Error, Stage::Semantic, {
                                        Label("",{loc})
                                    }));
                                throw SemanticAbort();
                                });
                } else {
                    idx = ASRUtils::select_generic_procedure(args, *p, x.base.base.loc,
                            [&](const std::string &msg, const Location &loc) {
                                diag.add(Diagnostic(
                                    msg,
                                    Level::Error, Stage::Semantic, {
                                        Label("",{loc})
                                    }));
                                throw SemanticAbort();
                                });
                }
                ASR::symbol_t* func_sym = p->m_procs[idx];

                // Case: GenericProcedure is present in abstract class and called from derived class object
                if (x.n_member >= 1) {
                    func_sym = resolve_deriv_type_proc(x.base.base.loc, ASRUtils::symbol_name(func_sym),
                                    to_lower(x.m_member[x.n_member - 1].m_name), v_expr,
                                    ASRUtils::type_get_past_pointer(ASRUtils::expr_type(v_expr)), scope);
                }
                // Create ExternalSymbol for procedures in different modules.
                if( ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(func_sym)) ) {
                    f = ASR::down_cast<ASR::Function_t>(ASRUtils::symbol_get_past_external(func_sym));
                }
                final_sym = ASRUtils::import_class_procedure(al, x.base.base.loc,
                    func_sym, current_scope);
                break;
            }
            case (ASR::symbolType::StructMethodDeclaration) : {
                final_sym = original_sym;
                original_sym = nullptr;
                break;
            }
            case (ASR::symbolType::ExternalSymbol) : {
                ASR::ExternalSymbol_t *p = ASR::down_cast<ASR::ExternalSymbol_t>(original_sym);
                final_sym = p->m_external;
                // Enforced by verify(), but we ensure anyway that
                // ExternalSymbols are not chained:
                LCOMPILERS_ASSERT(!ASR::is_a<ASR::ExternalSymbol_t>(*final_sym))
                if (ASR::is_a<ASR::GenericProcedure_t>(*final_sym)) {
                    ASR::GenericProcedure_t *g = ASR::down_cast<ASR::GenericProcedure_t>(final_sym);
                    int idx = ASRUtils::select_generic_procedure(args, *g, x.base.base.loc,
                                [&](const std::string &msg, const Location &loc) {
                                    diag.add(Diagnostic(
                                        msg,
                                        Level::Error, Stage::Semantic, {
                                            Label("",{loc})
                                        }));
                                    throw SemanticAbort();
                                    });
                    // FIXME
                    // Create ExternalSymbol for the final subroutine here
                    final_sym = ASRUtils::symbol_get_past_external(g->m_procs[idx]);
                    if (!ASR::is_a<ASR::Function_t>(*final_sym)) {
                        diag.add(Diagnostic(
                            "ExternalSymbol must point to a Subroutine",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.base.base.loc})
                            }));
                        throw SemanticAbort();
                    }
                    f = ASR::down_cast<ASR::Function_t>(final_sym);
                    // We mangle the new ExternalSymbol's local name as:
                    //   generic_procedure_local_name @
                    //     specific_procedure_remote_name
                    std::string local_sym = std::string(to_lower(p->m_name)) + "@"
                        + ASRUtils::symbol_name(final_sym);
                    if (current_scope->get_symbol(local_sym) == nullptr) {
                        Str name;
                        name.from_str(al, local_sym);
                        char *cname = name.c_str(al);
                        ASR::asr_t *sub = ASR::make_ExternalSymbol_t(
                            al, p->base.base.loc,
                            /* a_symtab */ current_scope,
                            /* a_name */ cname,
                            final_sym,
                            ASRUtils::symbol_name(ASRUtils::get_asr_owner(final_sym)),
                            nullptr, 0, ASRUtils::symbol_name(final_sym),
                            ASR::accessType::Private);
                        final_sym = ASR::down_cast<ASR::symbol_t>(sub);
                        current_scope->add_symbol(local_sym, final_sym);
                    } else {
                        final_sym = current_scope->get_symbol(local_sym);
                    }
                } else if (ASR::is_a<ASR::StructMethodDeclaration_t>(*final_sym)) {
                    ASR::StructMethodDeclaration_t* class_proc = ASR::down_cast<ASR::StructMethodDeclaration_t>(final_sym);
                    nopass = class_proc->m_is_nopass;
                    final_sym = original_sym;
                    original_sym = nullptr;
                    ASR::symbol_t* f4 = class_proc->m_proc;
                    if (!ASR::is_a<ASR::Function_t>(*f4)) {
                        diag.add(Diagnostic(
                            std::string(class_proc->m_proc_name) + " is not a subroutine.",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.base.base.loc})
                            }));
                        throw SemanticAbort();
                    }
                    f = ASR::down_cast<ASR::Function_t>(f4);
                } else if (ASR::is_a<ASR::Variable_t>(*final_sym)) {
                    nopass = true;
                    final_sym = original_sym;
                    original_sym = nullptr;
                } else {
                    if (!ASR::is_a<ASR::Function_t>(*final_sym) &&
                        !ASR::is_a<ASR::StructMethodDeclaration_t>(*final_sym)) {
                        diag.add(Diagnostic(
                            "ExternalSymbol must point to a Subroutine",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.base.base.loc})
                            }));
                        throw SemanticAbort();
                    }
                    final_sym=original_sym;
                    original_sym = nullptr;
                }
                break;
            }
            case (ASR::symbolType::Variable) : {
                if (compiler_options.implicit_interface &&
                    !ASRUtils::is_symbol_procedure_variable(original_sym)
                ) {
                    // In case of implicit_interface, we redefine the symbol
                    // from a Variable to Function. Example:
                    //
                    //     real :: x
                    //     call x(3, 4)
                    //
                    // TODO: We currently do not explictly handle the case
                    // of previously using the variable as a variable and now
                    // using it as a function. This will (eventually) fail
                    // in verify(). But we should give an error earlier as well.
                    ASR::ttype_t* old_type = ASR::down_cast<ASR::Variable_t>(original_sym)->m_type;
                    current_scope->erase_symbol(sub_name);
                    create_implicit_interface_function(x, sub_name, false, old_type);
                    original_sym = current_scope->resolve_symbol(sub_name);
                    LCOMPILERS_ASSERT(original_sym!=nullptr);

                    // One issue to solve is if `sub_name` is an argument of
                    // the current function, such as in:
                    //
                    //     subroutine(f)
                    //     call f(3, 4)
                    //
                    // With implicit typing `f` would already be declared
                    // as `real :: f`, so we need to change it.
                    //
                    // TODO: we are in the body visitor, which means
                    // the function might have already been used with the
                    // incorrect interface, and most likely LFortran gave
                    // an error message.
                    //
                    // We simply redo function arguments based on the updated
                    // symbol table:
                    LCOMPILERS_ASSERT(current_scope->asr_owner != nullptr)
                    ASR::Function_t *current_function = ASR::down_cast2
                        <ASR::Function_t>(current_scope->asr_owner);
                    redo_function_argument(*current_function, sub_name);
                } else if (!ASRUtils::is_symbol_procedure_variable(original_sym)) {
                    // Calling a non-procedure variable without implicit-interface
                    diag.add(Diagnostic(
                        "Cannot call '" + sub_name + "' as a procedure; it is a variable. "
                        "Use --implicit-interface to enable implicit procedure interfaces.",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                final_sym = original_sym;
                original_sym = nullptr;
                break;
            }
            default : {
                diag.add(Diagnostic(
                    "Symbol type not supported",
                    Level::Error, Stage::Semantic, {
                        Label("",{x.base.base.loc})
                    }));
                throw SemanticAbort();
            }
        }
        if (ASRUtils::symbol_parent_symtab(final_sym)->get_counter() != current_scope->get_counter()
            && !ASR::is_a<ASR::Variable_t>(*final_sym)) {
            // check if asr owner is associate block.
            ADD_ASR_DEPENDENCIES(current_scope, final_sym, current_function_dependencies);
        }
        ASRUtils::insert_module_dependency(final_sym, al, current_module_dependencies);
        if (f) {
            const int offset { (v_expr == nullptr || nopass) ? 0 : 1 };
            if (args.size() + offset > f->n_args) {
                const Location args_loc { ASRUtils::get_vec_loc(args) };
                diag.add(diag::Diagnostic(
                    "More actual than formal arguments in procedure call",
                    diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("", {args_loc})}));
                    throw SemanticAbort();
            }

            // Validate required arguments are provided
            for (size_t i = 0; i + offset < f->n_args; i++) {
                ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(f->m_args[i + offset]);

                if (ASR::is_a<ASR::Variable_t>(*var->m_v)) {
                    ASR::Variable_t* v = ASRUtils::EXPR2VAR(f->m_args[i + offset]);

                    if (v->m_presence != ASR::presenceType::Optional) {
                        if (i >= args.size()) {
                            if (args.empty()) {
                                diag.add(diag::Diagnostic(
                                    "Required argument `" + std::string(v->m_name) +
                                    "` is missing in function call",
                                    diag::Level::Error, diag::Stage::Semantic, {
                                        diag::Label("", {x.base.base.loc})
                                    }));
                                throw SemanticAbort();
                            }

                            const Location args_loc { ASRUtils::get_vec_loc(args) };
                            diag.add(diag::Diagnostic(
                                "Required argument `" + std::string(v->m_name) +
                                "` at position " + std::to_string(i + 1) +
                                " is missing in function call",
                                diag::Level::Error, diag::Stage::Semantic, {
                                    diag::Label("", {args_loc})
                                }));
                            throw SemanticAbort();
                        }
                    }
                    if (i < args.size() && args[i].m_value != nullptr) {
                        ASR::expr_t* passed_arg = args[i].m_value;
                        ASR::ttype_t* passed_type = ASRUtils::expr_type(passed_arg);
                        ASR::ttype_t* param_type = v->m_type;
                        // Check if parameter expects a procedure type
                        if (ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_array(param_type))){
                            if (!ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_array(passed_type))){
                                std::string passed_str =ASRUtils::type_to_str_fortran_expr(passed_type, nullptr);
                                diag.add(diag::Diagnostic(
                                    "Type mismatch in argument `" + std::string(v->m_name) +
                                    "`: expected a procedure but got `" + passed_str + "`",
                                    diag::Level::Error, diag::Stage::Semantic, {
                                        diag::Label("", {passed_arg->base.loc})
                                    }));
                                throw SemanticAbort();
                            }
                            // Create interface for procedure variable passed as argument
                            // using the expected parameter type for the correct signature.
                            if (compiler_options.implicit_interface &&
                                    ASR::is_a<ASR::Var_t>(*passed_arg)) {
                                ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(passed_arg)->m_v;
                                if (ASRUtils::is_symbol_procedure_variable(sym)) {
                                    ASR::Variable_t* proc_var = ASR::down_cast<ASR::Variable_t>(sym);
                                    ASR::FunctionType_t* expected_ft = ASR::down_cast<ASR::FunctionType_t>(
                                        ASRUtils::type_get_past_array(param_type));
                                    // Create interface if none exists. If one exists but has no
                                    // arg info and expected has arg info, update it. Don't override
                                    // existing arg info with empty expected (preserves info from calls).
                                    bool has_type_decl = proc_var->m_type_declaration != nullptr;
                                    bool has_arg_info = false;
                                    if (has_type_decl) {
                                        // Follow ExternalSymbol to get actual Function
                                        ASR::symbol_t* actual_decl = ASRUtils::symbol_get_past_external(
                                            proc_var->m_type_declaration);
                                        if (ASR::is_a<ASR::Function_t>(*actual_decl)) {
                                            has_arg_info = ASR::down_cast<ASR::FunctionType_t>(
                                                ASR::down_cast<ASR::Function_t>(actual_decl)->m_function_signature
                                            )->n_arg_types > 0;
                                        }
                                    }
                                    if (!has_type_decl || (!has_arg_info && expected_ft->n_arg_types > 0)) {
                                        create_interface_for_procedure_variable(
                                            proc_var, passed_arg->base.loc, expected_ft);
                                    } else if (has_arg_info && expected_ft->n_arg_types == 0) {
                                        // Reverse: passed has arg_types but param doesn't.
                                        // Update the parameter's type to match what we're passing.
                                        ASR::symbol_t* actual_decl = ASRUtils::symbol_get_past_external(
                                            proc_var->m_type_declaration);
                                        ASR::FunctionType_t* passed_ft = ASR::down_cast<ASR::FunctionType_t>(
                                            ASR::down_cast<ASR::Function_t>(actual_decl)->m_function_signature);
                                        // Update the parameter's FunctionType
                                        ASR::FunctionType_t* param_ft = ASR::down_cast<ASR::FunctionType_t>(
                                            ASRUtils::type_get_past_array(v->m_type));
                                        param_ft->m_arg_types = passed_ft->m_arg_types;
                                        param_ft->n_arg_types = passed_ft->n_arg_types;
                                        // Also update the callee's function signature
                                        ASR::FunctionType_t* callee_ft = ASR::down_cast<ASR::FunctionType_t>(
                                            f->m_function_signature);
                                        callee_ft->m_arg_types[i + offset] = v->m_type;
                                    }
                                } else if (ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(sym))) {
                                    // Passed argument is a Function (not a procedure variable).
                                    // If the parameter has incomplete type info but the passed
                                    // Function has complete type info, update the parameter's interface.
                                    ASR::Function_t* passed_func = ASR::down_cast<ASR::Function_t>(
                                        ASRUtils::symbol_get_past_external(sym));
                                    ASR::FunctionType_t* passed_ft = ASR::down_cast<ASR::FunctionType_t>(
                                        passed_func->m_function_signature);
                                    ASR::FunctionType_t* param_ft = ASR::down_cast<ASR::FunctionType_t>(
                                        ASRUtils::type_get_past_array(param_type));
                                    // If parameter has no arg info but passed function does,
                                    // create/update the parameter's interface using the passed function's type info.
                                    if (param_ft->n_arg_types == 0 && passed_ft->n_arg_types > 0) {
                                        if (v->m_type_declaration) {
                                            // Interface exists, update it
                                            ASR::symbol_t* iface_sym = ASRUtils::symbol_get_past_external(
                                                v->m_type_declaration);
                                            if (ASR::is_a<ASR::Function_t>(*iface_sym)) {
                                                ASR::Function_t* iface_func = ASR::down_cast<ASR::Function_t>(iface_sym);
                                                ASR::FunctionType_t* iface_ft = ASR::down_cast<ASR::FunctionType_t>(
                                                    iface_func->m_function_signature);
                                                iface_ft->m_arg_types = passed_ft->m_arg_types;
                                                iface_ft->n_arg_types = passed_ft->n_arg_types;
                                                v->m_type = iface_func->m_function_signature;
                                                ASR::FunctionType_t* callee_ft = ASR::down_cast<ASR::FunctionType_t>(
                                                    f->m_function_signature);
                                                callee_ft->m_arg_types[i + offset] = v->m_type;
                                            }
                                        } else {
                                            // No interface yet, create one with the passed function's type info.
                                            // Create the interface in the callee's parent scope.
                                            SymbolTable* callee_scope = f->m_symtab;
                                            SymbolTable* parent_scope = callee_scope->parent ? callee_scope->parent : callee_scope;
                                            std::string var_name = v->m_name;
                                            std::string iface_name = "~implicit_interface_" + var_name + "_" +
                                                parent_scope->get_counter();
                                            SymbolTable* fn_scope = al.make_new<SymbolTable>(parent_scope);
                                            // Create args for the interface
                                            Vec<ASR::expr_t*> iface_args;
                                            iface_args.reserve(al, passed_ft->n_arg_types);
                                            Vec<ASR::ttype_t*> iface_arg_types;
                                            iface_arg_types.reserve(al, passed_ft->n_arg_types);
                                            for (size_t j = 0; j < passed_ft->n_arg_types; j++) {
                                                ASR::ttype_t* arg_type = passed_ft->m_arg_types[j];
                                                std::string arg_name = iface_name + "_arg_" + std::to_string(j);
                                                ASR::symbol_t* arg_sym = ASR::down_cast<ASR::symbol_t>(
                                                    ASR::make_Variable_t(al, passed_arg->base.loc, fn_scope, s2c(al, arg_name),
                                                        nullptr, 0, ASR::intentType::Unspecified, nullptr, nullptr,
                                                        ASR::storage_typeType::Default, arg_type, nullptr,
                                                        ASR::abiType::BindC, ASR::accessType::Public,
                                                        ASR::presenceType::Required, false, false, false, nullptr, false, false));
                                                fn_scope->add_symbol(arg_name, arg_sym);
                                                iface_args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, passed_arg->base.loc, arg_sym)));
                                                iface_arg_types.push_back(al, arg_type);
                                            }
                                            // Create return var
                                            ASR::ttype_t* return_type = param_ft->m_return_var_type;
                                            if (!return_type) {
                                                return_type = ASRUtils::TYPE(ASR::make_Real_t(al, passed_arg->base.loc, 8));
                                            }
                                            std::string return_var_name = iface_name + "_return_var";
                                            ASR::symbol_t* return_sym = ASR::down_cast<ASR::symbol_t>(
                                                ASR::make_Variable_t(al, passed_arg->base.loc, fn_scope, s2c(al, return_var_name),
                                                    nullptr, 0, ASR::intentType::ReturnVar, nullptr, nullptr,
                                                    ASR::storage_typeType::Default, return_type, nullptr,
                                                    ASR::abiType::BindC, ASR::accessType::Public,
                                                    ASR::presenceType::Required, false, false, false, nullptr, false, false));
                                            fn_scope->add_symbol(return_var_name, return_sym);
                                            ASR::expr_t* return_var = ASRUtils::EXPR(ASR::make_Var_t(al, passed_arg->base.loc, return_sym));
                                            // Create interface FunctionType
                                            ASR::ttype_t* iface_type = ASRUtils::TYPE(ASR::make_FunctionType_t(
                                                al, passed_arg->base.loc, iface_arg_types.p, iface_arg_types.size(), return_type,
                                                ASR::abiType::BindC, ASR::deftypeType::Interface, nullptr,
                                                false, false, false, false, false, nullptr, 0, false));
                                            // Create interface Function
                                            ASR::symbol_t* iface = ASR::down_cast<ASR::symbol_t>(
                                                ASR::make_Function_t(
                                                    al, passed_arg->base.loc, fn_scope, s2c(al, iface_name),
                                                    iface_type, nullptr, 0, iface_args.p, iface_args.size(), nullptr, 0,
                                                    return_var, ASR::accessType::Public, false, false,
                                                    nullptr, nullptr, nullptr));
                                            parent_scope->add_symbol(iface_name, iface);
                                            // Update the parameter's type and type_declaration
                                            v->m_type = iface_type;
                                            v->m_type_declaration = iface;
                                            // Update the callee function's signature
                                            ASR::FunctionType_t* callee_ft = ASR::down_cast<ASR::FunctionType_t>(
                                                f->m_function_signature);
                                            callee_ft->m_arg_types[i + offset] = iface_type;
                                        }
                                    } else if (passed_ft->n_arg_types == 0 && param_ft->n_arg_types > 0) {
                                        // Reverse propagation: parameter has type info (from being called
                                        // in the callee) but passed function doesn't (not called in caller).
                                        // Update the passed Function's interface with the parameter's arg types.
                                        passed_ft->m_arg_types = param_ft->m_arg_types;
                                        passed_ft->n_arg_types = param_ft->n_arg_types;

                                        // Create matching argument variables in the passed Function's symtab
                                        Vec<ASR::expr_t*> new_args;
                                        new_args.reserve(al, param_ft->n_arg_types);
                                        for (size_t j = 0; j < param_ft->n_arg_types; j++) {
                                            ASR::ttype_t* arg_type = param_ft->m_arg_types[j];
                                            std::string arg_name = std::string(passed_func->m_name) + "_arg_" + std::to_string(j);
                                            ASR::symbol_t* arg_sym = ASR::down_cast<ASR::symbol_t>(
                                                ASR::make_Variable_t(al, passed_arg->base.loc, passed_func->m_symtab,
                                                    s2c(al, arg_name), nullptr, 0, ASR::intentType::Unspecified,
                                                    nullptr, nullptr, ASR::storage_typeType::Default, arg_type,
                                                    nullptr, ASR::abiType::BindC, ASR::accessType::Public,
                                                    ASR::presenceType::Required, false, false, false, nullptr, false, false));
                                            passed_func->m_symtab->add_symbol(arg_name, arg_sym);
                                            new_args.push_back(al, ASRUtils::EXPR(
                                                ASR::make_Var_t(al, passed_arg->base.loc, arg_sym)));
                                        }
                                        passed_func->m_args = new_args.p;
                                        passed_func->n_args = new_args.size();
                                    } else if (passed_ft->n_arg_types == 0 && param_ft->n_arg_types == 0) {
                                        // Both have no arg_types - may need post-processing
                                        // if callee's param gets types after we visit callee's body
                                        needs_implicit_interface_postprocessing = true;
                                    }
                                }
                            }
                        }
                        // Skip type checking for polymorphic types (class), function types
                        bool skip_check = ASRUtils::is_class_type(ASRUtils::type_get_past_array(passed_type)) ||
                                            ASRUtils::is_class_type(ASRUtils::type_get_past_array(param_type)) ||
                                            ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_array(passed_type)) ||
                                            ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_array(param_type));
                        // For implicit_argument_casting, skip type checking in most cases
                        // EXCEPT for arrays where both are integers but with different kinds -
                        // Call_t_body cannot handle this case and would assert
                        if (compiler_options.implicit_argument_casting && !skip_check) {
                            ASR::ttype_t* passed_elem = ASRUtils::type_get_past_array(passed_type);
                            ASR::ttype_t* param_elem = ASRUtils::type_get_past_array(param_type);
                            bool both_are_arrays = ASRUtils::is_array(passed_type) && ASRUtils::is_array(param_type);
                            bool both_are_integers = ASR::is_a<ASR::Integer_t>(*passed_elem) &&
                                                     ASR::is_a<ASR::Integer_t>(*param_elem);
                            bool different_kinds = ASRUtils::extract_kind_from_ttype_t(passed_elem) !=
                                                   ASRUtils::extract_kind_from_ttype_t(param_elem);
                            // Only reject arrays of integers with different kinds
                            // All other cases can be handled by Call_t_body
                            if (!(both_are_arrays && both_are_integers && different_kinds)) {
                                skip_check = true;
                            }
                        }
                        // Check if types are equal
                        if (!skip_check && !ASRUtils::check_equal_type(passed_type, param_type, passed_arg, f->m_args[i+offset])) {
                            std::string passed_type_str = ASRUtils::type_to_str_with_kind(passed_type, nullptr);
                            std::string param_type_str = ASRUtils::type_to_str_with_kind(param_type, nullptr);
                            diag.add(diag::Diagnostic(
                                "Type mismatch in argument `" + std::string(v->m_name) +
                                "`: expected `" + param_type_str + "` but got `" +passed_type_str + "`",
                                diag::Level::Error, diag::Stage::Semantic, {
                                        diag::Label("", {passed_arg->base.loc})
                                }));
                            throw SemanticAbort();
                        }
                    }
                } else if (ASR::is_a<ASR::Function_t>(*var->m_v) &&
                           compiler_options.implicit_interface) {
                    // Handle procedure parameters represented as Function (interface) symbols.
                    // When passing a Function with complete type info to a parameter with
                    // incomplete type info, update the parameter's interface.
                    ASR::Function_t* param_func = ASR::down_cast<ASR::Function_t>(var->m_v);
                    ASR::FunctionType_t* param_ft = ASR::down_cast<ASR::FunctionType_t>(
                        param_func->m_function_signature);

                    if (i < args.size() && args[i].m_value != nullptr) {
                        ASR::expr_t* passed_arg = args[i].m_value;
                        if (ASR::is_a<ASR::Var_t>(*passed_arg)) {
                            ASR::symbol_t* passed_sym = ASR::down_cast<ASR::Var_t>(passed_arg)->m_v;
                            passed_sym = ASRUtils::symbol_get_past_external(passed_sym);

                            ASR::FunctionType_t* passed_ft = nullptr;

                            // Handle passed Function symbol
                            if (ASR::is_a<ASR::Function_t>(*passed_sym)) {
                                ASR::Function_t* passed_func = ASR::down_cast<ASR::Function_t>(passed_sym);
                                passed_ft = ASR::down_cast<ASR::FunctionType_t>(
                                    passed_func->m_function_signature);
                            }
                            // Handle passed procedure variable (Variable with FunctionType)
                            else if (ASR::is_a<ASR::Variable_t>(*passed_sym)) {
                                ASR::Variable_t* passed_var = ASR::down_cast<ASR::Variable_t>(passed_sym);
                                ASR::ttype_t* passed_type = ASRUtils::type_get_past_array(passed_var->m_type);
                                if (ASR::is_a<ASR::FunctionType_t>(*passed_type)) {
                                    passed_ft = ASR::down_cast<ASR::FunctionType_t>(passed_type);
                                }
                            }

                            // If passed has arg info but param doesn't, update param interface
                            if (passed_ft && passed_ft->n_arg_types > 0 && param_ft->n_arg_types == 0) {
                                // Update the FunctionType's arg_types
                                param_ft->m_arg_types = passed_ft->m_arg_types;
                                param_ft->n_arg_types = passed_ft->n_arg_types;

                                // Create matching argument variables in the Function's symtab
                                Vec<ASR::expr_t*> new_args;
                                new_args.reserve(al, passed_ft->n_arg_types);
                                for (size_t j = 0; j < passed_ft->n_arg_types; j++) {
                                    ASR::ttype_t* arg_type = passed_ft->m_arg_types[j];
                                    std::string arg_name = std::string(param_func->m_name) + "_arg_" + std::to_string(j);
                                    ASR::symbol_t* arg_sym = ASR::down_cast<ASR::symbol_t>(
                                        ASR::make_Variable_t(al, passed_arg->base.loc, param_func->m_symtab,
                                            s2c(al, arg_name), nullptr, 0, ASR::intentType::Unspecified,
                                            nullptr, nullptr, ASR::storage_typeType::Default, arg_type,
                                            nullptr, ASR::abiType::BindC, ASR::accessType::Public,
                                            ASR::presenceType::Required, false, false, false, nullptr, false, false));
                                    param_func->m_symtab->add_symbol(arg_name, arg_sym);
                                    new_args.push_back(al, ASRUtils::EXPR(
                                        ASR::make_Var_t(al, passed_arg->base.loc, arg_sym)));
                                }
                                param_func->m_args = new_args.p;
                                param_func->n_args = new_args.size();

                                // Update the callee's signature arg_types as well
                                ASR::FunctionType_t* callee_ft = ASR::down_cast<ASR::FunctionType_t>(
                                    f->m_function_signature);
                                callee_ft->m_arg_types[i + offset] = param_func->m_function_signature;
                            } else if (passed_ft && passed_ft->n_arg_types == 0 && param_ft->n_arg_types > 0) {
                                // Reverse propagation: parameter has type info but passed doesn't.
                                // Only handle when passed_sym is a Function_t.
                                if (ASR::is_a<ASR::Function_t>(*passed_sym)) {
                                    ASR::Function_t* passed_func = ASR::down_cast<ASR::Function_t>(passed_sym);
                                    passed_ft->m_arg_types = param_ft->m_arg_types;
                                    passed_ft->n_arg_types = param_ft->n_arg_types;

                                    // Create matching argument variables in the passed Function's symtab
                                    Vec<ASR::expr_t*> new_args;
                                    new_args.reserve(al, param_ft->n_arg_types);
                                    for (size_t j = 0; j < param_ft->n_arg_types; j++) {
                                        ASR::ttype_t* arg_type = param_ft->m_arg_types[j];
                                        std::string arg_name = std::string(passed_func->m_name) + "_arg_" + std::to_string(j);
                                        ASR::symbol_t* arg_sym = ASR::down_cast<ASR::symbol_t>(
                                            ASR::make_Variable_t(al, passed_arg->base.loc, passed_func->m_symtab,
                                                s2c(al, arg_name), nullptr, 0, ASR::intentType::Unspecified,
                                                nullptr, nullptr, ASR::storage_typeType::Default, arg_type,
                                                nullptr, ASR::abiType::BindC, ASR::accessType::Public,
                                                ASR::presenceType::Required, false, false, false, nullptr, false, false));
                                        passed_func->m_symtab->add_symbol(arg_name, arg_sym);
                                        new_args.push_back(al, ASRUtils::EXPR(
                                            ASR::make_Var_t(al, passed_arg->base.loc, arg_sym)));
                                    }
                                    passed_func->m_args = new_args.p;
                                    passed_func->n_args = new_args.size();
                                }
                            } else if (passed_ft && passed_ft->n_arg_types == 0 && param_ft->n_arg_types == 0) {
                                // Both have no arg_types - may need post-processing
                                // if callee's param gets types after we visit callee's body
                                needs_implicit_interface_postprocessing = true;
                            }
                        }
                    }
                }
            }

            ASRUtils::set_absent_optional_arguments_to_null(args, f, al, v_expr, nopass);
        }
        ASR::stmt_t* cast_stmt = nullptr;
        tmp = ASRUtils::make_SubroutineCall_t_util(al, x.base.base.loc,
                final_sym, original_sym, args.p, args.size(), v_expr, &cast_stmt, compiler_options.implicit_argument_casting, current_scope, current_function_dependencies);

        if (cast_stmt != nullptr) {
            current_body->push_back(al, cast_stmt);
        }
    }

    // Changes argument `sub_name` to the new symbol from the current symbol
    // table of the function `x`.
    void redo_function_argument(ASR::Function_t &x, const std::string &sub_name) {
        for (size_t i=0; i<x.n_args; i++) {
            ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(x.m_args[i])->m_v;
            std::string arg_s = ASRUtils::symbol_name(sym);
            if (arg_s == sub_name) {
                ASR::symbol_t *var = x.m_symtab->get_symbol(arg_s);
                x.m_args[i] = ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc, var));
                return;
            }
        }
        throw LCompilersException("Argument not found");
    }

    ASR::asr_t* construct_leading_space(const Location &loc) {
        ASR::ttype_t *str_type_len_0 = ASRUtils::TYPE(ASR::make_String_t(
            al, loc, 1,
            ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 0,
                ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)))),
            ASR::string_length_kindType::ExpressionLength,
            ASR::string_physical_typeType::DescriptorString));
        ASR::expr_t *empty_string = ASRUtils::EXPR(ASR::make_StringConstant_t(
            al, loc, s2c(al, ""), str_type_len_0));
        ASR::ttype_t *str_type_len_1 = ASRUtils::TYPE(ASR::make_String_t(
            al, loc, 1,
            ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1,
                ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)))),
            ASR::string_length_kindType::ExpressionLength,
            ASR::string_physical_typeType::DescriptorString));
        ASR::expr_t *space = ASRUtils::EXPR(ASR::make_StringConstant_t(
            al, loc, s2c(al, " "), str_type_len_1));
        Vec<ASR::expr_t*> args;
        args.reserve(al, 1);
        args.push_back(al, space);
        return ASR::make_FileWrite_t(al, loc, 0, nullptr, nullptr,
            nullptr, nullptr, args.p, args.size(), nullptr, empty_string, nullptr, true, nullptr, nullptr);
    }

    void visit_Print(const AST::Print_t &x) {
        Vec<ASR::expr_t*> body;
        body.reserve(al, x.n_values);
        ASR::expr_t *fmt=nullptr;
        if (x.m_fmt != nullptr) {
            this->visit_expr(*x.m_fmt);
            fmt = ASRUtils::EXPR(tmp);
            //check if fmt is integer variable
            ASR::ttype_t* fmt_type = ASRUtils::expr_type(fmt);
            if (ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(fmt_type)) &&
                ASR::is_a<ASR::Var_t>(*fmt)) {
                diag.add(Diagnostic(
                    "Assigned format (using integer variable as format specifier) is not supported. "
                    "Use character variables instead.",
                    Level::Error, Stage::Semantic, {
                        Label("", {x.base.base.loc})
                    }));
                throw SemanticAbort();
            }
            
            if (ASR::is_a<ASR::StringConstant_t>(*fmt)) {
                ASR::StringConstant_t *fmt_const = ASR::down_cast<ASR::StringConstant_t>(fmt);
                std::string fmt_str = std::string(fmt_const->m_s);
                validate_format_string(fmt_str, fmt->base.loc, diag);
            }
        } else {
            if (compiler_options.print_leading_space) {
                current_body->push_back(al, ASRUtils::STMT(construct_leading_space(x.base.base.loc)));
            }
        }

        for (size_t i=0; i<x.n_values; i++) {
            this->visit_expr(*x.m_values[i]);
            ASR::expr_t *expr = ASRUtils::EXPR(tmp);
            if (ASRUtils::is_assumed_rank_array(ASRUtils::expr_type(expr))) {
                ASR::Var_t* v = ASR::down_cast<ASR::Var_t>(expr);
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(v->m_v);
                std::string var_name = var->m_name;
                if (assumed_rank_arrays.find(var_name) == assumed_rank_arrays.end()) {
                    diag.add(diag::Diagnostic(
                        "Assumed-rank arrays are not supported in print statements",
                        diag::Level::Error, diag::Stage::Semantic, {
                            diag::Label("", {expr->base.loc})
                        }));
                    throw SemanticAbort();
                } else {
                    int rank = assumed_rank_arrays[var_name];
                    Vec<ASR::dimension_t> dims; dims.reserve(al, rank);
                    for (int r = 0; r < rank; r++) {
                        ASR::dimension_t dim;
                        dim.loc = x.base.base.loc;
                        dim.m_start = nullptr;
                        dim.m_length = nullptr;
                        dims.push_back(al, dim);
                    }
                    ASR::ttype_t* elem_type = ASRUtils::type_get_past_array(ASRUtils::expr_type(expr));
                    ASR::ttype_t* desc_type = ASRUtils::make_Array_t_util(al, x.base.base.loc,
                        elem_type, dims.p, dims.size(), ASR::abiType::Source, false,
                        ASR::array_physical_typeType::DescriptorArray, false, false, true
                    );
                    ASR::asr_t* array_cast = ASRUtils::make_ArrayPhysicalCast_t_util(
                        al, expr->base.loc, expr, ASR::array_physical_typeType::AssumedRankArray,
                        ASR::array_physical_typeType::DescriptorArray, desc_type, nullptr
                    );
                    expr = ASRUtils::EXPR(array_cast);
                }
            }
            body.push_back(al, expr);
        }
        if (fmt && ASR::is_a<ASR::IntegerConstant_t>(*fmt)) {
            ASR::IntegerConstant_t *f = ASR::down_cast<ASR::IntegerConstant_t>(fmt);
            int64_t label = f->m_n;
            if (format_statements.find(label) == format_statements.end()) {
                ASR::ttype_t *char_type = ASRUtils::TYPE(ASR::make_Allocatable_t(al, x.base.base.loc,
                    ASRUtils::TYPE(ASR::make_String_t(
                        al, x.base.base.loc, 1, nullptr,
                        ASR::string_length_kindType::DeferredLength,
                        ASR::string_physical_typeType::DescriptorString))));
                tmp =  ASR::make_Print_t(al, x.base.base.loc,
                    ASRUtils::EXPR(ASR::make_StringFormat_t(al, x.base.base.loc, nullptr, body.p, body.size(),
                    ASR::string_format_kindType::FormatFortran, char_type, nullptr)));
                print_statements[tmp] = std::make_pair(&x.base,label);
                return;
            }
            ASR::ttype_t *fmt_type = ASRUtils::TYPE(ASR::make_String_t(
                al, fmt->base.loc, 1,
                ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, fmt->base.loc,
                    format_statements[label].size(),
                    ASRUtils::TYPE(ASR::make_Integer_t(al, fmt->base.loc, 4)))),
                ASR::string_length_kindType::ExpressionLength,
                ASR::string_physical_typeType::DescriptorString));
            ASR::expr_t *fmt_constant = ASRUtils::EXPR(ASR::make_StringConstant_t(
                al, fmt->base.loc, s2c(al, format_statements[label]), fmt_type));
            ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Allocatable_t(al, x.base.base.loc,
                ASRUtils::TYPE(ASR::make_String_t(
                    al, x.base.base.loc, 1, nullptr,
                    ASR::string_length_kindType::DeferredLength,
                    ASR::string_physical_typeType::DescriptorString))));
            ASR::expr_t* string_format = ASRUtils::EXPR(ASRUtils::make_StringFormat_t_util(al, fmt->base.loc,
                fmt_constant, body.p, body.size(), ASR::string_format_kindType::FormatFortran,
                type, nullptr));


            tmp = ASR::make_Print_t(al, x.base.base.loc, string_format);
        } else if (!fmt && body.size() == 1
                        && ASR::is_a<ASR::String_t>(*ASRUtils::expr_type(body[0]))
                        && !ASR::is_a<ASR::ImpliedDoLoop_t>(*body[0])
                        && !(current_select_type_guard && ASR::is_a<ASR::String_t>(*current_select_type_guard))) {
            tmp = ASR::make_Print_t(al, x.base.base.loc, body[0]);
        } else {
            ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Allocatable_t(al, x.base.base.loc,
                ASRUtils::TYPE(ASR::make_String_t(
                    al, x.base.base.loc, 1, nullptr,
                    ASR::string_length_kindType::DeferredLength,
                    ASR::string_physical_typeType::DescriptorString))));
            ASR::expr_t* string_format = ASRUtils::EXPR(ASRUtils::make_StringFormat_t_util(al, fmt?fmt->base.loc:x.base.base.loc,
                fmt, body.p, body.size(), ASR::string_format_kindType::FormatFortran,
                type, nullptr));

            Vec<ASR::expr_t*> print_args;
            print_args.reserve(al, 1);
            print_args.push_back(al, string_format);

            tmp = ASR::make_Print_t(al, x.base.base.loc, string_format);
        }
    }

    void visit_If(const AST::If_t &x) {
        all_blocks_nesting++;
        visit_expr(*x.m_test);
        ASR::expr_t *test = ASRUtils::EXPR(tmp);
        ASR::ttype_t *test_type = ASRUtils::type_get_past_pointer(ASRUtils::expr_type(test));
        if (!ASR::is_a<ASR::Logical_t>(*test_type)) {
            diag.add(diag::Diagnostic("Expected logical expression in if statement, but recieved " +
                ASRUtils::type_to_str_with_kind(test_type, test) + " instead",
                diag::Level::Error, diag::Stage::Semantic, {
                diag::Label(ASRUtils::type_to_str_with_kind(test_type, test) + " expression, expected logical", {test->base.loc})}));
            throw SemanticAbort();
        }
        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body);
        if(pragma_in_block) {
            nesting_lvl_inside_pragma++;
            do_in_pragma=true;
        }
        transform_stmts(body, x.n_body, x.m_body);
        if(pragma_in_block) {
            nesting_lvl_inside_pragma--;
        }
        if(nesting_lvl_inside_pragma==0) {
            do_in_pragma=false;
        }
        Vec<ASR::stmt_t*> orelse;
        orelse.reserve(al, x.n_orelse);
        transform_stmts(orelse, x.n_orelse, x.m_orelse);
        tmp = ASR::make_If_t(al, x.base.base.loc, x.m_stmt_name, test, body.p,
                body.size(), orelse.p, orelse.size());
        all_blocks_nesting--;
    }

    void visit_IfArithmetic(const AST::IfArithmetic_t &x) {
        visit_expr(*x.m_test);
        ASR::expr_t *test_int = ASRUtils::EXPR(tmp);
        ASR::ttype_t *test_int_type = ASRUtils::expr_type(test_int);
        bool is_int  = ASR::is_a<ASR::Integer_t>(*test_int_type);
        bool is_real = ASR::is_a<ASR::Real_t>(*test_int_type);
        if (!is_int && !is_real) {
            diag.add(diag::Diagnostic(
                "Arithmetic if (x) requires an integer or real for `x`",
                diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {test_int->base.loc})}));
            throw SemanticAbort();
        }
        ASR::expr_t *test_lt, *test_gt;
        int kind = ASRUtils::extract_kind_from_ttype_t(test_int_type);
        if (is_int) {
            ASR::ttype_t *type0 = ASRUtils::TYPE(
                ASR::make_Integer_t(al, x.base.base.loc, kind));
            ASR::expr_t *right = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al,
                x.base.base.loc, 0, type0));
            ASR::ttype_t *type = ASRUtils::TYPE(
                ASR::make_Logical_t(al, x.base.base.loc, compiler_options.po.default_integer_kind));
            ASR::expr_t *value = nullptr;
            test_lt = ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, test_int->base.loc,
                test_int, ASR::cmpopType::Lt, right, type, value));
            test_gt = ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, test_int->base.loc,
                test_int, ASR::cmpopType::Gt, right, type, value));
        } else {
            ASR::ttype_t *type0 = ASRUtils::TYPE(
                ASR::make_Real_t(al, x.base.base.loc, kind));
            ASR::expr_t *right = ASRUtils::EXPR(ASR::make_RealConstant_t(al,
                x.base.base.loc, 0, type0));
            ASR::ttype_t *type = ASRUtils::TYPE(
                ASR::make_Logical_t(al, x.base.base.loc, compiler_options.po.default_integer_kind));
            ASR::expr_t *value = nullptr;
            test_lt = ASRUtils::EXPR(ASR::make_RealCompare_t(al, test_int->base.loc,
                test_int, ASR::cmpopType::Lt, right, type, value));
            test_gt = ASRUtils::EXPR(ASR::make_RealCompare_t(al, test_int->base.loc,
                test_int, ASR::cmpopType::Gt, right, type, value));
        }
        Vec<ASR::stmt_t*> body;
        body.reserve(al, 1);
        body.push_back(al, ASRUtils::STMT(
            ASR::make_GoTo_t(al, x.base.base.loc, x.m_lt_label,
                s2c(al, std::to_string(x.m_lt_label)))));
        Vec<ASR::stmt_t*> orelse;
        orelse.reserve(al, 1);

        Vec<ASR::stmt_t*> body_gt;
        body_gt.reserve(al, 1);
        body_gt.push_back(al, ASRUtils::STMT(
            ASR::make_GoTo_t(al, x.base.base.loc, x.m_gt_label,
                s2c(al, std::to_string(x.m_gt_label)))));
        Vec<ASR::stmt_t*> orelse_gt;
        orelse_gt.reserve(al, 1);
        orelse_gt.push_back(al, ASRUtils::STMT(
            ASR::make_GoTo_t(al, x.base.base.loc, x.m_eq_label,
                s2c(al, std::to_string(x.m_eq_label)))));

        orelse.push_back(al, ASRUtils::STMT(
            ASR::make_If_t(al, x.base.base.loc, x.m_stmt_name, test_gt, body_gt.p,
                body_gt.size(), orelse_gt.p, orelse_gt.size())));
        tmp = ASR::make_If_t(al, x.base.base.loc, x.m_stmt_name, test_lt, body.p,
                body.size(), orelse.p, orelse.size());
    }

    void visit_Where(const AST::Where_t &x) {
        visit_expr(*x.m_test);
        ASR::expr_t *test = ASRUtils::EXPR(tmp);
        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body);
        transform_stmts(body, x.n_body, x.m_body);
        Vec<ASR::stmt_t*> orelse;
        orelse.reserve(al, x.n_orelse);
        transform_stmts(orelse, x.n_orelse, x.m_orelse);
        if (ASRUtils::is_array(ASRUtils::expr_type(test))) {
            if (ASR::is_a<ASR::Logical_t>(*ASRUtils::extract_type(ASRUtils::expr_type(test)))) {
                // verify that `test` is *not* the ttype of an expression as we then
                // are sure that it is a single standalone logical array
                if  (!ASR::is_a<ASR::IntegerCompare_t>(*test)
                    && !ASR::is_a<ASR::RealCompare_t>(*test)
                    && !ASR::is_a<ASR::LogicalBinOp_t>(*test)) {
                        // Rewrite into a form "X == true" as a workaround
                        // until https://github.com/lfortran/lfortran/issues/4330 is fixed
                        ASR::expr_t* logical_true = ASRUtils::EXPR(
                                                        ASR::make_LogicalConstant_t(al, x.base.base.loc, true,
                                                        ASRUtils::TYPE(ASR::make_Logical_t(al, x.base.base.loc, 4))));
                        test = ASRUtils::EXPR(ASR::make_LogicalBinOp_t(al, x.base.base.loc, test,
                                    ASR::logicalbinopType::Eqv, logical_true, ASRUtils::expr_type(test), nullptr));
                }
            } else {
                diag.add(Diagnostic(
                    "the first array argument to `where` must be of type logical",
                    Level::Error, Stage::Semantic, {
                        Label("",{test->base.loc})
                    }));
                throw SemanticAbort();
            }
        } else {
            diag.add(Diagnostic(
                "the argument to `where` must be an array",
                Level::Error, Stage::Semantic, {
                    Label("",{test->base.loc})
                }));
            throw SemanticAbort();
        }
        tmp = ASR::make_Where_t(al, x.base.base.loc, test, body.p, body.size(), orelse.p, orelse.size());
    }

    void visit_WhileLoop(const AST::WhileLoop_t &x) {
        all_loops_blocks_nesting += 1;
        visit_expr(*x.m_test);
        ASR::expr_t *test = ASRUtils::EXPR(tmp);
        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body);
        transform_stmts(body, x.n_body, x.m_body);
        tmp = ASR::make_WhileLoop_t(al, x.base.base.loc, x.m_stmt_name, test, body.p,
                body.size(), nullptr, 0);
        all_loops_blocks_nesting -= 1;
    }

    #define cast_as_loop_var(conv_candidate) \
        ASR::ttype_t *des_type = ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(var)); \
        ASR::ttype_t *src_type = ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(*conv_candidate)); \
        ImplicitCastRules::set_converted_value(al, x.base.base.loc, conv_candidate, src_type, des_type, diag);

    void visit_DoLoop(const AST::DoLoop_t &x) {
        char* loop_name = x.m_stmt_name;
        if (loop_name) {
            std::string loop_name_str = to_lower(loop_name);
            if (current_scope->get_symbol(loop_name_str) != nullptr) {
                diag.add(Diagnostic(
                    "DO loop label '" + loop_name_str + "' already defined",
                    Level::Error, Stage::Semantic, {
                        Label("",{x.base.base.loc})
                    }));
                throw SemanticAbort();
            }
        }
        loop_nesting += 1;
        all_loops_blocks_nesting += 1;
        all_blocks_nesting++;
        ASR::expr_t *var, *start, *end;
        ASR::ttype_t* type = nullptr;
        var = start = end = nullptr;
        if (x.m_var) {
            var = replace_with_common_block_variables(ASRUtils::EXPR(resolve_variable(x.base.base.loc, to_lower(x.m_var))));
        }
        if (x.m_start) {
            visit_expr(*x.m_start);
            start = ASRUtils::EXPR(tmp);
            type = ASRUtils::expr_type(start);
            if (!ASR::is_a<ASR::Integer_t>(*type)) {
                diag.semantic_warning_label(
                    "Start expression in DO loop must be integer",
                    {start->base.loc},
                    ""
                );
            }
            cast_as_loop_var(&start);
        }
        if (x.m_end) {
            visit_expr(*x.m_end);
            end = ASRUtils::EXPR(tmp);
            type = ASRUtils::expr_type(end);
            if (!ASR::is_a<ASR::Integer_t>(*type)) {
                diag.semantic_warning_label(
                    "End expression in DO loop must be integer",
                    {end->base.loc},
                    ""
                );
            }
            cast_as_loop_var(&end);
        }

        ASR::expr_t* increment;
        if (x.m_increment) {
            visit_expr(*x.m_increment);
            increment = ASRUtils::EXPR(tmp);
            // Check that the increment is not zero
            if (ASR::is_a<ASR::IntegerConstant_t>(*increment)) {
                ASR::IntegerConstant_t* inc = ASR::down_cast<ASR::IntegerConstant_t>(increment);
                if (inc->m_n == 0) {
                    diag.add(Diagnostic(
                        "Step expression (Increment) in DO loop cannot be zero",
                        Level::Error, Stage::Semantic, {
                            Label("",{increment->base.loc})
                        }));
                    throw SemanticAbort();
                }
            } else {
                type = ASRUtils::expr_type(increment);
                if (!ASR::is_a<ASR::Integer_t>(*type)) {
                    diag.semantic_warning_label(
                        "Step expression (increment) in DO loop must be integer",
                        {increment->base.loc},
                        ""
                    );
                }
            }
            cast_as_loop_var(&increment);
        } else {
            increment = nullptr;
        }

        if (var) {
            if (ASR::is_a<ASR::Var_t>(*var)) {
                ASR::Var_t* loop_var = ASR::down_cast<ASR::Var_t>(var);
                ASR::symbol_t* loop_var_sym = loop_var->m_v;
                do_loop_variables.push_back(loop_var_sym);
            }
        }
        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body);
        if(pragma_in_block) {
            nesting_lvl_inside_pragma++;
            do_in_pragma=true;
        }
        transform_stmts(body, x.n_body, x.m_body);
        if(pragma_in_block) {
            nesting_lvl_inside_pragma--;
        }
        if(nesting_lvl_inside_pragma==0) {
            do_in_pragma=false;
        }
        ASR::do_loop_head_t head;
        head.m_v = var;
        head.m_start = start;
        head.m_end = end;
        head.m_increment = increment;
        if (head.m_v != nullptr) {
            head.loc = head.m_v->base.loc;
            tmp = ASR::make_DoLoop_t(al, x.base.base.loc, x.m_stmt_name,
                head, body.p, body.size(), nullptr, 0);
            if (do_loop_variables.size() > 0) {
                do_loop_variables.pop_back();
            }
        } else {
            ASR::ttype_t* cond_type
                = ASRUtils::TYPE(ASR::make_Logical_t(al, x.base.base.loc, compiler_options.po.default_integer_kind));
            ASR::expr_t* cond = ASRUtils::EXPR(
                ASR::make_LogicalConstant_t(al, x.base.base.loc, true, cond_type));
            tmp = ASR::make_WhileLoop_t(al, x.base.base.loc, x.m_stmt_name, cond, body.p, body.size(), nullptr, 0);
        }
        loop_nesting -= 1;
        all_loops_blocks_nesting -= 1;
        all_blocks_nesting--;
    }

    void visit_DoConcurrentLoop(const AST::DoConcurrentLoop_t &x) {
        all_loops_blocks_nesting += 1;
        Vec<ASR::do_loop_head_t> heads;  // Create a vector of loop heads
        heads.reserve(al,x.n_control);
        for(size_t i=0;i<x.n_control;i++) {
            AST::ConcurrentControl_t &h = *(AST::ConcurrentControl_t*) x.m_control[i];
            if (! h.m_var) {
                diag.add(Diagnostic(
                    "Do loop: loop variable is required for now",
                    Level::Error, Stage::Semantic, {
                        Label("",{x.base.base.loc})
                    }));
                throw SemanticAbort();
            }
            if (! h.m_start) {
                diag.add(Diagnostic(
                    "Do loop: start condition required for now",
                    Level::Error, Stage::Semantic, {
                        Label("",{x.base.base.loc})
                    }));
                throw SemanticAbort();
            }
            if (! h.m_end) {
                diag.add(Diagnostic(
                    "Do loop: end condition required for now",
                    Level::Error, Stage::Semantic, {
                        Label("",{x.base.base.loc})
                    }));
                throw SemanticAbort();
            }
            ASR::expr_t *var = ASRUtils::EXPR(resolve_variable(x.base.base.loc, to_lower(h.m_var)));
            visit_expr(*h.m_start);
            ASR::expr_t *start = ASRUtils::EXPR(tmp);
            visit_expr(*h.m_end);
            ASR::expr_t *end = ASRUtils::EXPR(tmp);
            ASR::expr_t *increment;
            if (h.m_increment) {
                visit_expr(*h.m_increment);
                increment = ASRUtils::EXPR(tmp);
            } else {
                increment = nullptr;
            }
            ASR::do_loop_head_t head;
            head.m_v = var;
            head.m_start = start;
            head.m_end = end;
            head.m_increment = increment;
            head.loc = head.m_v->base.loc;
            heads.push_back(al, head);
        }
        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body);
        transform_stmts(body, x.n_body, x.m_body);
        Vec<ASR::reduction_expr_t> reductions; reductions.reserve(al, 1);
        Vec<ASR::expr_t*> shared_expr; shared_expr.reserve(al, 1);
        Vec<ASR::expr_t*> local_expr; local_expr.reserve(al, 1);
        for (size_t i = 0; i < x.n_locality; i++ ) {
            AST::concurrent_locality_t *locality = x.m_locality[i];
            if ( locality->type == AST::concurrent_localityType::ConcurrentReduce ) {
                AST::ConcurrentReduce_t *reduce = AST::down_cast<AST::ConcurrentReduce_t>(locality);
                AST::reduce_opType op = reduce->m_op;
                for ( size_t j = 0; j < reduce->n_vars; j++ ) {
                    ASR::reduction_expr_t red; red.loc = x.base.base.loc;
                    if ( op == AST::reduce_opType::ReduceAdd ) {
                        red.m_op = ASR::reduction_opType::ReduceAdd;
                    } else if ( op == AST::reduce_opType::ReduceMAX ) {
                        red.m_op = ASR::reduction_opType::ReduceMAX;
                    } else if ( op == AST::reduce_opType::ReduceMIN ) {
                        red.m_op = ASR::reduction_opType::ReduceMIN;
                    } else if ( op == AST::reduce_opType::ReduceMul ) {
                        red.m_op = ASR::reduction_opType::ReduceMul;
                    } else {
                        diag.add(Diagnostic(
                            "Unknown reduction operation",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.base.base.loc})
                            }));
                        throw SemanticAbort();
                    }
                    red.m_arg = ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc, current_scope->resolve_symbol(to_lower(reduce->m_vars[j]))));
                    reductions.push_back(al, red);
                }
            } else if ( locality->type == AST::concurrent_localityType::ConcurrentShared ) {
                AST::ConcurrentShared_t *shared = AST::down_cast<AST::ConcurrentShared_t>(locality);
                for ( size_t j = 0; j < shared->n_vars; j++ ) {
                    shared_expr.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc, current_scope->resolve_symbol(to_lower(shared->m_vars[j])))));
                }
            } else if ( locality->type == AST::concurrent_localityType::ConcurrentLocal ) {
                AST::ConcurrentLocal_t *private_ = AST::down_cast<AST::ConcurrentLocal_t>(locality);
                for ( size_t j = 0; j < private_->n_vars; j++ ) {
                    // check if loop variable is part of local expr
                    for(size_t k=0;k<x.n_control;k++) {
                        AST::ConcurrentControl_t &h = *(AST::ConcurrentControl_t*) x.m_control[k];
                        ASR::expr_t *var = ASRUtils::EXPR(resolve_variable(x.base.base.loc, to_lower(h.m_var)));
                        if (current_scope->resolve_symbol(to_lower(private_->m_vars[j])) == ASR::down_cast<ASR::Var_t>(var)->m_v ) {
                            diag.add(Diagnostic(
                                "Do concurrent loop variable `" + std::string(private_->m_vars[j]) + "` cannot be part of local expression",
                                Level::Error, Stage::Semantic, {
                                    Label("",{private_->base.base.loc})
                                }));
                            throw SemanticAbort();
                        }
                    }
                    local_expr.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc, current_scope->resolve_symbol(to_lower(private_->m_vars[j])))));
                }
            }
        }
        tmp = ASR::make_DoConcurrentLoop_t(al, x.base.base.loc, heads.p, heads.n, shared_expr.p, shared_expr.n, local_expr.p, local_expr.n, reductions.p, reductions.n, body.p,
                body.size());
        all_loops_blocks_nesting -= 1;
    }

    void visit_ForAllSingle(const AST::ForAllSingle_t &x) {
        all_loops_blocks_nesting += 1;
        if (x.n_control != 1) {
            diag.add(Diagnostic(
                "Forall statement: exactly one control statement is required for now",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }
        AST::ConcurrentControl_t &h = *(AST::ConcurrentControl_t*) x.m_control[0];
        if (! h.m_var) {
            diag.add(Diagnostic(
                "Forall statement: loop variable is required",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }
        if (! h.m_start) {
            diag.add(Diagnostic(
                "Forall statement: start condition is required",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }
        if (! h.m_end) {
            diag.add(Diagnostic(
                "Forall statement: end condition is required",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }
        ASR::expr_t *var = ASRUtils::EXPR(
            resolve_variable(x.base.base.loc, to_lower(h.m_var))
        );
        visit_expr(*h.m_start);
        ASR::expr_t *start = ASRUtils::EXPR(tmp);
        visit_expr(*h.m_end);
        ASR::expr_t *end = ASRUtils::EXPR(tmp);
        ASR::expr_t *increment;
        if (h.m_increment) {
            visit_expr(*h.m_increment);
            increment = ASRUtils::EXPR(tmp);
        } else {
            increment = nullptr;
        }

        ASR::stmt_t* assign_stmt;
        this->visit_stmt(*x.m_assign);
        LCOMPILERS_ASSERT(tmp) // TODO Handle constant array
        assign_stmt = ASRUtils::STMT(tmp);
        ASR::do_loop_head_t head;
        head.m_v = var;
        head.m_start = start;
        head.m_end = end;
        head.m_increment = increment;
        head.loc = head.m_v->base.loc;
        tmp = ASR::make_ForAllSingle_t(al, x.base.base.loc, head, assign_stmt);
        all_loops_blocks_nesting -= 1;
    }

    void visit_ForAll(const AST::ForAll_t &x) {
        all_loops_blocks_nesting += 1;
        if (x.n_control != 1) {
            diag.add(Diagnostic(
                "Forall block: one control statement is supported for now",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }
        if (x.n_body != 1) {
            diag.add(Diagnostic(
                "Forall block: one body statement is supported for now",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }
        
        AST::ConcurrentControl_t &h = *(AST::ConcurrentControl_t*) x.m_control[0];
        if (!h.m_var) {
            diag.add(Diagnostic(
                "Forall block: loop variable is required",
                Level::Error, Stage::Semantic, {
                    Label("", {x.base.base.loc})
                }));
            throw SemanticAbort();
        }
        if (!h.m_start) {
            diag.add(Diagnostic(
                "Forall block: start condition is required",
                Level::Error, Stage::Semantic, {
                    Label("", {x.base.base.loc})
                }));
            throw SemanticAbort();
        }
        if (!h.m_end) {
            diag.add(Diagnostic(
                "Forall block: end condition is required",
                Level::Error, Stage::Semantic, {
                    Label("", {x.base.base.loc})
                }));
            throw SemanticAbort();
        }
        
        ASR::expr_t *var = ASRUtils::EXPR(resolve_variable(x.base.base.loc, to_lower(h.m_var)));
        visit_expr(*h.m_start);
        ASR::expr_t *start = ASRUtils::EXPR(tmp);
        visit_expr(*h.m_end);
        ASR::expr_t *end = ASRUtils::EXPR(tmp);
        ASR::expr_t *increment;
        if (h.m_increment) {
            visit_expr(*h.m_increment);
            increment = ASRUtils::EXPR(tmp);
        } else {
            increment = nullptr;
        }
        
        ASR::do_loop_head_t head;
        head.m_v = var;
        head.m_start = start;
        head.m_end = end;
        head.m_increment = increment;
        head.loc = head.m_v->base.loc;
        
        this->visit_stmt(*x.m_body[0]);
        ASR::stmt_t* stmt = ASRUtils::STMT(tmp);
        
        tmp = ASR::make_ForAllSingle_t(al, x.base.base.loc, head, stmt);
        all_loops_blocks_nesting -= 1;
    }

    void visit_Exit(const AST::Exit_t &x) {
        if (all_loops_blocks_nesting == 0) {
            diag.add(Diagnostic("`exit` statements cannot be outside of loops or blocks",
                                Level::Error,
                                Stage::Semantic,
                                { Label("", { x.base.base.loc }) }));
            throw SemanticAbort();
        }
        tmp = ASR::make_Exit_t(al, x.base.base.loc, x.m_stmt_name);
    }

    void visit_Cycle(const AST::Cycle_t &x) {
        tmp = ASR::make_Cycle_t(al, x.base.base.loc, x.m_stmt_name);
    }

    void visit_Continue(const AST::Continue_t &/*x*/) {
        // TODO: add a check here that we are inside a While loop
        // Nothing to generate, we return a null pointer
        tmp = nullptr;
    }

    void visit_GoTo(const AST::GoTo_t &x) {
        if (x.m_goto_label) {
            if (AST::is_a<AST::Num_t>(*x.m_goto_label)) {
                int goto_label = AST::down_cast<AST::Num_t>(x.m_goto_label)->m_n;
                tmp = ASR::make_GoTo_t(al, x.base.base.loc, goto_label,
                        s2c(al, std::to_string(goto_label)));
            } else {
                this->visit_expr(*x.m_goto_label);
                ASR::expr_t *goto_label = ASRUtils::EXPR(tmp);

                // n_labels GOTO
                Vec<ASR::case_stmt_t*> a_body_vec;
                a_body_vec.reserve(al, x.n_labels);

                // 1 label SELECT
                Vec<ASR::stmt_t*> def_body;
                def_body.reserve(al, 1);
                for (size_t i = 0; i < x.n_labels; ++i) {
                    if (!AST::is_a<AST::Num_t>(*x.m_labels[i])) {
                        diag.add(Diagnostic(
                            "Only integer labels are supported in GOTO.",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.base.base.loc})
                            }));
                        throw SemanticAbort();
                    } else {
                        auto l = AST::down_cast<AST::Num_t>(x.m_labels[i]); // l->m_n gets the target -> if l->m_n == (i+1) ...
                        Vec<ASR::stmt_t*> body;
                        body.reserve(al, 1);
                        body.push_back(al, ASRUtils::STMT(ASR::make_GoTo_t(al, x.base.base.loc, l->m_n, s2c(al, std::to_string(l->m_n)))));
                        Vec<ASR::expr_t*> comparator_one;
                        comparator_one.reserve(al, 1);
                        ASR::ttype_t *int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, compiler_options.po.default_integer_kind));
                        comparator_one.push_back(al, ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, i+1, int_type)));
                        a_body_vec.push_back(al, ASR::down_cast<ASR::case_stmt_t>(ASR::make_CaseStmt_t(al, x.base.base.loc, comparator_one.p, 1, body.p, 1, false)));
                    }
                }
                tmp = ASR::make_Select_t(al, x.base.base.loc, nullptr, goto_label, a_body_vec.p,
                           a_body_vec.size(), def_body.p, def_body.size(), false);
            }
        } else if (x.m_int_var) {
            std::string label{x.m_int_var};
            if (std::find(labels.begin(), labels.end(), label) == labels.end()) {
                diag.add(Diagnostic(
                    "Cannot GOTO unknown label",
                    Level::Error, Stage::Semantic, {
                        Label("",{x.base.base.loc})
                    }));
                throw SemanticAbort();
            }
            auto sym = current_scope->resolve_symbol(label);

            if(starting_m_body == nullptr) {
                diag.add(Diagnostic(
                    "Cannot compute GOTO.",
                    Level::Error, Stage::Semantic, {
                        Label("",{x.base.base.loc})
                    }));
                throw SemanticAbort();
            }

            // n_labels GOTO
            Vec<ASR::case_stmt_t*> a_body_vec;
            a_body_vec.reserve(al, x.n_labels);
            // 1 label SELECT
            Vec<ASR::stmt_t*> def_body;
            def_body.reserve(al, 1);

            auto is_integer = [] (const std::string & s) {
                    return !s.empty() && std::all_of(s.begin(), s.end(), [](char c) {
                        return ::isdigit(c) || c == ' ';
                    });
            };

            // if there are no labels to iterate over, iterate over _all_ labels available in current scope
            if (!x.n_labels) {
                for (const auto &label : labels) {
                    if (!is_integer(label)) continue;
                    int32_t num = std::stoi(label);
                    Vec<ASR::stmt_t*> body;
                    body.reserve(al, 1);
                    body.push_back(al, ASRUtils::STMT(ASR::make_GoTo_t(al, x.base.base.loc, num, s2c(al, std::to_string(num)))));
                    Vec<ASR::expr_t*> comparator_one;
                    comparator_one.reserve(al, 1);
                    ASR::ttype_t *int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, compiler_options.po.default_integer_kind));
                    comparator_one.push_back(al, ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, num, int_type)));
                    a_body_vec.push_back(al, ASR::down_cast<ASR::case_stmt_t>(ASR::make_CaseStmt_t(al, x.base.base.loc, comparator_one.p, 1, body.p, 1, false)));
                }
            } else {
                for (size_t i = 0; i < x.n_labels; ++i) {
                    if (!AST::is_a<AST::Num_t>(*x.m_labels[i])) {
                        diag.add(Diagnostic(
                            "Only integer labels are supported in GOTO.",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.base.base.loc})
                            }));
                        throw SemanticAbort();
                    } else {
                        auto l = AST::down_cast<AST::Num_t>(x.m_labels[i]);
                        Vec<ASR::stmt_t*> body;
                        body.reserve(al, 1);
                        body.push_back(al, ASRUtils::STMT(ASR::make_GoTo_t(al, x.base.base.loc, l->m_n, s2c(al, std::to_string(l->m_n)))));
                        Vec<ASR::expr_t*> comparator_one;
                        comparator_one.reserve(al, 1);
                        ASR::ttype_t *int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, compiler_options.po.default_integer_kind));
                        comparator_one.push_back(al, ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, l->m_n, int_type)));
                        a_body_vec.push_back(al, ASR::down_cast<ASR::case_stmt_t>(ASR::make_CaseStmt_t(al, x.base.base.loc, comparator_one.p, 1, body.p, 1, false)));
                    }
                }
            }
            ASR::expr_t* var_expr = ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc, sym));
            tmp = ASR::make_Select_t(al, x.base.base.loc, nullptr, var_expr, a_body_vec.p,
                           a_body_vec.size(), def_body.p, def_body.size(), false);
        } else {
            diag.add(Diagnostic(
                "There must be a target to GOTO.",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }
    }

    void visit_Stop(const AST::Stop_t &x) {
        ASR::expr_t *code;
        if (x.m_code) {
            visit_expr(*x.m_code);
            code = ASRUtils::EXPR(tmp);
            code = cast_stop_code_to_int32(code, x.base.base.loc);
        } else {
            code = nullptr;
        }
        tmp = ASR::make_Stop_t(al, x.base.base.loc, code);
    }

    void visit_ErrorStop(const AST::ErrorStop_t &x) {
        ASR::expr_t *code;
        if (x.m_code) {
            visit_expr(*x.m_code);
            code = ASRUtils::EXPR(tmp);
            code = cast_stop_code_to_int32(code, x.base.base.loc);
        } else {
            code = nullptr;
        }
        tmp = ASR::make_ErrorStop_t(al, x.base.base.loc, code);
    }

    void visit_Nullify(const AST::Nullify_t &x) {
        Vec<ASR::expr_t*> arg_vec;
        arg_vec.reserve(al, x.n_args);
        for( size_t i = 0; i < x.n_args; i++ ) {
            this->visit_expr(*(x.m_args[i]));
            ASR::expr_t* tmp_expr = ASRUtils::EXPR(tmp);
            if (ASRUtils::is_pointer(ASRUtils::expr_type(tmp_expr)) || ASR::is_a<ASR::FunctionType_t>(*ASRUtils::expr_type(tmp_expr))) {
                if(ASR::is_a<ASR::StructInstanceMember_t>(*tmp_expr) || ASR::is_a<ASR::Var_t>(*tmp_expr)) {
                    arg_vec.push_back(al, tmp_expr);
                }
                else {
                    diag.add(Diagnostic(
                    "Pointer must be of Variable type or StructInstanceMember type in order to get nullified.",
                    Level::Error, Stage::Semantic, {
                        Label("",{tmp_expr->base.loc})
                    }));
                throw SemanticAbort();
                }
            } else {
                diag.add(Diagnostic(
                    "Only a pointer variable symbol "
                    "can be nullified.",
                    Level::Error, Stage::Semantic, {
                        Label("",{tmp_expr->base.loc})
                    }));
                throw SemanticAbort();
            }
        }
        tmp = ASR::make_Nullify_t(al, x.base.base.loc, arg_vec.p, arg_vec.size());
    }

    void visit_Requirement(const AST::Requirement_t /*&x*/) {

    }

    void visit_Require(const AST::Require_t /*&x*/) {

    }

    void collect_omp_body(ASR::omp_region_typeType region_type) {
        if (omp_region_body.size() > 0) {
            Vec<ASR::stmt_t*> last_pragma_body;
            last_pragma_body.reserve(al,0);
            //Collect till we encounter required OMPRegion of region_type
            while(!omp_region_body.empty() && !(omp_region_body.back()->type == ASR::stmtType::OMPRegion && (ASR::down_cast<ASR::OMPRegion_t>(omp_region_body.back())->m_region == region_type))) {
                last_pragma_body.push_back(al, omp_region_body.back());
                omp_region_body.pop_back();
            }
            // Reverse the vector of body to maintain the order
            Vec<ASR::stmt_t*> tmp_body;
            tmp_body.reserve(al, 0);
            for(size_t i=0; i<last_pragma_body.size(); i++) {
                tmp_body.push_back(al, last_pragma_body[last_pragma_body.size()-i-1]);
            }
            ASR::OMPRegion_t* omp_region = ASR::down_cast<ASR::OMPRegion_t>(omp_region_body.back());
            omp_region->m_body = tmp_body.p;
            omp_region->n_body = tmp_body.size();
        }
    }

    Vec<ASR::omp_clause_t*> get_clauses(const AST::Pragma_t &x) {
        Location loc = x.base.base.loc;
        Vec<ASR::omp_clause_t*> clauses;
        clauses.reserve(al, x.n_clauses);

        for (size_t i = 0; i < x.n_clauses; i++) {
            std::string clause = AST::down_cast<AST::String_t>(x.m_clauses[i])->m_s;
            std::string clause_name = clause.substr(0, clause.find('('));
            if (clause_name == "private" || clause_name == "reduction" || clause_name == "shared" || clause_name == "firstprivate" || clause_name == "collapse" || clause_name == "num_teams" || clause_name == "thread_limit" || clause_name == "schedule" || clause_name == "num_threads" || clause_name == "map" || clause_name == "device") {
                std::string list = clause.substr(clause.find('(') + 1, clause.size() - clause_name.size() - 2);
                Vec<ASR::expr_t*> vars;
                vars.reserve(al, 1);
                ASR::reduction_opType op = ASR::reduction_opType::ReduceAdd;
                ASR::map_typeType map_type = ASR::map_typeType::To;
                if (clause_name == "reduction") {
                    std::string reduction_op = list.substr(0, list.find(':'));
                    if ( reduction_op == "+" ) {
                        op = ASR::reduction_opType::ReduceAdd;
                    } else if ( reduction_op == "-" ) {
                        op = ASR::reduction_opType::ReduceSub;
                    } else if ( reduction_op == "*" ) {
                        op = ASR::reduction_opType::ReduceMul;
                    } else if ( reduction_op == "max" ) {
                        op = ASR::reduction_opType::ReduceMAX;
                    } else if ( reduction_op == "min" ) {
                        op = ASR::reduction_opType::ReduceMIN;
                    } else {
                        diag.add(Diagnostic(
                            "The reduction operator "+ reduction_op
                            +" is not supported yet",
                            Level::Error, Stage::Semantic, {
                                Label("",{loc})
                            }));
                        throw SemanticAbort();
                    }
                        list = list.substr(list.find(':')+1);
                } else if (clause_name == "map") {
                    std::string map_str = list.substr(0, list.find(':'));
                    if (map_str == "to") {
                        map_type = ASR::map_typeType::To;
                    } else if (map_str == "from") {
                        map_type = ASR::map_typeType::From;
                    } else if (map_str == "tofrom") {
                        map_type = ASR::map_typeType::ToFrom;
                    } else {
                        diag.add(Diagnostic("The map type `" + map_str + "` is not supported", Level::Error, Stage::Semantic, {Label("", {loc})}));
                        throw SemanticAbort();
                    }
                    list = list.substr(list.find(':')+1);
                } else if (clause_name == "collapse") {
                    int collapse_value = std::stoi(list.erase(0, list.find_first_not_of(" "))); // Get the value of N
                    clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(ASR::make_OMPCollapse_t(al, loc, ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, collapse_value, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)))))));
                    continue;
                } else if (clause_name == "num_threads") {
                    int num_threads = std::stoi(list.erase(0, list.find_first_not_of(" "))); // Get the value of N
                    clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(ASR::make_OMPNumThreads_t(al, loc, ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, num_threads, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)))))));
                    continue;
                } else if (clause_name == "thread_limit") {
                    int thread_limit = std::stoi(list.erase(0, list.find_first_not_of(" "))); // Get the value of N
                    clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(ASR::make_OMPThreadLimit_t(al, loc, ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, thread_limit, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)))))));
                    continue;
                } else if (clause_name == "num_teams") {
                    int num_teams = std::stoi(list.erase(0, list.find_first_not_of(" "))); // Get the value of N
                    clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(ASR::make_OMPNumTeams_t(al, loc, ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, num_teams, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)))))));
                    continue;
                } else if (clause_name == "device") {
                    int dev = std::stoi(list.erase(0, list.find_first_not_of(" "))); // Get the value of N
                    clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(ASR::make_OMPDevice_t(al, loc, ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, dev, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)))))));
                    continue;
                } else if (clause_name == "schedule") {
                    ASR::schedule_typeType schedule_type;
                    if (LCompilers::startswith(list, "static")) {
                        schedule_type = ASR::schedule_typeType::Static;
                    } else if (LCompilers::startswith(list, "dynamic")) {
                        schedule_type = ASR::schedule_typeType::Dynamic;
                    } else if (LCompilers::startswith(list, "guided")) {
                        schedule_type = ASR::schedule_typeType::Guided;
                    } else if (LCompilers::startswith(list, "auto")) {
                        schedule_type = ASR::schedule_typeType::Auto;
                    } else if (LCompilers::startswith(list, "runtime")) {
                        schedule_type = ASR::schedule_typeType::Runtime;
                    } else {
                        diag.add(Diagnostic("The schedule type `" + list + "` is not supported", Level::Error, Stage::Semantic, {Label("", {loc})}));
                        throw SemanticAbort();
                    }
                    int chunk_size = 0;
                    if(list.find(',') != std::string::npos) {
                        list = list.substr(list.find(',')+1);
                        chunk_size = std::stoi(list.erase(0, list.find_first_not_of(" ")));
                    }
                    if(chunk_size == 0) {
                        clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
                        ASR::make_OMPSchedule_t(al, loc, schedule_type, nullptr)));
                    } else {
                        clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
                            ASR::make_OMPSchedule_t(al, loc, schedule_type, ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, chunk_size, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)))))));
                    }
                    continue;
                }
                for (auto &s : LCompilers::string_split(list, ",", false)) {
                    s.erase(0, s.find_first_not_of(" "));
                    bool is_array_section = false;
                    std::string section;
                    if(s[s.size()-1] == ')') {
                        size_t pos = s.find('(');
                        if(pos != s.size() - 1) {
                            section = s.substr(pos + 1, s.size() - 2);
                            s = s.substr(0, pos);
                            is_array_section = true;
                        } else {
                            diag.add(Diagnostic("The clause variable `" + s + "` is not declared or not a variable", Level::Error, Stage::Semantic, {Label("", {loc})}));
                            throw SemanticAbort();
                        }
                    }
                    ASR::symbol_t *sym = current_scope->get_symbol(s);
                    if (!sym || !ASR::is_a<ASR::Variable_t>(*sym)) {
                        diag.add(Diagnostic("The clause variable `" + s + "` is not declared or not a variable", Level::Error, Stage::Semantic, {Label("", {loc})}));
                        throw SemanticAbort();
                    }
                    if(is_array_section) {
                        ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
                        if (!ASRUtils::is_array(var->m_type)) {
                            diag.add(Diagnostic("The clause variable `" + s + "` is not an array", Level::Error, Stage::Semantic, {Label("", {loc})}));
                            throw SemanticAbort();
                        } else {
                            Vec<ASR::array_index_t> args;
                            ASRUtils::ASRBuilder b(al, loc);
                            args.reserve(al, 1);
                            std::string start = section.substr(0, section.find(':'));
                            std::string end = section.substr(section.find(':') + 1);
                            ASR::array_index_t idx;
                            idx.loc = loc;
                            idx.m_left = b.i32(stoi(start));
                            idx.m_right = b.i32(stoi(end));
                            idx.m_step = b.i32(1);
                            args.push_back(al, idx);
                            vars.push_back(al, ASRUtils::EXPR(ASR::make_ArraySection_t(al, loc, ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym)),
                                args.p, args.n, ASRUtils::type_get_past_allocatable(var->m_type), nullptr)));
                        }
                    } else {
                        vars.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym)));
                    }
                }
                if (clause_name == "private") {
                    clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
                        ASR::make_OMPPrivate_t(al, loc, vars.p, vars.n)));
                } else if (clause_name == "shared") {
                    clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
                        ASR::make_OMPShared_t(al, loc, vars.p, vars.n)));
                } else if (clause_name == "firstprivate") {
                    clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
                        ASR::make_OMPFirstPrivate_t(al, loc, vars.p, vars.n)));
                } else if (clause_name == "map") {
                    clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
                        ASR::make_OMPMap_t(al, loc, map_type, vars.p, vars.n)));
                } else {
                    clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
                        ASR::make_OMPReduction_t(al, loc, op, vars.p, vars.n)));
                }
            } else {
                diag.add(Diagnostic("The clause " + clause_name + " is not supported for parallel sections", Level::Error, Stage::Semantic, {Label("", {loc})}));
                throw SemanticAbort();
            }
        }
        return clauses;
    }

    void visit_Pragma(const AST::Pragma_t &x) {
        if ( !compiler_options.openmp ) {
            return;
        }
        Location loc = x.base.base.loc;
        if (x.m_type == AST::OMPPragma) {
            if (x.m_end) {
                pragma_nesting_level_2--;
                if (LCompilers::startswith(x.m_construct_name, "parallel sections")) {
                    // Collect the body of the previous section
                    pragma_nesting_level_2--;
                    collect_omp_body(ASR::omp_region_typeType::Section);
                    // Now collect stmts of all Sections inside the parallel sections and add it in body of ParallelSections OMPRegion_t
                    collect_omp_body(ASR::omp_region_typeType::ParallelSections);
                } else if (LCompilers::startswith(x.m_construct_name, "sections")) {
                    // Collect the body of the previous section
                    pragma_nesting_level_2--;
                    collect_omp_body(ASR::omp_region_typeType::Section);
                    // Now collect stmts of all Sections inside the parallel sections and add it in body of ParallelSections OMPRegion_t
                    collect_omp_body(ASR::omp_region_typeType::Sections);
                } else if (LCompilers::startswith(x.m_construct_name, "single")) {
                    collect_omp_body(ASR::omp_region_typeType::Single);
                } else if (LCompilers::startswith(x.m_construct_name, "master")) {
                    collect_omp_body(ASR::omp_region_typeType::Master);
                } else if (LCompilers::startswith(x.m_construct_name, "taskloop")) {
                    collect_omp_body(ASR::omp_region_typeType::Taskloop);
                } else if (LCompilers::startswith(x.m_construct_name, "task")) {
                    collect_omp_body(ASR::omp_region_typeType::Task);
                } else if (LCompilers::startswith(x.m_construct_name, "parallel do")) {
                    collect_omp_body(ASR::omp_region_typeType::ParallelDo);
                } else if (LCompilers::startswith(x.m_construct_name, "parallel")) {
                    collect_omp_body(ASR::omp_region_typeType::Parallel);
                } else if(LCompilers::startswith(x.m_construct_name, "do")) {
                    collect_omp_body(ASR::omp_region_typeType::Do);
                } else if (LCompilers::startswith(x.m_construct_name, "critical")) {
                    collect_omp_body(ASR::omp_region_typeType::Critical);
                } else if (LCompilers::startswith(x.m_construct_name, "teams distribute")) {
                    collect_omp_body(ASR::omp_region_typeType::TeamsDistribute);
                } else if (LCompilers::startswith(x.m_construct_name, "teams")) {
                    collect_omp_body(ASR::omp_region_typeType::Teams);
                } else if (LCompilers::startswith(x.m_construct_name, "distribute parallel do")) {
                    collect_omp_body(ASR::omp_region_typeType::DistributeParallelDo);
                } else if (LCompilers::startswith(x.m_construct_name, "distribute")) {
                    collect_omp_body(ASR::omp_region_typeType::Distribute);
                } else if (LCompilers::startswith(x.m_construct_name, "atomic")) {
                    collect_omp_body(ASR::omp_region_typeType::Atomic);
                } else if (LCompilers::startswith(x.m_construct_name, "target")) {
                    collect_omp_body(ASR::omp_region_typeType::Target);
                }
                pragma_in_block=false;
                if((pragma_nesting_level_2 == 0 && omp_region_body.size()==1) || all_blocks_nesting>0) {
                    tmp=(ASR::asr_t*)(ASR::down_cast<ASR::OMPRegion_t>(omp_region_body.back()));
                    omp_region_body.pop_back();
                }
                return;
            }

            if(all_blocks_nesting>0) pragma_in_block=true;
            if (to_lower(x.m_construct_name) == "parallel") {
                pragma_nesting_level_2++;
                Vec<ASR::omp_clause_t*> clauses;
                clauses = get_clauses(x);

                Vec<ASR::stmt_t*> body;
                body.reserve(al, 0);
                omp_region_body.push_back(ASRUtils::STMT(
                    ASR::make_OMPRegion_t(al, loc, ASR::omp_region_typeType::Parallel, clauses.p, clauses.n, body.p, body.n)));
            } else if ( to_lower(x.m_construct_name) == "do" ) {
                pragma_nesting_level_2++;
                Vec<ASR::omp_clause_t*> clauses;
                clauses = get_clauses(x);
                
                Vec<ASR::stmt_t*> body;
                body.reserve(al, 0);
                omp_region_body.push_back(ASRUtils::STMT(
                    ASR::make_OMPRegion_t(al, loc, ASR::omp_region_typeType::Do, clauses.p, clauses.n, body.p, body.n)));
            } else if (to_lower(x.m_construct_name) == "parallel do") {
                pragma_nesting_level_2++;
                Vec<ASR::omp_clause_t*> clauses;
                clauses = get_clauses(x);
                
                Vec<ASR::stmt_t*> body;
                body.reserve(al, 0);
                omp_region_body.push_back(ASRUtils::STMT(
                    ASR::make_OMPRegion_t(al, loc, ASR::omp_region_typeType::ParallelDo, clauses.p, clauses.n, body.p, body.n)));
            } else if (to_lower(x.m_construct_name) == "sections") {
                pragma_nesting_level_2++;
                Vec<ASR::omp_clause_t*> clauses;
                clauses = get_clauses(x);
                is_first_section = true;
                
                Vec<ASR::stmt_t*> body;
                body.reserve(al, 0);
                omp_region_body.push_back(ASRUtils::STMT(
                    ASR::make_OMPRegion_t(al, loc, ASR::omp_region_typeType::Sections, clauses.p, clauses.n, body.p, body.n)));
            } else if (to_lower(x.m_construct_name) == "parallel sections") {
                pragma_nesting_level_2++;
                is_first_section = true;
                // Initialize OMPRegion for parallel sections
                // Get Clauses
                Vec<ASR::omp_clause_t*> clauses;
                clauses = get_clauses(x);

                Vec<ASR::stmt_t*> body;
                body.reserve(al, 0);
                omp_region_body.push_back(ASRUtils::STMT(
                    ASR::make_OMPRegion_t(al, loc, ASR::omp_region_typeType::ParallelSections, clauses.p, clauses.n, body.p, body.n)));
            } else if (to_lower(x.m_construct_name) == "section") {
                pragma_nesting_level_2++;
                if(!is_first_section){
                    // Collect body of previous section and put it in its OMPRegion
                    pragma_nesting_level_2--;
                    collect_omp_body(ASR::omp_region_typeType::Section);
                } else {
                    is_first_section = false;
                }
                // Add OMPRegion of Current Section
                ASR::asr_t* section = ASR::make_OMPRegion_t(
                    al, x.base.base.loc, ASR::omp_region_typeType::Section,
                    nullptr, 0, nullptr, 0);
                omp_region_body.push_back(ASRUtils::STMT(section));
            } else if(to_lower(x.m_construct_name) == "single") {
                pragma_nesting_level_2++;
                Vec<ASR::omp_clause_t*> clauses;
                clauses = get_clauses(x);
                Vec<ASR::stmt_t*> body;
                body.reserve(al, 0);
                omp_region_body.push_back(ASRUtils::STMT(
                    ASR::make_OMPRegion_t(al, loc, ASR::omp_region_typeType::Single, clauses.p, clauses.n, body.p, body.n)));
            
            } else if(to_lower(x.m_construct_name) == "master") {
                pragma_nesting_level_2++;
                Vec<ASR::omp_clause_t*> clauses;
                clauses = get_clauses(x);
                Vec<ASR::stmt_t*> body;
                body.reserve(al, 0);
                omp_region_body.push_back(ASRUtils::STMT(
                    ASR::make_OMPRegion_t(al, loc, ASR::omp_region_typeType::Master, clauses.p, clauses.n, body.p, body.n)));
            } else if (to_lower(x.m_construct_name) == "task") {
                pragma_nesting_level_2++;
                Vec<ASR::omp_clause_t*> clauses;
                clauses = get_clauses(x);
                Vec<ASR::stmt_t*> body;
                body.reserve(al, 0);
                omp_region_body.push_back(ASRUtils::STMT(
                    ASR::make_OMPRegion_t(al, loc, ASR::omp_region_typeType::Task, clauses.p, clauses.n, body.p, body.n)));
            } else if (to_lower(x.m_construct_name) == "critical") {
                pragma_nesting_level_2++;
                Vec<ASR::omp_clause_t*> clauses;
                clauses = get_clauses(x);
                Vec<ASR::stmt_t*> body;
                body.reserve(al, 0);
                omp_region_body.push_back(ASRUtils::STMT(
                    ASR::make_OMPRegion_t(al, loc, ASR::omp_region_typeType::Critical, clauses.p, clauses.n, body.p, body.n)));
            } else if (to_lower(x.m_construct_name) == "barrier") {
                // pragma_nesting_level_2++;
                Vec<ASR::omp_clause_t*> clauses;
                clauses = get_clauses(x);
                Vec<ASR::stmt_t*> body;
                body.reserve(al, 0);
                omp_region_body.push_back(ASRUtils::STMT(
                    ASR::make_OMPRegion_t(al, loc, ASR::omp_region_typeType::Barrier, clauses.p, clauses.n, body.p, body.n)));
            } else if (to_lower(x.m_construct_name) == "taskwait") {
                // pragma_nesting_level_2++;
                Vec<ASR::omp_clause_t*> clauses;
                clauses = get_clauses(x);
                Vec<ASR::stmt_t*> body;
                body.reserve(al, 0);
                omp_region_body.push_back(ASRUtils::STMT(
                    ASR::make_OMPRegion_t(al, loc, ASR::omp_region_typeType::Taskwait, clauses.p, clauses.n, body.p, body.n)));
            } else if (to_lower(x.m_construct_name) == "taskloop") {
                pragma_nesting_level_2++;
                Vec<ASR::omp_clause_t*> clauses;
                clauses = get_clauses(x);
                Vec<ASR::stmt_t*> body;
                body.reserve(al, 0);
                omp_region_body.push_back(ASRUtils::STMT(
                    ASR::make_OMPRegion_t(al, loc, ASR::omp_region_typeType::Taskloop, clauses.p, clauses.n, body.p, body.n)));
            } else if (to_lower(x.m_construct_name) == "teams") {
                pragma_nesting_level_2++;
                Vec<ASR::omp_clause_t*> clauses;
                clauses = get_clauses(x);
                Vec<ASR::stmt_t*> body;
                body.reserve(al, 0);
                omp_region_body.push_back(ASRUtils::STMT(
                    ASR::make_OMPRegion_t(al, loc, ASR::omp_region_typeType::Teams, clauses.p, clauses.n, body.p, body.n)));
            } else if (to_lower(x.m_construct_name) == "distribute") {
                pragma_nesting_level_2++;
                Vec<ASR::omp_clause_t*> clauses;
                clauses = get_clauses(x);
                Vec<ASR::stmt_t*> body;
                body.reserve(al, 0);
                omp_region_body.push_back(ASRUtils::STMT(
                    ASR::make_OMPRegion_t(al, loc, ASR::omp_region_typeType::Distribute, clauses.p, clauses.n, body.p, body.n)));
            } else if (to_lower(x.m_construct_name) == "teams distribute") {
                pragma_nesting_level_2++;
                Vec<ASR::omp_clause_t*> clauses;
                clauses = get_clauses(x);
                Vec<ASR::stmt_t*> body;
                body.reserve(al, 0);
                omp_region_body.push_back(ASRUtils::STMT(
                    ASR::make_OMPRegion_t(al, loc, ASR::omp_region_typeType::TeamsDistribute, clauses.p, clauses.n, body.p, body.n)));
            } else if (to_lower(x.m_construct_name) == "atomic") {
                pragma_nesting_level_2++;
                Vec<ASR::omp_clause_t*> clauses;
                clauses = get_clauses(x);
                Vec<ASR::stmt_t*> body;
                body.reserve(al, 0);
                omp_region_body.push_back(ASRUtils::STMT(
                    ASR::make_OMPRegion_t(al, loc, ASR::omp_region_typeType::Atomic, clauses.p, clauses.n, body.p, body.n)));
            } else if (to_lower(x.m_construct_name) == "target") {
                pragma_nesting_level_2++;
                Vec<ASR::omp_clause_t*> clauses;
                clauses = get_clauses(x);
                Vec<ASR::stmt_t*> body;
                body.reserve(al, 0);
                omp_region_body.push_back(ASRUtils::STMT(
                    ASR::make_OMPRegion_t(al, loc, ASR::omp_region_typeType::Target, clauses.p, clauses.n, body.p, body.n)));
            } else if (to_lower(x.m_construct_name) == "distribute parallel do") {
                pragma_nesting_level_2++;
                Vec<ASR::omp_clause_t*> clauses;
                clauses = get_clauses(x);
                Vec<ASR::stmt_t*> body;
                body.reserve(al, 0);
                omp_region_body.push_back(ASRUtils::STMT(
                    ASR::make_OMPRegion_t(al, loc, ASR::omp_region_typeType::DistributeParallelDo, clauses.p, clauses.n, body.p, body.n)));
            }else {
                diag.add(Diagnostic(
                    "The construct "+ std::string(x.m_construct_name)
                    +" is not supported yet",
                    Level::Error, Stage::Semantic, {
                        Label("",{loc})
                    }));
                throw SemanticAbort();
            }
        } else {
            diag.add(Diagnostic(
                "The pragma type is not supported yet",
                Level::Error, Stage::Semantic, {
                    Label("",{loc})
                }));
            throw SemanticAbort();
        }
    }

    void visit_Template(const AST::Template_t &x){
        is_template = true;

        SymbolTable* old_scope = current_scope;
        ASR::symbol_t* t = current_scope->get_symbol(to_lower(x.m_name));
        ASR::Template_t* v = ASR::down_cast<ASR::Template_t>(t);
        current_scope = v->m_symtab;
        for (size_t i=0; i<x.n_decl; i++) {
            this->visit_unit_decl2(*x.m_decl[i]);
        }
        for (size_t i=0; i<x.n_contains; i++) {
            this->visit_program_unit(*x.m_contains[i]);
        }
        current_scope = old_scope;

        is_template = false;
    }

};

Result<ASR::TranslationUnit_t*> body_visitor(Allocator &al,
        AST::TranslationUnit_t &ast,
        diag::Diagnostics &diagnostics,
        ASR::asr_t *unit,
        CompilerOptions &compiler_options,
        std::map<uint64_t, std::map<std::string, ASR::ttype_t*>>& implicit_mapping,
        std::map<uint64_t, ASR::symbol_t*>& common_variables_hash,
        std::map<uint64_t, size_t>& common_variables_byte_offset,
        std::map<uint64_t, std::vector<std::string>>& external_procedures_mapping,
        std::map<uint64_t, std::vector<std::string>>& explicit_intrinsic_procedures_mapping,
        std::map<uint32_t, std::map<std::string, std::pair<ASR::ttype_t*, ASR::symbol_t*>>> &instantiate_types,
        std::map<uint32_t, std::map<std::string, ASR::symbol_t*>> &instantiate_symbols,
        std::map<std::string, std::map<std::string, std::vector<AST::stmt_t*>>> &entry_functions,
        std::map<std::string, std::vector<int>> &entry_function_arguments_mapping,
        std::map<uint32_t, std::vector<ASR::stmt_t*>> &data_structure,
        LCompilers::LocationManager &lm)
{
    BodyVisitor b(al, unit, diagnostics, compiler_options, implicit_mapping,
        common_variables_hash, common_variables_byte_offset,
        external_procedures_mapping,
        explicit_intrinsic_procedures_mapping,
        instantiate_types, instantiate_symbols, entry_functions,
        entry_function_arguments_mapping, data_structure, lm
    );
    try {
        b.is_body_visitor = true;
        b.visit_TranslationUnit(ast);
        b.is_body_visitor = false;
    } catch (const SemanticAbort &) {
        Error error;
        return error;
    }
    ASR::TranslationUnit_t *tu = ASR::down_cast2<ASR::TranslationUnit_t>(unit);

    // Post-processing: propagate procedure types for implicit interfaces.
    // This handles the case where callee body is visited after caller body,
    // so the callee's parameter types weren't available during caller visit.
    // Only run if we encountered cases that might need deferred propagation.
    if (compiler_options.implicit_interface && b.needs_implicit_interface_postprocessing) {
        for (auto& item : tu->m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(item.second);
                for (size_t si = 0; si < func->n_body; si++) {
                    if (ASR::is_a<ASR::SubroutineCall_t>(*func->m_body[si])) {
                        ASR::SubroutineCall_t* call = ASR::down_cast<ASR::SubroutineCall_t>(func->m_body[si]);
                        ASR::symbol_t* callee_sym = ASRUtils::symbol_get_past_external(call->m_name);
                        if (!ASR::is_a<ASR::Function_t>(*callee_sym)) continue;
                        ASR::Function_t* callee = ASR::down_cast<ASR::Function_t>(callee_sym);

                        for (size_t i = 0; i < call->n_args; i++) {
                            if (call->m_args[i].m_value == nullptr) continue;
                            if (!ASR::is_a<ASR::Var_t>(*call->m_args[i].m_value)) continue;
                            ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(call->m_args[i].m_value);
                            ASR::symbol_t* passed_sym = ASRUtils::symbol_get_past_external(var->m_v);

                            // Handle passed Function with no arg_types
                            if (ASR::is_a<ASR::Function_t>(*passed_sym)) {
                                ASR::Function_t* passed_func = ASR::down_cast<ASR::Function_t>(passed_sym);
                                ASR::FunctionType_t* passed_ft = ASR::down_cast<ASR::FunctionType_t>(
                                    passed_func->m_function_signature);
                                if (passed_ft->n_arg_types > 0) continue;

                                if (i >= callee->n_args) continue;
                                if (callee->m_args[i] == nullptr) continue;
                                if (!ASR::is_a<ASR::Var_t>(*callee->m_args[i])) continue;
                                ASR::symbol_t* param_sym = ASR::down_cast<ASR::Var_t>(callee->m_args[i])->m_v;
                                ASR::FunctionType_t* param_ft = nullptr;

                                // Handle Variable_t parameter with FunctionType
                                if (ASR::is_a<ASR::Variable_t>(*param_sym)) {
                                    ASR::Variable_t* param = ASR::down_cast<ASR::Variable_t>(param_sym);
                                    ASR::ttype_t* param_type = ASRUtils::type_get_past_array(param->m_type);
                                    if (ASR::is_a<ASR::FunctionType_t>(*param_type)) {
                                        param_ft = ASR::down_cast<ASR::FunctionType_t>(param_type);
                                    }
                                }
                                // Handle Function_t parameter (interface)
                                else if (ASR::is_a<ASR::Function_t>(*param_sym)) {
                                    ASR::Function_t* param_func = ASR::down_cast<ASR::Function_t>(param_sym);
                                    param_ft = ASR::down_cast<ASR::FunctionType_t>(param_func->m_function_signature);
                                }

                                if (param_ft == nullptr || param_ft->n_arg_types == 0) continue;

                                // Reverse propagation: param has type info, passed doesn't
                                passed_ft->m_arg_types = param_ft->m_arg_types;
                                passed_ft->n_arg_types = param_ft->n_arg_types;

                                // Create argument variables in passed Function's symtab
                                Vec<ASR::expr_t*> new_args;
                                new_args.reserve(al, param_ft->n_arg_types);
                                for (size_t j = 0; j < param_ft->n_arg_types; j++) {
                                    ASR::ttype_t* arg_type = param_ft->m_arg_types[j];
                                    std::string arg_name = std::string(passed_func->m_name) + "_arg_" + std::to_string(j);
                                    if (passed_func->m_symtab->get_symbol(arg_name) == nullptr) {
                                        ASR::symbol_t* arg_sym = ASR::down_cast<ASR::symbol_t>(
                                            ASR::make_Variable_t(al, call->base.base.loc, passed_func->m_symtab,
                                                s2c(al, arg_name), nullptr, 0, ASR::intentType::Unspecified,
                                                nullptr, nullptr, ASR::storage_typeType::Default, arg_type,
                                                nullptr, ASR::abiType::BindC, ASR::accessType::Public,
                                                ASR::presenceType::Required, false, false, false, nullptr, false, false));
                                        passed_func->m_symtab->add_symbol(arg_name, arg_sym);
                                        new_args.push_back(al, ASRUtils::EXPR(
                                            ASR::make_Var_t(al, call->base.base.loc, arg_sym)));
                                    }
                                }
                                if (passed_func->n_args == 0 && new_args.size() > 0) {
                                    passed_func->m_args = new_args.p;
                                    passed_func->n_args = new_args.size();
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    return tu;
}

} // namespace LCompilers::LFortran
