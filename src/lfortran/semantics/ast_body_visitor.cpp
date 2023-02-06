#include <fstream>
#include <iostream>
#include <limits>
#include <map>
#include <memory>
#include <string>
#include <cmath>
#include <set>

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

public:
    ASR::asr_t *asr;
    bool from_block;
    std::set<std::string> labels;
    size_t starting_n_body = 0;
    AST::stmt_t **starting_m_body = nullptr;

    BodyVisitor(Allocator &al, ASR::asr_t *unit, diag::Diagnostics &diagnostics,
            CompilerOptions &compiler_options, std::map<uint64_t, std::map<std::string, ASR::ttype_t*>> &implicit_mapping)
        : CommonVisitor(al, nullptr, diagnostics, compiler_options, implicit_mapping),
        asr{unit}, from_block{false} {}

    void visit_Declaration(const AST::Declaration_t& x) {
        if( from_block ) {
            visit_DeclarationUtil(x);
        }
    }

    void visit_Block(const AST::Block_t &x) {
        from_block = true;
        SymbolTable *parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);

        for (size_t i=0; i<x.n_use; i++) {
            visit_unit_decl1(*x.m_use[i]);
        }
        for (size_t i=0; i<x.n_decl; i++) {
            visit_unit_decl2(*x.m_decl[i]);
        }

        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body);
        transform_stmts(body, x.n_body, x.m_body);
        std::string name = parent_scope->get_unique_name("block");
        ASR::asr_t* block = ASR::make_Block_t(al, x.base.base.loc,
                                              current_scope, s2c(al, name),
                                              body.p, body.size());
        current_scope = parent_scope;
        current_scope->add_symbol(name, ASR::down_cast<ASR::symbol_t>(block));
        tmp = ASR::make_BlockCall_t(al, x.base.base.loc,  -1,
                                    ASR::down_cast<ASR::symbol_t>(block));
        from_block = false;
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
            this->visit_stmt(*m_body[i]);
            if (tmp != nullptr) {
                ASR::stmt_t* tmp_stmt = ASRUtils::STMT(tmp);
                if (tmp_stmt->type == ASR::stmtType::SubroutineCall) {
                    ASR::stmt_t* impl_decl = create_implicit_deallocate_subrout_call(tmp_stmt);
                    if (impl_decl != nullptr) {
                        body.push_back(al, impl_decl);
                    }
                }
                body.push_back(al, tmp_stmt);
            }
            // To avoid last statement to be entered twice once we exit this node
            tmp = nullptr;
        }
        current_body = current_body_copy;
    }

    void visit_TranslationUnit(const AST::TranslationUnit_t &x) {
        ASR::TranslationUnit_t *unit = ASR::down_cast2<ASR::TranslationUnit_t>(asr);
        current_scope = unit->m_global_scope;
        Vec<ASR::asr_t*> items;
        items.reserve(al, x.n_items);
        for (size_t i=0; i<x.n_items; i++) {
            tmp = nullptr;
            visit_ast(*x.m_items[i]);
            if (tmp) {
                items.push_back(al, tmp);
            }
        }
        unit->m_items = items.p;
        unit->n_items = items.size();
    }

    void visit_Open(const AST::Open_t& x) {
        ASR::expr_t *a_newunit = nullptr, *a_filename = nullptr, *a_status = nullptr;
        if( x.n_args > 1 ) {
            throw SemanticError("Number of arguments cannot be more than 1 in Open statement.",
                                x.base.base.loc);
        }
        if( x.n_args == 1 ) {
            this->visit_expr(*x.m_args[0]);
            a_newunit = ASRUtils::EXPR(tmp);
        }
        for( std::uint32_t i = 0; i < x.n_kwargs; i++ ) {
            AST::keyword_t kwarg = x.m_kwargs[i];
            std::string m_arg_str(kwarg.m_arg);
            if( m_arg_str == std::string("newunit") ||
                m_arg_str == std::string("unit") ) {
                if( a_newunit != nullptr ) {
                    throw SemanticError(R"""(Duplicate value of `unit` found, `unit` has already been specified via argument or keyword arguments)""",
                                        x.base.base.loc);
                }
                this->visit_expr(*kwarg.m_value);
                a_newunit = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_newunit_type = ASRUtils::expr_type(a_newunit);
                if( ( m_arg_str == std::string("newunit") &&
                      a_newunit->type != ASR::exprType::Var ) ||
                    ( !ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(a_newunit_type))
                    ) ) {
                        throw SemanticError("`newunit`/`unit` must be a variable of type, Integer or IntegerPointer", x.base.base.loc);
                }
            } else if( m_arg_str == std::string("file") ) {
                if( a_filename != nullptr ) {
                    throw SemanticError(R"""(Duplicate value of `file` found, unit has already been specified via arguments or keyword arguments)""",
                                        x.base.base.loc);
                }
                this->visit_expr(*kwarg.m_value);
                a_filename = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_filename_type = ASRUtils::expr_type(a_filename);
                if (!ASR::is_a<ASR::Character_t>(*ASRUtils::type_get_past_pointer(a_filename_type))) {
                        throw SemanticError("`file` must be of type, Character or CharacterPointer", x.base.base.loc);
                }
            } else if( m_arg_str == std::string("status") ) {
                if( a_status != nullptr ) {
                    throw SemanticError(R"""(Duplicate value of `status` found, unit has already been specified via arguments or keyword arguments)""",
                                        x.base.base.loc);
                }
                this->visit_expr(*kwarg.m_value);
                a_status = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_status_type = ASRUtils::expr_type(a_status);
                if (!ASR::is_a<ASR::Character_t>(*ASRUtils::type_get_past_pointer(a_status_type))) {
                        throw SemanticError("`status` must be of type, Character or CharacterPointer", x.base.base.loc);
                }
            }
        }
        if( a_newunit == nullptr ) {
            throw SemanticError("`newunit` or `unit` must be specified either in argument or keyword arguments.",
                                x.base.base.loc);
        }
        tmp = ASR::make_FileOpen_t(al, x.base.base.loc, x.m_label,
                               a_newunit, a_filename, a_status);
    }

    void visit_Close(const AST::Close_t& x) {
        ASR::expr_t *a_unit = nullptr, *a_iostat = nullptr, *a_iomsg = nullptr;
        ASR::expr_t *a_err = nullptr, *a_status = nullptr;
        if( x.n_args > 1 ) {
            throw SemanticError("Number of arguments cannot be more than 1 in Close statement.",
                        x.base.base.loc);
        }
        if( x.n_args == 1 ) {
            this->visit_expr(*x.m_args[0]);
            a_unit = ASRUtils::EXPR(tmp);
        }
        for( std::uint32_t i = 0; i < x.n_kwargs; i++ ) {
            AST::keyword_t kwarg = x.m_kwargs[i];
            std::string m_arg_str(kwarg.m_arg);
            if( m_arg_str == std::string("unit") ) {
                if( a_unit != nullptr ) {
                    throw SemanticError(R"""(Duplicate value of `unit` found, `unit` has already been specified via argument or keyword arguments)""",
                                        x.base.base.loc);
                }
                this->visit_expr(*kwarg.m_value);
                a_unit = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_newunit_type = ASRUtils::expr_type(a_unit);
                if (!ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(a_newunit_type))) {
                        throw SemanticError("`unit` must be of type, Integer or IntegerPointer", x.base.base.loc);
                }
            } else if( m_arg_str == std::string("iostat") ) {
                if( a_iostat != nullptr ) {
                    throw SemanticError(R"""(Duplicate value of `iostat` found, unit has already been specified via arguments or keyword arguments)""",
                                        x.base.base.loc);
                }
                this->visit_expr(*kwarg.m_value);
                a_iostat = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_iostat_type = ASRUtils::expr_type(a_iostat);
                if( a_iostat->type != ASR::exprType::Var ||
                    (!ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(a_iostat_type))) ) {
                        throw SemanticError("`iostat` must be a variable of type, Integer or IntegerPointer", x.base.base.loc);
                }
            } else if( m_arg_str == std::string("iomsg") ) {
                if( a_iomsg != nullptr ) {
                    throw SemanticError(R"""(Duplicate value of `iomsg` found, unit has already been specified via arguments or keyword arguments)""",
                                        x.base.base.loc);
                }
                this->visit_expr(*kwarg.m_value);
                a_iomsg = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_iomsg_type = ASRUtils::expr_type(a_iomsg);
                if( a_iomsg->type != ASR::exprType::Var ||
                    (!ASR::is_a<ASR::Character_t>(*ASRUtils::type_get_past_pointer(a_iomsg_type))) ) {
                        throw SemanticError("`iomsg` must be of type, Character or CharacterPointer", x.base.base.loc);
                    }
            } else if( m_arg_str == std::string("status") ) {
                if( a_status != nullptr ) {
                    throw SemanticError(R"""(Duplicate value of `status` found, unit has already been specified via arguments or keyword arguments)""",
                                        x.base.base.loc);
                }
                this->visit_expr(*kwarg.m_value);
                a_status = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_status_type = ASRUtils::expr_type(a_status);
                if (!ASR::is_a<ASR::Character_t>(*ASRUtils::type_get_past_pointer(a_status_type))) {
                        throw SemanticError("`status` must be of type, Character or CharacterPointer", x.base.base.loc);
                }
            } else if( m_arg_str == std::string("err") ) {
                if( a_err != nullptr ) {
                    throw SemanticError(R"""(Duplicate value of `err` found, `err` has already been specified via arguments or keyword arguments)""",
                                        x.base.base.loc);
                }
                if( kwarg.m_value->type != AST::exprType::Num ) {
                    throw SemanticError("`err` must be a literal integer", x.base.base.loc);
                }
                this->visit_expr(*kwarg.m_value);
                a_err = ASRUtils::EXPR(tmp);
            }
        }
        if( a_unit == nullptr ) {
            throw SemanticError("`newunit` or `unit` must be specified either in argument or keyword arguments.",
                                x.base.base.loc);
        }
        tmp = ASR::make_FileClose_t(al, x.base.base.loc, x.m_label, a_unit, a_iostat, a_iomsg, a_err, a_status);
    }

    void visit_Backspace(const AST::Backspace_t& x) {
        ASR::expr_t *a_unit = nullptr, *a_iostat = nullptr;
        ASR::expr_t *a_err = nullptr;
        if( x.n_args > 1 ) {
            throw SemanticError("Number of arguments cannot be more than 1 in Backspace statement.",
                        x.base.base.loc);
        }
        if( x.n_args == 1 ) {
            this->visit_expr(*x.m_args[0]);
            a_unit = ASRUtils::EXPR(tmp);
        }
        for( std::uint32_t i = 0; i < x.n_kwargs; i++ ) {
            AST::keyword_t kwarg = x.m_kwargs[i];
            std::string m_arg_str(kwarg.m_arg);
            if( m_arg_str == std::string("unit") ) {
                if( a_unit != nullptr ) {
                    throw SemanticError(R"""(Duplicate value of `unit` found, `unit` "
                        " has already been specified via argument or keyword arguments)""",
                        x.base.base.loc);
                }
                this->visit_expr(*kwarg.m_value);
                a_unit = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_newunit_type = ASRUtils::expr_type(a_unit);
                if (!ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(a_newunit_type))) {
                        throw SemanticError("`unit` must be of type, Integer or IntegerPointer",
                        x.base.base.loc);
                }
            } else if( m_arg_str == std::string("iostat") ) {
                if( a_iostat != nullptr ) {
                    throw SemanticError(R"""(Duplicate value of `iostat` found, unit has "
                        " already been specified via arguments or keyword arguments)""",
                        x.base.base.loc);
                }
                this->visit_expr(*kwarg.m_value);
                a_iostat = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_iostat_type = ASRUtils::expr_type(a_iostat);
                if( a_iostat->type != ASR::exprType::Var ||
                    (!ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(a_iostat_type))) ) {
                        throw SemanticError("`iostat` must be a variable of type, Integer "
                            " or IntegerPointer", x.base.base.loc);
                }
            } else if( m_arg_str == std::string("err") ) {
                if( a_err != nullptr ) {
                    throw SemanticError(R"""(Duplicate value of `err` found, `err` has "
                        " already been specified via arguments or keyword arguments)""",
                        x.base.base.loc);
                }
                if( kwarg.m_value->type != AST::exprType::Num ) {
                    throw SemanticError("`err` must be a literal integer", x.base.base.loc);
                }
                this->visit_expr(*kwarg.m_value);
                a_err = ASRUtils::EXPR(tmp);
            }
        }
        if( a_unit == nullptr ) {
            throw SemanticError("`unit` must be specified either in argument or keyword arguments.",
                                x.base.base.loc);
        }
        tmp = ASR::make_FileBackspace_t(al, x.base.base.loc, x.m_label, a_unit, a_iostat, a_err);
    }

    void create_read_write_ASR_node(const AST::stmt_t& read_write_stmt, AST::stmtType _type) {
        int64_t m_label = -1;
        AST::argstar_t* m_args = nullptr; size_t n_args = 0;
        AST::kw_argstar_t* m_kwargs = nullptr; size_t n_kwargs = 0;
        AST::expr_t** m_values = nullptr; size_t n_values = 0;
        const Location& loc = read_write_stmt.base.loc;
        if( _type == AST::stmtType::Write ) {
            AST::Write_t* w = (AST::Write_t*)(&read_write_stmt);
            m_label = w->m_label;
            m_args = w->m_args; n_args = w->n_args;
            m_kwargs = w->m_kwargs; n_kwargs = w->n_kwargs;
            m_values = w->m_values; n_values = w->n_values;
        } else if( _type == AST::stmtType::Read ) {
            AST::Read_t* r = (AST::Read_t*)(&read_write_stmt);
            m_label = r->m_label;
            m_args = r->m_args; n_args = r->n_args;
            m_kwargs = r->m_kwargs; n_kwargs = r->n_kwargs;
            m_values = r->m_values; n_values = r->n_values;
        }

        ASR::expr_t *a_unit, *a_fmt, *a_iomsg, *a_iostat, *a_id, *a_separator, *a_end;
        a_unit = a_fmt = a_iomsg = a_iostat = a_id = a_separator = a_end = nullptr;
        Vec<ASR::expr_t*> a_values_vec;
        a_values_vec.reserve(al, n_values);

        if( n_args > 2 ) {
            throw SemanticError("Number of arguments cannot be more than 2 in Read/Write statement.",
                                loc);
        }
        std::vector<ASR::expr_t**> args = {&a_unit, &a_fmt};
        for( std::uint32_t i = 0; i < n_args; i++ ) {
            if( m_args[i].m_value != nullptr ) {
                this->visit_expr(*m_args[i].m_value);
                *args[i] = ASRUtils::EXPR(tmp);
            }
        }
        for( std::uint32_t i = 0; i < n_kwargs; i++ ) {
            AST::kw_argstar_t kwarg = m_kwargs[i];
            std::string m_arg_str(kwarg.m_arg);
            if( m_arg_str == std::string("unit") ) {
                if( a_unit != nullptr ) {
                    throw SemanticError(R"""(Duplicate value of `unit` found, `unit` has already been specified via argument or keyword arguments)""",
                                        loc);
                }
                if (kwarg.m_value != nullptr) {
                    this->visit_expr(*kwarg.m_value);
                    a_unit = ASRUtils::EXPR(tmp);
                    ASR::ttype_t* a_unit_type = ASRUtils::expr_type(a_unit);
                    if  (!ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(a_unit_type))) {
                            throw SemanticError("`unit` must be of type, Integer", loc);
                    }
                }
            } else if( m_arg_str == std::string("iostat") ) {
                if( a_iostat != nullptr ) {
                    throw SemanticError(R"""(Duplicate value of `iostat` found, unit has already been specified via arguments or keyword arguments)""",
                                        loc);
                }
                this->visit_expr(*kwarg.m_value);
                a_iostat = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_iostat_type = ASRUtils::expr_type(a_iostat);
                if( a_iostat->type != ASR::exprType::Var ||
                    (!ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(a_iostat_type))) ) {
                        throw SemanticError("`iostat` must be of type, Integer", loc);
                }
            } else if( m_arg_str == std::string("iomsg") ) {
                if( a_iomsg != nullptr ) {
                    throw SemanticError(R"""(Duplicate value of `iomsg` found, it has already been specified via arguments or keyword arguments)""",
                                        loc);
                }
                this->visit_expr(*kwarg.m_value);
                a_iomsg = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_iomsg_type = ASRUtils::expr_type(a_iomsg);
                if( a_iomsg->type != ASR::exprType::Var ||
                   (!ASR::is_a<ASR::Character_t>(*ASRUtils::type_get_past_pointer(a_iomsg_type))) ) {
                        throw SemanticError("`iomsg` must be of type, Character", loc);
                    }
            } else if( m_arg_str == std::string("id") ) {
                if( a_id != nullptr ) {
                    throw SemanticError(R"""(Duplicate value of `id` found, it has already been specified via arguments or keyword arguments)""",
                                        loc);
                }
                this->visit_expr(*kwarg.m_value);
                a_id = ASRUtils::EXPR(tmp);
                ASR::ttype_t* a_status_type = ASRUtils::expr_type(a_id);
                if (!ASR::is_a<ASR::Character_t>(*ASRUtils::type_get_past_pointer(a_status_type))) {
                        throw SemanticError("`status` must be of type Character", loc);
                }
            } else if( m_arg_str == std::string("fmt") ) {
                if( a_fmt != nullptr ) {
                    throw SemanticError(R"""(Duplicate value of `fmt` found, it has already been specified via arguments or keyword arguments)""",
                                        loc);
                }
                if (kwarg.m_value != nullptr) {
                    tmp = nullptr;
                    this->visit_expr(*kwarg.m_value);
                    if (tmp == nullptr) {
                        throw SemanticError("?", loc);
                    }
                    a_fmt = ASRUtils::EXPR(tmp);
                    ASR::ttype_t* a_fmt_type = ASRUtils::expr_type(a_fmt);
                    if (!ASR::is_a<ASR::Character_t>(*ASRUtils::type_get_past_pointer(a_fmt_type))) {
                            throw SemanticError("`fmt` must be of type Character", loc);
                    }
                }
            }
        }
        for( std::uint32_t i = 0; i < n_values; i++ ) {
            this->visit_expr(*m_values[i]);
            a_values_vec.push_back(al, ASRUtils::EXPR(tmp));
        }
        if( _type == AST::stmtType::Write ) {
            tmp = ASR::make_FileWrite_t(al, loc, m_label, a_unit, a_fmt,
                                    a_iomsg, a_iostat, a_id, a_values_vec.p, n_values, a_separator, a_end);
        } else if( _type == AST::stmtType::Read ) {
            tmp = ASR::make_FileRead_t(al, loc, m_label, a_unit, a_fmt,
                                   a_iomsg, a_iostat, a_id, a_values_vec.p, n_values);
        }
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
            throw SemanticError("Incorrect number of arguments passed to " + stmt_name + "."
                                " It accepts a total of 3 arguments namely unit, iostat and err.",
                                x.base.base.loc);
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
                std::string m_arg_string = to_lower(std::string(x.m_kwargs[i].m_arg));
                if( args[argname2idx[m_arg_string]] ) {
                    throw SemanticError(m_arg_string + " has already been specified.", x.base.base.loc);
                }
                visit_expr(*x.m_kwargs[i].m_value);
                args[argname2idx[m_arg_string]] = ASRUtils::EXPR(tmp);
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

    void visit_Instantiate(const AST::Instantiate_t &x){
        is_instantiate = true;
        std::string template_name = x.m_name;
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, 0);

        // Check if the template exists
        if (template_arg_map.find(template_name) == template_arg_map.end()) {
            throw SemanticError("Use of unspecified template", x.base.base.loc);
        }

        std::map<int, std::string> current_template_arg = template_arg_map[template_name];
        std::map<std::string, ASR::asr_t*> current_template_asr = template_asr_map[template_name];

        // Check if number of type parameters match
        if (current_template_arg.size() != x.n_args) {
            throw SemanticError("Number of template arguments don't match", x.base.base.loc);
        }

        std::map<std::string, ASR::ttype_t*> subs;
        std::map<std::string, ASR::symbol_t*> restriction_subs;

        for (size_t i = 0; i < x.n_args; i++) {
            std::string arg = to_lower(x.m_args[i]);
            ASR::asr_t *template_param = current_template_asr[current_template_arg[i]];
            // Handle type parameters
            if (ASR::is_a<ASR::ttype_t>(*template_param)) {
                ASR::TypeParameter_t *type_param = ASR::down_cast2<ASR::TypeParameter_t>(template_param);
                ASR::ttype_t *type_arg;
                if (arg.compare("real") == 0) {
                    type_arg = ASRUtils::TYPE(ASR::make_Real_t(al, x.base.base.loc, 4, nullptr, 0));
                } else if (arg.compare("integer") == 0) {
                    type_arg = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, 4, nullptr, 0));
                } else {
                    throw SemanticError(
                        "The type " + arg + " is not yet handled for generic instantiation",
                        x.base.base.loc);
                }
                subs[type_param->m_param] = type_arg;
            // Handle function restrictions
            } else if (ASR::is_a<ASR::symbol_t>(*template_param)) {
                ASR::Function_t *restriction = ASR::down_cast2<ASR::Function_t>(template_param);
                ASR::symbol_t *f_arg = current_scope->resolve_symbol(arg);
                if (!f_arg) {
                    throw SemanticError("The function argument " + arg + " is not found",
                        x.base.base.loc);
                }
                ASR::symbol_t *f_arg2 = ASRUtils::symbol_get_past_external(f_arg);
                if (!ASR::is_a<ASR::Function_t>(*f_arg2)) {
                    throw SemanticError(
                        "The argument for " + current_template_arg[i] + " must be a function",
                        x.base.base.loc);
                }
                check_restriction(subs, restriction_subs, restriction, f_arg, x.base.base.loc);
            }
        }

        for (size_t i = 0; i < x.n_symbols; i++){
            AST::UseSymbol_t* use_symbol = AST::down_cast<AST::UseSymbol_t>(x.m_symbols[i]);
            std::string generic_name = to_lower(use_symbol->m_remote_sym);
            ASR::symbol_t* s = resolve_symbol(x.base.base.loc, generic_name);
            ASR::symbol_t* s2 = ASRUtils::symbol_get_past_external(s);
            // TODO: Improve error message
            if (!ASR::is_a<ASR::Function_t>(*s2)) {
              throw SemanticError("Only functions can be instantiated", x.base.base.loc);
            }
            std::string new_f_name = to_lower(use_symbol->m_local_rename);
            pass_instantiate_generic_function(al, subs, restriction_subs, current_scope,
                new_f_name, s);
            current_function_dependencies.erase(std::string(ASRUtils::symbol_name(s)));
            current_function_dependencies.insert(new_f_name);
        }

        is_instantiate = false;
    }

    void check_restriction(std::map<std::string, ASR::ttype_t*> subs,
            std::map<std::string, ASR::symbol_t*>& restriction_subs,
            ASR::Function_t* restriction, ASR::symbol_t *sym_arg, const Location& loc) {
        std::string restriction_name = restriction->m_name;
        ASR::Function_t *arg = ASR::down_cast<ASR::Function_t>(ASRUtils::symbol_get_past_external(sym_arg));
        std::string arg_name = arg->m_name;
        if (restriction->n_args != arg->n_args) {
            // TODO: use diagnostics showing both the restriction and the argument
            std::string msg = "The argument " + arg_name;
                + " has different number of arguments with the restriction "
                + restriction_name;
            throw SemanticError(msg, loc);
        }
        for (size_t i = 0; i < restriction->n_args; i++) {
            ASR::ttype_t *restriction_parameter = ASRUtils::expr_type(restriction->m_args[i]);
            ASR::ttype_t *arg_parameter = ASRUtils::expr_type(arg->m_args[i]);
            if (ASR::is_a<ASR::TypeParameter_t>(*restriction_parameter)) {
                ASR::TypeParameter_t *restriction_tp
                    = ASR::down_cast<ASR::TypeParameter_t>(restriction_parameter);
                if (!ASRUtils::check_equal_type(subs[restriction_tp->m_param],
                                                arg_parameter)) {
                    throw SemanticError("Restriction type mismatch with provided "
                        "type arguments", loc);
                }
            }
        }
        if (restriction->m_return_var) {
            if (!arg->m_return_var) {
                std::string msg = "The restriction argument " + arg_name
                    + " should have a return value";
                throw SemanticError(msg, loc);
            }
            ASR::ttype_t *restriction_return = ASRUtils::expr_type(restriction->m_return_var);
            ASR::ttype_t *arg_return = ASRUtils::expr_type(arg->m_return_var);
            if (ASR::is_a<ASR::TypeParameter_t>(*restriction_return)) {
                ASR::TypeParameter_t *return_tp
                    = ASR::down_cast<ASR::TypeParameter_t>(restriction_return);
                if (!ASRUtils::check_equal_type(subs[return_tp->m_param], arg_return)) {
                    throw SemanticError("Restriction type mismatch with provided "
                        "type arguments", loc);
                }
            }
        } else {
            if (arg->m_return_var) {
                std::string msg = "The restriction argument " + arg_name
                    + " should not have a return value";
                throw SemanticError(msg, loc);
            }
        }
        restriction_subs[restriction_name] = sym_arg;
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
            {"carriagecontrol", 28}, {"iolength", 29}};
        std::vector<ASR::expr_t*> args;
        std::string node_name = "Inquire";
        fill_args_for_rewind_inquire_flush(x, 29, args, 30, argname2idx, node_name);
        ASR::expr_t *unit = args[0], *file = args[1], *iostat = args[2], *err = args[3];
        ASR::expr_t *exist = args[4], *opened = args[5], *number = args[6], *named = args[7];
        ASR::expr_t *name = args[8], *access = args[9], *sequential = args[10], *direct = args[11];
        ASR::expr_t *form = args[12], *formatted = args[13], *unformatted = args[14], *recl = args[15];
        ASR::expr_t *nextrec = args[16], *blank = args[17], *position = args[18], *action = args[19];
        ASR::expr_t *read = args[20], *write = args[21], *readwrite = args[22], *delim = args[23];
        ASR::expr_t *pad = args[24], *flen = args[25], *blocksize = args[26], *convert = args[27];
        ASR::expr_t *carriagecontrol = args[28], *iolength = args[29];
        bool is_iolength_present = iolength != nullptr;
        for( size_t i = 0; i < args.size() - 1; i++ ) {
            if( is_iolength_present && args[i] ) {
                throw SemanticError("No argument should be specified when iolength is already present.",
                                    x.base.base.loc);
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
                                  carriagecontrol, iolength);
    }

    void visit_Flush(const AST::Flush_t& x) {
        std::map<std::string, size_t> argname2idx = {{"unit", 0}, {"err", 1}, {"iomsg", 2}, {"iostat", 3}};
        std::vector<ASR::expr_t*> args;
        std::string node_name = "Flush";
        fill_args_for_rewind_inquire_flush(x, 4, args, 4, argname2idx, node_name);
        if( !args[0] ) {
            throw SemanticError("unit must be present in flush statement arguments", x.base.base.loc);
        }
        ASR::expr_t *unit = args[0], *err = args[1], *iomsg = args[2], *iostat = args[3];
        tmp = ASR::make_Flush_t(al, x.base.base.loc, x.m_label, unit, err, iomsg, iostat);
    }

    void visit_Associate(const AST::Associate_t& x) {
        this->visit_expr(*(x.m_target));
        ASR::expr_t* target = ASRUtils::EXPR(tmp);
        ASR::ttype_t* target_type = ASRUtils::expr_type(target);
        current_variable_type_ = target_type;
        this->visit_expr(*(x.m_value));
        ASR::expr_t* value = ASRUtils::EXPR(tmp);
        ASR::ttype_t* value_type = ASRUtils::expr_type(value);
        bool is_target_pointer = ASRUtils::is_pointer(target_type);
        if ( !is_target_pointer ) {
            throw SemanticError("Only a pointer variable can be associated with another variable.", x.base.base.loc);
        }
        if( ASRUtils::types_equal(target_type, value_type) ) {
            tmp = ASR::make_Associate_t(al, x.base.base.loc, target, value);
        }
    }

    void visit_AssociateBlock(const AST::AssociateBlock_t& x) {
        SymbolTable* new_scope = al.make_new<SymbolTable>(current_scope);
        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body);
        for( size_t i = 0; i < x.n_syms; i++ ) {
            this->visit_expr(*x.m_syms[i].m_initializer);
            ASR::expr_t* tmp_expr = ASRUtils::EXPR(tmp);
            ASR::ttype_t* tmp_type = ASRUtils::expr_type(tmp_expr);
            ASR::storage_typeType tmp_storage = ASR::storage_typeType::Default;
            bool create_associate_stmt = false;
            if( ASR::is_a<ASR::Var_t>(*tmp_expr) ) {
                ASR::Var_t* tmp_var = ASR::down_cast<ASR::Var_t>(tmp_expr);
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Variable_t>(*(tmp_var->m_v)));
                ASR::Variable_t* variable = ASR::down_cast<ASR::Variable_t>(tmp_var->m_v);
                tmp_storage = variable->m_storage;
                tmp_type = ASRUtils::TYPE(ASR::make_Pointer_t(al, tmp_type->base.loc, variable->m_type));
                create_associate_stmt = true;
            }
            std::string name = to_lower(x.m_syms[i].m_name);
            char *name_c = s2c(al, name);
            Vec<char*> variable_dependencies_vec;
            variable_dependencies_vec.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, tmp_type);
            ASR::asr_t *v = ASR::make_Variable_t(al, x.base.base.loc, new_scope,
                                                 name_c, variable_dependencies_vec.p, variable_dependencies_vec.size(),
                                                 ASR::intentType::Local, nullptr, nullptr, tmp_storage, tmp_type,
                                                 ASR::abiType::Source, ASR::accessType::Private, ASR::presenceType::Required,
                                                 false);
            new_scope->add_symbol(name, ASR::down_cast<ASR::symbol_t>(v));
            ASR::expr_t* target_var = ASRUtils::EXPR(ASR::make_Var_t(al, v->loc, ASR::down_cast<ASR::symbol_t>(v)));
            if( create_associate_stmt ) {
                ASR::stmt_t* associate_stmt = ASRUtils::STMT(ASR::make_Associate_t(al, tmp_expr->base.loc, target_var, tmp_expr));
                body.push_back(al, associate_stmt);
            } else {
                ASR::stmt_t* assign_stmt = ASRUtils::STMT(ASR::make_Assignment_t(al, tmp_expr->base.loc, target_var, tmp_expr, nullptr));
                body.push_back(al, assign_stmt);
            }
        }
        SymbolTable* current_scope_copy = current_scope;
        current_scope = new_scope;
        transform_stmts(body, x.n_body, x.m_body);
        current_scope = current_scope_copy;
        std::string name = current_scope->get_unique_name("associate_block");
        ASR::asr_t* associate_block = ASR::make_AssociateBlock_t(al, x.base.base.loc,
                                                                 new_scope, s2c(al, name),
                                                                 body.p, body.size());
        current_scope->add_symbol(name, ASR::down_cast<ASR::symbol_t>(associate_block));
        tmp = ASR::make_AssociateBlockCall_t(al, x.base.base.loc, ASR::down_cast<ASR::symbol_t>(associate_block));
    }

    void visit_Allocate(const AST::Allocate_t& x) {
        Vec<ASR::alloc_arg_t> alloc_args_vec;
        alloc_args_vec.reserve(al, x.n_args);
        ASR::ttype_t *int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc,
                                                            4, nullptr, 0));
        ASR::expr_t* const_1 = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, 1, int32_type));
        for( size_t i = 0; i < x.n_args; i++ ) {
            ASR::alloc_arg_t new_arg;
            new_arg.loc = x.base.base.loc;
            if( x.m_args[i].m_end && !x.m_args[i].m_start && !x.m_args[i].m_step ) {
                this->visit_expr(*(x.m_args[i].m_end));
            } else if( x.m_args[i].m_start && !x.m_args[i].m_end && x.m_args[i].m_step ) {
                this->visit_expr(*(x.m_args[i].m_step));
            }
            // Assume that tmp is an `ArraySection` or `ArrayItem`
            ASR::expr_t* tmp_stmt = ASRUtils::EXPR(tmp);
            if( ASR::is_a<ASR::ArraySection_t>(*tmp_stmt) ) {
                ASR::ArraySection_t* array_ref = ASR::down_cast<ASR::ArraySection_t>(tmp_stmt);
                new_arg.m_a = array_ref->m_v;
                Vec<ASR::dimension_t> dims_vec;
                dims_vec.reserve(al, array_ref->n_args);
                for( size_t j = 0; j < array_ref->n_args; j++ ) {
                    ASR::dimension_t new_dim;
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
                    ASR::dimension_t new_dim;
                    new_dim.loc = array_ref->m_args[j].loc;
                    new_dim.m_start = const_1;
                    new_dim.m_length = ASRUtils::compute_length_from_start_end(al, new_dim.m_start,
                                            array_ref->m_args[j].m_right);
                    dims_vec.push_back(al, new_dim);
                }
                new_arg.m_dims = dims_vec.p;
                new_arg.n_dims = dims_vec.size();
                alloc_args_vec.push_back(al, new_arg);
            } else if( ASR::is_a<ASR::Var_t>(*tmp_stmt) ) {
                new_arg.m_a = tmp_stmt;
                new_arg.m_dims = nullptr;
                new_arg.n_dims = 0;
                alloc_args_vec.push_back(al, new_arg);
            }
        }

        bool cond = x.n_keywords == 0;
        bool stat_cond = false, errmsg_cond = false, source_cond = false;
        ASR::expr_t *stat = nullptr, *errmsg = nullptr, *source = nullptr;
        if( x.n_keywords >= 1 ) {
            stat_cond = !stat_cond && (to_lower(x.m_keywords[0].m_arg) == "stat");
            errmsg_cond = !errmsg_cond && (to_lower(x.m_keywords[0].m_arg) == "errmsg");
            source_cond = !source_cond && (to_lower(x.m_keywords[0].m_arg) == "source");
            cond = cond || (stat_cond || errmsg_cond || source_cond);
            if( stat_cond ) {
                this->visit_expr(*(x.m_keywords[0].m_value));
                stat = ASRUtils::EXPR(tmp);
            } else if( errmsg_cond ) {
                this->visit_expr(*(x.m_keywords[0].m_value));
                errmsg = ASRUtils::EXPR(tmp);
            } else if( source_cond ) {
                this->visit_expr(*(x.m_keywords[0].m_value));
                source = ASRUtils::EXPR(tmp);
            }
        }

        if( x.n_keywords >= 2 ) {
            stat_cond = !stat_cond && (to_lower(x.m_keywords[1].m_arg) == "stat");
            errmsg_cond = !errmsg_cond && (to_lower(x.m_keywords[1].m_arg) == "errmsg");
            source_cond = !source_cond && (to_lower(x.m_keywords[1].m_arg) == "source");
            cond = cond && (stat_cond || errmsg_cond || source_cond);
            if( stat_cond ) {
                this->visit_expr(*(x.m_keywords[1].m_value));
                stat = ASRUtils::EXPR(tmp);
            } else if( errmsg_cond ) {
                this->visit_expr(*(x.m_keywords[1].m_value));
                errmsg = ASRUtils::EXPR(tmp);
            } else if( source_cond ) {
                this->visit_expr(*(x.m_keywords[1].m_value));
                source = ASRUtils::EXPR(tmp);
            }
        }

        if( x.n_keywords >= 3 ) {
            stat_cond = !stat_cond && (to_lower(x.m_keywords[2].m_arg) == "stat");
            errmsg_cond = !errmsg_cond && (to_lower(x.m_keywords[2].m_arg) == "errmsg");
            source_cond = !source_cond && (to_lower(x.m_keywords[2].m_arg) == "source");
            cond = cond && (stat_cond || errmsg_cond || source_cond);
            if( stat_cond ) {
                this->visit_expr(*(x.m_keywords[2].m_value));
                stat = ASRUtils::EXPR(tmp);
            } else if( errmsg_cond ) {
                this->visit_expr(*(x.m_keywords[2].m_value));
                errmsg = ASRUtils::EXPR(tmp);
            } else if( source_cond ) {
                this->visit_expr(*(x.m_keywords[2].m_value));
                source = ASRUtils::EXPR(tmp);
            }
        }


        if( !cond ) {
            throw SemanticError("`allocate` statement only "
                                "accepts three keyword arguments,"
                                "`stat`, `errmsg` and `source`",
                                x.base.base.loc);
        }

        tmp = ASR::make_Allocate_t(al, x.base.base.loc,
                                    alloc_args_vec.p, alloc_args_vec.size(),
                                    stat, errmsg, source);
    }

// If there are allocatable variables in the local scope it inserts an ImplicitDeallocate node
// with their list. The ImplicitDeallocate node will deallocate them if they are allocated,
// otherwise does nothing.
    ASR::stmt_t* create_implicit_deallocate(const Location& loc) {
        Vec<ASR::symbol_t*> del_syms;
        del_syms.reserve(al, 0);
        for( auto& item: current_scope->get_scope() ) {
            if( item.second->type == ASR::symbolType::Variable ) {
                const ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(item.second);
                ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(sym);
                if( var->m_storage == ASR::storage_typeType::Allocatable &&
                    var->m_intent == ASR::intentType::Local ) {
                    del_syms.push_back(al, item.second);
                }
            }
        }
        if( del_syms.size() == 0 ) {
            return nullptr;
        }
        return ASRUtils::STMT(ASR::make_ImplicitDeallocate_t(al, loc,
                    del_syms.p, del_syms.size()));
    }

    inline void check_for_deallocation(ASR::symbol_t* tmp_sym, const Location& loc) {
        tmp_sym = ASRUtils::symbol_get_past_external(tmp_sym);
        if( !ASR::is_a<ASR::Variable_t>(*tmp_sym) ) {
            throw SemanticError("Only an allocatable variable symbol "
                                "can be deallocated.", loc);
        }

        ASR::Variable_t* tmp_v = ASR::down_cast<ASR::Variable_t>(tmp_sym);
        if( tmp_v->m_storage != ASR::storage_typeType::Allocatable &&
            tmp_v->m_storage != ASR::storage_typeType::Save ) {
            // If it is not allocatable, it can also be a pointer
            if (ASR::is_a<ASR::Pointer_t>(*tmp_v->m_type)) {
                // OK
            } else {
                throw SemanticError("Only an allocatable or a pointer variable "
                                    "can be deallocated.", loc);
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
                throw SemanticError("Cannot deallocate variables in expression " +
                                    std::to_string(tmp_expr->type),
                                    tmp_expr->base.loc);
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
        switch(x.type) {
            case AST::case_stmtType::CaseStmt: {
                AST::CaseStmt_t* Case_Stmt = (AST::CaseStmt_t*)(&(x.base));
                if (Case_Stmt->n_test == 0) {
                    throw SemanticError("Case statement must have at least one condition",
                                        x.base.loc);
                }
                if (AST::is_a<AST::CaseCondExpr_t>(*(Case_Stmt->m_test[0]))) {
                    // For now we only support a list of expressions
                    Vec<ASR::expr_t*> a_test_vec;
                    a_test_vec.reserve(al, Case_Stmt->n_test);
                    for( std::uint32_t i = 0; i < Case_Stmt->n_test; i++ ) {
                        if (!AST::is_a<AST::CaseCondExpr_t>(*(Case_Stmt->m_test[i]))) {
                            throw SemanticError("Not implemented yet: range expression not in first position",
                                                x.base.loc);
                        }
                        AST::CaseCondExpr_t *condexpr
                            = AST::down_cast<AST::CaseCondExpr_t>(Case_Stmt->m_test[i]);
                        this->visit_expr(*condexpr->m_cond);
                        a_test_vec.push_back(al, ASRUtils::EXPR(tmp));
                    }
                    Vec<ASR::stmt_t*> case_body_vec;
                    case_body_vec.reserve(al, Case_Stmt->n_body);
                    transform_stmts(case_body_vec, Case_Stmt->n_body, Case_Stmt->m_body);
                    tmp = ASR::make_CaseStmt_t(al, x.base.loc, a_test_vec.p, a_test_vec.size(),
                                        case_body_vec.p, case_body_vec.size());
                    break;
                } else {
                    // For now we only support exactly one range
                    if (Case_Stmt->n_test != 1) {
                        throw SemanticError("Not implemented: more than one range condition",
                                            x.base.loc);
                    }
                    AST::CaseCondRange_t *condrange
                        = AST::down_cast<AST::CaseCondRange_t>(Case_Stmt->m_test[0]);
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
                    tmp = ASR::make_CaseStmt_Range_t(al, x.base.loc, m_start, m_end,
                                        case_body_vec.p, case_body_vec.size());
                    break;
                }
            }
            default: {
                throw SemanticError(R"""(Case statement can only support a valid expression
                                    that reduces to a constant or range defined by : separator)""",
                                    x.base.loc);
            }
        }
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
                    throw SemanticError("Default case present more than once",
                        x.base.base.loc);
                }
                AST::CaseStmt_Default_t *d =
                        AST::down_cast<AST::CaseStmt_Default_t>(body);
                transform_stmts(def_body, d->n_body, d->m_body);
            } else {
                this->visit_case_stmt(*body);
                a_body_vec.push_back(al, ASR::down_cast<ASR::case_stmt_t>(tmp));
            }
        }
        tmp = ASR::make_Select_t(al, x.base.base.loc, a_test, a_body_vec.p,
                           a_body_vec.size(), def_body.p, def_body.size());
    }

    void visit_SelectType(const AST::SelectType_t& x) {
        if( !x.m_selector ) {
            throw SemanticError("Selector expression is missing in select type statement.",
                                x.base.base.loc);
        }
        visit_expr(*x.m_selector);
        ASR::expr_t* m_selector = ASRUtils::EXPR(tmp);
        Vec<ASR::stmt_t*> select_type_default;
        select_type_default.reserve(al, 1);
        Vec<ASR::type_stmt_t*> select_type_body;
        select_type_body.reserve(al, x.n_body);
        ASR::Variable_t* selector_variable = nullptr;
        ASR::ttype_t* selector_variable_type = nullptr;
        char** selector_variable_dependencies = nullptr;
        size_t selector_variable_n_dependencies = 0;
        if( ASR::is_a<ASR::Var_t>(*m_selector) ) {
            ASR::symbol_t* selector_sym = ASR::down_cast<ASR::Var_t>(m_selector)->m_v;
            LCOMPILERS_ASSERT(ASR::is_a<ASR::Variable_t>(*selector_sym));
            selector_variable = ASR::down_cast<ASR::Variable_t>(selector_sym);
            selector_variable_type = selector_variable->m_type;
            selector_variable_dependencies = selector_variable->m_dependencies;
            selector_variable_n_dependencies = selector_variable->n_dependencies;
        }
        for( size_t i = 0; i < x.n_body; i++ ) {
            SymbolTable* parent_scope = current_scope;
            current_scope = al.make_new<SymbolTable>(parent_scope);
            ASR::Variable_t* assoc_variable = nullptr;
            if( x.m_assoc_name ) {
                ASR::symbol_t* assoc_sym = ASR::down_cast<ASR::symbol_t>(ASR::make_Variable_t(
                    al, x.base.base.loc, current_scope, x.m_assoc_name,
                    nullptr, 0, ASR::intentType::Local, m_selector, nullptr,
                    ASR::storage_typeType::Default, nullptr, ASR::abiType::Source,
                    ASR::accessType::Public, ASR::presenceType::Required, false));
                current_scope->add_symbol(std::string(x.m_assoc_name), assoc_sym);
                assoc_variable = ASR::down_cast<ASR::Variable_t>(assoc_sym);
            } else if( selector_variable ) {
                assoc_variable = selector_variable;
            }
            switch( x.m_body[i]->type ) {
                case AST::type_stmtType::ClassStmt: {
                    AST::ClassStmt_t* class_stmt = AST::down_cast<AST::ClassStmt_t>(x.m_body[i]);
                    ASR::symbol_t* sym = current_scope->resolve_symbol(std::string(class_stmt->m_id));
                    if( assoc_variable ) {
                        ASR::ttype_t* selector_type = nullptr;
                        ASR::symbol_t* sym_underlying = ASRUtils::symbol_get_past_external(sym);
                        if( ASR::is_a<ASR::StructType_t>(*sym_underlying) ) {
                            selector_type = ASRUtils::TYPE(ASR::make_Struct_t(al, sym->base.loc, sym, nullptr, 0));
                        } else if( ASR::is_a<ASR::ClassType_t>(*sym_underlying) ) {
                            selector_type = ASRUtils::TYPE(ASR::make_Class_t(al, sym->base.loc, sym, nullptr, 0));
                        } else {
                            throw SemanticError("Only class and derived type in select type test expressions.",
                                                class_stmt->base.base.loc);
                        }
                        Vec<char*> assoc_deps;
                        assoc_deps.reserve(al, 1);
                        ASRUtils::collect_variable_dependencies(al, assoc_deps, selector_type, m_selector);
                        assoc_variable->m_dependencies = assoc_deps.p;
                        assoc_variable->n_dependencies = assoc_deps.size();
                        assoc_variable->m_type = selector_type;
                    }
                    Vec<ASR::stmt_t*> class_stmt_body;
                    class_stmt_body.reserve(al, class_stmt->n_body);
                    transform_stmts(class_stmt_body, class_stmt->n_body, class_stmt->m_body);
                    std::string block_name = parent_scope->get_unique_name("~select_type_block_");
                    ASR::symbol_t* block_sym = ASR::down_cast<ASR::symbol_t>(ASR::make_Block_t(
                                                    al, class_stmt->base.base.loc,
                                                    current_scope, s2c(al, block_name), class_stmt_body.p,
                                                    class_stmt_body.size()));
                    parent_scope->add_symbol(block_name, block_sym);
                    Vec<ASR::stmt_t*> block_call_stmt;
                    block_call_stmt.reserve(al, 1);
                    block_call_stmt.push_back(al, ASRUtils::STMT(ASR::make_BlockCall_t(al, class_stmt->base.base.loc, -1, block_sym)));
                    select_type_body.push_back(al, ASR::down_cast<ASR::type_stmt_t>(ASR::make_TypeStmtName_t(al,
                        class_stmt->base.base.loc, sym, block_call_stmt.p, block_call_stmt.size())));
                    break;
                }
                case AST::type_stmtType::TypeStmtName: {
                    AST::TypeStmtName_t* type_stmt_name = AST::down_cast<AST::TypeStmtName_t>(x.m_body[i]);
                    ASR::symbol_t* sym = current_scope->resolve_symbol(std::string(type_stmt_name->m_name));
                    if( assoc_variable ) {
                        ASR::ttype_t* selector_type = nullptr;
                        ASR::symbol_t* sym_underlying = ASRUtils::symbol_get_past_external(sym);
                        if( ASR::is_a<ASR::StructType_t>(*sym_underlying) ) {
                            selector_type = ASRUtils::TYPE(ASR::make_Struct_t(al, sym->base.loc, sym, nullptr, 0));
                        } else if( ASR::is_a<ASR::ClassType_t>(*sym_underlying) ) {
                            selector_type = ASRUtils::TYPE(ASR::make_Class_t(al, sym->base.loc, sym, nullptr, 0));
                        } else {
                            throw SemanticError("Only class and derived type in select type test expressions.",
                                                type_stmt_name->base.base.loc);
                        }
                        Vec<char*> assoc_deps;
                        assoc_deps.reserve(al, 1);
                        ASRUtils::collect_variable_dependencies(al, assoc_deps, selector_type, m_selector);
                        assoc_variable->m_dependencies = assoc_deps.p;
                        assoc_variable->n_dependencies = assoc_deps.size();
                        assoc_variable->m_type = selector_type;
                    }
                    Vec<ASR::stmt_t*> type_stmt_name_body;
                    type_stmt_name_body.reserve(al, type_stmt_name->n_body);
                    transform_stmts(type_stmt_name_body, type_stmt_name->n_body, type_stmt_name->m_body);
                    std::string block_name = parent_scope->get_unique_name("~select_type_block_");
                    ASR::symbol_t* block_sym = ASR::down_cast<ASR::symbol_t>(ASR::make_Block_t(
                                                    al, type_stmt_name->base.base.loc,
                                                    current_scope, s2c(al, block_name), type_stmt_name_body.p,
                                                    type_stmt_name_body.size()));
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
                    if( assoc_variable ) {
                        Vec<ASR::dimension_t> m_dims;
                        m_dims.reserve(al, 1);
                        std::string assoc_variable_name = std::string(assoc_variable->m_name);
                        selector_type = determine_type(type_stmt_type->base.base.loc,
                                                       assoc_variable_name,
                                                       type_stmt_type->m_vartype, false, m_dims);
                        Vec<char*> assoc_deps;
                        assoc_deps.reserve(al, 1);
                        ASRUtils::collect_variable_dependencies(al, assoc_deps, selector_type, m_selector);
                        assoc_variable->m_dependencies = assoc_deps.p;
                        assoc_variable->n_dependencies = assoc_deps.size();
                        assoc_variable->m_type = selector_type;
                    }
                    Vec<ASR::stmt_t*> type_stmt_type_body;
                    type_stmt_type_body.reserve(al, type_stmt_type->n_body);
                    transform_stmts(type_stmt_type_body, type_stmt_type->n_body, type_stmt_type->m_body);
                    std::string block_name = parent_scope->get_unique_name("~select_type_block_");
                    ASR::symbol_t* block_sym = ASR::down_cast<ASR::symbol_t>(ASR::make_Block_t(
                                                    al, type_stmt_type->base.base.loc,
                                                    current_scope, s2c(al, block_name), type_stmt_type_body.p,
                                                    type_stmt_type_body.size()));
                    parent_scope->add_symbol(block_name, block_sym);
                    Vec<ASR::stmt_t*> block_call_stmt;
                    block_call_stmt.reserve(al, 1);
                    block_call_stmt.push_back(al, ASRUtils::STMT(ASR::make_BlockCall_t(al, type_stmt_type->base.base.loc, -1, block_sym)));
                    select_type_body.push_back(al, ASR::down_cast<ASR::type_stmt_t>(ASR::make_TypeStmtType_t(al,
                        type_stmt_type->base.base.loc, selector_type, block_call_stmt.p, block_call_stmt.size())));
                    break;
                }
                case AST::type_stmtType::ClassDefault: {
                    SymbolTable* current_scope_copy = current_scope;
                    current_scope = parent_scope;
                    AST::ClassDefault_t* class_default = AST::down_cast<AST::ClassDefault_t>(x.m_body[i]);
                    transform_stmts(select_type_default, class_default->n_body, class_default->m_body);
                    current_scope = current_scope_copy;
                    break;
                }
                default: {
                    throw SemanticError(std::to_string(x.m_body[i]->type) + " statement not supported yet in select type",
                                        x.m_body[i]->base.loc);
                }
            }
            current_scope = parent_scope;
        }

        selector_variable->m_type = selector_variable_type;
        selector_variable->m_dependencies = selector_variable_dependencies;
        selector_variable->n_dependencies = selector_variable_n_dependencies;

        tmp = ASR::make_SelectType_t(al, x.base.base.loc, select_type_body.p, select_type_body.size(),
                                     select_type_default.p, select_type_default.size());
    }

    void visit_Submodule(const AST::Submodule_t &x) {
        SymbolTable *old_scope = current_scope;
        ASR::symbol_t *t = current_scope->get_symbol(to_lower(x.m_name));
        ASR::Module_t *v = ASR::down_cast<ASR::Module_t>(t);
        current_scope = v->m_symtab;
        current_module = v;

        for (size_t i=0; i<x.n_contains; i++) {
            visit_program_unit(*x.m_contains[i]);
        }

        current_scope = old_scope;
        current_module = nullptr;
        tmp = nullptr;
    }

    void visit_Module(const AST::Module_t &x) {
        SymbolTable *old_scope = current_scope;
        ASR::symbol_t *t = current_scope->get_symbol(to_lower(x.m_name));
        ASR::Module_t *v = ASR::down_cast<ASR::Module_t>(t);
        current_module_dependencies.n = 0;
        current_scope = v->m_symtab;
        current_module = v;

        for (size_t i=0; i<x.n_decl; i++) {
            if(x.m_decl[i]->type == AST::unit_decl2Type::Template){
                visit_unit_decl2(*x.m_decl[i]);
            }
        }

        // We have to visit unit_decl_2 because in the example, the Template is directly inside the module and
        // Template is a unit_decl_2

        for (size_t i=0; i<x.n_contains; i++) {
            visit_program_unit(*x.m_contains[i]);
        }

        if( current_module_dependencies.size() > 0 ) {
            Vec<char*> module_dependencies;
            module_dependencies.from_pointer_n_copy(al, v->m_dependencies, v->n_dependencies);
            for( size_t i = 0; i < current_module_dependencies.size(); i++ ) {
                if( !present(module_dependencies, current_module_dependencies[i]) ) {
                    module_dependencies.push_back(al, current_module_dependencies[i]);
                }
            }
            v->m_dependencies = module_dependencies.p;
            v->n_dependencies = module_dependencies.size();
        }

        current_scope = old_scope;
        current_module = nullptr;
        tmp = nullptr;
    }

    void visit_Program(const AST::Program_t &x) {
        SymbolTable *old_scope = current_scope;
        ASR::symbol_t *t = current_scope->get_symbol(to_lower(x.m_name));
        ASR::Program_t *v = ASR::down_cast<ASR::Program_t>(t);
        current_scope = v->m_symtab;
        starting_m_body = x.m_body;
        starting_n_body = x.n_body;

        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body);
        transform_stmts(body, x.n_body, x.m_body);
        ASR::stmt_t* impl_del = create_implicit_deallocate(x.base.base.loc);
        if( impl_del != nullptr ) {
            body.push_back(al, impl_del);
        }
        v->m_body = body.p;
        v->n_body = body.size();

        for (size_t i=0; i<x.n_contains; i++) {
            visit_program_unit(*x.m_contains[i]);
        }

        starting_m_body = nullptr;
        starting_n_body =  0;
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
        Vec<ASR::symbol_t*> del_syms;
        del_syms.reserve(al, 1);
        for( size_t i = 0; i < subrout_call->n_args; i++ ) {
            if( subrout_call->m_args[i].m_value &&
                subrout_call->m_args[i].m_value->type == ASR::exprType::Var ) {
                const ASR::Var_t* arg_var = ASR::down_cast<ASR::Var_t>(subrout_call->m_args[i].m_value);
                const ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(arg_var->m_v);
                if( sym->type == ASR::symbolType::Variable ) {
                    ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(sym);
                    const ASR::Var_t* orig_arg_var = ASR::down_cast<ASR::Var_t>(subrout->m_args[i]);
                    const ASR::symbol_t* orig_sym = ASRUtils::symbol_get_past_external(orig_arg_var->m_v);
                    ASR::Variable_t* orig_var = ASR::down_cast<ASR::Variable_t>(orig_sym);
                    if( var->m_storage == ASR::storage_typeType::Allocatable &&
                        orig_var->m_storage == ASR::storage_typeType::Allocatable &&
                        orig_var->m_intent == ASR::intentType::Out ) {
                        del_syms.push_back(al, arg_var->m_v);
                    }
                }
            }
        }
        if( del_syms.size() == 0 ) {
            return nullptr;
        }
        return ASRUtils::STMT(ASR::make_ImplicitDeallocate_t(al, x->base.loc,
                    del_syms.p, del_syms.size()));
    }

    void visit_Subroutine(const AST::Subroutine_t &x) {
    // TODO: add SymbolTable::lookup_symbol(), which will automatically return
    // an error
    // TODO: add SymbolTable::get_symbol(), which will only check in Debug mode
        SymbolTable *old_scope = current_scope;
        ASR::symbol_t *t = current_scope->get_symbol(to_lower(x.m_name));
        starting_m_body = x.m_body;
        starting_n_body = x.n_body;
        if( t->type == ASR::symbolType::GenericProcedure ) {
            std::string subrout_name = to_lower(x.m_name) + "~genericprocedure";
            t = current_scope->get_symbol(subrout_name);
        }
        for (size_t i=0; i<x.n_decl; i++) {
            if(x.m_decl[i]->type == AST::unit_decl2Type::Instantiate)
                visit_unit_decl2(*x.m_decl[i]);
        }
        ASR::Function_t *v = ASR::down_cast<ASR::Function_t>(t);
        current_scope = v->m_symtab;
        Vec<ASR::stmt_t*> body;
        current_function_dependencies.clear();
        body.reserve(al, x.n_body);
        transform_stmts(body, x.n_body, x.m_body);
        Vec<char*> func_deps;
        func_deps.from_pointer_n_copy(al, v->m_dependencies, v->n_dependencies);
        for( auto& itr: current_function_dependencies ) {
            func_deps.push_back(al, s2c(al, itr));
        }
        ASR::stmt_t* impl_del = create_implicit_deallocate(x.base.base.loc);
        if( impl_del != nullptr ) {
            body.push_back(al, impl_del);
        }
        v->m_body = body.p;
        v->n_body = body.size();
        v->m_dependencies = func_deps.p;
        v->n_dependencies = func_deps.size();

        for (size_t i=0; i<x.n_contains; i++) {
            visit_program_unit(*x.m_contains[i]);
        }

        starting_m_body = nullptr;
        starting_n_body = 0;

        current_scope = old_scope;
        tmp = nullptr;
    }

    void visit_Function(const AST::Function_t &x) {
        starting_m_body = x.m_body;
        starting_n_body = x.n_body;
        SymbolTable *old_scope = current_scope;
        ASR::symbol_t *t = current_scope->get_symbol(to_lower(x.m_name));
        if( t->type == ASR::symbolType::GenericProcedure ) {
            t = current_scope->get_symbol(to_lower(x.m_name) + "~genericprocedure");
        }
        ASR::Function_t *v = ASR::down_cast<ASR::Function_t>(t);
        current_scope = v->m_symtab;
        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body);
        Vec<ASR::symbol_t*> rts;
        rts.reserve(al, rt_vec.size());
        current_function_dependencies.clear();
        transform_stmts(body, x.n_body, x.m_body);
        Vec<char*> func_deps;
        func_deps.from_pointer_n_copy(al, v->m_dependencies, v->n_dependencies);
        for( auto& itr: current_function_dependencies ) {
            func_deps.push_back(al, s2c(al, itr));
        }
        ASR::stmt_t* impl_del = create_implicit_deallocate(x.base.base.loc);
        if( impl_del != nullptr ) {
            body.push_back(al, impl_del);
        }
        v->m_body = body.p;
        v->n_body = body.size();
        v->m_dependencies = func_deps.p;
        v->n_dependencies = func_deps.size();
        v->m_restrictions = rts.p;
        v->n_restrictions = rts.size();

        for (size_t i=0; i<x.n_contains; i++) {
            visit_program_unit(*x.m_contains[i]);
        }

        for (size_t i=0; i<x.n_decl; i++) {
            if(x.m_decl[i]->type == AST::unit_decl2Type::Instantiate)
                visit_unit_decl2(*x.m_decl[i]);
        }

        starting_m_body = nullptr;
        starting_n_body = 0;
        current_scope = old_scope;
        tmp = nullptr;
    }

    void visit_Assign(const AST::Assign_t &x) {
        std::string var_name = to_lower(std::string{x.m_variable});
        ASR::symbol_t *sym = current_scope->resolve_symbol(var_name);
        ASR::ttype_t *int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, 4, nullptr, 0));
        if (!sym) {
            labels.insert(var_name);
            Str a_var_name_f;
            a_var_name_f.from_str(al, var_name);
            Vec<char*> variable_dependencies_vec;
            variable_dependencies_vec.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, int32_type);
            ASR::asr_t* a_variable = ASR::make_Variable_t(al, x.base.base.loc, current_scope, a_var_name_f.c_str(al),
                                                            variable_dependencies_vec.p, variable_dependencies_vec.size(),
                                                            ASR::intentType::Local, nullptr, nullptr,
                                                            ASR::storage_typeType::Default, int32_type,
                                                            ASR::abiType::Source, ASR::Public, ASR::presenceType::Optional, false);
            current_scope->add_symbol(var_name, ASR::down_cast<ASR::symbol_t>(a_variable));
            sym = ASR::down_cast<ASR::symbol_t>(a_variable);
        } else {
            // symbol found but we need to have consistent types
            if (!ASR::is_a<ASR::Variable_t>(*sym)) {
                throw SemanticError("Assign target needs to be a variable.", x.base.base.loc);
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
        tmp = (ASR::asr_t*)ASRUtils::STMT(ASR::make_Assignment_t(al, x.base.base.loc, target_var, ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, x.m_assign_label, int32_type)), nullptr));
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
            // Look for the type of *x.m_target in symbol table, if it is integer or nullptr then it is a statement function
            std::string var_name = AST::down_cast<AST::FuncCallOrArray_t>(x.m_target)->m_func;
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
                    if (ASR::is_a<ASR::Integer_t>(*v->m_type) || ASR::is_a<ASR::Real_t>(*v->m_type) || ASR::is_a<ASR::Logical_t>(*v->m_type)) {
                        if (ASRUtils::is_array(v->m_type)) {
                            return false;
                        } else {
                            return true;
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
        SymbolTable *parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);

        //create a new function, and add it to the symbol table
        std::string var_name = AST::down_cast<AST::FuncCallOrArray_t>(x.m_target)->m_func;
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
                throw SemanticError("Statement function can only contain variables as arguments.", x.base.base.loc);
            }
            ASR::Variable_t* variable = ASR::down_cast<ASR::Variable_t>(tmp_var->m_v);
            std::string arg_name = variable->m_name;
            arg_name = to_lower(arg_name);
            Vec<char*> variable_dependencies_vec;
            variable_dependencies_vec.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, ASRUtils::expr_type(end));
            ASR::asr_t *arg_var = ASR::make_Variable_t(al, x.base.base.loc,
                current_scope, s2c(al, arg_name),
                variable_dependencies_vec.p, variable_dependencies_vec.size(),
                ASRUtils::intent_in, nullptr, nullptr,
                ASR::storage_typeType::Default, ASRUtils::expr_type(end),
                ASR::abiType::Source, ASR::Public, ASR::presenceType::Required,
                false);
            current_scope->add_symbol(arg_name, ASR::down_cast<ASR::symbol_t>(arg_var));
            args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc,
                current_scope->get_symbol(arg_name))));
        }

        // extract the type of var_name from symbol table
        ASR::symbol_t *sym = current_scope->resolve_symbol(var_name);
        ASR::ttype_t *type;

        if (sym==nullptr) {
            if (compiler_options.implicit_typing) {
                type = implicit_dictionary[std::string(1, to_lower(var_name)[0])];
            } else {
                throw SemanticError("Statement function needs to be declared.", x.base.base.loc);
            }
        } else {
            if (ASR::is_a<ASR::Variable_t>(*sym)) {
                auto v = ASR::down_cast<ASR::Variable_t>(sym);
                type = v->m_type;
            } else {
                throw SemanticError("Statement function needs to be declared.", x.base.base.loc);
            }
        }

        // Assign where to_return
        std::string return_var_name = var_name + "_return_var_name";
        Vec<char*> variable_dependencies_vec;
        variable_dependencies_vec.reserve(al, 1);
        ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, type);
        ASR::asr_t *return_var = ASR::make_Variable_t(al, x.base.base.loc,
            current_scope, s2c(al, return_var_name),
            variable_dependencies_vec.p, variable_dependencies_vec.size(),
            ASRUtils::intent_return_var, nullptr, nullptr,
            ASR::storage_typeType::Default, type,
            ASR::abiType::Source, ASR::Public, ASR::presenceType::Required,
            false);
        current_scope->add_symbol(return_var_name, ASR::down_cast<ASR::symbol_t>(return_var));
        ASR::expr_t* to_return = ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc,
            ASR::down_cast<ASR::symbol_t>(return_var)));

        Vec<ASR::stmt_t*> body;
        body.reserve(al, 1);
        this->visit_expr(*x.m_value);
        ASR::expr_t *value = ASRUtils::EXPR(tmp);
        ImplicitCastRules::set_converted_value(al, x.base.base.loc, &value,
                                        ASRUtils::expr_type(value),ASRUtils::expr_type(to_return));
        if (!ASRUtils::check_equal_type(ASRUtils::expr_type(to_return),
                                    ASRUtils::expr_type(value))) {
            std::string ltype = ASRUtils::type_to_str(ASRUtils::expr_type(to_return));
            std::string rtype = ASRUtils::type_to_str(ASRUtils::expr_type(value));
            diag.semantic_error_label(
                "Type mismatch in statement function, the types must be compatible",
                {to_return->base.loc, value->base.loc},
                "type mismatch (" + ltype + " and " + rtype + ")"
            );
            throw SemanticAbort();
        }
        body.push_back(al, ASR::down_cast<ASR::stmt_t>(ASR::make_Assignment_t(al, x.base.base.loc, to_return, value, nullptr)));

        tmp = ASRUtils::make_Function_t_util(
            al, x.base.base.loc,
            /* a_symtab */ current_scope,
            /* a_name */ s2c(al, var_name),
            nullptr, 0,
            /* a_args */ args.p,
            /* n_args */ args.size(),
            /* a_body */ body.p,
            /* n_body */ body.size(),
            /* a_return_var */ to_return,
            ASR::abiType::Source, ASR::accessType::Public, ASR::deftypeType::Implementation,
            nullptr, false, false, false, false, false, /* a_type_parameters */ nullptr,
            /* n_type_parameters */ 0, nullptr, 0, false);
        parent_scope->add_symbol(var_name, ASR::down_cast<ASR::symbol_t>(tmp));
        current_scope = parent_scope;
    }

    void visit_Assignment(const AST::Assignment_t &x) {
        if (is_statement_function(x)) {
            create_statement_function(x);
            tmp = nullptr;
            return;
        }
        this->visit_expr(*x.m_target);
        ASR::expr_t *target = ASRUtils::EXPR(tmp);
        this->visit_expr(*x.m_value);
        ASR::expr_t *value = ASRUtils::EXPR(tmp);
        ASR::stmt_t *overloaded_stmt = nullptr;
        if( ASRUtils::use_overloaded_assignment(target, value,
            current_scope, asr, al, x.base.base.loc, current_function_dependencies,
            current_module_dependencies,
            [&](const std::string &msg, const Location &loc) { throw SemanticError(msg, loc); }) ) {
            overloaded_stmt = ASRUtils::STMT(asr);
        }
        ASR::ttype_t *target_type = ASRUtils::expr_type(target);
        if( target->type != ASR::exprType::Var &&
            target->type != ASR::exprType::ArrayItem &&
            target->type != ASR::exprType::ArraySection &&
            target->type != ASR::exprType::StructInstanceMember )
        {
            throw SemanticError(
                "The LHS of assignment can only be a variable or an array reference",
                x.base.base.loc
            );
        }
        ASR::ttype_t *value_type = ASRUtils::expr_type(value);
        if( target->type == ASR::exprType::Var && !ASRUtils::is_array(target_type) &&
            value->type == ASR::exprType::ArrayConstant ) {
            throw SemanticError("ArrayInitalizer expressions can only be assigned array references", x.base.base.loc);
        }
        if( overloaded_stmt == nullptr ) {
            if ((target->type == ASR::exprType::Var ||
                target->type == ASR::exprType::ArrayItem ||
                target->type == ASR::exprType::ArraySection) &&
                !ASRUtils::check_equal_type(target_type, value_type)) {
                ImplicitCastRules::set_converted_value(al, x.base.base.loc, &value,
                                                        value_type, target_type);

            }
            if (!ASRUtils::check_equal_type(ASRUtils::expr_type(target),
                                        ASRUtils::expr_type(value))) {
                std::string ltype = ASRUtils::type_to_str(ASRUtils::expr_type(target));
                std::string rtype = ASRUtils::type_to_str(ASRUtils::expr_type(value));
                diag.semantic_error_label(
                    "Type mismatch in assignment, the types must be compatible",
                    {target->base.loc, value->base.loc},
                    "type mismatch (" + ltype + " and " + rtype + ")"
                );
                throw SemanticAbort();
            }
        }
        tmp = ASR::make_Assignment_t(al, x.base.base.loc, target, value,
                                     overloaded_stmt);
    }

    ASR::asr_t* create_CFPointer(const AST::SubroutineCall_t& x) {
        std::vector<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"shape"};
        handle_intrinsic_node_args<AST::SubroutineCall_t>(
            x, args, kwarg_names, 2, 3, std::string("c_f_ptr"));
        ASR::expr_t *cptr = args[0], *fptr = args[1], *shape = args[2];
        ASR::ttype_t* fptr_type = ASRUtils::expr_type(fptr);
        bool is_fptr_array = ASRUtils::is_array(fptr_type);
        bool is_ptr = ASR::is_a<ASR::Pointer_t>(*fptr_type);
        if( !is_ptr ) {
            throw SemanticError("fptr is not a pointer.", fptr->base.loc);
        }
        if(!is_fptr_array && shape) {
            throw SemanticError("shape argument specified in c_f_pointer "
                                "even though fptr is not an array.",
                                shape->base.loc);
        }
        if(is_fptr_array && !shape) {
            throw SemanticError("shape argument not specified in c_f_pointer "
                                "even though fptr is an array.",
                                shape->base.loc);
        }
        ASR::dimension_t* shape_dims;
        if( shape ) {
            int shape_rank = ASRUtils::extract_dimensions_from_ttype(
                                ASRUtils::expr_type(shape),
                                shape_dims);
            if( shape_rank != 1 ) {
                throw SemanticError("shape array passed to c_f_pointer "
                                    "must be of rank 1 but given rank is " +
                                    std::to_string(shape_rank),
                                    shape->base.loc);
            }
        }
        return ASR::make_CPtrToPointer_t(al, x.base.base.loc, cptr, fptr, shape);
    }

    void visit_SubroutineCall(const AST::SubroutineCall_t &x) {
        SymbolTable* scope = current_scope;
        std::string sub_name = to_lower(x.m_name);
        ASR::symbol_t *original_sym;
        ASR::expr_t *v_expr = nullptr;
        // If this is a type bound procedure (in a class) it won't be in the
        // main symbol table. Need to check n_member.
        if (x.n_member >= 1) {
            visit_NameUtil(x.m_member, x.n_member - 1,
                x.m_member[x.n_member - 1].m_name, x.base.base.loc);
            v_expr = ASRUtils::EXPR(tmp);
            original_sym = resolve_deriv_type_proc(x.base.base.loc, sub_name,
                            to_lower(x.m_member[x.n_member - 1].m_name),
                            ASRUtils::type_get_past_pointer(ASRUtils::expr_type(v_expr)), scope);
            original_sym = ASRUtils::import_class_procedure(al, x.base.base.loc,
                original_sym, current_scope);
        } else {
            original_sym = current_scope->resolve_symbol(sub_name);
        }
        if (!original_sym) {
            original_sym = resolve_intrinsic_function(x.base.base.loc, sub_name);
            if (!original_sym && compiler_options.implicit_interface) {
                create_implicit_interface_function(x, sub_name, false);
                original_sym = current_scope->resolve_symbol(sub_name);
                LCOMPILERS_ASSERT(original_sym!=nullptr);
            }
        }
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(original_sym);
        if (ASR::is_a<ASR::Function_t>(*sym)) {
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(sym);
            if (ASRUtils::is_intrinsic_procedure(f)) {
                if (intrinsic_module_procedures_as_asr_nodes.find(sub_name) !=
                    intrinsic_module_procedures_as_asr_nodes.end()) {
                    if (sub_name == "c_f_pointer") {
                        tmp = create_CFPointer(x);
                    } else {
                        LCOMPILERS_ASSERT(false)
                    }
                    return;
                }
            }
        }
        Vec<ASR::call_arg_t> args;
        visit_expr_list(x.m_args, x.n_args, args);
        if (x.n_keywords > 0) {
            ASR::symbol_t* f2 = ASRUtils::symbol_get_past_external(original_sym);
            if (ASR::is_a<ASR::Function_t>(*f2)) {
                ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(f2);
                diag::Diagnostics diags;
                visit_kwargs(args, x.m_keywords, x.n_keywords,
                    f->m_args, f->n_args, x.base.base.loc, f,
                    diags, x.n_member);
                if( diags.has_error() ) {
                    diag.diagnostics.insert(diag.diagnostics.end(),
                            diags.diagnostics.begin(), diags.diagnostics.end());
                    throw SemanticAbort();
                }
            } else if (ASR::is_a<ASR::ClassProcedure_t>(*f2)) {
                ASR::ClassProcedure_t* f3 = ASR::down_cast<ASR::ClassProcedure_t>(f2);
                ASR::symbol_t* f4 = f3->m_proc;
                if( !ASR::is_a<ASR::Function_t>(*f4) ) {
                    throw SemanticError(std::string(f3->m_proc_name) + " is not a subroutine.", x.base.base.loc);
                }
                ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(f4);
                diag::Diagnostics diags;
                visit_kwargs(args, x.m_keywords, x.n_keywords,
                    f->m_args, f->n_args, x.base.base.loc, f,
                    diags, x.n_member);
                if( diags.has_error() ) {
                    diag.diagnostics.insert(diag.diagnostics.end(),
                            diags.diagnostics.begin(), diags.diagnostics.end());
                    throw SemanticAbort();
                }
            } else if (ASR::is_a<ASR::GenericProcedure_t>(*f2)) {
                ASR::GenericProcedure_t* f3 = ASR::down_cast<ASR::GenericProcedure_t>(f2);
                bool function_found = false;
                for( size_t i = 0; i < f3->n_procs && !function_found; i++ ) {
                    ASR::symbol_t* f4 = ASRUtils::symbol_get_past_external(f3->m_procs[i]);
                    if( !ASR::is_a<ASR::Function_t>(*f4) ) {
                        throw SemanticError(std::string(ASRUtils::symbol_name(f4)) +
                            " is not a function/subroutine.", x.base.base.loc);
                    }
                    ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(f4);
                    diag::Diagnostics diags;
                    Vec<ASR::call_arg_t> args_;
                    args_.from_pointer_n_copy(al, args.p, args.size());
                    visit_kwargs(args_, x.m_keywords, x.n_keywords,
                        f->m_args, f->n_args, x.base.base.loc, f,
                        diags, x.n_member);
                    if( !diags.has_error() ) {
                        if( ASRUtils::select_generic_procedure(args_, *f3, x.base.base.loc,
                            [&](const std::string &msg, const Location &loc) { throw SemanticError(msg, loc); },
                            false) != -1 ) {
                            function_found = true;
                            args.n = 0;
                            args.from_pointer_n_copy(al, args_.p, args_.size());
                        }
                    }
                }
                if( !function_found ) {
                    throw SemanticError("No matching function found for the "
                        "call to generic procedure, " + std::string(f3->m_name),
                        x.base.base.loc);
                }
            } else {
                throw SemanticError(
                    "Keyword arguments are not implemented for generic subroutines yet, symbol type: " + std::to_string(f2->type),
                    x.base.base.loc);
            }
        }
        Vec<ASR::call_arg_t> args_with_mdt;
        if( x.n_member >= 1 ) {
            args_with_mdt.reserve(al, x.n_args + 1);
            ASR::call_arg_t v_expr_call_arg;
            v_expr_call_arg.loc = v_expr->base.loc, v_expr_call_arg.m_value = v_expr;
            args_with_mdt.push_back(al, v_expr_call_arg);
            for( size_t i = 0; i < args.size(); i++ ) {
                args_with_mdt.push_back(al, args[i]);
            }
        }
        ASR::symbol_t *final_sym=nullptr;
        switch (original_sym->type) {
            case (ASR::symbolType::Function) : {
                final_sym=original_sym;
                original_sym = nullptr;
                break;
            }
            case (ASR::symbolType::GenericProcedure) : {
                ASR::GenericProcedure_t *p = ASR::down_cast<ASR::GenericProcedure_t>(original_sym);
                int idx;
                if( x.n_member == 1 ) {
                    idx = ASRUtils::select_generic_procedure(args_with_mdt, *p, x.base.base.loc,
                            [&](const std::string &msg, const Location &loc) { throw SemanticError(msg, loc); });
                } else {
                    idx = ASRUtils::select_generic_procedure(args, *p, x.base.base.loc,
                            [&](const std::string &msg, const Location &loc) { throw SemanticError(msg, loc); });
                }
                // Create ExternalSymbol for procedures in different modules.
                final_sym = ASRUtils::import_class_procedure(al, x.base.base.loc,
                    p->m_procs[idx], current_scope);
                break;
            }
            case (ASR::symbolType::ClassProcedure) : {
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
                                [&](const std::string &msg, const Location &loc) { throw SemanticError(msg, loc); });
                    // FIXME
                    // Create ExternalSymbol for the final subroutine here
                    final_sym = ASRUtils::symbol_get_past_external(g->m_procs[idx]);
                    if (!ASR::is_a<ASR::Function_t>(*final_sym)) {
                        throw SemanticError("ExternalSymbol must point to a Subroutine", x.base.base.loc);
                    }
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
                } else {
                    if (!ASR::is_a<ASR::Function_t>(*final_sym) &&
                        !ASR::is_a<ASR::ClassProcedure_t>(*final_sym)) {
                        throw SemanticError("ExternalSymbol must point to a Subroutine", x.base.base.loc);
                    }
                    final_sym=original_sym;
                    original_sym = nullptr;
                }
                break;
            }
            case (ASR::symbolType::Variable) : {
                if (compiler_options.implicit_interface) {
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
                    current_scope->erase_symbol(sub_name);
                    create_implicit_interface_function(x, sub_name, false);
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
                }
                final_sym = original_sym;
                original_sym = nullptr;
                break;
            }
            default : {
                throw SemanticError("Symbol type not supported", x.base.base.loc);
            }
        }
        current_function_dependencies.insert(std::string(ASRUtils::symbol_name(final_sym)));
        tmp = ASR::make_SubroutineCall_t(al, x.base.base.loc,
                final_sym, original_sym, args.p, args.size(), v_expr);
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

    void visit_Print(const AST::Print_t &x) {
        Vec<ASR::expr_t*> body;
        body.reserve(al, x.n_values);
        for (size_t i=0; i<x.n_values; i++) {
            visit_expr(*x.m_values[i]);
            ASR::expr_t *expr = ASRUtils::EXPR(tmp);
            body.push_back(al, expr);
        }
        ASR::expr_t *fmt=nullptr;
        if (x.m_fmt != nullptr) {
            this->visit_expr(*x.m_fmt);
            fmt = ASRUtils::EXPR(tmp);
        }
        tmp = ASR::make_Print_t(al, x.base.base.loc, fmt,
            body.p, body.size(), nullptr, nullptr);
    }

    void visit_If(const AST::If_t &x) {
        visit_expr(*x.m_test);
        ASR::expr_t *test = ASRUtils::EXPR(tmp);
        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body);
        transform_stmts(body, x.n_body, x.m_body);
        Vec<ASR::stmt_t*> orelse;
        orelse.reserve(al, x.n_orelse);
        transform_stmts(orelse, x.n_orelse, x.m_orelse);
        tmp = ASR::make_If_t(al, x.base.base.loc, test, body.p,
                body.size(), orelse.p, orelse.size());
    }

    void visit_IfArithmetic(const AST::IfArithmetic_t &x) {
        visit_expr(*x.m_test);
        ASR::expr_t *test_int = ASRUtils::EXPR(tmp);
        ASR::ttype_t *test_int_type = ASRUtils::expr_type(test_int);
        bool is_int  = ASR::is_a<ASR::Integer_t>(*test_int_type);
        bool is_real = ASR::is_a<ASR::Real_t>(*test_int_type);
        if (!is_int && !is_real) {
            throw SemanticError("Arithmetic if (x) requires an integer or real for `x`", test_int->base.loc);
        }
        ASR::expr_t *test_lt, *test_gt;
        int kind = ASRUtils::extract_kind_from_ttype_t(test_int_type);
        if (is_int) {
            ASR::ttype_t *type0 = ASRUtils::TYPE(
                ASR::make_Integer_t(al, x.base.base.loc, kind, nullptr, 0));
            ASR::expr_t *right = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al,
                x.base.base.loc, 0, type0));
            ASR::ttype_t *type = ASRUtils::TYPE(
                ASR::make_Logical_t(al, x.base.base.loc, 4, nullptr, 0));
            ASR::expr_t *value = nullptr;
            test_lt = ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, test_int->base.loc,
                test_int, ASR::cmpopType::Lt, right, type, value));
            test_gt = ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, test_int->base.loc,
                test_int, ASR::cmpopType::Gt, right, type, value));
        } else {
            ASR::ttype_t *type0 = ASRUtils::TYPE(
                ASR::make_Real_t(al, x.base.base.loc, kind, nullptr, 0));
            ASR::expr_t *right = ASRUtils::EXPR(ASR::make_RealConstant_t(al,
                x.base.base.loc, 0, type0));
            ASR::ttype_t *type = ASRUtils::TYPE(
                ASR::make_Logical_t(al, x.base.base.loc, 4, nullptr, 0));
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
            ASR::make_If_t(al, x.base.base.loc, test_gt, body_gt.p,
                body_gt.size(), orelse_gt.p, orelse_gt.size())));
        tmp = ASR::make_If_t(al, x.base.base.loc, test_lt, body.p,
                body.size(), orelse.p, orelse.size());
    }

    void visit_WhileLoop(const AST::WhileLoop_t &x) {
        visit_expr(*x.m_test);
        ASR::expr_t *test = ASRUtils::EXPR(tmp);
        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body);
        transform_stmts(body, x.n_body, x.m_body);
        tmp = ASR::make_WhileLoop_t(al, x.base.base.loc, test, body.p,
                body.size());
    }


    void visit_DoLoop(const AST::DoLoop_t &x) {
        ASR::expr_t *var, *start, *end;
        var = start = end = nullptr;
        if (x.m_var) {
            var = ASRUtils::EXPR(resolve_variable(x.base.base.loc, to_lower(x.m_var)));
        }
        if (x.m_start) {
            visit_expr(*x.m_start);
            start = ASRUtils::EXPR(tmp);
        }
        if (x.m_end) {
            visit_expr(*x.m_end);
            end = ASRUtils::EXPR(tmp);
        }

        ASR::expr_t* increment;
        if (x.m_increment) {
            visit_expr(*x.m_increment);
            increment = ASRUtils::EXPR(tmp);
        } else {
            increment = nullptr;
        }

        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body);
        transform_stmts(body, x.n_body, x.m_body);
        ASR::do_loop_head_t head;
        head.m_v = var;
        head.m_start = start;
        head.m_end = end;
        head.m_increment = increment;
        if (head.m_v != nullptr) {
            head.loc = head.m_v->base.loc;
            tmp = ASR::make_DoLoop_t(al, x.base.base.loc, head, body.p, body.size());
        } else {
            ASR::ttype_t* cond_type
                = ASRUtils::TYPE(ASR::make_Logical_t(al, x.base.base.loc, 4, nullptr, 0));
            ASR::expr_t* cond = ASRUtils::EXPR(
                ASR::make_LogicalConstant_t(al, x.base.base.loc, true, cond_type));
            tmp = ASR::make_WhileLoop_t(al, x.base.base.loc, cond, body.p, body.size());
        }
    }

    void visit_DoConcurrentLoop(const AST::DoConcurrentLoop_t &x) {
        if (x.n_control != 1) {
            throw SemanticError("Do concurrent: exactly one control statement is required for now",
            x.base.base.loc);
        }
        AST::ConcurrentControl_t &h = *(AST::ConcurrentControl_t*) x.m_control[0];
        if (! h.m_var) {
            throw SemanticError("Do loop: loop variable is required for now",
                x.base.base.loc);
        }
        if (! h.m_start) {
            throw SemanticError("Do loop: start condition required for now",
                x.base.base.loc);
        }
        if (! h.m_end) {
            throw SemanticError("Do loop: end condition required for now",
                x.base.base.loc);
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

        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body);
        transform_stmts(body, x.n_body, x.m_body);
        ASR::do_loop_head_t head;
        head.m_v = var;
        head.m_start = start;
        head.m_end = end;
        head.m_increment = increment;
        head.loc = head.m_v->base.loc;
        tmp = ASR::make_DoConcurrentLoop_t(al, x.base.base.loc, head, body.p,
                body.size());
    }

    void visit_ForAllSingle(const AST::ForAllSingle_t &x) {
        if (x.n_control != 1) {
            throw SemanticError("Forall statement: exactly one control statement is required for now",
            x.base.base.loc);
        }
        AST::ConcurrentControl_t &h = *(AST::ConcurrentControl_t*) x.m_control[0];
        if (! h.m_var) {
            throw SemanticError("Forall statement: loop variable is required",
                x.base.base.loc);
        }
        if (! h.m_start) {
            throw SemanticError("Forall statement: start condition is required",
                x.base.base.loc);
        }
        if (! h.m_end) {
            throw SemanticError("Forall statement: end condition is required",
                x.base.base.loc);
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
        assign_stmt = ASRUtils::STMT(tmp);
        ASR::do_loop_head_t head;
        head.m_v = var;
        head.m_start = start;
        head.m_end = end;
        head.m_increment = increment;
        head.loc = head.m_v->base.loc;
        tmp = ASR::make_ForAllSingle_t(al, x.base.base.loc, head, assign_stmt);
    }

    void visit_Exit(const AST::Exit_t &x) {
        // TODO: add a check here that we are inside a While loop
        tmp = ASR::make_Exit_t(al, x.base.base.loc);
    }

    void visit_Cycle(const AST::Cycle_t &x) {
        // TODO: add a check here that we are inside a While loop
        tmp = ASR::make_Cycle_t(al, x.base.base.loc);
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
                        throw SemanticError("Only integer labels are supported in GOTO.",
                            x.base.base.loc);
                    } else {
                        auto l = AST::down_cast<AST::Num_t>(x.m_labels[i]); // l->m_n gets the target -> if l->m_n == (i+1) ...
                        Vec<ASR::stmt_t*> body;
                        body.reserve(al, 1);
                        body.push_back(al, ASRUtils::STMT(ASR::make_GoTo_t(al, x.base.base.loc, l->m_n, s2c(al, std::to_string(l->m_n)))));
                        Vec<ASR::expr_t*> comparator_one;
                        comparator_one.reserve(al, 1);
                        ASR::ttype_t *int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, 4, nullptr, 0));
                        comparator_one.push_back(al, ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, i+1, int32_type)));
                        a_body_vec.push_back(al, ASR::down_cast<ASR::case_stmt_t>(ASR::make_CaseStmt_t(al, x.base.base.loc, comparator_one.p, 1, body.p, 1)));
                    }
                }
                tmp = ASR::make_Select_t(al, x.base.base.loc, goto_label, a_body_vec.p,
                           a_body_vec.size(), def_body.p, def_body.size());
            }
        } else if (x.m_int_var) {
            std::string label{x.m_int_var};
            if (std::find(labels.begin(), labels.end(), label) == labels.end()) {
                throw SemanticError("Cannot GOTO unknown label", x.base.base.loc);
            }
            auto sym = current_scope->resolve_symbol(label);

            // get all labels in current scope
            if (starting_m_body != nullptr) {
                // collect all labels
                for (size_t i = 0; i < starting_n_body; ++i) {
                    int64_t label = stmt_label(starting_m_body[i]);
                    if (label != 0) {
                        labels.insert(std::to_string(label));
                    }
                }
            } else {
                // cannot perform expected behavior
                throw SemanticError("Cannot compute GOTO.", x.base.base.loc);
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
                    ASR::ttype_t *int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, 4, nullptr, 0));
                    comparator_one.push_back(al, ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, num, int32_type)));
                    a_body_vec.push_back(al, ASR::down_cast<ASR::case_stmt_t>(ASR::make_CaseStmt_t(al, x.base.base.loc, comparator_one.p, 1, body.p, 1)));
                }
            } else {
                for (size_t i = 0; i < x.n_labels; ++i) {
                    if (!AST::is_a<AST::Num_t>(*x.m_labels[i])) {
                        throw SemanticError("Only integer labels are supported in GOTO.",
                            x.base.base.loc);
                    } else {
                        auto l = AST::down_cast<AST::Num_t>(x.m_labels[i]);
                        Vec<ASR::stmt_t*> body;
                        body.reserve(al, 1);
                        body.push_back(al, ASRUtils::STMT(ASR::make_GoTo_t(al, x.base.base.loc, l->m_n, s2c(al, std::to_string(l->m_n)))));
                        Vec<ASR::expr_t*> comparator_one;
                        comparator_one.reserve(al, 1);
                        ASR::ttype_t *int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, 4, nullptr, 0));
                        comparator_one.push_back(al, ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, l->m_n, int32_type)));
                        a_body_vec.push_back(al, ASR::down_cast<ASR::case_stmt_t>(ASR::make_CaseStmt_t(al, x.base.base.loc, comparator_one.p, 1, body.p, 1)));
                    }
                }
            }
            ASR::expr_t* var_expr = ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc, sym));
            tmp = ASR::make_Select_t(al, x.base.base.loc, var_expr, a_body_vec.p,
                           a_body_vec.size(), def_body.p, def_body.size());
        } else {
            throw SemanticError("There must be a target to GOTO.",
                x.base.base.loc);
        }
    }

    void visit_Stop(const AST::Stop_t &x) {
        ASR::expr_t *code;
        if (x.m_code) {
            visit_expr(*x.m_code);
            code = ASRUtils::EXPR(tmp);
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
        } else {
            code = nullptr;
        }
        tmp = ASR::make_ErrorStop_t(al, x.base.base.loc, code);
    }

    void visit_Nullify(const AST::Nullify_t &x) {
        Vec<ASR::symbol_t*> arg_vec;
        arg_vec.reserve(al, x.n_args);
        for( size_t i = 0; i < x.n_args; i++ ) {
            this->visit_expr(*(x.m_args[i]));
            ASR::expr_t* tmp_expr = ASRUtils::EXPR(tmp);
            if( tmp_expr->type != ASR::exprType::Var ) {
                throw SemanticError("Only a pointer variable symbol "
                                    "can be nullified.",
                                    tmp_expr->base.loc);
            } else {
                const ASR::Var_t* tmp_var = ASR::down_cast<ASR::Var_t>(tmp_expr);
                ASR::symbol_t* tmp_sym = tmp_var->m_v;
                if( ASRUtils::symbol_get_past_external(tmp_sym)->type
                    != ASR::symbolType::Variable ) {
                    throw SemanticError("Only a pointer variable symbol "
                                        "can be nullified.",
                                        tmp_expr->base.loc);
                } else {
                    ASR::Variable_t* tmp_v = ASR::down_cast<ASR::Variable_t>(tmp_sym);
                    if (ASR::is_a<ASR::Pointer_t>(*tmp_v->m_type)) {
                        arg_vec.push_back(al, tmp_sym);
                    } else {
                        throw SemanticError("Only a pointer variable symbol "
                                            "can be nullified.",
                                            tmp_expr->base.loc);
                    }
                }
            }
        }
        tmp = ASR::make_Nullify_t(al, x.base.base.loc, arg_vec.p, arg_vec.size());
    }

    void visit_Requires(const AST::Requires_t &x) {
        std::string req_name = x.m_name;
        called_requirement = requirement_map[req_name];
    }

    void visit_Template(const AST::Template_t &x){
        is_template = true;
        for (size_t i=0; i<x.n_decl; i++) {
            if (AST::is_a<AST::Requires_t>(*x.m_decl[i])) {
                this->visit_unit_decl2(*x.m_decl[i]);
            }
        }
        for (size_t i=0; i<x.n_contains; i++) {
            this->visit_program_unit(*x.m_contains[i]);
        }
        is_template = false;
        std::map<int, std::string> current_template_arg_map;
        std::map<std::string, ASR::asr_t*> current_template_asr_map;
        for (size_t i=0; i<x.n_namelist; i++) {
            std::string arg_name = to_lower(x.m_namelist[i]);
            if (called_requirement.find(arg_name) != called_requirement.end()) {
                current_template_arg_map[i] = arg_name;
                current_template_asr_map[arg_name] = called_requirement[arg_name];
            }
        }
        template_arg_map[to_lower(x.m_name)] = current_template_arg_map;
        template_asr_map[to_lower(x.m_name)] = current_template_asr_map;
        called_requirement.clear();
    }
};

Result<ASR::TranslationUnit_t*> body_visitor(Allocator &al,
        AST::TranslationUnit_t &ast,
        diag::Diagnostics &diagnostics,
        ASR::asr_t *unit,
        CompilerOptions &compiler_options,
        std::map<std::string, std::map<std::string, ASR::asr_t*>>& requirement_map,
        std::map<uint64_t, std::map<std::string, ASR::ttype_t*>>& implicit_mapping)
{
    BodyVisitor b(al, unit, diagnostics, compiler_options, implicit_mapping);
    try {
        b.is_body_visitor = true;
        b.requirement_map = requirement_map;
        b.visit_TranslationUnit(ast);
        b.is_body_visitor = false;
    } catch (const SemanticError &e) {
        Error error;
        diagnostics.diagnostics.push_back(e.d);
        return error;
    } catch (const SemanticAbort &) {
        Error error;
        return error;
    }
    ASR::TranslationUnit_t *tu = ASR::down_cast2<ASR::TranslationUnit_t>(unit);
    return tu;
}

} // namespace LCompilers::LFortran
