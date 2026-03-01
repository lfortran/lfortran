#include <string>

#include <libasr/config.h>
#include <libasr/serialization.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/bwriter.h>
#include <libasr/string_utils.h>
#include <libasr/exception.h>
#include <libasr/asr_serialization_visitor.h>
#include <libasr/asr_deserialization_visitor.h>

using LCompilers::ASRUtils::symbol_parent_symtab;
using LCompilers::ASRUtils::symbol_name;

namespace LCompilers {


class ASRSerializationVisitor :
#ifdef WITH_LFORTRAN_BINARY_MODFILES
        public BinaryWriter,
#else
        public TextWriter,
#endif
        public ASR::SerializationBaseVisitor<ASRSerializationVisitor>
{
public:
    void write_bool(bool b) {
        if (b) {
            write_int8(1);
        } else {
            write_int8(0);
        }
    }

    void write_symbol(const ASR::symbol_t &x) {
        write_int64(symbol_parent_symtab(&x)->counter);
        write_int8(x.type);
        write_string(symbol_name(&x));
    }

    void visit_StringConstant(const ASR::StringConstant_t &x) {
        write_int8(x.base.type);
        write_int64(x.base.base.loc.first);
        write_int64(x.base.base.loc.last);

        int64_t len = 0;
        bool len_found = false;
        if (x.m_type && ASR::is_a<ASR::String_t>(*x.m_type)) {
            ASR::String_t* t = ASR::down_cast<ASR::String_t>(x.m_type);
            if (t->m_len && ASR::is_a<ASR::IntegerConstant_t>(*t->m_len)) {
                ASR::IntegerConstant_t* ic = ASR::down_cast<ASR::IntegerConstant_t>(t->m_len);
                len = ic->m_n;
                len_found = true;
            }
        }
        
        if (!len_found) {
            len = strlen(x.m_s);
        }

        std::string str_data(x.m_s, len);
        write_string(str_data);

        visit_ttype(*x.m_type);
    }
};

std::string serialize(const ASR::asr_t &asr) {
    ASRSerializationVisitor v;
    v.write_int8(asr.type);
    v.visit_asr(asr);
    return v.get_str();
}

std::string serialize(const ASR::TranslationUnit_t &unit) {
    return serialize((ASR::asr_t&)(unit));
}

class ASRDeserializationVisitor :
#ifdef WITH_LFORTRAN_BINARY_MODFILES
        public BinaryReader,
#else
        public TextReader,
#endif
        public ASR::DeserializationBaseVisitor<ASRDeserializationVisitor>
{
public:
    ASRDeserializationVisitor(Allocator &al, const std::string &s,
        bool load_symtab_id, uint32_t offset) :
#ifdef WITH_LFORTRAN_BINARY_MODFILES
            BinaryReader(s),
#else
            TextReader(s),
#endif
            DeserializationBaseVisitor(al, load_symtab_id, offset) {}

    bool read_bool() {
        uint8_t b = read_int8();
        return (b == 1);
    }

    char* read_cstring() {
        std::string s = read_string();
        LCompilers::Str cs;
        cs.from_str_view(s);
        char* p = cs.c_str(al);
        return p;
    }

#define READ_SYMBOL_CASE(x)                                \
    case (ASR::symbolType::x) : {                          \
        s = (ASR::symbol_t*)al.make_new<ASR::x##_t>();     \
        s->type = ASR::symbolType::x;                      \
        s->base.type = ASR::asrType::symbol;               \
        s->base.loc.first = 0;                             \
        s->base.loc.last = 0;                              \
        break;                                             \
    }

#define INSERT_SYMBOL_CASE(x)                              \
    case (ASR::symbolType::x) : {                          \
        memcpy(sym2, sym, sizeof(ASR::x##_t));             \
        break;                                             \
    }

    ASR::symbol_t *read_symbol() {
        uint64_t symtab_id = read_int64();
        // TODO: read the symbol's location information here, after saving
        // it in write_symbol() above
        uint64_t symbol_type = read_int8();
        std::string symbol_name  = read_string();
        if (id_symtab_map.find(symtab_id) == id_symtab_map.end()) {
            throw LCompilersException(
                "Deserialization failed: symbol '" + symbol_name
                + "' references symbol table with ID "
                + std::to_string(symtab_id)
                + " which has not been deserialized yet. "
                + "This likely indicates a missing ExternalSymbol in the ASR.");
        }
        SymbolTable *symtab = id_symtab_map[symtab_id];
        if (symtab->get_symbol(symbol_name) == nullptr) {
            // Symbol is not in the symbol table yet. We construct an empty
            // symbol of the correct type and put it in the symbol table.
            // Later when constructing the symbol table, we will check for this
            // and fill it in correctly.
            ASR::symbolType ty = static_cast<ASR::symbolType>(symbol_type);
            ASR::symbol_t *s;
            switch (ty) {
                READ_SYMBOL_CASE(Program)
                READ_SYMBOL_CASE(Module)
                READ_SYMBOL_CASE(Function)
                READ_SYMBOL_CASE(GenericProcedure)
                READ_SYMBOL_CASE(CustomOperator)
                READ_SYMBOL_CASE(ExternalSymbol)
                READ_SYMBOL_CASE(Struct)
                READ_SYMBOL_CASE(Variable)
                READ_SYMBOL_CASE(StructMethodDeclaration)
                default : throw LCompilersException("Symbol type not supported");
            }
            symtab->add_symbol(symbol_name, s);
        }
        ASR::symbol_t *sym = symtab->get_symbol(symbol_name);
        return sym;
    }

    void symtab_insert_symbol(SymbolTable &symtab, const std::string &name,
        ASR::symbol_t *sym) {
        if (symtab.get_symbol(name) == nullptr) {
            symtab.add_symbol(name, sym);
        } else {
            // We have to copy the contents of `sym` into `sym2` without
            // changing the `sym2` pointer already in the table
            ASR::symbol_t *sym2 = symtab.get_symbol(name);
            switch (sym->type) {
                INSERT_SYMBOL_CASE(Program)
                INSERT_SYMBOL_CASE(Module)
                INSERT_SYMBOL_CASE(Function)
                INSERT_SYMBOL_CASE(GenericProcedure)
                INSERT_SYMBOL_CASE(CustomOperator)
                INSERT_SYMBOL_CASE(ExternalSymbol)
                INSERT_SYMBOL_CASE(Struct)
                INSERT_SYMBOL_CASE(Variable)
                INSERT_SYMBOL_CASE(StructMethodDeclaration)
                default : throw LCompilersException("Symbol type not supported");
            }
        }
    }
};

namespace ASR {

class FixParentSymtabVisitor : public BaseWalkVisitor<FixParentSymtabVisitor>
{
private:
    SymbolTable *current_symtab;
public:
    void visit_TranslationUnit(const TranslationUnit_t &x) {
        current_symtab = x.m_symtab;
        x.m_symtab->asr_owner = (asr_t*)&x;
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
    }

    void visit_Program(const Program_t &x) {
        SymbolTable *parent_symtab = current_symtab;
        current_symtab = x.m_symtab;
        x.m_symtab->parent = parent_symtab;
        x.m_symtab->asr_owner = (asr_t*)&x;
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
        current_symtab = parent_symtab;
    }

    void visit_Module(const Module_t &x) {
        SymbolTable *parent_symtab = current_symtab;
        current_symtab = x.m_symtab;
        x.m_symtab->parent = parent_symtab;
        x.m_symtab->asr_owner = (asr_t*)&x;
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
        current_symtab = parent_symtab;
    }

    void visit_AssociateBlock(const AssociateBlock_t &x) {
        SymbolTable *parent_symtab = current_symtab;
        current_symtab = x.m_symtab;
        x.m_symtab->parent = parent_symtab;
        x.m_symtab->asr_owner = (asr_t*)&x;
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
        current_symtab = parent_symtab;
    }

    void visit_Block(const Block_t &x) {
        SymbolTable *parent_symtab = current_symtab;
        current_symtab = x.m_symtab;
        x.m_symtab->parent = parent_symtab;
        x.m_symtab->asr_owner = (asr_t*)&x;
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
        current_symtab = parent_symtab;
    }

    void visit_Function(const Function_t &x) {
        SymbolTable *parent_symtab = current_symtab;
        current_symtab = x.m_symtab;
        x.m_symtab->parent = parent_symtab;
        x.m_symtab->asr_owner = (asr_t*)&x;
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
        current_symtab = parent_symtab;
    }

    void visit_Struct(const Struct_t &x) {
        SymbolTable *parent_symtab = current_symtab;
        current_symtab = x.m_symtab;
        x.m_symtab->parent = parent_symtab;
        x.m_symtab->asr_owner = (asr_t*)&x;
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
        current_symtab = parent_symtab;
    }

    void visit_Enum(const Enum_t &x) {
        SymbolTable *parent_symtab = current_symtab;
        current_symtab = x.m_symtab;
        x.m_symtab->parent = parent_symtab;
        x.m_symtab->asr_owner = (asr_t*)&x;
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
        current_symtab = parent_symtab;
    }

    void visit_Requirement(const Requirement_t &x) {
        SymbolTable *parent_symtab = current_symtab;
        current_symtab = x.m_symtab;
        x.m_symtab->parent = parent_symtab;
        x.m_symtab->asr_owner = (asr_t*)&x;
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
        current_symtab = parent_symtab;
    }

    void visit_Template(const Template_t &x) {
        SymbolTable *parent_symtab = current_symtab;
        current_symtab = x.m_symtab;
        x.m_symtab->parent = parent_symtab;
        x.m_symtab->asr_owner = (asr_t*)&x;
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
        current_symtab = parent_symtab;
    }

};

class FixExternalSymbolsVisitor : public BaseWalkVisitor<FixExternalSymbolsVisitor>
{
private:
    SymbolTable *global_symtab;
    SymbolTable *current_scope;
    SymbolTable *external_symtab;
public:
    int attempt;
    bool fixed_external_syms;


    FixExternalSymbolsVisitor(SymbolTable &symtab) : external_symtab{&symtab},
    attempt{0}, fixed_external_syms{true} {}

    void visit_TranslationUnit(const TranslationUnit_t &x) {
        global_symtab = x.m_symtab;
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
    }

    void visit_Module(const Module_t& x) {
        SymbolTable* current_scope_copy = current_scope;
        current_scope = x.m_symtab;
        BaseWalkVisitor<FixExternalSymbolsVisitor>::visit_Module(x);
        current_scope = current_scope_copy;
    }

    void visit_Function(const Function_t& x) {
        SymbolTable* current_scope_copy = current_scope;
        current_scope = x.m_symtab;
        BaseWalkVisitor<FixExternalSymbolsVisitor>::visit_Function(x);
        current_scope = current_scope_copy;
    }

    void visit_AssociateBlock(const AssociateBlock_t& x) {
        SymbolTable* current_scope_copy = current_scope;
        current_scope = x.m_symtab;
        BaseWalkVisitor<FixExternalSymbolsVisitor>::visit_AssociateBlock(x);
        current_scope = current_scope_copy;
    }
    /**
     * Searches for an enum symbol by name across all loaded modules in the global symbol table.
     * Enum symbols are defined within module scopes; returns the symbol if found, otherwise nullptr.
     */
    ASR::symbol_t* enum_in_module(const std::string &ext_sym_enum_name){
        for(auto &sym : global_symtab->get_scope()){
            if(ASR::is_a<ASR::Module_t>(*sym.second)){
                ASR::symbol_t* enum_sym = ASR::down_cast<ASR::Module_t>(sym.second)->m_symtab->get_symbol(ext_sym_enum_name);
                if(enum_sym) { return enum_sym; }
            }
        }
        return nullptr;
    }
    void visit_ExternalSymbol(const ExternalSymbol_t &x) {
        if (x.m_external != nullptr) {
            // Nothing to do, the external symbol is already resolved
            return;
        }
        LCOMPILERS_ASSERT(x.m_external == nullptr);
        if (x.m_module_name == nullptr) {
            throw LCompilersException("The ExternalSymbol was referenced in some ASR node, but it was not loaded as part of the SymbolTable");
        }
        std::string module_name = x.m_module_name;
        std::string original_name = x.m_original_name;
        if (startswith(module_name, "lfortran_intrinsic_iso")) {
            module_name = module_name.substr(19);
        }

        if (global_symtab->get_symbol(module_name) != nullptr) {
            Module_t *m = down_cast<Module_t>(global_symtab->get_symbol(module_name));
            symbol_t *sym = m->m_symtab->find_scoped_symbol(original_name, x.n_scope_names, x.m_scope_names);
            if (sym) {
                // FIXME: this is a hack, we need to pass in a non-const `x`.
                ExternalSymbol_t &xx = const_cast<ExternalSymbol_t&>(x);
                xx.m_external = sym;
            } else {
                throw LCompilersException("ExternalSymbol cannot be resolved, the symbol '"
                    + original_name + "' was not found in the module '"
                    + module_name + "' (but the module was found)");
            }
        } else if (external_symtab->get_symbol(module_name) != nullptr) {
            Module_t *m = down_cast<Module_t>(external_symtab->get_symbol(module_name));
            symbol_t *sym = m->m_symtab->find_scoped_symbol(original_name, x.n_scope_names, x.m_scope_names);
            if (sym) {
                // FIXME: this is a hack, we need to pass in a non-const `x`.
                ExternalSymbol_t &xx = const_cast<ExternalSymbol_t&>(x);
                xx.m_external = sym;
            } else {
                throw LCompilersException("ExternalSymbol cannot be resolved, the symbol '"
                    + original_name + "' was not found in the module '"
                    + module_name + "' (but the module was found)");
            }
        } else if (current_scope->resolve_symbol(module_name) != nullptr) {
            ASR::symbol_t* m_sym = ASRUtils::symbol_get_past_external(
                                    current_scope->resolve_symbol(module_name));
            if( !m_sym ) {
                fixed_external_syms = false;
                return ;
            }

            symbol_t *sym = nullptr;
            if( ASR::is_a<ASR::Struct_t>(*m_sym) ) {
                Struct_t *m = down_cast<Struct_t>(m_sym);
                sym = m->m_symtab->find_scoped_symbol(original_name,
                        x.n_scope_names, x.m_scope_names);
            } else if( ASR::is_a<ASR::Enum_t>(*m_sym) ) {
                Enum_t *m = down_cast<Enum_t>(m_sym);
                sym = m->m_symtab->find_scoped_symbol(original_name,
                        x.n_scope_names, x.m_scope_names);
            } else if( ASR::is_a<ASR::Function_t>(*m_sym) ) {
                Function_t *m = down_cast<Function_t>(m_sym);
                sym = m->m_symtab->find_scoped_symbol(original_name,
                        x.n_scope_names, x.m_scope_names);
            }
            if (sym) {
                // FIXME: this is a hack, we need to pass in a non-const `x`.
                ExternalSymbol_t &xx = const_cast<ExternalSymbol_t&>(x);
                xx.m_external = sym;
            } else {
                throw LCompilersException("ExternalSymbol cannot be resolved, the symbol '"
                    + original_name + "' was not found in the module '"
                    + module_name + "' (but the module was found)");
            }
        } else if(ASR::symbol_t* enum_sym = enum_in_module(module_name)){ // External -> External in module -> points to internal enum symboltable
            ASR::Enum_t* enum_ = ASR::down_cast<ASR::Enum_t>(enum_sym);
            ExternalSymbol_t &xx = const_cast<ExternalSymbol_t&>(x);
            xx.m_external = enum_->m_symtab->get_symbol(x.m_original_name);;
            if(!xx.m_external) { 
                throw LCompilersException("Enumerator variable : '"+ original_name +"' not found, but enum symtab found.");
            }
        } else {
            if( attempt <= 1 ) {
                fixed_external_syms = false;
                return ;
            }
            throw LCompilersException("ExternalSymbol cannot be resolved, the module '"
                + module_name + "' was not found, so the symbol '"
                + original_name + "' could not be resolved");
        }
    }


};

} // namespace ASR

// Fix ExternalSymbol's symbol to point to symbols from `external_symtab`
// or from `unit`.
void fix_external_symbols(ASR::TranslationUnit_t &unit,
        SymbolTable &external_symtab) {
    ASR::FixExternalSymbolsVisitor e(external_symtab);
    e.fixed_external_syms = true;
    e.attempt = 1;
    e.visit_TranslationUnit(unit);
    if( !e.fixed_external_syms ) {
        e.attempt = 2;
        e.visit_TranslationUnit(unit);
    }
}

ASR::asr_t* deserialize_asr(Allocator &al, const std::string &s,
        bool load_symtab_id, SymbolTable & /*external_symtab*/, uint32_t offset) {
    return deserialize_asr(al, s, load_symtab_id, offset);
}

ASR::asr_t* deserialize_asr(Allocator &al, const std::string &s,
        bool load_symtab_id, uint32_t offset) {
    ASRDeserializationVisitor v(al, s, load_symtab_id, offset);
    ASR::asr_t *node = v.deserialize_node();
    ASR::TranslationUnit_t *tu = ASR::down_cast2<ASR::TranslationUnit_t>(node);

    // Connect the `parent` member of symbol tables
    // Also set the `asr_owner` member correctly for all symbol tables
    ASR::FixParentSymtabVisitor p;
    p.visit_TranslationUnit(*tu);

#if defined(WITH_LFORTRAN_ASSERT)
    diag::Diagnostics diagnostics;
    if (!asr_verify(*tu, false, diagnostics)) {
        std::cerr << diagnostics.render2();
        throw LCompilersException("Verify failed");
    };
 #endif

    return node;
}

} // namespace LCompilers
