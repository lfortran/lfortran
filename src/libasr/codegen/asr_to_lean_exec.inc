// Executable Lean 4 backend. Included from asr_to_lean.cpp inside namespace
// LCompilers. The proof translator above is unchanged: it emits noncomputable
// code over FPModel. This one emits code `lean --run` can execute.

namespace {

class ExecError {
public:
    diag::Diagnostic d;

    ExecError(const std::string &msg, const Location &loc)
        : d{diag::Diagnostic(msg, diag::Level::Error, diag::Stage::CodeGen,
            {diag::Label("", {loc})})} {}
};

[[noreturn]] void exec_unsupported(const std::string &what, const Location &loc) {
    throw ExecError(what + " not supported by the Lean backend", loc);
}

constexpr int epAtom = 1024;
constexpr int epApp = 1023;
constexpr int epMul = 70;
constexpr int epAdd = 65;
constexpr int epCmp = 50;
constexpr int epAnd = 35;
constexpr int epOr = 30;

const std::set<std::string> exec_reserved = {
    "at", "break", "by", "class", "continue", "def", "do", "else", "end",
    "false", "for", "from", "fun", "have", "if", "import", "in", "inductive",
    "instance", "let", "main", "match", "mut", "namespace", "open", "return",
    "section", "show", "sorry", "structure", "then", "theorem", "true",
    "universe", "variable", "where", "while", "with", "end", "private",
    "public", "protected", "partial", "unsafe", "abbrev", "axiom", "opaque",
    "noncomputable", "mutual", "local", "scoped", "rec"};

struct Val {
    std::string src;
    int prec = epAtom;
};

struct LoopFrame {
    std::string name;
    std::string flag;
};

struct ArgPlan {
    ASR::Variable_t *var;
    bool pass_in;
    bool pass_out;
};

struct ProcInfo {
    ASR::Function_t *fn = nullptr;
    bool io = false;
    std::vector<ArgPlan> args;
    bool has_result = false;
    std::string ret_expr;
    std::string ret_type;
    std::set<ASR::symbol_t*> callees;
};

std::string exec_atom(const Val &v) {
    return v.prec < epAtom ? "(" + v.src + ")" : v.src;
}

std::string lean_string_lit(const std::string &s) {
    std::string o = "\"";
    for (unsigned char c : s) {
        if (c == '\\' || c == '"') {
            o += '\\';
            o += static_cast<char>(c);
        } else if (c == '\n') {
            o += "\\n";
        } else if (c == '\r') {
            o += "\\r";
        } else if (c == '\t') {
            o += "\\t";
        } else if (c < 32 || c == 127) {
            char buf[8];
            std::snprintf(buf, sizeof(buf), "\\u%04x", c);
            o += buf;
        } else {
            o += static_cast<char>(c);
        }
    }
    o += '"';
    return o;
}

std::string hex_bits(uint64_t u) {
    std::ostringstream os;
    os << "0x" << std::hex << std::uppercase << u;
    return os.str();
}

std::vector<ASR::symbol_t*> exec_symbols_in_order(SymbolTable *symtab) {
    std::vector<ASR::symbol_t*> syms;
    for (auto &item : symtab->get_scope()) {
        syms.push_back(item.second);
    }
    std::stable_sort(syms.begin(), syms.end(),
        [](ASR::symbol_t *a, ASR::symbol_t *b) {
            return a->base.loc.first < b->base.loc.first;
        });
    return syms;
}

class EffectScan : public ASR::BaseWalkVisitor<EffectScan> {
public:
    bool direct = false;
    std::set<ASR::symbol_t*> calls;
    SymbolTable *fn_scope = nullptr;
    std::set<ASR::Variable_t*> *escaped = nullptr;

    EffectScan() { visit_compile_time_value = false; }

    void visit_Var(const ASR::Var_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_v);
        if (!ASR::is_a<ASR::Variable_t>(*s)) return;
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
        if (v->m_storage == ASR::storage_typeType::Parameter) return;
        SymbolTable *p = v->m_parent_symtab;
        if (!p || p == fn_scope || !p->asr_owner
                || !ASR::is_a<ASR::symbol_t>(*p->asr_owner)) {
            return;
        }
        ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(p->asr_owner);
        if (ASR::is_a<ASR::Block_t>(*owner)
                || ASR::is_a<ASR::AssociateBlock_t>(*owner)) {
            return;
        }
        if (ASR::is_a<ASR::Module_t>(*owner) || ASR::is_a<ASR::Program_t>(*owner)
                || ASR::is_a<ASR::Function_t>(*owner)) {
            direct = true;
            if (escaped) escaped->insert(v);
        }
    }

    void visit_Print(const ASR::Print_t &x) {
        direct = true;
        ASR::BaseWalkVisitor<EffectScan>::visit_Print(x);
    }
    void visit_Stop(const ASR::Stop_t &x) {
        direct = true;
        ASR::BaseWalkVisitor<EffectScan>::visit_Stop(x);
    }
    void visit_ErrorStop(const ASR::ErrorStop_t &x) {
        direct = true;
        ASR::BaseWalkVisitor<EffectScan>::visit_ErrorStop(x);
    }
    void visit_FileWrite(const ASR::FileWrite_t &x) {
        direct = true;
        ASR::BaseWalkVisitor<EffectScan>::visit_FileWrite(x);
    }
    void visit_Assert(const ASR::Assert_t &x) {
        direct = true;
        ASR::BaseWalkVisitor<EffectScan>::visit_Assert(x);
    }
    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        calls.insert(ASRUtils::symbol_get_past_external(x.m_name));
        ASR::BaseWalkVisitor<EffectScan>::visit_FunctionCall(x);
    }
    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        calls.insert(ASRUtils::symbol_get_past_external(x.m_name));
        ASR::BaseWalkVisitor<EffectScan>::visit_SubroutineCall(x);
    }
};

class ASRToLeanExec {
public:
    std::string out;
    std::string filename;
    int indent_level = 0;
    SymbolTable *scope = nullptr;
    std::vector<SymbolTable*> scope_stack;
    std::set<ASR::Variable_t*> escaped;
    // Derived types referenced by a variable we emit. Unreferenced type
    // declarations stay out of the Lean file, so one unused recursive type
    // does not reject a program that never mentions it.
    std::set<ASR::Struct_t*> struct_roots;
    std::set<ASR::Struct_t*> structs_done;
    std::set<ASR::Struct_t*> structs_visiting;
    struct DtField {
        ASR::Variable_t *var;
        std::string name;
    };
    std::map<ASR::Struct_t*, std::vector<DtField>> field_cache;
    std::string current_ns;
    int tmp_n = 0;
    int leave_n = 0;
    std::vector<LoopFrame> loops;
    std::map<ASR::symbol_t*, ProcInfo> info;
    ProcInfo *proc = nullptr;

    std::string indent() const {
        return std::string(2 * indent_level, ' ');
    }

    std::string fresh(const std::string &hint) {
        return hint + std::to_string(tmp_n++);
    }

    [[noreturn]] void unsupported(const std::string &what, const Location &loc) {
        exec_unsupported(what, loc);
    }

    std::string lean_name(const std::string &name, const Location &loc) {
        if (exec_reserved.count(name) || (!name.empty() && name[0] == '_')) {
            if (exec_reserved.count(name)) {
                return name + "_";
            }
        }
        for (unsigned char c : name) {
            if (!(std::isalnum(c) || c == '_')) {
                unsupported("the name '" + name + "' is", loc);
            }
        }
        if (exec_reserved.count(name)) {
            return name + "_";
        }
        return name;
    }

    ASR::ttype_t *past_wrappers(ASR::ttype_t *t) {
        while (t && (ASR::is_a<ASR::Pointer_t>(*t)
                || ASR::is_a<ASR::Allocatable_t>(*t))) {
            if (ASR::is_a<ASR::Pointer_t>(*t)) {
                t = ASR::down_cast<ASR::Pointer_t>(t)->m_type;
            } else {
                t = ASR::down_cast<ASR::Allocatable_t>(t)->m_type;
            }
        }
        return t;
    }

    bool is_arr(ASR::ttype_t *t) {
        t = past_wrappers(t);
        return t && ASR::is_a<ASR::Array_t>(*t);
    }

    ASR::ttype_t *elem_type(ASR::ttype_t *t) {
        t = past_wrappers(t);
        if (t && ASR::is_a<ASR::Array_t>(*t)) {
            return ASR::down_cast<ASR::Array_t>(t)->m_type;
        }
        return t;
    }

    int kind_of(ASR::ttype_t *t, const Location &loc) {
        t = elem_type(past_wrappers(t));
        if (!t) {
            unsupported("this type is", loc);
        }
        if (ASR::is_a<ASR::Integer_t>(*t)) {
            return ASR::down_cast<ASR::Integer_t>(t)->m_kind;
        }
        if (ASR::is_a<ASR::Real_t>(*t)) {
            return ASR::down_cast<ASR::Real_t>(t)->m_kind;
        }
        if (ASR::is_a<ASR::Logical_t>(*t)) {
            return ASR::down_cast<ASR::Logical_t>(t)->m_kind;
        }
        if (ASR::is_a<ASR::String_t>(*t)) {
            return ASR::down_cast<ASR::String_t>(t)->m_kind;
        }
        if (ASR::is_a<ASR::Complex_t>(*t)) {
            return ASR::down_cast<ASR::Complex_t>(t)->m_kind;
        }
        unsupported("type (" + std::to_string(t ? (int) t->type : -1) + ") is", loc);
    }

    std::string int_ty(int k, const Location &loc) {
        switch (k) {
            case 1: return "Int8";
            case 2: return "Int16";
            case 4: return "Int32";
            case 8: return "Int64";
            default: unsupported("integer kind " + std::to_string(k) + " is", loc);
        }
    }

    std::string real_ty(int k, const Location &loc) {
        switch (k) {
            case 4: return "Float32";
            case 8: return "Float";
            default: unsupported("real kind " + std::to_string(k) + " is", loc);
        }
    }

    std::string width_suffix(int k, const Location &loc) {
        switch (k) {
            case 1: return "8";
            case 2: return "16";
            case 4: return "32";
            case 8: return "64";
            default: unsupported("integer kind " + std::to_string(k) + " is", loc);
        }
    }

    std::string lean_scalar(ASR::ttype_t *t, const Location &loc,
            ASR::symbol_t *dt = nullptr) {
        t = past_wrappers(t);
        if (!t) {
            unsupported("this type is", loc);
        }
        if (ASR::is_a<ASR::Array_t>(*t)) {
            unsupported("an array used as a scalar is", loc);
        }
        if (ASR::is_a<ASR::StructType_t>(*t)) {
            auto *st = ASR::down_cast<ASR::StructType_t>(t);
            if (st->m_is_unlimited_polymorphic) {
                unsupported("an unlimited polymorphic value is", loc);
            }
            if (!dt) unsupported("this derived type is", loc);
            dt = ASRUtils::symbol_get_past_external(dt);
            if (!ASR::is_a<ASR::Struct_t>(*dt)) {
                unsupported("this derived type is", loc);
            }
            return struct_ref(ASR::down_cast<ASR::Struct_t>(dt));
        }
        if (ASR::is_a<ASR::Integer_t>(*t)) {
            return int_ty(ASR::down_cast<ASR::Integer_t>(t)->m_kind, loc);
        }
        if (ASR::is_a<ASR::Real_t>(*t)) {
            return real_ty(ASR::down_cast<ASR::Real_t>(t)->m_kind, loc);
        }
        if (ASR::is_a<ASR::Logical_t>(*t)) {
            return "Bool";
        }
        if (ASR::is_a<ASR::String_t>(*t)) {
            if (ASR::down_cast<ASR::String_t>(t)->m_kind != 1) {
                unsupported("character kind "
                    + std::to_string(ASR::down_cast<ASR::String_t>(t)->m_kind)
                    + " is", loc);
            }
            return "String";
        }
        if (ASR::is_a<ASR::Complex_t>(*t)) {
            int k = ASR::down_cast<ASR::Complex_t>(t)->m_kind;
            if (k == 4) return "LFortran.Exec.C32";
            if (k == 8) return "LFortran.Exec.C64";
            unsupported("complex kind " + std::to_string(k) + " is", loc);
        }
        unsupported("type (" + std::to_string(t ? (int) t->type : -1) + ") is", loc);
    }

    std::string lean_type(ASR::ttype_t *t, const Location &loc,
            ASR::symbol_t *dt = nullptr) {
        // Decision: allocatable and pointer variables are the value itself.
        // `allocate` replaces an array; pointer assignment copies. Aliasing
        // through two pointer names is not modeled.
        // A derived type is a Lean structure. Parent components are flattened
        // into the child. Components are values, so a pointer component is
        // not an alias. A type that contains itself is rejected.
        t = past_wrappers(t);
        if (is_arr(t)) {
            std::string e = lean_scalar(elem_type(t), loc, dt);
            return "LFortran.Exec.Arr (" + e + ")";
        }
        return lean_scalar(t, loc, dt);
    }

    bool deferred_shape(ASR::ttype_t *t) {
        t = past_wrappers(t);
        if (!t || !ASR::is_a<ASR::Array_t>(*t)) return false;
        ASR::dimension_t *dims = nullptr;
        size_t n = ASRUtils::extract_dimensions_from_ttype(t, dims);
        if (n == 0) return true;
        for (size_t i = 0; i < n; i++) {
            if (!dims[i].m_length) return true;
        }
        return false;
    }

    std::string of_int(ASR::ttype_t *t, int64_t n, const Location &loc) {
        std::string ty = int_ty(kind_of(elem_type(t), loc), loc);
        std::string lit = n < 0 ? "(" + std::to_string(n) + ")" : std::to_string(n);
        return ty + ".ofInt " + lit;
    }

    std::string real_zero(ASR::ttype_t *t, const Location &loc) {
        int k = kind_of(elem_type(t), loc);
        if (k == 8) {
            return "Float.ofBits (0 : UInt64)";
        }
        if (k == 4) {
            return "Float32.ofBits (0 : UInt32)";
        }
        unsupported("real kind " + std::to_string(k) + " is", loc);
    }

    std::string real_one_bits(int k, const Location &loc) {
        if (k == 8) {
            return "Float.ofBits (0x3FF0000000000000 : UInt64)";
        }
        if (k == 4) {
            return "Float32.ofBits (0x3F800000 : UInt32)";
        }
        unsupported("real kind " + std::to_string(k) + " is", loc);
    }

    int64_t const_strlen(ASR::ttype_t *t) {
        t = elem_type(past_wrappers(t));
        if (!t || !ASR::is_a<ASR::String_t>(*t)) {
            return -1;
        }
        ASR::String_t *s = ASR::down_cast<ASR::String_t>(t);
        if (s->m_len_kind != ASR::string_length_kindType::ExpressionLength
                || !s->m_len || !ASR::is_a<ASR::IntegerConstant_t>(*s->m_len)) {
            return -1;
        }
        return ASR::down_cast<ASR::IntegerConstant_t>(s->m_len)->m_n;
    }

    bool assumed_strlen(ASR::ttype_t *t) {
        t = elem_type(past_wrappers(t));
        return t && ASR::is_a<ASR::String_t>(*t)
            && ASR::down_cast<ASR::String_t>(t)->m_len_kind
                == ASR::string_length_kindType::AssumedLength;
    }

    std::string fit_string(ASR::ttype_t *dst, const std::string &src,
            const std::string &dst_name) {
        int64_t n = const_strlen(dst);
        if (n >= 0) {
            return "LFortran.Exec.fitString (" + src + ") " + std::to_string(n);
        }
        if (assumed_strlen(dst) && !dst_name.empty()) {
            return "LFortran.Exec.fitString (" + src + ") (" + dst_name
                + ".toList.length)";
        }
        return src;
    }

    std::string scalar_zero(ASR::ttype_t *t, const Location &loc,
            ASR::symbol_t *dt = nullptr) {
        t = elem_type(past_wrappers(t));
        if (t && ASR::is_a<ASR::StructType_t>(*t)) {
            auto *st = ASR::down_cast<ASR::StructType_t>(t);
            if (st->m_is_unlimited_polymorphic) {
                unsupported("an unlimited polymorphic value is", loc);
            }
            if (!dt) unsupported("this derived type is", loc);
            dt = ASRUtils::symbol_get_past_external(dt);
            if (!ASR::is_a<ASR::Struct_t>(*dt)) {
                unsupported("this derived type is", loc);
            }
            return struct_value(ASR::down_cast<ASR::Struct_t>(dt), loc);
        }
        if (ASR::is_a<ASR::Integer_t>(*t)) {
            return of_int(t, 0, loc);
        }
        if (ASR::is_a<ASR::Real_t>(*t)) {
            return real_zero(t, loc);
        }
        if (ASR::is_a<ASR::Logical_t>(*t)) {
            return "false";
        }
        if (ASR::is_a<ASR::Complex_t>(*t)) {
            std::string ns = lean_scalar(t, loc);
            std::string z = kind_of(t, loc) == 4
                ? "Float32.ofBits (0 : UInt32)" : "Float.ofBits (0 : UInt64)";
            return "({ re := " + z + ", im := " + z + " } : " + ns + ")";
        }
        if (ASR::is_a<ASR::String_t>(*t)) {
            int64_t n = const_strlen(t);
            if (n >= 0) {
                return "LFortran.Exec.fitString \"\" " + std::to_string(n);
            }
            return "\"\"";
        }
        unsupported("type (" + std::to_string(t ? (int) t->type : -1) + ") is", loc);
    }

    std::string array_make(ASR::ttype_t *arr, const std::string &fill,
            const Location &loc) {
        ASR::dimension_t *dims = nullptr;
        size_t n = ASRUtils::extract_dimensions_from_ttype(arr, dims);
        if (n == 0) {
            unsupported("an array of unknown rank is", loc);
        }
        std::string lbs = "#[";
        std::string exts = "#[";
        for (size_t i = 0; i < n; i++) {
            if (i) {
                lbs += ", ";
                exts += ", ";
            }
            if (!dims[i].m_length) {
                unsupported("an assumed-shape array is", loc);
            }
            if (dims[i].m_start) {
                Val b = lower(dims[i].m_start);
                lbs += "(" + b.src + ").toInt";
            } else {
                lbs += "(1 : Int)";
            }
            Val e = lower(dims[i].m_length);
            exts += "(" + e.src + ").toInt.toNat";
        }
        lbs += "]";
        exts += "]";
        return "LFortran.Exec.Arr.make " + lbs + " " + exts + " (" + fill + ")";
    }

    std::string zero_value(ASR::ttype_t *t, const Location &loc,
            ASR::symbol_t *dt = nullptr) {
        if (is_arr(t)) {
            if (deferred_shape(t)) {
                return "(LFortran.Exec.Arr.empty : " + lean_type(t, loc, dt) + ")";
            }
            return array_make(t, scalar_zero(elem_type(t), loc, dt), loc);
        }
        return scalar_zero(t, loc, dt);
    }

    ASR::symbol_t *dt_of_var(ASR::Variable_t *v) {
        if (!v || !v->m_type_declaration) return nullptr;
        return ASRUtils::symbol_get_past_external(v->m_type_declaration);
    }

    // Null when `e` is not a derived type. Throws if the expression shape is
    // a derived type this helper does not know; the sweep reports that.
    ASR::symbol_t *dt_of_expr(ASR::expr_t *e) {
        if (!e) return nullptr;
        ASR::ttype_t *t = past_wrappers(elem_type(ASRUtils::expr_type(e)));
        if (!t || !ASR::is_a<ASR::StructType_t>(*t)) return nullptr;
        if (ASR::down_cast<ASR::StructType_t>(t)->m_is_unlimited_polymorphic) {
            return nullptr;
        }
        ASR::symbol_t *s = ASRUtils::get_struct_sym_from_struct_expr(e);
        if (!s) return nullptr;
        s = ASRUtils::symbol_get_past_external(s);
        if (!s || !ASR::is_a<ASR::Struct_t>(*s)) return nullptr;
        return s;
    }

    std::string struct_ref(ASR::Struct_t *st) {
        std::string n = lean_name(st->m_name, st->base.base.loc);
        if (ASR::Module_t *m = ASRUtils::get_sym_module((ASR::symbol_t*) st)) {
            std::string mn = lean_name(m->m_name, m->base.base.loc);
            if (mn != current_ns) return mn + "." + n;
        }
        return n;
    }

    std::vector<DtField> fields_of(ASR::Struct_t *st) {
        auto found = field_cache.find(st);
        if (found != field_cache.end()) return found->second;
        std::set<ASR::Struct_t*> stack;
        std::vector<DtField> fields;
        std::function<void(ASR::Struct_t*)> walk = [&](ASR::Struct_t *cur) {
            if (!stack.insert(cur).second) {
                unsupported("a recursive derived type is", cur->base.base.loc);
            }
            if (cur->m_parent) {
                ASR::symbol_t *p = ASRUtils::symbol_get_past_external(cur->m_parent);
                if (!ASR::is_a<ASR::Struct_t>(*p)) {
                    unsupported("an extended derived type is", cur->base.base.loc);
                }
                walk(ASR::down_cast<ASR::Struct_t>(p));
            }
            for (size_t i = 0; i < cur->n_members; i++) {
                ASR::symbol_t *s = cur->m_symtab->get_symbol(cur->m_members[i]);
                if (!s) {
                    unsupported("this derived type is", cur->base.base.loc);
                }
                s = ASRUtils::symbol_get_past_external(s);
                if (!ASR::is_a<ASR::Variable_t>(*s)) continue;
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
                if (v->m_type_declaration) {
                    ASR::symbol_t *d = ASRUtils::symbol_get_past_external(
                        v->m_type_declaration);
                    if (ASR::is_a<ASR::Struct_t>(*d)
                            && stack.count(ASR::down_cast<ASR::Struct_t>(d))) {
                        unsupported("a recursive derived type is",
                            v->base.base.loc);
                    }
                }
                fields.push_back(DtField{v, lean_name(v->m_name,
                    v->base.base.loc)});
            }
            stack.erase(cur);
        };
        walk(st);
        field_cache[st] = fields;
        return fields;
    }

    std::string struct_value(ASR::Struct_t *st, const Location &loc) {
        std::vector<DtField> fields = fields_of(st);
        if (fields.empty()) {
            return "({} : " + struct_ref(st) + ")";
        }
        std::string s = "({";
        for (size_t i = 0; i < fields.size(); i++) {
            if (i) s += ", ";
            ASR::Variable_t *v = fields[i].var;
            std::string rhs;
            if (v->m_value) {
                rhs = lower(v->m_value).src;
            } else {
                rhs = zero_value(v->m_type, loc, v->m_type_declaration);
            }
            s += fields[i].name + " := " + rhs;
        }
        s += "} : " + struct_ref(st) + ")";
        return s;
    }

    std::string show_src(const std::string &src, ASR::ttype_t *t,
            ASR::symbol_t *dt, const Location &loc) {
        if (is_arr(t)) {
            std::string one = show_src("x", elem_type(t), dt, loc);
            return "String.intercalate \" \" ((" + src
                + ".data.toList).map (fun x => " + one + "))";
        }
        t = past_wrappers(t);
        if (t && ASR::is_a<ASR::StructType_t>(*t)) {
            if (!dt || !ASR::is_a<ASR::Struct_t>(*
                    ASRUtils::symbol_get_past_external(dt))) {
                unsupported("printing this value is", loc);
            }
            ASR::Struct_t *st = ASR::down_cast<ASR::Struct_t>(
                ASRUtils::symbol_get_past_external(dt));
            std::vector<DtField> fields = fields_of(st);
            if (fields.empty()) return "\"()\"";
            std::string parts;
            for (size_t i = 0; i < fields.size(); i++) {
                if (i) parts += ", ";
                parts += show_src("(" + src + ")." + fields[i].name,
                    fields[i].var->m_type, fields[i].var->m_type_declaration, loc);
            }
            return "String.intercalate \" \" [" + parts + "]";
        }
        if (t && ASR::is_a<ASR::Logical_t>(*t)) {
            return "(if " + src + " then \"T\" else \"F\")";
        }
        if (t && ASR::is_a<ASR::Integer_t>(*t)) {
            return "toString (" + src + ").toInt";
        }
        if (t && ASR::is_a<ASR::String_t>(*t)) return "(" + src + ")";
        if (t && ASR::is_a<ASR::Real_t>(*t)) return "toString (" + src + ")";
        if (t && ASR::is_a<ASR::Complex_t>(*t)) {
            return "(\"(\" ++ toString (" + src + ").re ++ \",\" ++ toString ("
                + src + ").im ++ \")\")";
        }
        unsupported("printing this value is", loc);
    }

    // A broadcast onto an assumed-shape array lowers to the scalar. Callers
    // that look only at the ASR type would treat it as an array.
    bool value_is_array(ASR::expr_t *e) {
        e = skip_phys(e);
        if (e && ASR::is_a<ASR::ArrayBroadcast_t>(*e)) {
            auto &bc = *ASR::down_cast<ASR::ArrayBroadcast_t>(e);
            if (deferred_shape(bc.m_type)
                    && !is_arr(ASRUtils::expr_type(bc.m_array))) {
                return false;
            }
        }
        return e && is_arr(ASRUtils::expr_type(e));
    }

    bool var_visible(ASR::Variable_t *v) const {
        if (v->m_parent_symtab == scope) return true;
        for (SymbolTable *st : scope_stack) {
            if (st == v->m_parent_symtab) return true;
        }
        return false;
    }

    // A module variable, or a local used by a contained procedure, cannot be
    // a `let mut` of every `def` that mentions it.
    bool var_escaped(ASR::Variable_t *v) const {
        return escaped.count(v) != 0;
    }

    std::string cell_key(ASR::Variable_t *v) {
        std::string owner = "g";
        SymbolTable *p = v->m_parent_symtab;
        if (p && p->asr_owner && ASR::is_a<ASR::symbol_t>(*p->asr_owner)) {
            owner = ASRUtils::symbol_name(
                ASR::down_cast<ASR::symbol_t>(p->asr_owner));
        }
        return lean_string_lit(owner + "." + std::string(v->m_name));
    }

    // Tag used in LFortran.Exec.cellGet<Tag> / cellSet<Tag>.
    std::string cell_tag(ASR::ttype_t *t, const Location &loc) {
        bool arr = is_arr(t);
        ASR::ttype_t *e = elem_type(past_wrappers(t));
        std::string tag;
        if (ASR::is_a<ASR::Integer_t>(*e)) {
            tag = "I" + width_suffix(kind_of(e, loc), loc);
        } else if (ASR::is_a<ASR::Real_t>(*e)) {
            tag = kind_of(e, loc) == 4 ? "F32" : "F64";
        } else if (ASR::is_a<ASR::Logical_t>(*e)) {
            tag = "B";
        } else if (ASR::is_a<ASR::String_t>(*e)) {
            tag = "S";
        } else if (ASR::is_a<ASR::Complex_t>(*e)) {
            tag = kind_of(e, loc) == 4 ? "C32" : "C64";
        } else if (ASR::is_a<ASR::StructType_t>(*e)) {
            // Module and host-associated derived types need a typed cell.
            // Each `lean --run` would need one table per structure, which the
            // runtime does not have. Locals of derived type still work.
            unsupported("a saved derived-type variable is", loc);
        } else {
            unsupported("a saved variable of this type is", loc);
        }
        if (arr) {
            if (is_arr(e)) unsupported("an array of arrays is", loc);
            tag = "A" + tag;
        }
        return tag;
    }

    std::string load_var(ASR::Variable_t *v) {
        if (!var_escaped(v)) return var_lean(v);
        std::string tmp = fresh("lfG");
        std::string tag = cell_tag(v->m_type, v->base.base.loc);
        std::string def = zero_value(v->m_type, v->base.base.loc,
            v->m_type_declaration);
        out += indent() + "let " + tmp + " ← LFortran.Exec.cellGet" + tag + " "
            + cell_key(v) + " (" + def + ")\n";
        return tmp;
    }

    void save_var(ASR::Variable_t *v, const std::string &rhs) {
        if (!var_escaped(v)) {
            out += indent() + var_lean(v) + " := " + rhs + "\n";
            return;
        }
        std::string tag = cell_tag(v->m_type, v->base.base.loc);
        out += indent() + "LFortran.Exec.cellSet" + tag + " " + cell_key(v)
            + " (" + rhs + ")\n";
    }

    ASR::expr_t *skip_phys(ASR::expr_t *e) {
        while (e && ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
            e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
        }
        while (e && ASR::is_a<ASR::StringPhysicalCast_t>(*e)) {
            e = ASR::down_cast<ASR::StringPhysicalCast_t>(e)->m_arg;
        }
        return e;
    }

    std::string var_lean(ASR::Variable_t *v) {
        if (proc && proc->has_result && proc->fn->m_return_var
                && ASRUtils::EXPR2VAR(proc->fn->m_return_var) == v) {
            return "lfResult";
        }
        return lean_name(v->m_name, v->base.base.loc);
    }

    bool is_param(ASR::Variable_t *v) {
        return v->m_storage == ASR::storage_typeType::Parameter;
    }

    std::string bind_val(const Val &v, const std::string &hint) {
        std::string n = fresh(hint);
        out += indent() + "let " + n + " := " + v.src + "\n";
        return n;
    }

    Val infix(const Val &l, const std::string &op, const Val &r, int p) {
        std::string ls = l.prec < p ? "(" + l.src + ")" : l.src;
        std::string rs = r.prec <= p ? "(" + r.src + ")" : r.src;
        return {ls + " " + op + " " + rs, p};
    }

    Val map_or_zip(ASR::expr_t *left, ASR::expr_t *right, const std::string &op,
            bool flip_scalar_left) {
        bool la = value_is_array(left);
        bool ra = value_is_array(right);
        Val l = lower(left);
        Val r = lower(right);
        if (la && ra) {
            return {"LFortran.Exec.Arr.zipWith (· " + op + " ·) "
                + exec_atom(l) + " " + exec_atom(r), epApp};
        }
        if (la) {
            return {"LFortran.Exec.Arr.map (fun lfX => lfX " + op + " "
                + exec_atom(r) + ") " + exec_atom(l), epApp};
        }
        if (ra) {
            (void)flip_scalar_left;
            return {"LFortran.Exec.Arr.map (fun lfX => " + exec_atom(l) + " "
                + op + " lfX) " + exec_atom(r), epApp};
        }
        return {};
    }

    Val bin_num(ASR::expr_t *left, ASR::expr_t *right, ASR::binopType op,
            ASR::ttype_t *type, const Location &loc) {
        bool arr = is_arr(type) || is_arr(ASRUtils::expr_type(left))
            || is_arr(ASRUtils::expr_type(right));
        if (op == ASR::binopType::Pow) {
            return pow_op(left, right, type, loc, arr);
        }
        std::string sop;
        int p = epAdd;
        switch (op) {
            case ASR::binopType::Add: sop = "+"; p = epAdd; break;
            case ASR::binopType::Sub: sop = "-"; p = epAdd; break;
            case ASR::binopType::Mul: sop = "*"; p = epMul; break;
            case ASR::binopType::Div: sop = "/"; p = epMul; break;
            case ASR::binopType::BitAnd: sop = "&&&"; p = epMul; break;
            case ASR::binopType::BitOr: sop = "|||"; p = epAdd; break;
            case ASR::binopType::BitXor: sop = "^^^"; p = epAdd; break;
            default: unsupported("this operator is", loc);
        }
        if (arr) {
            Val m = map_or_zip(left, right, sop, false);
            if (!m.src.empty()) {
                return m;
            }
        }
        return infix(lower(left), sop, lower(right), p);
    }

    Val pow_op(ASR::expr_t *left, ASR::expr_t *right, ASR::ttype_t *type,
            const Location &loc, bool arr) {
        ASR::ttype_t *lt = elem_type(ASRUtils::expr_type(left));
        ASR::ttype_t *rt = elem_type(ASRUtils::expr_type(right));
        Val l = lower(left);
        Val r = lower(right);
        std::string fn;
        if (ASR::is_a<ASR::Integer_t>(*past_wrappers(lt))
                && ASR::is_a<ASR::Integer_t>(*past_wrappers(rt))) {
            int k = kind_of(type, loc);
            fn = "LFortran.Exec.ipow" + width_suffix(k, loc);
        } else if (ASR::is_a<ASR::Real_t>(*past_wrappers(lt))) {
            std::string ns = real_ty(kind_of(lt, loc), loc);
            if (ASR::is_a<ASR::Integer_t>(*past_wrappers(rt))) {
                std::string exp = ns + ".ofInt (" + exec_atom(r) + ").toInt";
                if (arr && is_arr(ASRUtils::expr_type(left))
                        && !is_arr(ASRUtils::expr_type(right))) {
                    return {"LFortran.Exec.Arr.map (fun lfX => " + ns
                        + ".pow lfX (" + exp + ")) " + exec_atom(l), epApp};
                }
                return {ns + ".pow " + exec_atom(l) + " (" + exp + ")", epApp};
            }
            fn = ns + ".pow";
        } else {
            unsupported("this power is", loc);
        }
        if (arr && is_arr(ASRUtils::expr_type(left))
                && is_arr(ASRUtils::expr_type(right))) {
            return {"LFortran.Exec.Arr.zipWith " + fn + " " + exec_atom(l)
                + " " + exec_atom(r), epApp};
        }
        if (arr && is_arr(ASRUtils::expr_type(left))) {
            return {"LFortran.Exec.Arr.map (fun lfX => " + fn + " lfX "
                + exec_atom(r) + ") " + exec_atom(l), epApp};
        }
        return {fn + " " + exec_atom(l) + " " + exec_atom(r), epApp};
    }

    std::string cmp_op(ASR::cmpopType op) {
        switch (op) {
            case ASR::cmpopType::Eq: return "==";
            case ASR::cmpopType::NotEq: return "!=";
            case ASR::cmpopType::Lt: return "<";
            case ASR::cmpopType::LtE: return "<=";
            case ASR::cmpopType::Gt: return ">";
            case ASR::cmpopType::GtE: return ">=";
        }
        return "==";
    }

    Val compare(ASR::expr_t *left, ASR::cmpopType op, ASR::expr_t *right,
            bool strings) {
        if (strings) {
            Val l = lower(left);
            Val r = lower(right);
            std::string rel;
            switch (op) {
                case ASR::cmpopType::Eq:
                    rel = "LFortran.Exec.strEq"; break;
                case ASR::cmpopType::NotEq:
                    return {"(!LFortran.Exec.strEq " + exec_atom(l) + " "
                        + exec_atom(r) + ")", epApp};
                case ASR::cmpopType::Lt:
                    rel = "Ordering.lt"; break;
                case ASR::cmpopType::LtE:
                    return {"(LFortran.Exec.strCmp " + exec_atom(l) + " "
                        + exec_atom(r) + " != Ordering.gt)", epApp};
                case ASR::cmpopType::Gt:
                    rel = "Ordering.gt"; break;
                case ASR::cmpopType::GtE:
                    return {"(LFortran.Exec.strCmp " + exec_atom(l) + " "
                        + exec_atom(r) + " != Ordering.lt)", epApp};
            }
            if (op == ASR::cmpopType::Eq) {
                return {"LFortran.Exec.strEq " + exec_atom(l) + " "
                    + exec_atom(r), epApp};
            }
            return {"(LFortran.Exec.strCmp " + exec_atom(l) + " " + exec_atom(r)
                + " == " + rel + ")", epApp};
        }
        bool arr = is_arr(ASRUtils::expr_type(left))
            || is_arr(ASRUtils::expr_type(right));
        // `<` `>` `<=` `>=` are `Prop` in Lean. An array of those cannot be
        // stored or negated; `decide` makes the comparison a `Bool`.
        bool rel = op != ASR::cmpopType::Eq && op != ASR::cmpopType::NotEq;
        if (arr && rel) {
            bool la = value_is_array(left);
            bool ra = value_is_array(right);
            if (la || ra) {
                Val l = lower(left);
                Val r = lower(right);
                std::string sop = cmp_op(op);
                if (la && ra) {
                    return {"LFortran.Exec.Arr.zipWith (fun a b => decide (a "
                        + sop + " b)) " + exec_atom(l) + " " + exec_atom(r),
                        epApp};
                }
                if (la) {
                    return {"LFortran.Exec.Arr.map (fun lfX => decide (lfX "
                        + sop + " " + exec_atom(r) + ")) " + exec_atom(l),
                        epApp};
                }
                return {"LFortran.Exec.Arr.map (fun lfX => decide ("
                    + exec_atom(l) + " " + sop + " lfX)) " + exec_atom(r),
                    epApp};
            }
        }
        if (arr) {
            return map_or_zip(left, right, cmp_op(op), false);
        }
        Val c = infix(lower(left), cmp_op(op), lower(right), epCmp);
        // `decide e` binds only the first application, so the comparison
        // itself has to be parenthesized.
        if (rel) return {"(decide (" + c.src + "))", epApp};
        return c;
    }

    std::string cast_scalar(const std::string &arg, ASR::ttype_t *from,
            ASR::cast_kindType kind, ASR::ttype_t *to, const Location &loc) {
        switch (kind) {
            case ASR::cast_kindType::IntegerToInteger:
                return int_ty(kind_of(to, loc), loc) + ".ofInt (" + arg + ").toInt";
            case ASR::cast_kindType::IntegerToReal:
                return real_ty(kind_of(to, loc), loc) + ".ofInt (" + arg + ").toInt";
            case ASR::cast_kindType::RealToInteger:
                return int_ty(kind_of(to, loc), loc) + ".ofInt ("
                    + real_ty(kind_of(from, loc), loc) + ".toInt64 " + arg
                    + ").toInt";
            case ASR::cast_kindType::RealToReal:
                if (kind_of(to, loc) == 8) {
                    return "Float32.toFloat " + arg;
                }
                return "Float.toFloat32 " + arg;
            case ASR::cast_kindType::IntegerToLogical:
                return "((" + arg + ") != " + of_int(from, 0, loc) + ")";
            case ASR::cast_kindType::RealToLogical:
                return "((" + arg + ") != " + real_zero(from, loc) + ")";
            case ASR::cast_kindType::LogicalToInteger:
                return "(if " + arg + " then " + of_int(to, 1, loc)
                    + " else " + of_int(to, 0, loc) + ")";
            case ASR::cast_kindType::LogicalToReal:
                return "(if " + arg + " then "
                    + real_ty(kind_of(to, loc), loc) + ".ofInt 1 else "
                    + real_ty(kind_of(to, loc), loc) + ".ofInt 0)";
            case ASR::cast_kindType::LogicalToLogical:
            case ASR::cast_kindType::StringToString:
                return arg;
            case ASR::cast_kindType::RealToComplex: {
                std::string ns = lean_scalar(to, loc);
                std::string z = kind_of(to, loc) == 4
                    ? "Float32.ofBits (0 : UInt32)" : "Float.ofBits (0 : UInt64)";
                return "({ re := " + arg + ", im := " + z + " } : " + ns + ")";
            }
            case ASR::cast_kindType::IntegerToComplex: {
                std::string ns = lean_scalar(to, loc);
                std::string rty = real_ty(kind_of(to, loc), loc);
                std::string z = kind_of(to, loc) == 4
                    ? "Float32.ofBits (0 : UInt32)" : "Float.ofBits (0 : UInt64)";
                return "({ re := " + rty + ".ofInt (" + arg + ").toInt, im := "
                    + z + " } : " + ns + ")";
            }
            case ASR::cast_kindType::ComplexToReal:
                return "(" + arg + ").re";
            case ASR::cast_kindType::ComplexToInteger:
                return int_ty(kind_of(to, loc), loc) + ".ofInt ("
                    + real_ty(kind_of(from, loc), loc) + ".toInt64 (" + arg
                    + ").re).toInt";
            case ASR::cast_kindType::ComplexToComplex: {
                int from_k = kind_of(from, loc);
                int to_k = kind_of(to, loc);
                if (from_k == to_k) return arg;
                std::string ns = lean_scalar(to, loc);
                if (to_k == 8) {
                    return "({ re := Float32.toFloat (" + arg
                        + ").re, im := Float32.toFloat (" + arg
                        + ").im } : " + ns + ")";
                }
                return "({ re := Float.toFloat32 (" + arg
                    + ").re, im := Float.toFloat32 (" + arg
                    + ").im } : " + ns + ")";
            }
            default:
                unsupported("this type conversion is", loc);
        }
    }

    std::string norm_intrinsic(std::string n) {
        for (char &c : n) {
            if (c >= 'A' && c <= 'Z') {
                c = static_cast<char>(c - 'A' + 'a');
            }
        }
        n.erase(std::remove(n.begin(), n.end(), '_'), n.end());
        return n;
    }

    bool name_is(const std::string &k, const char *a, const char *b = nullptr) {
        return k == a || (b && k == b);
    }

    Val intrinsic_call(const std::string &raw,
            const std::vector<ASR::expr_t*> &args, ASR::ttype_t *type,
            const Location &loc) {
        std::string k = norm_intrinsic(raw);
        auto need = [&](size_t n) {
            if (args.size() < n) {
                unsupported("intrinsic '" + raw + "' is", loc);
            }
        };
        auto arg1 = [&]() { need(1); return lower(args[0]); };
        bool arr = type && is_arr(type);
        if (name_is(k, "abs")) {
            need(1);
            ASR::ttype_t *et = elem_type(ASRUtils::expr_type(args[0]));
            Val a = lower(args[0]);
            std::string body;
            if (ASR::is_a<ASR::Real_t>(*past_wrappers(et))) {
                body = real_ty(kind_of(et, loc), loc) + ".abs lfX";
            } else if (ASR::is_a<ASR::Complex_t>(*past_wrappers(et))) {
                body = lean_scalar(et, loc) + ".abs lfX";
            } else {
                body = "(if lfX < 0 then -lfX else lfX)";
            }
            if (is_arr(ASRUtils::expr_type(args[0]))) {
                return {"LFortran.Exec.Arr.map (fun lfX => " + body + ") "
                    + exec_atom(a), epApp};
            }
            if (ASR::is_a<ASR::Real_t>(*past_wrappers(et))) {
                return {real_ty(kind_of(et, loc), loc) + ".abs " + exec_atom(a),
                    epApp};
            }
            if (ASR::is_a<ASR::Complex_t>(*past_wrappers(et))) {
                return {lean_scalar(et, loc) + ".abs " + exec_atom(a), epApp};
            }
            return {"(if " + a.src + " < 0 then -" + exec_atom(a) + " else "
                + a.src + ")", epAtom};
        }
        if (name_is(k, "min") || name_is(k, "max")) {
            need(1);
            bool is_min = k == "min";
            Val acc = lower(args[0]);
            for (size_t i = 1; i < args.size(); i++) {
                Val b = lower(args[i]);
                std::string cmp = is_min ? " < " : " > ";
                acc = {"(if " + acc.src + cmp + b.src + " then " + acc.src
                    + " else " + b.src + ")", epAtom};
            }
            (void)arr;
            return acc;
        }
        if (name_is(k, "mod")) {
            need(2);
            int w = kind_of(type, loc);
            Val a = lower(args[0]);
            Val b = lower(args[1]);
            return {"LFortran.Exec.fmod" + width_suffix(w, loc) + " "
                + exec_atom(a) + " " + exec_atom(b), epApp};
        }
        if (name_is(k, "modulo")) {
            need(2);
            int w = kind_of(type, loc);
            Val a = lower(args[0]);
            Val b = lower(args[1]);
            return {"LFortran.Exec.fmodulo" + width_suffix(w, loc) + " "
                + exec_atom(a) + " " + exec_atom(b), epApp};
        }
        if (name_is(k, "sign")) {
            need(2);
            ASR::ttype_t *et = elem_type(type);
            Val a = lower(args[0]);
            Val b = lower(args[1]);
            std::string fn = "LFortran.Exec.fsign";
            if (ASR::is_a<ASR::Real_t>(*past_wrappers(et))) {
                fn += kind_of(et, loc) == 4 ? "F32" : "F";
            } else {
                fn += width_suffix(kind_of(et, loc), loc);
            }
            return {fn + " " + exec_atom(a) + " " + exec_atom(b), epApp};
        }
        if (name_is(k, "merge")) {
            need(3);
            Val t = lower(args[0]);
            Val f = lower(args[1]);
            Val m = lower(args[2]);
            return {"(if " + m.src + " then " + t.src + " else " + f.src + ")",
                epAtom};
        }
        if (name_is(k, "not")) {
            need(1);
            Val a = lower(args[0]);
            if (ASR::is_a<ASR::Logical_t>(*past_wrappers(
                    ASRUtils::expr_type(args[0])))) {
                return {"(!" + exec_atom(a) + ")", epApp};
            }
            return {"(" + a.src + " ^^^ " + of_int(
                ASRUtils::expr_type(args[0]), -1, loc) + ")", epAtom};
        }
        if (name_is(k, "iand") || name_is(k, "ior") || name_is(k, "ieor")
                || name_is(k, "and") || name_is(k, "or") || name_is(k, "xor")) {
            need(2);
            std::string op = "&&&";
            int p = epMul;
            bool is_logical = type && ASR::is_a<ASR::Logical_t>(
                *past_wrappers(elem_type(type)));
            if (is_logical) {
                if (k == "or") { op = "||"; p = epOr; }
                else if (k == "xor") { op = "!="; p = epCmp; }
                else { op = "&&"; p = epAnd; }
            } else if (k == "ior" || k == "or") {
                op = "|||"; p = epAdd;
            } else if (k == "ieor" || k == "xor") {
                op = "^^^"; p = epAdd;
            }
            bool arr = is_arr(ASRUtils::expr_type(args[0]))
                || is_arr(ASRUtils::expr_type(args[1]));
            if (arr) {
                return map_or_zip(args[0], args[1], op, false);
            }
            return infix(lower(args[0]), op, lower(args[1]), p);
        }
        if (k == "sqrt" || k == "sin" || k == "cos" || k == "tan"
                || k == "asin" || k == "acos" || k == "atan"
                || k == "sinh" || k == "cosh" || k == "tanh"
                || k == "exp" || k == "log" || k == "log10") {
            need(1);
            std::string fname = k == "log10" ? "log10" : k;
            ASR::ttype_t *et = elem_type(ASRUtils::expr_type(args[0]));
            std::string ns = real_ty(kind_of(et, loc), loc);
            Val a = lower(args[0]);
            if (is_arr(ASRUtils::expr_type(args[0]))) {
                return {"LFortran.Exec.Arr.map (fun lfX => " + ns + "."
                    + fname + " lfX) " + exec_atom(a), epApp};
            }
            return {ns + "." + fname + " " + exec_atom(a), epApp};
        }
        if (k == "floor" || k == "ceiling") {
            need(1);
            ASR::ttype_t *et = elem_type(ASRUtils::expr_type(args[0]));
            std::string ns = real_ty(kind_of(et, loc), loc);
            Val a = lower(args[0]);
            std::string rnd = ns + "." + (k == "floor" ? std::string("floor")
                : std::string("ceil")) + " " + exec_atom(a);
            if (type && ASR::is_a<ASR::Integer_t>(*past_wrappers(elem_type(type)))) {
                return {int_ty(kind_of(type, loc), loc) + ".ofInt (" + ns
                    + ".toInt64 (" + rnd + ")).toInt", epApp};
            }
            return {rnd, epApp};
        }
        if (k == "nint" || k == "idnint" || k == "anint" || k == "aint") {
            need(1);
            ASR::ttype_t *et = elem_type(ASRUtils::expr_type(args[0]));
            std::string ns = real_ty(kind_of(et, loc), loc);
            Val a = lower(args[0]);
            if (k == "aint") {
                return {"(" + ns + ".ofInt (" + ns + ".toInt64 "
                    + exec_atom(a) + ").toInt)", epAtom};
            }
            std::string rnd = ns + ".round " + exec_atom(a);
            if (k == "anint") {
                return {rnd, epApp};
            }
            return {int_ty(kind_of(type, loc), loc) + ".ofInt (" + ns
                + ".toInt64 (" + rnd + ")).toInt", epApp};
        }
        if (k == "int" || k == "ifix" || k == "idint" || k == "real"
                || k == "sngl" || k == "logical") {
            need(1);
            return lower_convert(args[0], type, loc);
        }
        if (k == "kind") {
            need(1);
            int kk = kind_of(ASRUtils::expr_type(args[0]), loc);
            return {of_int(type, kk, loc), epApp};
        }
        if (k == "selectedintkind") {
            need(1);
            Val r = lower(args[0]);
            std::string ty = int_ty(kind_of(type, loc), loc);
            auto lit = [&](int n) { return ty + ".ofInt " + std::to_string(n); };
            return {"(let lfR := (" + r.src + ").toInt; if lfR <= 2 then "
                + lit(1) + " else if lfR <= 4 then " + lit(2)
                + " else if lfR <= 9 then " + lit(4)
                + " else if lfR <= 18 then " + lit(8) + " else " + lit(-1)
                + ")", epAtom};
        }
        if (k == "selectedrealkind") {
            need(1);
            Val p = lower(args[0]);
            std::string ty = int_ty(kind_of(type, loc), loc);
            auto lit = [&](int n) { return ty + ".ofInt " + std::to_string(n); };
            return {"(let lfP := (" + p.src + ").toInt; if lfP <= 6 then "
                + lit(4) + " else if lfP <= 15 then " + lit(8) + " else "
                + lit(-1) + ")", epAtom};
        }
        if (k == "huge" || k == "tiny" || k == "epsilon" || k == "digits"
                || k == "precision" || k == "radix" || k == "range"
                || k == "bitsize" || k == "minexponent" || k == "maxexponent") {
            need(1);
            return numeric_inquiry(k, ASRUtils::expr_type(args[0]), type, loc);
        }
        if (k == "newline") {
            return {"\"\\n\"", epAtom};
        }
        if (k == "lentrim" || k == "stringlentrim") {
            need(1);
            Val s = lower(args[0]);
            return {int_ty(kind_of(type, loc), loc)
                + ".ofNat (LFortran.Exec.lenTrim " + exec_atom(s) + ")", epApp};
        }
        if (k == "trim" || k == "stringtrim") {
            need(1);
            Val s = lower(args[0]);
            return {"LFortran.Exec.trimRight " + exec_atom(s), epApp};
        }
        if (k == "adjustl" || k == "adjustr") {
            need(1);
            Val s = lower(args[0]);
            std::string fn = k == "adjustl" ? "adjustl" : "adjustr";
            return {"LFortran.Exec." + fn + " " + exec_atom(s), epApp};
        }
        if (k == "ichar" || k == "iachar") {
            need(1);
            Val s = lower(args[0]);
            return {int_ty(kind_of(type, loc), loc)
                + ".ofNat (match " + exec_atom(s)
                + ".toList.head? with | some c => c.toNat | none => 0)", epApp};
        }
        if (k == "char" || k == "achar") {
            need(1);
            Val n = lower(args[0]);
            return {"(String.ofList [Char.ofNat (" + n.src + ").toInt.toNat])",
                epAtom};
        }
        if (k == "repeat") {
            need(2);
            Val s = lower(args[0]);
            Val n = lower(args[1]);
            return {"LFortran.Exec.repeatString " + exec_atom(s) + " ("
                + n.src + ").toInt.toNat", epApp};
        }
        if (k == "shiftl" || k == "shiftr" || k == "ishft") {
            need(2);
            std::string ty = int_ty(kind_of(type, loc), loc);
            Val a = lower(args[0]);
            Val b = lower(args[1]);
            std::string aa = ty + ".ofInt (" + a.src + ").toInt";
            std::string bb = ty + ".ofInt (" + b.src + ").toInt";
            if (k == "shiftl") {
                return {ty + ".shiftLeft (" + aa + ") (" + bb + ")", epApp};
            }
            if (k == "shiftr") {
                return {ty + ".shiftRight (" + aa + ") (" + bb + ")", epApp};
            }
            return {"(if (" + bb + ") >= 0 then " + ty + ".shiftLeft (" + aa
                + ") (" + bb + ") else " + ty + ".shiftRight (" + aa
                + ") (-(" + bb + ")))", epAtom};
        }
        if (k == "btest") {
            need(2);
            std::string ty = int_ty(kind_of(ASRUtils::expr_type(args[0]), loc), loc);
            Val a = lower(args[0]);
            Val b = lower(args[1]);
            return {"(((" + a.src + ") &&& (" + ty + ".shiftLeft ("
                + ty + ".ofInt 1) (" + ty + ".ofInt (" + b.src
                + ").toInt))) != " + of_int(ASRUtils::expr_type(args[0]), 0, loc)
                + ")", epAtom};
        }
        if (k == "floordiv") {
            need(2);
            std::string ty = int_ty(kind_of(type, loc), loc);
            Val a = lower(args[0]);
            Val b = lower(args[1]);
            return {ty + ".ofInt ((" + a.src + ").toInt.fdiv (" + b.src
                + ").toInt)", epApp};
        }
        if (k == "dim") {
            need(2);
            Val a = lower(args[0]);
            Val b = lower(args[1]);
            return {"(if " + a.src + " > " + b.src + " then " + a.src + " - "
                + b.src + " else " + scalar_zero(type, loc) + ")", epAtom};
        }
        if (k == "hypot") {
            need(2);
            ASR::ttype_t *et = elem_type(type);
            std::string ns = real_ty(kind_of(et, loc), loc);
            Val a = lower(args[0]);
            Val b = lower(args[1]);
            return {ns + ".sqrt ((" + a.src + ") * (" + a.src + ") + ("
                + b.src + ") * (" + b.src + "))", epApp};
        }
        if (k == "fma") {
            need(3);
            Val a = lower(args[0]);
            Val b = lower(args[1]);
            Val c = lower(args[2]);
            return {"((" + a.src + ") * (" + b.src + ") + (" + c.src + "))",
                epAtom};
        }
        if (k == "stringconcat") {
            need(2);
            Val acc = lower(args[0]);
            for (size_t i = 1; i < args.size(); i++) {
                Val b = lower(args[i]);
                acc = {"((" + acc.src + ") ++ (" + b.src + "))", epAtom};
            }
            return acc;
        }
        unsupported("intrinsic '" + raw + "' is", loc);
    }

    Val numeric_inquiry(const std::string &k, ASR::ttype_t *arg,
            ASR::ttype_t *type, const Location &loc) {
        ASR::ttype_t *et = elem_type(past_wrappers(arg));
        auto iret = [&](int64_t n) {
            return Val{of_int(type ? type : arg, n, loc), epApp};
        };
        if (!et) {
            unsupported("intrinsic '" + k + "' is", loc);
        }
        if (ASR::is_a<ASR::Integer_t>(*et)) {
            int w = ASR::down_cast<ASR::Integer_t>(et)->m_kind;
            if (k == "radix") return iret(2);
            if (k == "bitsize") return iret(8 * w);
            if (k == "digits") return iret(w == 1 ? 7 : w == 2 ? 15 : w == 4 ? 31 : 63);
            if (k == "range") return iret(w == 1 ? 2 : w == 2 ? 4 : w == 4 ? 9 : 18);
            if (k == "huge") {
                int64_t m = w == 1 ? 127 : w == 2 ? 32767
                    : w == 4 ? 2147483647LL : 9223372036854775807LL;
                return {of_int(et, m, loc), epApp};
            }
            unsupported("intrinsic '" + k + "' is", loc);
        }
        if (ASR::is_a<ASR::Real_t>(*et)) {
            int w = ASR::down_cast<ASR::Real_t>(et)->m_kind;
            std::string ns = real_ty(w, loc);
            if (k == "radix") return iret(2);
            if (k == "digits") return iret(w == 4 ? 24 : 53);
            if (k == "precision") return iret(w == 4 ? 6 : 15);
            if (k == "range" || k == "maxexponent") return iret(w == 4 ? 37 : 307);
            if (k == "minexponent") return iret(w == 4 ? -37 : -307);
            if (k == "bitsize") return iret(w == 4 ? 32 : 64);
            if (k == "huge") {
                std::string bits = w == 4 ? "0x7F7FFFFF" : "0x7FEFFFFFFFFFFFFF";
                std::string cast = w == 4 ? " : UInt32)" : " : UInt64)";
                return {ns + ".ofBits (" + bits + cast, epApp};
            }
            if (k == "tiny") {
                std::string bits = w == 4 ? "0x00800000" : "0x0010000000000000";
                std::string cast = w == 4 ? " : UInt32)" : " : UInt64)";
                return {ns + ".ofBits (" + bits + cast, epApp};
            }
            if (k == "epsilon") {
                std::string bits = w == 4 ? "0x34000000" : "0x3CB0000000000000";
                std::string cast = w == 4 ? " : UInt32)" : " : UInt64)";
                return {ns + ".ofBits (" + bits + cast, epApp};
            }
        }
        unsupported("intrinsic '" + k + "' is", loc);
    }

    Val lower_convert(ASR::expr_t *arg, ASR::ttype_t *to, const Location &loc) {
        ASR::ttype_t *from = elem_type(ASRUtils::expr_type(arg));
        Val a = lower(arg);
        if (to && ASR::is_a<ASR::Logical_t>(*past_wrappers(elem_type(to)))) {
            if (ASR::is_a<ASR::Logical_t>(*past_wrappers(from))) {
                return a;
            }
            if (ASR::is_a<ASR::Integer_t>(*past_wrappers(from))) {
                return {"((" + a.src + ") != " + of_int(from, 0, loc) + ")", epAtom};
            }
            return {"((" + a.src + ") != " + real_zero(from, loc) + ")", epAtom};
        }
        if (to && ASR::is_a<ASR::Integer_t>(*past_wrappers(elem_type(to)))) {
            if (ASR::is_a<ASR::Real_t>(*past_wrappers(from))) {
                return {cast_scalar(exec_atom(a), from,
                    ASR::cast_kindType::RealToInteger, to, loc), epApp};
            }
            return {cast_scalar(exec_atom(a), from,
                ASR::cast_kindType::IntegerToInteger, to, loc), epApp};
        }
        if (ASR::is_a<ASR::Real_t>(*past_wrappers(from))) {
            return {cast_scalar(exec_atom(a), from,
                ASR::cast_kindType::RealToReal, to, loc), epApp};
        }
        return {cast_scalar(exec_atom(a), from,
            ASR::cast_kindType::IntegerToReal, to, loc), epApp};
    }

    Val array_builtin(const std::string &raw,
            const std::vector<ASR::expr_t*> &args, ASR::ttype_t *type,
            const Location &loc) {
        if (args.size() != 1) {
            unsupported("intrinsic '" + raw + "' with this argument list is", loc);
        }
        std::string k = norm_intrinsic(raw);
        Val a = lower(args[0]);
        std::string t = bind_val(a, "lfA");
        ASR::ttype_t *et = elem_type(ASRUtils::expr_type(args[0]));
        if (k == "all") {
            return {"Array.foldl (· && ·) true " + t + ".data", epApp};
        }
        if (k == "any") {
            return {"Array.foldl (· || ·) false " + t + ".data", epApp};
        }
        if (k == "count") {
            return {int_ty(kind_of(type, loc), loc)
                + ".ofNat (Array.foldl (fun n b => n + if b then 1 else 0) 0 "
                + t + ".data)", epApp};
        }
        if (k == "sum" || k == "product") {
            std::string z = k == "sum" ? scalar_zero(et, loc)
                : (ASR::is_a<ASR::Real_t>(*past_wrappers(et))
                    ? real_one_bits(kind_of(et, loc), loc)
                    : of_int(et, 1, loc));
            std::string op = k == "sum" ? "+" : "*";
            return {"Array.foldl (· " + op + " ·) (" + z + ") " + t + ".data",
                epApp};
        }
        if (k == "minval" || k == "maxval") {
            std::string cmp = k == "minval" ? "<" : ">";
            std::string z = scalar_zero(et, loc);
            return {"(if " + t + ".data.size = 0 then " + z
                + " else Array.foldl (fun acc x => if acc " + cmp
                + " x then acc else x) " + t + ".data[0]! " + t + ".data)",
                epAtom};
        }
        unsupported("intrinsic '" + raw + "' is", loc);
    }

    std::string index_list(const ASR::ArrayItem_t &ai) {
        std::string s = "#[";
        for (size_t i = 0; i < ai.n_args; i++) {
            const ASR::array_index_t &ix = ai.m_args[i];
            if (ix.m_left || ix.m_step || !ix.m_right) {
                unsupported("an array section is", ai.base.base.loc);
            }
            if (i) s += ", ";
            Val v = lower(ix.m_right);
            s += "(" + v.src + ").toInt";
        }
        s += "]";
        return s;
    }

    void store(ASR::expr_t *target, const std::string &value, const Location &loc) {
        target = skip_phys(target);
        if (ASR::is_a<ASR::Var_t>(*target)) {
            ASR::Variable_t *v = ASRUtils::EXPR2VAR(target);
            if (is_param(v) || v->m_intent == ASR::intentType::In) {
                unsupported("assignment to this variable is", loc);
            }
            std::string rhs = is_arr(v->m_type) ? value
                : fit_string(v->m_type, value, var_escaped(v) ? "" : var_lean(v));
            save_var(v, rhs);
            return;
        }
        if (ASR::is_a<ASR::ArrayItem_t>(*target)) {
            ASR::ArrayItem_t &ai = *ASR::down_cast<ASR::ArrayItem_t>(target);
            ASR::expr_t *base = skip_phys(ai.m_v);
            if (!ASR::is_a<ASR::Var_t>(*base)) {
                unsupported("assignment to this array element is", loc);
            }
            ASR::Variable_t *v = ASRUtils::EXPR2VAR(base);
            if (is_param(v) || v->m_intent == ASR::intentType::In) {
                unsupported("assignment to this variable is", loc);
            }
            std::string cur = load_var(v);
            std::string idxs = index_list(ai);
            ASR::ttype_t *et = elem_type(v->m_type);
            std::string rhs = fit_string(et, value, "");
            save_var(v, "LFortran.Exec.Arr.set " + cur + " " + idxs
                + " (" + rhs + ")");
            return;
        }
        if (ASR::is_a<ASR::ArraySection_t>(*target)) {
            store_section(*ASR::down_cast<ASR::ArraySection_t>(target), value, loc);
            return;
        }
        if (ASR::is_a<ASR::StructInstanceMember_t>(*target)) {
            store_member(*ASR::down_cast<ASR::StructInstanceMember_t>(target),
                value, loc);
            return;
        }
        unsupported("this assignment is", loc);
    }

    bool assignable(ASR::expr_t *e) {
        e = skip_phys(e);
        if (!e) return false;
        if (ASR::is_a<ASR::Var_t>(*e)) return true;
        if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
            return ASR::is_a<ASR::Var_t>(*skip_phys(
                ASR::down_cast<ASR::ArrayItem_t>(e)->m_v));
        }
        if (ASR::is_a<ASR::ArraySection_t>(*e)) {
            return ASR::is_a<ASR::Var_t>(*skip_phys(
                ASR::down_cast<ASR::ArraySection_t>(e)->m_v));
        }
        if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
            return assignable(ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v);
        }
        return false;
    }

    std::string call_name(ASR::symbol_t *sym) {
        sym = ASRUtils::symbol_get_past_external(sym);
        std::string n = lean_name(ASRUtils::symbol_name(sym), sym->base.loc);
        if (ASR::Module_t *m = ASRUtils::get_sym_module(sym)) {
            std::string mn = lean_name(m->m_name, m->base.base.loc);
            if (mn != current_ns) {
                return mn + "." + n;
            }
        }
        return n;
    }

    std::string proj(const std::string &t, size_t i, size_t n) {
        if (n <= 1) return t;
        std::string s = t;
        for (size_t k = 0; k < i; k++) s += ".2";
        if (i + 1 != n) s += ".1";
        return s;
    }

    Val user_call(ASR::symbol_t *sym, ASR::call_arg_t *args, size_t n,
            bool as_stmt, ASR::ttype_t *ret_type, const Location &loc) {
        sym = ASRUtils::symbol_get_past_external(sym);
        auto it = info.find(sym);
        if (it == info.end()) {
            if (ASR::is_a<ASR::Function_t>(*sym)) {
                ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(sym);
                ASR::FunctionType_t *ft = ASRUtils::get_FunctionType(fn);
                if (ft->m_abi == ASR::abiType::Intrinsic) {
                    std::vector<ASR::expr_t*> ea;
                    for (size_t i = 0; i < n; i++) {
                        if (!args[i].m_value) {
                            unsupported("an optional argument is", loc);
                        }
                        ea.push_back(args[i].m_value);
                    }
                    return intrinsic_call(ASRUtils::symbol_name(sym), ea,
                        ret_type, loc);
                }
            }
            unsupported("a call to an external procedure is", loc);
        }
        ProcInfo &p = it->second;
        if (n != p.fn->n_args) {
            unsupported("a call with this argument list is", loc);
        }
        std::vector<std::string> passed;
        struct Back { ASR::expr_t *target; size_t slot; };
        std::vector<Back> backs;
        size_t slot = p.has_result ? 1 : 0;
        size_t pi = 0;
        ASR::Variable_t *result = p.has_result
            ? ASRUtils::EXPR2VAR(p.fn->m_return_var) : nullptr;
        for (size_t i = 0; i < n; i++) {
            if (!args[i].m_value) {
                unsupported("an optional argument is", loc);
            }
            ASR::Variable_t *dummy = ASRUtils::EXPR2VAR(p.fn->m_args[i]);
            if (result && dummy == result) continue;
            if (pi >= p.args.size()) {
                unsupported("a call with this argument list is", loc);
            }
            ArgPlan &plan = p.args[pi++];
            if (plan.pass_in) {
                Val v = lower(args[i].m_value);
                passed.push_back(exec_atom(v));
            }
            if (plan.pass_out) {
                bool must = dummy->m_intent == ASR::intentType::Out
                    || dummy->m_intent == ASR::intentType::InOut;
                if (assignable(args[i].m_value)) {
                    backs.push_back({args[i].m_value, slot});
                } else if (must) {
                    unsupported("an expression actual for an intent(out) argument is",
                        loc);
                }
                slot++;
            }
        }
        std::string call = call_name(sym);
        for (const std::string &s : passed) call += " " + s;
        size_t arity = 1;
        if (p.ret_type == "Unit" || p.ret_expr.empty()) {
            arity = 1;
        } else {
            arity = 1;
            for (char c : p.ret_expr) {
                if (c == ',') arity++;
            }
        }
        auto project = [&](const std::string &tmp, size_t i) {
            return proj(tmp, i, arity == 0 ? 1 : arity);
        };
        if (p.io) {
            if (p.ret_type == "Unit") {
                out += indent() + call + "\n";
                return {"()", epAtom};
            }
            std::string t = fresh("lfTmp");
            out += indent() + "let " + t + " ← " + call + "\n";
            for (const Back &b : backs) {
                store(b.target, project(t, b.slot), loc);
            }
            if (p.has_result) return {project(t, 0), epApp};
            return {"()", epAtom};
        }
        if (backs.empty() && p.has_result) {
            return {call, epApp};
        }
        if (p.ret_type == "Unit") {
            // A pure `Unit` call is not an `IO` action. Bind it so the
            // statement is valid inside `main`'s `IO` do-block.
            if (as_stmt) out += indent() + "let _ := " + call + "\n";
            return {"()", epAtom};
        }
        std::string t = fresh("lfTmp");
        out += indent() + "let " + t + " := " + call + "\n";
        for (const Back &b : backs) {
            store(b.target, project(t, b.slot), loc);
        }
        if (p.has_result) return {project(t, 0), epApp};
        return {"()", epAtom};
    }

    std::vector<ASR::expr_t*> expr_vec(ASR::expr_t **args, size_t n) {
        std::vector<ASR::expr_t*> v;
        v.reserve(n);
        for (size_t i = 0; i < n; i++) v.push_back(args[i]);
        return v;
    }

    Val lower(ASR::expr_t *e) {
        if (!e) {
            unsupported("a missing expression is", {0, 0});
        }
        const Location &loc = e->base.loc;
        switch (e->type) {
            case ASR::exprType::Var: {
                ASR::Var_t &x = *ASR::down_cast<ASR::Var_t>(e);
                ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_v);
                if (!ASR::is_a<ASR::Variable_t>(*s)) {
                    unsupported("this reference is", loc);
                }
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
                if (is_param(v)) {
                    if (v->m_value) return lower(v->m_value);
                    if (v->m_symbolic_value) return lower(v->m_symbolic_value);
                    unsupported("a parameter without a value is", loc);
                }
                if (var_escaped(v)) {
                    return {load_var(v), epAtom};
                }
                if (!var_visible(v)) {
                    unsupported("a variable from another scope is", loc);
                }
                return {var_lean(v), epAtom};
            }
            case ASR::exprType::IntegerConstant: {
                auto &x = *ASR::down_cast<ASR::IntegerConstant_t>(e);
                return {of_int(x.m_type, x.m_n, loc), epApp};
            }
            case ASR::exprType::RealConstant: {
                auto &x = *ASR::down_cast<ASR::RealConstant_t>(e);
                int k = kind_of(x.m_type, loc);
                double r = x.m_r;
                if (k == 8) {
                    uint64_t u;
                    std::memcpy(&u, &r, 8);
                    return {"Float.ofBits (" + hex_bits(u) + " : UInt64)", epApp};
                }
                float f = static_cast<float>(r);
                uint32_t u;
                std::memcpy(&u, &f, 4);
                return {"Float32.ofBits (" + hex_bits(u) + " : UInt32)", epApp};
            }
            case ASR::exprType::LogicalConstant: {
                auto &x = *ASR::down_cast<ASR::LogicalConstant_t>(e);
                return {x.m_value ? "true" : "false", epAtom};
            }
            case ASR::exprType::StringConstant: {
                auto &x = *ASR::down_cast<ASR::StringConstant_t>(e);
                std::string lit = lean_string_lit(x.m_s ? x.m_s : "");
                int64_t n = const_strlen(x.m_type);
                if (n >= 0) {
                    return {"LFortran.Exec.fitString " + lit + " "
                        + std::to_string(n), epApp};
                }
                return {lit, epAtom};
            }
            case ASR::exprType::IntegerBinOp: {
                auto &x = *ASR::down_cast<ASR::IntegerBinOp_t>(e);
                return bin_num(x.m_left, x.m_right, x.m_op, x.m_type, loc);
            }
            case ASR::exprType::RealBinOp: {
                auto &x = *ASR::down_cast<ASR::RealBinOp_t>(e);
                return bin_num(x.m_left, x.m_right, x.m_op, x.m_type, loc);
            }
            case ASR::exprType::IntegerUnaryMinus:
            case ASR::exprType::RealUnaryMinus: {
                ASR::expr_t *arg = e->type == ASR::exprType::IntegerUnaryMinus
                    ? ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg
                    : ASR::down_cast<ASR::RealUnaryMinus_t>(e)->m_arg;
                Val a = lower(arg);
                if (is_arr(ASRUtils::expr_type(arg))) {
                    return {"LFortran.Exec.Arr.map (fun lfX => -lfX) "
                        + exec_atom(a), epApp};
                }
                return {"(-(" + a.src + "))", epAtom};
            }
            case ASR::exprType::IntegerBitNot: {
                auto &x = *ASR::down_cast<ASR::IntegerBitNot_t>(e);
                Val a = lower(x.m_arg);
                return {"((" + a.src + ") ^^^ " + of_int(x.m_type, -1, loc) + ")",
                    epAtom};
            }
            case ASR::exprType::LogicalNot: {
                auto &x = *ASR::down_cast<ASR::LogicalNot_t>(e);
                Val a = lower(x.m_arg);
                if (is_arr(ASRUtils::expr_type(x.m_arg))) {
                    return {"LFortran.Exec.Arr.map (fun lfX => !lfX) "
                        + exec_atom(a), epApp};
                }
                return {"(!" + exec_atom(a) + ")", epApp};
            }
            case ASR::exprType::IntegerCompare: {
                auto &x = *ASR::down_cast<ASR::IntegerCompare_t>(e);
                return compare(x.m_left, x.m_op, x.m_right, false);
            }
            case ASR::exprType::RealCompare: {
                auto &x = *ASR::down_cast<ASR::RealCompare_t>(e);
                return compare(x.m_left, x.m_op, x.m_right, false);
            }
            case ASR::exprType::LogicalCompare: {
                auto &x = *ASR::down_cast<ASR::LogicalCompare_t>(e);
                return compare(x.m_left, x.m_op, x.m_right, false);
            }
            case ASR::exprType::StringCompare: {
                auto &x = *ASR::down_cast<ASR::StringCompare_t>(e);
                return compare(x.m_left, x.m_op, x.m_right, true);
            }
            case ASR::exprType::LogicalBinOp: {
                auto &x = *ASR::down_cast<ASR::LogicalBinOp_t>(e);
                std::string op = "&&";
                int p = epAnd;
                if (x.m_op == ASR::logicalbinopType::Or) { op = "||"; p = epOr; }
                else if (x.m_op == ASR::logicalbinopType::Xor
                        || x.m_op == ASR::logicalbinopType::NEqv) {
                    op = "!="; p = epCmp;
                } else if (x.m_op == ASR::logicalbinopType::Eqv) {
                    op = "=="; p = epCmp;
                }
                bool arr = is_arr(ASRUtils::expr_type(x.m_left))
                    || is_arr(ASRUtils::expr_type(x.m_right));
                if (arr) return map_or_zip(x.m_left, x.m_right, op, false);
                return infix(lower(x.m_left), op, lower(x.m_right), p);
            }
            case ASR::exprType::StringConcat: {
                auto &x = *ASR::down_cast<ASR::StringConcat_t>(e);
                Val l = lower(x.m_left);
                Val r = lower(x.m_right);
                return {"((" + l.src + ") ++ (" + r.src + "))", epAtom};
            }
            case ASR::exprType::StringRepeat: {
                auto &x = *ASR::down_cast<ASR::StringRepeat_t>(e);
                Val l = lower(x.m_left);
                Val r = lower(x.m_right);
                return {"LFortran.Exec.repeatString " + exec_atom(l) + " ("
                    + r.src + ").toInt.toNat", epApp};
            }
            case ASR::exprType::StringLen: {
                auto &x = *ASR::down_cast<ASR::StringLen_t>(e);
                Val a = lower(x.m_arg);
                return {int_ty(kind_of(x.m_type, loc), loc)
                    + ".ofNat (" + exec_atom(a) + ".toList.length)", epApp};
            }
            case ASR::exprType::StringItem: {
                auto &x = *ASR::down_cast<ASR::StringItem_t>(e);
                Val a = lower(x.m_arg);
                Val i = lower(x.m_idx);
                return {"LFortran.Exec.charAt " + exec_atom(a) + " ("
                    + i.src + ").toInt", epApp};
            }
            case ASR::exprType::StringSection: {
                auto &x = *ASR::down_cast<ASR::StringSection_t>(e);
                if (x.m_step) {
                    unsupported("a character section with a stride is", loc);
                }
                Val a = lower(x.m_arg);
                std::string s = bind_val(a, "lfS");
                std::string lo = x.m_start
                    ? "(" + lower(x.m_start).src + ").toInt" : "(1 : Int)";
                std::string hi = x.m_end
                    ? "(" + lower(x.m_end).src + ").toInt"
                    : "(Int.ofNat " + s + ".toList.length)";
                return {"LFortran.Exec.substr " + s + " " + lo + " " + hi, epApp};
            }
            case ASR::exprType::Cast: {
                auto &x = *ASR::down_cast<ASR::Cast_t>(e);
                if (is_arr(ASRUtils::expr_type(x.m_arg)) && is_arr(x.m_type)) {
                    Val a = lower(x.m_arg);
                    std::string body = cast_scalar("lfX",
                        elem_type(ASRUtils::expr_type(x.m_arg)), x.m_kind,
                        elem_type(x.m_type), loc);
                    return {"LFortran.Exec.Arr.map (fun lfX => " + body + ") "
                        + exec_atom(a), epApp};
                }
                Val a = lower(x.m_arg);
                return {cast_scalar(exec_atom(a), ASRUtils::expr_type(x.m_arg),
                    x.m_kind, x.m_type, loc), epApp};
            }
            case ASR::exprType::IfExp: {
                auto &x = *ASR::down_cast<ASR::IfExp_t>(e);
                Val c = lower(x.m_test);
                Val a = lower(x.m_body);
                Val b = lower(x.m_orelse);
                return {"(if " + c.src + " then " + a.src + " else " + b.src + ")",
                    epAtom};
            }
            case ASR::exprType::ArrayPhysicalCast: {
                return lower(ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg);
            }
            case ASR::exprType::StringPhysicalCast: {
                return lower(ASR::down_cast<ASR::StringPhysicalCast_t>(e)->m_arg);
            }
            case ASR::exprType::ArrayItem: {
                auto &x = *ASR::down_cast<ASR::ArrayItem_t>(e);
                Val b = lower(skip_phys(x.m_v));
                std::string idxs = index_list(x);
                return {"LFortran.Exec.Arr.get " + exec_atom(b) + " " + idxs,
                    epApp};
            }
            case ASR::exprType::ArraySize: {
                auto &x = *ASR::down_cast<ASR::ArraySize_t>(e);
                std::string t = bind_val(lower(x.m_v), "lfA");
                std::string ty = int_ty(kind_of(x.m_type, loc), loc);
                if (!x.m_dim) {
                    return {ty + ".ofInt (LFortran.Exec.Arr.total " + t + ")",
                        epApp};
                }
                Val d = lower(x.m_dim);
                return {ty + ".ofInt (LFortran.Exec.Arr.extent " + t + " ("
                    + d.src + ").toInt)", epApp};
            }
            case ASR::exprType::ArrayBound: {
                auto &x = *ASR::down_cast<ASR::ArrayBound_t>(e);
                if (!x.m_dim) {
                    unsupported("a bound without a dimension is", loc);
                }
                std::string t = bind_val(lower(x.m_v), "lfA");
                Val d = lower(x.m_dim);
                std::string which = x.m_bound == ASR::arrayboundType::LBound
                    ? "lbound" : "ubound";
                std::string ty = int_ty(kind_of(x.m_type, loc), loc);
                return {ty + ".ofInt (LFortran.Exec.Arr." + which + " " + t
                    + " (" + d.src + ").toInt)", epApp};
            }
            case ASR::exprType::ArrayRank: {
                auto &x = *ASR::down_cast<ASR::ArrayRank_t>(e);
                std::string t = bind_val(lower(x.m_v), "lfA");
                return {int_ty(kind_of(x.m_type, loc), loc)
                    + ".ofNat " + t + ".exts.size", epApp};
            }
            case ASR::exprType::ArrayBroadcast: {
                auto &x = *ASR::down_cast<ASR::ArrayBroadcast_t>(e);
                if (x.m_value && ASR::is_a<ASR::ArrayConstant_t>(*x.m_value)) {
                    return lower(x.m_value);
                }
                // The shape of an assumed-shape broadcast is not in the type.
                // Keep the scalar and let the array operand supply the shape
                // (`map`) or the assignment fill the existing array.
                if (deferred_shape(x.m_type)) {
                    return lower(x.m_array);
                }
                Val s = lower(x.m_array);
                return {array_make(x.m_type, s.src, loc), epApp};
            }
            case ASR::exprType::ArrayConstant: {
                auto &x = *ASR::down_cast<ASR::ArrayConstant_t>(e);
                return lower_array_constant(x);
            }
            case ASR::exprType::ArrayConstructor: {
                auto &x = *ASR::down_cast<ASR::ArrayConstructor_t>(e);
                return lower_array_ctor(x);
            }
            case ASR::exprType::FunctionCall: {
                auto &x = *ASR::down_cast<ASR::FunctionCall_t>(e);
                if (x.m_dt) unsupported("a type-bound call is", loc);
                return user_call(x.m_name, x.m_args, x.n_args, false, x.m_type, loc);
            }
            case ASR::exprType::IntrinsicElementalFunction: {
                auto &x = *ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
                return intrinsic_call(ASRUtils::get_intrinsic_name(x.m_intrinsic_id),
                    expr_vec(x.m_args, x.n_args), x.m_type, loc);
            }
            case ASR::exprType::IntrinsicArrayFunction: {
                auto &x = *ASR::down_cast<ASR::IntrinsicArrayFunction_t>(e);
                return array_builtin(
                    ASRUtils::get_array_intrinsic_name(x.m_arr_intrinsic_id),
                    expr_vec(x.m_args, x.n_args), x.m_type, loc);
            }
            case ASR::exprType::TypeInquiry: {
                auto &x = *ASR::down_cast<ASR::TypeInquiry_t>(e);
                if (x.m_value) return lower(x.m_value);
                unsupported("this type inquiry is", loc);
            }
            case ASR::exprType::RealCopySign: {
                auto &x = *ASR::down_cast<ASR::RealCopySign_t>(e);
                int k = kind_of(x.m_type, loc);
                Val a = lower(x.m_target);
                Val b = lower(x.m_source);
                std::string fn = k == 4 ? "LFortran.Exec.fsignF32"
                    : "LFortran.Exec.fsignF";
                return {fn + " " + exec_atom(a) + " " + exec_atom(b), epApp};
            }
            case ASR::exprType::ArraySection:
                return lower_section(*ASR::down_cast<ASR::ArraySection_t>(e));
            case ASR::exprType::ArrayReshape: {
                auto &x = *ASR::down_cast<ASR::ArrayReshape_t>(e);
                if (x.m_pad || x.m_order) {
                    unsupported("this reshape is", loc);
                }
                if (x.m_value && (ASR::is_a<ASR::ArrayConstant_t>(*x.m_value)
                        || ASR::is_a<ASR::ArrayConstructor_t>(*x.m_value))) {
                    return lower(x.m_value);
                }
                std::string src = bind_val(lower(x.m_array), "lfR");
                std::string sh = bind_val(lower(x.m_shape), "lfH");
                std::string ty = lean_type(x.m_type, loc, dt_of_expr(x.m_array));
                return {"({ lbs := Array.replicate " + sh
                    + ".data.size (1 : Int), exts := " + sh
                    + ".data.map (fun i => i.toInt.toNat), data := " + src
                    + ".data } : " + ty + ")", epAtom};
            }
            case ASR::exprType::StringFormat: {
                auto &x = *ASR::down_cast<ASR::StringFormat_t>(e);
                if (x.m_fmt) unsupported("formatted output is", loc);
                if (x.n_args == 0) return {"\"\"", epAtom};
                std::string parts;
                for (size_t i = 0; i < x.n_args; i++) {
                    if (i) parts += ", ";
                    parts += shown(x.m_args[i]);
                }
                return {"String.intercalate \" \" [" + parts + "]", epApp};
            }
            case ASR::exprType::BitCast: {
                auto &x = *ASR::down_cast<ASR::BitCast_t>(e);
                if (x.m_value) return lower(x.m_value);
                Val s = lower(x.m_source);
                ASR::symbol_t *dts = dt_of_expr(x.m_source);
                if (lean_type(ASRUtils::expr_type(x.m_source), loc, dts)
                        == lean_type(x.m_type, loc, dts ? dts : dt_of_expr(e))) {
                    return s;
                }
                unsupported("this bit cast is", loc);
            }
            case ASR::exprType::ImpliedDoLoop:
                unsupported("an implied do loop is", loc);
            case ASR::exprType::ComplexConstant: {
                auto &x = *ASR::down_cast<ASR::ComplexConstant_t>(e);
                int k = kind_of(x.m_type, loc);
                auto bits = [&](double r) {
                    if (k == 8) {
                        uint64_t u; std::memcpy(&u, &r, 8);
                        return "Float.ofBits (" + hex_bits(u) + " : UInt64)";
                    }
                    float f = static_cast<float>(r);
                    uint32_t u; std::memcpy(&u, &f, 4);
                    return "Float32.ofBits (" + hex_bits(u) + " : UInt32)";
                };
                return {"({ re := " + bits(x.m_re) + ", im := " + bits(x.m_im)
                    + " } : " + lean_scalar(x.m_type, loc) + ")", prec_atom};
            }
            case ASR::exprType::ComplexConstructor: {
                auto &x = *ASR::down_cast<ASR::ComplexConstructor_t>(e);
                Val re = lower(x.m_re);
                Val im = lower(x.m_im);
                return {"({ re := " + re.src + ", im := " + im.src + " } : "
                    + lean_scalar(x.m_type, loc) + ")", prec_atom};
            }
            case ASR::exprType::ComplexUnaryMinus: {
                auto &x = *ASR::down_cast<ASR::ComplexUnaryMinus_t>(e);
                Val a = lower(x.m_arg);
                std::string ns = lean_scalar(elem_type(x.m_type), loc);
                if (is_arr(ASRUtils::expr_type(x.m_arg))) {
                    return {"LFortran.Exec.Arr.map (fun lfX => " + ns
                        + ".neg lfX) " + exec_atom(a), prec_app};
                }
                return {ns + ".neg " + exec_atom(a), prec_app};
            }
            case ASR::exprType::ComplexCompare: {
                auto &x = *ASR::down_cast<ASR::ComplexCompare_t>(e);
                if (x.m_op != ASR::cmpopType::Eq && x.m_op != ASR::cmpopType::NotEq) {
                    unsupported("this complex comparison is", loc);
                }
                Val l = lower(x.m_left);
                Val r = lower(x.m_right);
                std::string ns = lean_scalar(elem_type(
                    ASRUtils::expr_type(x.m_left)), loc);
                std::string eq = ns + ".eq " + exec_atom(l) + " " + exec_atom(r);
                if (x.m_op == ASR::cmpopType::NotEq) eq = "(!" + eq + ")";
                return {eq, prec_app};
            }
            case ASR::exprType::ComplexBinOp: {
                auto &x = *ASR::down_cast<ASR::ComplexBinOp_t>(e);
                std::string fn;
                switch (x.m_op) {
                    case ASR::binopType::Add: fn = "add"; break;
                    case ASR::binopType::Sub: fn = "sub"; break;
                    case ASR::binopType::Mul: fn = "mul"; break;
                    case ASR::binopType::Div: fn = "div"; break;
                    default: unsupported("this complex operator is", loc);
                }
                std::string ns = lean_scalar(elem_type(x.m_type), loc) + "." + fn;
                bool la = is_arr(ASRUtils::expr_type(x.m_left));
                bool ra = is_arr(ASRUtils::expr_type(x.m_right));
                Val l = lower(x.m_left);
                Val r = lower(x.m_right);
                if (la && ra) {
                    return {"LFortran.Exec.Arr.zipWith " + ns + " "
                        + exec_atom(l) + " " + exec_atom(r), prec_app};
                }
                if (la || ra) unsupported("this complex operator is", loc);
                return {ns + " " + exec_atom(l) + " " + exec_atom(r), prec_app};
            }
            case ASR::exprType::StructInstanceMember:
                return lower_member(*ASR::down_cast<ASR::StructInstanceMember_t>(e));
            case ASR::exprType::StructConstructor: {
                auto &x = *ASR::down_cast<ASR::StructConstructor_t>(e);
                return lower_struct_ctor(x.m_dt_sym, x.m_args, x.n_args, loc);
            }
            case ASR::exprType::StructConstant: {
                auto &x = *ASR::down_cast<ASR::StructConstant_t>(e);
                return lower_struct_ctor(x.m_dt_sym, x.m_args, x.n_args, loc);
            }
            default:
                unsupported("expression (" + std::to_string((int) e->type)
                    + ") is", loc);
        }
    }

    Val lower_array_constant(const ASR::ArrayConstant_t &x) {
        const Location &loc = x.base.base.loc;
        ASR::ttype_t *et = elem_type(x.m_type);
        if (!ASRUtils::is_array_constant_element_type(et)) {
            unsupported("this array constant is", loc);
        }
        int64_t n = ASRUtils::get_fixed_size_of_array(x.m_type);
        if (n < 0) unsupported("this array constant is", loc);
        Allocator al(1 << 16);
        std::string data = "#[";
        for (int64_t i = 0; i < n; i++) {
            ASR::expr_t *el = nullptr;
            try {
                el = ASRUtils::fetch_ArrayConstant_value(al, x, i);
            } catch (const std::exception &) {
                unsupported("this array constant is", loc);
            }
            if (i) data += ", ";
            data += "(" + lower(el).src + ")";
        }
        data += "]";
        ASR::dimension_t *dims = nullptr;
        size_t nd = ASRUtils::extract_dimensions_from_ttype(x.m_type, dims);
        std::string lbs = "#[";
        std::string exts = "#[";
        if (nd == 0) {
            lbs += "(1 : Int)";
            exts += std::to_string(n);
        }
        for (size_t d = 0; d < nd; d++) {
            if (d) { lbs += ", "; exts += ", "; }
            if (dims[d].m_start && ASR::is_a<ASR::IntegerConstant_t>(*dims[d].m_start)) {
                lbs += "(" + lower(dims[d].m_start).src + ").toInt";
            } else {
                lbs += "(1 : Int)";
            }
            if (dims[d].m_length && ASR::is_a<ASR::IntegerConstant_t>(*dims[d].m_length)) {
                exts += "(" + lower(dims[d].m_length).src + ").toInt.toNat";
            } else if (nd == 1) {
                exts += std::to_string(n);
            } else {
                unsupported("this array constant is", loc);
            }
        }
        lbs += "]";
        exts += "]";
        return {"({ lbs := " + lbs + ", exts := " + exts + ", data := " + data
            + " } : " + lean_type(x.m_type, loc,
                dt_of_expr(const_cast<ASR::expr_t*>(&x.base))) + ")", epAtom};
    }

    Val lower_array_ctor(const ASR::ArrayConstructor_t &x) {
        const Location &loc = x.base.base.loc;
        std::string data = "#[";
        for (size_t i = 0; i < x.n_args; i++) {
            if (is_arr(ASRUtils::expr_type(x.m_args[i]))
                    || ASR::is_a<ASR::ImpliedDoLoop_t>(*x.m_args[i])) {
                unsupported("this array constructor is", loc);
            }
            if (i) data += ", ";
            data += "(" + lower(x.m_args[i]).src + ")";
        }
        data += "]";
        std::string ty = lean_type(x.m_type, loc,
            dt_of_expr(const_cast<ASR::expr_t*>(&x.base)));
        return {"({ lbs := #[(1 : Int)], exts := #[" + std::to_string(x.n_args)
            + "], data := " + data + " } : " + ty + ")", epAtom};
    }

    std::string shown(ASR::expr_t *e) {
        Val v = lower(e);
        std::string src = bind_val(v, "lfP");
        return show_src(src, ASRUtils::expr_type(e), dt_of_expr(e), e->base.loc);
    }

    Val lower_member(const ASR::StructInstanceMember_t &x) {
        const Location &loc = x.base.base.loc;
        ASR::symbol_t *mem = ASRUtils::symbol_get_past_external(x.m_m);
        if (ASR::is_a<ASR::Struct_t>(*mem)) {
            // `x%parent` is the parent subobject. The child structure stores
            // those components as its own fields.
            ASR::Struct_t *parent = ASR::down_cast<ASR::Struct_t>(mem);
            Val b = lower(x.m_v);
            std::vector<DtField> fields = fields_of(parent);
            if (fields.empty()) {
                return {"({} : " + struct_ref(parent) + ")", epAtom};
            }
            std::string s = "({";
            for (size_t i = 0; i < fields.size(); i++) {
                if (i) s += ", ";
                s += fields[i].name + " := (" + b.src + ")." + fields[i].name;
            }
            s += "} : " + struct_ref(parent) + ")";
            return {s, epAtom};
        }
        if (!ASR::is_a<ASR::Variable_t>(*mem)) {
            unsupported("this component is", loc);
        }
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(mem);
        Val b = lower(x.m_v);
        return {"(" + b.src + ")." + lean_name(v->m_name, v->base.base.loc),
            epAtom};
    }

    Val lower_struct_ctor(ASR::symbol_t *dt, ASR::call_arg_t *args, size_t n,
            const Location &loc) {
        if (!dt) unsupported("this structure constructor is", loc);
        dt = ASRUtils::symbol_get_past_external(dt);
        if (!ASR::is_a<ASR::Struct_t>(*dt)) {
            unsupported("this structure constructor is", loc);
        }
        ASR::Struct_t *st = ASR::down_cast<ASR::Struct_t>(dt);
        std::vector<DtField> fields = fields_of(st);
        if (n > fields.size()) {
            unsupported("this structure constructor is", loc);
        }
        if (fields.empty()) {
            return {"({} : " + struct_ref(st) + ")", epAtom};
        }
        std::string s = "({";
        for (size_t i = 0; i < fields.size(); i++) {
            if (i) s += ", ";
            ASR::Variable_t *v = fields[i].var;
            std::string rhs;
            if (i < n && args[i].m_value) {
                rhs = lower(args[i].m_value).src;
            } else if (v->m_value) {
                rhs = lower(v->m_value).src;
            } else {
                rhs = zero_value(v->m_type, loc, v->m_type_declaration);
            }
            if (!is_arr(v->m_type)) {
                rhs = fit_string(v->m_type, rhs, "");
            }
            s += fields[i].name + " := " + rhs;
        }
        s += "} : " + struct_ref(st) + ")";
        return {s, epAtom};
    }

    void store_member(const ASR::StructInstanceMember_t &x,
            const std::string &value, const Location &loc) {
        ASR::symbol_t *mem = ASRUtils::symbol_get_past_external(x.m_m);
        ASR::symbol_t *base_dt = dt_of_expr(x.m_v);
        if (!base_dt || !ASR::is_a<ASR::Struct_t>(*base_dt)) {
            unsupported("assignment to this component is", loc);
        }
        ASR::Struct_t *bst = ASR::down_cast<ASR::Struct_t>(base_dt);
        std::string base = bind_val(lower(x.m_v), "lfS");
        std::string updated;
        if (ASR::is_a<ASR::Struct_t>(*mem)) {
            std::vector<DtField> fields = fields_of(
                ASR::down_cast<ASR::Struct_t>(mem));
            updated = "({ " + base + " with ";
            if (fields.empty()) {
                updated = "({} : " + struct_ref(bst) + ")";
            } else {
                for (size_t i = 0; i < fields.size(); i++) {
                    if (i) updated += ", ";
                    updated += fields[i].name + " := (" + value + ")."
                        + fields[i].name;
                }
                updated += " } : " + struct_ref(bst) + ")";
            }
        } else if (ASR::is_a<ASR::Variable_t>(*mem)) {
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(mem);
            std::string rhs = value;
            if (!is_arr(v->m_type)) rhs = fit_string(v->m_type, value, "");
            updated = "({ " + base + " with "
                + lean_name(v->m_name, v->base.base.loc) + " := (" + rhs
                + ") } : " + struct_ref(bst) + ")";
        } else {
            unsupported("assignment to this component is", loc);
        }
        store(x.m_v, updated, loc);
    }

    bool exit_like(ASR::stmt_t *s) {
        return ASR::is_a<ASR::Exit_t>(*s) || ASR::is_a<ASR::Cycle_t>(*s)
            || ASR::is_a<ASR::Return_t>(*s) || ASR::is_a<ASR::Stop_t>(*s)
            || ASR::is_a<ASR::ErrorStop_t>(*s);
    }

    void leave_check() {
        if (loops.empty()) return;
        std::string cond;
        for (size_t i = 0; i < loops.size(); i++) {
            if (i) cond += " || ";
            cond += loops[i].flag;
        }
        out += indent() + "if " + cond + " then break\n";
    }

    void emit_body(ASR::stmt_t **body, size_t n) {
        if (n == 0) {
            out += indent() + "pure ()\n";
            return;
        }
        for (size_t i = 0; i < n; i++) {
            emit_stmt(body[i]);
            if (!loops.empty() && !exit_like(body[i])) leave_check();
        }
    }

    void stmts(ASR::stmt_t **body, size_t n) {
        indent_level++;
        emit_body(body, n);
        indent_level--;
    }

    void emit_halt(ASR::expr_t *code, bool error, const Location &loc) {
        if (!code) {
            out += indent() + "LFortran.Exec.halt " + (error ? "1" : "0") + "\n";
            return;
        }
        if (ASR::is_a<ASR::String_t>(*past_wrappers(
                elem_type(ASRUtils::expr_type(code))))) {
            Val s = lower(code);
            out += indent() + "IO.eprintln " + exec_atom(s) + "\n";
            out += indent() + "LFortran.Exec.halt " + (error ? "1" : "0") + "\n";
            return;
        }
        if (ASR::is_a<ASR::Integer_t>(*past_wrappers(
                elem_type(ASRUtils::expr_type(code))))) {
            if (ASR::is_a<ASR::IntegerConstant_t>(*code) && !error) {
                int64_t n = ASR::down_cast<ASR::IntegerConstant_t>(code)->m_n;
                if (n >= 0 && n <= 255) {
                    out += indent() + "LFortran.Exec.halt " + std::to_string(n) + "\n";
                    return;
                }
            }
            Val c = lower(code);
            out += indent() + "LFortran.Exec.halt (LFortran.Exec.stopCode ("
                + c.src + ").toInt)\n";
            return;
        }
        unsupported("this stop code is", loc);
    }

    std::string any_leave() {
        std::string cond;
        for (size_t i = 0; i < loops.size(); i++) {
            if (i) cond += " || ";
            cond += loops[i].flag;
        }
        return cond;
    }

    template <typename Body>
    void emit_counted(const ASR::do_loop_head_t &h, const char *loop_name,
            const Location &loc, Body body) {
        if (!h.m_v || !ASR::is_a<ASR::Var_t>(*h.m_v) || !h.m_start || !h.m_end) {
            unsupported("this do loop is", loc);
        }
        ASR::Variable_t *iv = ASRUtils::EXPR2VAR(h.m_v);
        if (!ASR::is_a<ASR::Integer_t>(*past_wrappers(iv->m_type))) {
            unsupported("this do loop is", loc);
        }
        std::string vn = var_lean(iv);
        std::string lo = bind_val(lower(h.m_start), "lfLo");
        std::string hi = bind_val(lower(h.m_end), "lfHi");
        std::string st;
        if (h.m_increment) st = bind_val(lower(h.m_increment), "lfSt");
        else {
            st = fresh("lfSt");
            out += indent() + "let " + st + " := " + of_int(iv->m_type, 1, loc) + "\n";
        }
        std::string flag = "lfLeave" + std::to_string(leave_n++);
        out += indent() + "let mut " + flag + " := false\n";
        out += indent() + vn + " := " + lo + "\n";
        std::string trip = fresh("lfTrip");
        out += indent() + "let " + trip + " := LFortran.Exec.tripCount ("
            + lo + ").toInt (" + hi + ").toInt (" + st + ").toInt\n";
        std::string k = fresh("lfK");
        out += indent() + "let mut " + k + " : Nat := 0\n";
        loops.push_back({loop_name ? loop_name : "", flag});
        std::string cond = k + " < " + trip;
        for (const LoopFrame &lp : loops) cond += " && !" + lp.flag;
        out += indent() + "while " + cond + " do\n";
        indent_level++;
        out += indent() + k + " := " + k + " + 1\n";
        out += indent() + "while true do\n";
        indent_level++;
        body();
        out += indent() + "break\n";
        indent_level--;
        out += indent() + "if " + any_leave() + " then break\n";
        out += indent() + vn + " := " + vn + " + " + st + "\n";
        indent_level--;
        loops.pop_back();
    }

    void emit_do(const ASR::DoLoop_t &x) {
        if (x.n_orelse) unsupported("a do loop with this ending is", x.base.base.loc);
        emit_counted(x.m_head, x.m_name, x.base.base.loc, [&]() {
            emit_body(x.m_body, x.n_body);
        });
    }

    void emit_doconcurrent(const ASR::DoConcurrentLoop_t &x) {
        if (x.n_reduction || x.n_head == 0) {
            unsupported("this do concurrent is", x.base.base.loc);
        }
        std::function<void(size_t)> nest = [&](size_t i) {
            if (i == x.n_head) {
                emit_body(x.m_body, x.n_body);
                return;
            }
            emit_counted(x.m_head[i], nullptr, x.base.base.loc, [&]() { nest(i + 1); });
        };
        nest(0);
    }

    void emit_while(const ASR::WhileLoop_t &x) {
        const Location &loc = x.base.base.loc;
        if (x.n_orelse) unsupported("this do while is", loc);
        size_t mark = out.size();
        Val t = lower(x.m_test);
        if (out.size() != mark) {
            unsupported("a loop test with a side effect is", loc);
        }
        std::string flag = "lfLeave" + std::to_string(leave_n++);
        out += indent() + "let mut " + flag + " := false\n";
        std::string name = x.m_name ? x.m_name : "";
        loops.push_back({name, flag});
        std::string cond = "(" + t.src + ")";
        for (const LoopFrame &lp : loops) cond += " && !" + lp.flag;
        out += indent() + "while " + cond + " do\n";
        indent_level++;
        out += indent() + "while true do\n";
        indent_level++;
        emit_body(x.m_body, x.n_body);
        out += indent() + "break\n";
        indent_level--;
        out += indent() + "if " + any_leave() + " then break\n";
        indent_level--;
        loops.pop_back();
    }

    std::string case_cond(const std::string &scr, ASR::case_stmt_t *c,
            bool strings) {
        if (ASR::is_a<ASR::CaseStmt_t>(*c)) {
            auto &cs = *ASR::down_cast<ASR::CaseStmt_t>(c);
            if (cs.m_fall_through) {
                unsupported("a fall-through case is", c->base.loc);
            }
            std::string cond;
            for (size_t i = 0; i < cs.n_test; i++) {
                if (i) cond += " || ";
                Val v = lower(cs.m_test[i]);
                if (strings) {
                    cond += "LFortran.Exec.strEq " + scr + " " + exec_atom(v);
                } else {
                    cond += scr + " == (" + v.src + ")";
                }
            }
            if (cond.empty()) cond = "false";
            return cond;
        }
        auto &cs = *ASR::down_cast<ASR::CaseStmt_Range_t>(c);
        std::string cond = "true";
        if (cs.m_start) {
            Val v = lower(cs.m_start);
            cond = scr + " >= (" + v.src + ")";
        }
        if (cs.m_end) {
            Val v = lower(cs.m_end);
            std::string hi = scr + " <= (" + v.src + ")";
            cond = cond == "true" ? hi : "(" + cond + ") && (" + hi + ")";
        }
        return cond;
    }

    void emit_select(const ASR::Select_t &x) {
        Val t = lower(x.m_test);
        std::string scr = bind_val(t, "lfCase");
        bool strings = ASR::is_a<ASR::String_t>(*past_wrappers(
            elem_type(ASRUtils::expr_type(x.m_test))));
        bool first = true;
        for (size_t i = 0; i < x.n_body; i++) {
            std::string cond = case_cond(scr, x.m_body[i], strings);
            out += indent() + (first ? "if " : "else if ") + cond + " then\n";
            first = false;
            if (ASR::is_a<ASR::CaseStmt_t>(*x.m_body[i])) {
                auto &cs = *ASR::down_cast<ASR::CaseStmt_t>(x.m_body[i]);
                stmts(cs.m_body, cs.n_body);
            } else {
                auto &cs = *ASR::down_cast<ASR::CaseStmt_Range_t>(x.m_body[i]);
                stmts(cs.m_body, cs.n_body);
            }
        }
        if (x.n_default) {
            out += indent() + (first ? "if true then\n" : "else\n");
            stmts(x.m_default, x.n_default);
        } else if (first) {
            out += indent() + "pure ()\n";
        }
    }

    bool section_scalar(const ASR::array_index_t &ix) {
        return !ix.m_left && !ix.m_step && ix.m_right;
    }

    std::string section_bound(ASR::expr_t *e, const std::string &fallback) {
        if (!e) return fallback;
        return "(" + lower(e).src + ").toInt";
    }

    Val lower_section(const ASR::ArraySection_t &x) {
        const Location &loc = x.base.base.loc;
        int range_at = -1;
        for (size_t i = 0; i < x.n_args; i++) {
            if (!section_scalar(x.m_args[i])) {
                if (range_at >= 0) unsupported("this array section is", loc);
                range_at = (int) i;
            }
        }
        if (range_at < 0) unsupported("this array section is", loc);
        std::string base = bind_val(lower(skip_phys(x.m_v)), "lfB");
        const ASR::array_index_t &rg = x.m_args[range_at];
        std::string lo = section_bound(rg.m_left,
            "LFortran.Exec.Arr.lbound " + base + " " + std::to_string(range_at + 1));
        std::string hi = section_bound(rg.m_right,
            "LFortran.Exec.Arr.ubound " + base + " " + std::to_string(range_at + 1));
        std::string st = section_bound(rg.m_step, "(1 : Int)");
        ASR::symbol_t *sdt = dt_of_expr(skip_phys(x.m_v));
        std::string et = lean_scalar(elem_type(x.m_type), loc, sdt);
        std::string idx = "#[";
        for (size_t i = 0; i < x.n_args; i++) {
            if (i) idx += ", ";
            if ((int) i == range_at) idx += "i";
            else idx += "(" + lower(x.m_args[i].m_right).src + ").toInt";
        }
        idx += "]";
        std::string tmp = fresh("lfSec");
        out += indent() + "let " + tmp + " : Array " + et + " := Id.run do\n";
        out += indent() + "  let mut d : Array " + et + " := #[]\n";
        out += indent() + "  let mut i : Int := " + lo + "\n";
        out += indent() + "  let hi : Int := " + hi + "\n";
        out += indent() + "  let st : Int := " + st + "\n";
        out += indent() + "  while (if st > 0 then i <= hi else i >= hi) do\n";
        out += indent() + "    d := d.push (LFortran.Exec.Arr.get " + base
            + " " + idx + ")\n";
        out += indent() + "    i := i + st\n";
        out += indent() + "  d\n";
        std::string ty = lean_type(x.m_type, loc, sdt);
        return {"({ lbs := #[(1 : Int)], exts := #[" + tmp + ".size], data := "
            + tmp + " } : " + ty + ")", epAtom};
    }

    void store_section(const ASR::ArraySection_t &x, const std::string &value,
            const Location &loc) {
        int range_at = -1;
        for (size_t i = 0; i < x.n_args; i++) {
            if (!section_scalar(x.m_args[i])) {
                if (range_at >= 0) unsupported("this array section is", loc);
                range_at = (int) i;
            }
        }
        if (range_at < 0) unsupported("this array section is", loc);
        ASR::expr_t *base_e = skip_phys(x.m_v);
        if (!ASR::is_a<ASR::Var_t>(*base_e)) {
            unsupported("assignment to this array section is", loc);
        }
        ASR::Variable_t *v = ASRUtils::EXPR2VAR(base_e);
        std::string val = fresh("lfV");
        out += indent() + "let " + val + " := " + value + "\n";
        std::string lo = section_bound(x.m_args[range_at].m_left,
            "LFortran.Exec.Arr.lbound " + load_var(v) + " "
            + std::to_string(range_at + 1));
        // load_var may emit a get; the lbound fallback above used a second
        // load. Re-read once into a name when escaped.
        std::string cur = load_var(v);
        lo = section_bound(x.m_args[range_at].m_left,
            "LFortran.Exec.Arr.lbound " + cur + " " + std::to_string(range_at + 1));
        std::string hi = section_bound(x.m_args[range_at].m_right,
            "LFortran.Exec.Arr.ubound " + cur + " " + std::to_string(range_at + 1));
        std::string st = section_bound(x.m_args[range_at].m_step, "(1 : Int)");
        std::string i = fresh("lfI");
        std::string k = fresh("lfJ");
        std::string hiN = fresh("lfHi");
        std::string stN = fresh("lfSt");
        out += indent() + "let mut " + i + " : Int := " + lo + "\n";
        out += indent() + "let " + hiN + " : Int := " + hi + "\n";
        out += indent() + "let " + stN + " : Int := " + st + "\n";
        out += indent() + "let mut " + k + " : Int := 1\n";
        out += indent() + "while (if " + stN + " > 0 then " + i + " <= " + hiN
            + " else " + i + " >= " + hiN + ") do\n";
        indent_level++;
        std::string idx = "#[";
        for (size_t d = 0; d < x.n_args; d++) {
            if (d) idx += ", ";
            if ((int) d == range_at) idx += i;
            else idx += "(" + lower(x.m_args[d].m_right).src + ").toInt";
        }
        idx += "]";
        std::string elem = "LFortran.Exec.Arr.get " + val + " #[" + k + "]";
        std::string updated = "LFortran.Exec.Arr.set " + cur + " " + idx
            + " (" + elem + ")";
        save_var(v, updated);
        out += indent() + i + " := " + i + " + " + stN + "\n";
        out += indent() + k + " := " + k + " + 1\n";
        if (var_escaped(v)) {
            cur = load_var(v);
        }
        indent_level--;
    }

    void emit_allocate(const ASR::Allocate_t &x) {
        const Location &loc = x.base.base.loc;
        if (x.m_source) {
            Val s = lower(x.m_source);
            for (size_t i = 0; i < x.n_args; i++) {
                store(x.m_args[i].m_a, s.src, loc);
            }
        } else {
            for (size_t i = 0; i < x.n_args; i++) {
                const ASR::alloc_arg_t &a = x.m_args[i];
                if (a.n_dims == 0) continue;
                std::string lbs = "#[";
                std::string exts = "#[";
                for (size_t d = 0; d < a.n_dims; d++) {
                    if (d) { lbs += ", "; exts += ", "; }
                    if (!a.m_dims[d].m_length) {
                        unsupported("this allocate is", loc);
                    }
                    lbs += a.m_dims[d].m_start
                        ? "(" + lower(a.m_dims[d].m_start).src + ").toInt"
                        : "(1 : Int)";
                    exts += "(" + lower(a.m_dims[d].m_length).src + ").toInt.toNat";
                }
                lbs += "]";
                exts += "]";
                ASR::ttype_t *et = elem_type(ASRUtils::expr_type(a.m_a));
                store(a.m_a, "LFortran.Exec.Arr.make " + lbs + " " + exts
                    + " (" + scalar_zero(et, loc, dt_of_expr(a.m_a)) + ")", loc);
            }
        }
        if (x.m_stat) {
            store(x.m_stat, of_int(ASRUtils::expr_type(x.m_stat), 0, loc), loc);
        }
    }

    void emit_deallocate(const ASR::ExplicitDeallocate_t &x) {
        for (size_t i = 0; i < x.n_vars; i++) {
            ASR::expr_t *v = x.m_vars[i];
            if (!is_arr(ASRUtils::expr_type(v))) continue;
            store(v, "(LFortran.Exec.Arr.empty : "
                + lean_type(ASRUtils::expr_type(v), v->base.loc, dt_of_expr(v))
                + ")",
                v->base.loc);
        }
    }

    void emit_write(const ASR::FileWrite_t &x) {
        const Location &loc = x.base.base.loc;
        if (x.m_overloaded || x.m_nml || x.m_rec || !x.m_is_formatted) {
            unsupported("this write statement is", loc);
        }
        bool stdout_unit = !x.m_unit;
        if (x.m_unit && ASR::is_a<ASR::IntegerConstant_t>(*x.m_unit)) {
            int64_t n = ASR::down_cast<ASR::IntegerConstant_t>(x.m_unit)->m_n;
            stdout_unit = n == 6 || n == 0;
        }
        if (!stdout_unit) unsupported("this write statement is", loc);
        std::string parts;
        for (size_t i = 0; i < x.n_values; i++) {
            if (i) parts += ", ";
            parts += shown(x.m_values[i]);
        }
        if (x.n_values == 0) out += indent() + "IO.println \"\"\n";
        else out += indent() + "IO.println (String.intercalate \" \" ["
            + parts + "])\n";
        if (x.m_iostat) {
            store(x.m_iostat, of_int(ASRUtils::expr_type(x.m_iostat), 0, loc), loc);
        }
    }

    void emit_block(SymbolTable *st, ASR::stmt_t **body, size_t n) {
        scope_stack.push_back(scope);
        scope = st;
        emit_locals(st, nullptr);
        emit_body(body, n);
        scope = scope_stack.back();
        scope_stack.pop_back();
    }

    // The mask is evaluated once. Each assignment is evaluated as a whole
    // array, then written back only where the mask is true (or false, in
    // the elsewhere arm). Nested where and non-assignment statements are
    // rejected.
    void emit_where(const ASR::Where_t &x) {
        const Location &loc = x.base.base.loc;
        if (!is_arr(ASRUtils::expr_type(x.m_test))) {
            Val t = lower(x.m_test);
            out += indent() + "if " + t.src + " then\n";
            stmts(x.m_body, x.n_body);
            if (x.n_orelse) {
                out += indent() + "else\n";
                stmts(x.m_orelse, x.n_orelse);
            }
            return;
        }
        std::string mask = bind_val(lower(x.m_test), "lfM");
        auto arm = [&](ASR::stmt_t **body, size_t n, bool keep) {
            for (size_t i = 0; i < n; i++) {
                if (!ASR::is_a<ASR::Assignment_t>(*body[i])) {
                    unsupported("this where statement is", body[i]->base.loc);
                }
                auto &as = *ASR::down_cast<ASR::Assignment_t>(body[i]);
                if (as.m_overloaded) {
                    unsupported("defined assignment is", as.base.base.loc);
                }
                ASR::expr_t *tgt = skip_phys(as.m_target);
                if (!is_arr(ASRUtils::expr_type(tgt))) {
                    unsupported("this where statement is", as.base.base.loc);
                }
                Val rv = lower(as.m_value);
                std::string old = bind_val(lower(tgt), "lfW");
                std::string neu;
                if (value_is_array(as.m_value)) {
                    neu = bind_val(rv, "lfN");
                } else {
                    neu = fresh("lfN");
                    out += indent() + "let " + neu
                        + " := LFortran.Exec.Arr.fill " + old + " ("
                        + rv.src + ")\n";
                }
                std::string data = fresh("lfD");
                std::string idx = fresh("lfI");
                std::string bit = "(" + mask + ".data[" + idx + "]!)";
                std::string cond = keep ? bit : "(!" + bit + ")";
                out += indent() + "let mut " + data + " := " + old + ".data\n";
                out += indent() + "for " + idx + " in [:" + data
                    + ".size] do\n";
                out += indent() + "  if " + cond + " then " + data + " := "
                    + data + ".set! " + idx + " " + neu + ".data[" + idx
                    + "]!\n";
                store(tgt, "{ " + old + " with data := " + data + " }",
                    as.base.base.loc);
            }
        };
        arm(x.m_body, x.n_body, true);
        arm(x.m_orelse, x.n_orelse, false);
    }

    void emit_stmt(ASR::stmt_t *s) {
        const Location &loc = s->base.loc;
        switch (s->type) {
            case ASR::stmtType::Assignment: {
                auto &x = *ASR::down_cast<ASR::Assignment_t>(s);
                if (x.m_overloaded) unsupported("defined assignment is", loc);
                Val v = lower(x.m_value);
                ASR::expr_t *tgt = skip_phys(x.m_target);
                if (is_arr(ASRUtils::expr_type(tgt))
                        && !value_is_array(x.m_value)
                        && (assignable(tgt)
                            || ASR::is_a<ASR::ArraySection_t>(*tgt))) {
                    Val cur = lower(tgt);
                    store(tgt, "LFortran.Exec.Arr.fill (" + cur.src + ") ("
                        + v.src + ")", loc);
                    break;
                }
                store(x.m_target, v.src, loc);
                break;
            }
            case ASR::stmtType::If: {
                auto &x = *ASR::down_cast<ASR::If_t>(s);
                Val t = lower(x.m_test);
                out += indent() + "if " + t.src + " then\n";
                stmts(x.m_body, x.n_body);
                if (x.n_orelse) {
                    out += indent() + "else\n";
                    stmts(x.m_orelse, x.n_orelse);
                }
                break;
            }
            case ASR::stmtType::DoLoop:
                emit_do(*ASR::down_cast<ASR::DoLoop_t>(s));
                break;
            case ASR::stmtType::DoConcurrentLoop:
                emit_doconcurrent(*ASR::down_cast<ASR::DoConcurrentLoop_t>(s));
                break;
            case ASR::stmtType::WhileLoop:
                emit_while(*ASR::down_cast<ASR::WhileLoop_t>(s));
                break;
            case ASR::stmtType::Where:
                emit_where(*ASR::down_cast<ASR::Where_t>(s));
                break;
            case ASR::stmtType::Exit: {
                auto &x = *ASR::down_cast<ASR::Exit_t>(s);
                if (loops.empty()) unsupported("an exit outside a loop is", loc);
                std::string flag = loops.back().flag;
                if (x.m_stmt_name && x.m_stmt_name[0]) {
                    bool found = false;
                    for (int i = (int) loops.size() - 1; i >= 0; i--) {
                        if (loops[i].name == x.m_stmt_name) {
                            flag = loops[i].flag;
                            found = true;
                            break;
                        }
                    }
                    if (!found) unsupported("this exit is", loc);
                }
                out += indent() + flag + " := true\n";
                out += indent() + "break\n";
                break;
            }
            case ASR::stmtType::Cycle: {
                auto &x = *ASR::down_cast<ASR::Cycle_t>(s);
                if (loops.empty()) unsupported("a cycle outside a loop is", loc);
                if (x.m_stmt_name && x.m_stmt_name[0]
                        && loops.back().name != x.m_stmt_name) {
                    unsupported("a named cycle of an outer loop is", loc);
                }
                out += indent() + "break\n";
                break;
            }
            case ASR::stmtType::Return: {
                if (!proc || proc->ret_expr.empty()) {
                    out += indent() + (proc ? "return\n" : "return 0\n");
                } else {
                    out += indent() + "return " + proc->ret_expr + "\n";
                }
                break;
            }
            case ASR::stmtType::Print: {
                auto &x = *ASR::down_cast<ASR::Print_t>(s);
                if (ASR::is_a<ASR::StringFormat_t>(*x.m_text)) {
                    auto &f = *ASR::down_cast<ASR::StringFormat_t>(x.m_text);
                    if (f.m_fmt) unsupported("formatted output is", loc);
                    std::string parts;
                    for (size_t i = 0; i < f.n_args; i++) {
                        if (i) parts += ", ";
                        parts += shown(f.m_args[i]);
                    }
                    if (f.n_args == 0) {
                        out += indent() + "IO.println \"\"\n";
                    } else {
                        out += indent() + "IO.println (String.intercalate \" \" ["
                            + parts + "])\n";
                    }
                } else {
                    Val t = lower(x.m_text);
                    out += indent() + "IO.println " + exec_atom(t) + "\n";
                }
                break;
            }
            case ASR::stmtType::ErrorStop: {
                auto &x = *ASR::down_cast<ASR::ErrorStop_t>(s);
                emit_halt(x.m_code, true, loc);
                break;
            }
            case ASR::stmtType::Stop: {
                auto &x = *ASR::down_cast<ASR::Stop_t>(s);
                emit_halt(x.m_code, false, loc);
                break;
            }
            case ASR::stmtType::SubroutineCall: {
                auto &x = *ASR::down_cast<ASR::SubroutineCall_t>(s);
                if (x.m_dt) unsupported("a type-bound call is", loc);
                user_call(x.m_name, x.m_args, x.n_args, true, nullptr, loc);
                break;
            }
            case ASR::stmtType::Select:
                emit_select(*ASR::down_cast<ASR::Select_t>(s));
                break;
            case ASR::stmtType::Expr: {
                lower(ASR::down_cast<ASR::Expr_t>(s)->m_expression);
                break;
            }
            case ASR::stmtType::Assert: {
                auto &x = *ASR::down_cast<ASR::Assert_t>(s);
                Val t = lower(x.m_test);
                out += indent() + "if (!" + exec_atom(t)
                    + ") then LFortran.Exec.halt 1\n";
                break;
            }
            case ASR::stmtType::ImplicitDeallocate:
            case ASR::stmtType::GoToTarget:
                break;
            case ASR::stmtType::Allocate:
                emit_allocate(*ASR::down_cast<ASR::Allocate_t>(s));
                break;
            case ASR::stmtType::ExplicitDeallocate:
                emit_deallocate(*ASR::down_cast<ASR::ExplicitDeallocate_t>(s));
                break;
            case ASR::stmtType::FileWrite:
                emit_write(*ASR::down_cast<ASR::FileWrite_t>(s));
                break;
            case ASR::stmtType::BlockCall: {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                if (!ASR::is_a<ASR::Block_t>(*b)) {
                    unsupported("this block is", loc);
                }
                emit_block(ASR::down_cast<ASR::Block_t>(b)->m_symtab,
                    ASR::down_cast<ASR::Block_t>(b)->m_body,
                    ASR::down_cast<ASR::Block_t>(b)->n_body);
                break;
            }
            case ASR::stmtType::AssociateBlockCall: {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::AssociateBlockCall_t>(s)->m_m);
                if (!ASR::is_a<ASR::AssociateBlock_t>(*b)) {
                    unsupported("this associate is", loc);
                }
                auto *ab = ASR::down_cast<ASR::AssociateBlock_t>(b);
                emit_block(ab->m_symtab, ab->m_body, ab->n_body);
                break;
            }
            case ASR::stmtType::Associate: {
                // Decision: pointer assignment copies the value. The two
                // names are not aliases afterwards.
                auto &x = *ASR::down_cast<ASR::Associate_t>(s);
                Val v = lower(x.m_value);
                store(x.m_target, v.src, loc);
                break;
            }
            default:
                unsupported("statement (" + std::to_string((int) s->type)
                    + ") is", loc);
        }
    }

    bool ok_fn(ASR::Function_t *fn) {
        ASR::FunctionType_t *ft = ASRUtils::get_FunctionType(fn);
        if (ft->m_deftype != ASR::deftypeType::Implementation) return false;
        return ft->m_abi == ASR::abiType::Source
            || ft->m_abi == ASR::abiType::LFortranModule;
    }

    void build_proc(ASR::Function_t *fn, ProcInfo &p) {
        p.fn = fn;
        p.has_result = fn->m_return_var != nullptr;
        ASR::Variable_t *result = nullptr;
        if (p.has_result && ASR::is_a<ASR::Var_t>(*fn->m_return_var)) {
            ASR::symbol_t *rs = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(fn->m_return_var)->m_v);
            if (ASR::is_a<ASR::Variable_t>(*rs)) {
                result = ASR::down_cast<ASR::Variable_t>(rs);
            }
        }
        if (p.has_result && !result) {
            unsupported("this function result is", fn->base.base.loc);
        }
        const Location &loc = fn->base.base.loc;
        for (size_t i = 0; i < fn->n_args; i++) {
            if (!ASR::is_a<ASR::Var_t>(*fn->m_args[i])) {
                unsupported("a procedure argument is", fn->m_args[i]->base.loc);
            }
            ASR::symbol_t *as = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v);
            if (!ASR::is_a<ASR::Variable_t>(*as)) {
                unsupported("a procedure dummy is", fn->m_args[i]->base.loc);
            }
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(as);
            if (result && v == result) continue;
            ArgPlan a;
            a.var = v;
            if (v->m_intent == ASR::intentType::In) {
                a.pass_in = true; a.pass_out = false;
            } else if (v->m_intent == ASR::intentType::Out) {
                a.pass_in = false; a.pass_out = true;
            } else {
                a.pass_in = true; a.pass_out = true;
            }
            // The caller owns the shape of an assumed-shape or deferred dummy.
            if (!a.pass_in && deferred_shape(v->m_type)) a.pass_in = true;
            p.args.push_back(a);
        }
        std::vector<std::string> types, exprs;
        if (p.has_result) {
            types.push_back(lean_type(result->m_type, loc,
                result->m_type_declaration));
            exprs.push_back("lfResult");
        }
        for (const ArgPlan &a : p.args) {
            if (!a.pass_out) continue;
            types.push_back(lean_type(a.var->m_type, a.var->base.base.loc,
                a.var->m_type_declaration));
            exprs.push_back(lean_name(a.var->m_name, a.var->base.base.loc));
        }
        if (types.empty()) {
            p.ret_type = "Unit";
            p.ret_expr = "";
        } else if (types.size() == 1) {
            p.ret_type = types[0];
            p.ret_expr = exprs[0];
        } else {
            p.ret_type = types[0];
            p.ret_expr = "(" + exprs[0];
            for (size_t i = 1; i < types.size(); i++) {
                p.ret_type += " × " + types[i];
                p.ret_expr += ", " + exprs[i];
            }
            p.ret_expr += ")";
        }
    }

    void scan_blocks(SymbolTable *st, EffectScan &scan) {
        for (ASR::symbol_t *s : exec_symbols_in_order(st)) {
            ASR::stmt_t **body = nullptr;
            size_t n = 0;
            SymbolTable *nested = nullptr;
            if (ASR::is_a<ASR::Block_t>(*s)) {
                auto *b = ASR::down_cast<ASR::Block_t>(s);
                body = b->m_body; n = b->n_body; nested = b->m_symtab;
            } else if (ASR::is_a<ASR::AssociateBlock_t>(*s)) {
                auto *b = ASR::down_cast<ASR::AssociateBlock_t>(s);
                body = b->m_body; n = b->n_body; nested = b->m_symtab;
            } else {
                continue;
            }
            for (size_t i = 0; i < n; i++) scan.visit_stmt(*body[i]);
            if (nested) scan_blocks(nested, scan);
        }
    }

    void analyze(ASR::Function_t *fn) {
        if (!ok_fn(fn) || info.count((ASR::symbol_t*) fn)) return;
        ProcInfo p;
        build_proc(fn, p);
        EffectScan scan;
        scan.fn_scope = fn->m_symtab;
        scan.escaped = &escaped;
        for (size_t i = 0; i < fn->n_body; i++) scan.visit_stmt(*fn->m_body[i]);
        scan_blocks(fn->m_symtab, scan);
        p.io = scan.direct;
        p.callees = std::move(scan.calls);
        info[(ASR::symbol_t*) fn] = std::move(p);
        for (ASR::symbol_t *s : exec_symbols_in_order(fn->m_symtab)) {
            if (ASR::is_a<ASR::Function_t>(*s)) {
                analyze(ASR::down_cast<ASR::Function_t>(s));
            }
        }
    }

    void mark_escaped_owners() {
        for (ASR::Variable_t *v : escaped) {
            for (auto &kv : info) {
                if (kv.second.fn->m_symtab == v->m_parent_symtab) {
                    kv.second.io = true;
                }
            }
        }
    }

    void emit_var(ASR::Variable_t *v, bool inout_shadow, bool out_only) {
        const Location &loc = v->base.base.loc;
        std::string name = var_lean(v);
        ASR::symbol_t *dt = v->m_type_declaration;
        std::string ty = lean_type(v->m_type, loc, dt);
        if (var_escaped(v)) {
            std::string rhs;
            ASR::expr_t *init = v->m_symbolic_value ? v->m_symbolic_value : v->m_value;
            if (init && !out_only) {
                Val iv = lower(init);
                rhs = is_arr(v->m_type) ? iv.src : fit_string(v->m_type, iv.src, "");
            } else {
                rhs = zero_value(v->m_type, loc, dt);
            }
            save_var(v, rhs);
            return;
        }
        if (inout_shadow) {
            out += indent() + "let mut " + name + " := " + name + "\n";
            return;
        }
        ASR::expr_t *init = nullptr;
        if (!out_only) {
            init = v->m_symbolic_value ? v->m_symbolic_value : v->m_value;
        }
        if (init) {
            Val iv = lower(init);
            std::string rhs = is_arr(v->m_type) ? iv.src
                : fit_string(v->m_type, iv.src, "");
            out += indent() + "let mut " + name + " : " + ty + " := " + rhs + "\n";
        } else {
            out += indent() + "let mut " + name + " : " + ty + " := "
                + zero_value(v->m_type, loc, dt) + "\n";
        }
    }

    void emit_locals(SymbolTable *st, ASR::Function_t *fn) {
        ASR::Variable_t *result = fn && fn->m_return_var
            ? ASRUtils::EXPR2VAR(fn->m_return_var) : nullptr;
        std::set<ASR::Variable_t*> dummies;
        if (fn) {
            for (const ArgPlan &a : info[(ASR::symbol_t*) fn].args) {
                dummies.insert(a.var);
            }
        }
        if (result) {
            out += indent() + "let mut lfResult : "
                + lean_type(result->m_type, result->base.base.loc,
                    result->m_type_declaration) + " := "
                + zero_value(result->m_type, result->base.base.loc,
                    result->m_type_declaration) + "\n";
        }
        for (ASR::symbol_t *s : exec_symbols_in_order(st)) {
            if (!ASR::is_a<ASR::Variable_t>(*s)) continue;
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
            if (v == result || is_param(v)) continue;
            if (v->m_storage == ASR::storage_typeType::HostAssociated) continue;
            if (dummies.count(v)) {
                bool in = false, outb = false;
                for (const ArgPlan &a : info[(ASR::symbol_t*) fn].args) {
                    if (a.var == v) { in = a.pass_in; outb = a.pass_out; }
                }
                if (in && outb) emit_var(v, true, false);
                else if (outb) emit_var(v, false, true);
                continue;
            }
            if (v->m_intent != ASR::intentType::Local
                    && v->m_intent != ASR::intentType::Unspecified
                    && v->m_intent != ASR::intentType::ReturnVar) {
                continue;
            }
            emit_var(v, false, false);
        }
    }

    void emit_fn(ProcInfo &p) {
        proc = &p;
        scope = p.fn->m_symtab;
        tmp_n = 0;
        const Location &loc = p.fn->base.base.loc;
        out += "def " + lean_name(p.fn->m_name, loc);
        for (const ArgPlan &a : p.args) {
            if (!a.pass_in) continue;
            out += " (" + lean_name(a.var->m_name, a.var->base.base.loc) + " : "
                + lean_type(a.var->m_type, a.var->base.base.loc,
                    a.var->m_type_declaration) + ")";
        }
        // An effectful procedure is an `IO` action. `Id.run` cannot perform
        // `cellGet` or `IO.println`, and a bare `do` needs a monad type.
        if (p.io) {
            out += " : IO (" + p.ret_type + ") := do\n";
        } else {
            out += " : " + p.ret_type + " := Id.run do\n";
        }
        indent_level = 1;
        emit_locals(p.fn->m_symtab, p.fn);
        emit_body(p.fn->m_body, p.fn->n_body);
        bool returned = p.fn->n_body
            && ASR::is_a<ASR::Return_t>(*p.fn->m_body[p.fn->n_body - 1]);
        if (!returned) {
            if (p.ret_expr.empty()) out += indent() + "return\n";
            else out += indent() + "return " + p.ret_expr + "\n";
        }
        indent_level = 0;
        proc = nullptr;
        scope = nullptr;
        out += "\n";
    }

    void collect_fns(SymbolTable *st, std::vector<ASR::Function_t*> &fns) {
        for (ASR::symbol_t *s : exec_symbols_in_order(st)) {
            if (!ASR::is_a<ASR::Function_t>(*s)) continue;
            ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(s);
            if (!ok_fn(fn)) continue;
            fns.push_back(fn);
            collect_fns(fn->m_symtab, fns);
        }
    }

    void emit_group(const std::vector<ASR::Function_t*> &fns,
            const std::string &ns) {
        std::vector<ProcInfo*> ps;
        for (ASR::Function_t *fn : fns) {
            auto it = info.find((ASR::symbol_t*) fn);
            if (it != info.end()) ps.push_back(&it->second);
        }
        if (ps.empty()) return;
        current_ns = ns;
        if (!ns.empty()) out += "namespace " + ns + "\n\n";
        if (ps.size() > 1) out += "mutual\n";
        for (ProcInfo *p : ps) emit_fn(*p);
        if (ps.size() > 1) out += "end\n\n";
        if (!ns.empty()) out += "end " + ns + "\n\n";
        current_ns.clear();
    }

    void note_var(ASR::Variable_t *v) {
        if (!v->m_type_declaration) return;
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(v->m_type_declaration);
        if (s && ASR::is_a<ASR::Struct_t>(*s)) {
            struct_roots.insert(ASR::down_cast<ASR::Struct_t>(s));
        }
    }

    void note_symtab(SymbolTable *st) {
        if (!st) return;
        for (ASR::symbol_t *s : exec_symbols_in_order(st)) {
            if (ASR::is_a<ASR::ExternalSymbol_t>(*s)) continue;
            if (ASR::is_a<ASR::Variable_t>(*s)) {
                note_var(ASR::down_cast<ASR::Variable_t>(s));
            } else if (ASR::is_a<ASR::Function_t>(*s)) {
                note_symtab(ASR::down_cast<ASR::Function_t>(s)->m_symtab);
            } else if (ASR::is_a<ASR::Block_t>(*s)) {
                note_symtab(ASR::down_cast<ASR::Block_t>(s)->m_symtab);
            } else if (ASR::is_a<ASR::AssociateBlock_t>(*s)) {
                note_symtab(ASR::down_cast<ASR::AssociateBlock_t>(s)->m_symtab);
            } else if (ASR::is_a<ASR::Program_t>(*s)) {
                note_symtab(ASR::down_cast<ASR::Program_t>(s)->m_symtab);
            }
        }
    }

    void emit_struct_decl(ASR::Struct_t *st) {
        if (structs_done.count(st)) return;
        if (!structs_visiting.insert(st).second) {
            unsupported("a recursive derived type is", st->base.base.loc);
        }
        if (st->m_parent) {
            ASR::symbol_t *p = ASRUtils::symbol_get_past_external(st->m_parent);
            if (ASR::is_a<ASR::Struct_t>(*p)) {
                emit_struct_decl(ASR::down_cast<ASR::Struct_t>(p));
            }
        }
        std::vector<DtField> fields = fields_of(st);
        for (const DtField &f : fields) {
            if (!f.var->m_type_declaration) continue;
            ASR::symbol_t *d = ASRUtils::symbol_get_past_external(
                f.var->m_type_declaration);
            if (d && ASR::is_a<ASR::Struct_t>(*d)) {
                emit_struct_decl(ASR::down_cast<ASR::Struct_t>(d));
            }
        }
        structs_visiting.erase(st);
        structs_done.insert(st);
        std::string ns;
        if (ASR::Module_t *m = ASRUtils::get_sym_module((ASR::symbol_t*) st)) {
            ns = lean_name(m->m_name, m->base.base.loc);
        }
        std::string saved_ns = current_ns;
        current_ns = ns;
        if (!ns.empty()) out += "namespace " + ns + "\n\n";
        out += "structure " + lean_name(st->m_name, st->base.base.loc) + " where\n";
        for (const DtField &f : fields) {
            out += "  " + f.name + " : " + lean_type(f.var->m_type,
                f.var->base.base.loc, f.var->m_type_declaration) + "\n";
        }
        out += "deriving Inhabited\n\n";
        if (!ns.empty()) out += "end " + ns + "\n\n";
        current_ns = saved_ns;
    }

    void translate(ASR::TranslationUnit_t &tu) {
        ASR::Program_t *prog = nullptr;
        std::vector<ASR::Module_t*> mods;
        std::vector<ASR::Function_t*> top;
        for (ASR::symbol_t *s : exec_symbols_in_order(tu.m_symtab)) {
            if (ASR::is_a<ASR::ExternalSymbol_t>(*s)
                    || ASR::is_a<ASR::GenericProcedure_t>(*s)
                    || ASR::is_a<ASR::CustomOperator_t>(*s)) {
                continue;
            }
            if (ASR::is_a<ASR::Program_t>(*s)) {
                if (prog) unsupported("a second program is", s->base.loc);
                prog = ASR::down_cast<ASR::Program_t>(s);
                continue;
            }
            if (ASR::is_a<ASR::Module_t>(*s)) {
                ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(s);
                if (!m->m_intrinsic) mods.push_back(m);
                continue;
            }
            if (ASR::is_a<ASR::Function_t>(*s)) {
                ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(s);
                if (ok_fn(fn)) top.push_back(fn);
                continue;
            }
            if (ASR::is_a<ASR::Variable_t>(*s)) {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
                if (is_param(v)) continue;
                unsupported("a global variable is", s->base.loc);
            }
            unsupported("this top-level entity is", s->base.loc);
        }
        if (!prog) {
            unsupported("a file with no program is", tu.base.base.loc);
        }
        for (ASR::Module_t *m : mods) {
            for (ASR::symbol_t *s : exec_symbols_in_order(m->m_symtab)) {
                if (!ASR::is_a<ASR::Variable_t>(*s)) continue;
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
                if (!is_param(v)) escaped.insert(v);
            }
        }
        for (ASR::Module_t *m : mods) {
            for (ASR::symbol_t *s : exec_symbols_in_order(m->m_symtab)) {
                if (ASR::is_a<ASR::Function_t>(*s)) {
                    analyze(ASR::down_cast<ASR::Function_t>(s));
                }
            }
        }
        for (ASR::Function_t *fn : top) analyze(fn);
        for (ASR::symbol_t *s : exec_symbols_in_order(prog->m_symtab)) {
            if (ASR::is_a<ASR::Function_t>(*s)) {
                analyze(ASR::down_cast<ASR::Function_t>(s));
            }
        }
        // Owners of escaped variables perform IO. Propagate after that, so a
        // caller of such an owner is not left as a pure `Id` action.
        mark_escaped_owners();
        bool changed = true;
        while (changed) {
            changed = false;
            for (auto &kv : info) {
                if (kv.second.io) continue;
                for (ASR::symbol_t *c : kv.second.callees) {
                    auto it = info.find(c);
                    if (it != info.end() && it->second.io) {
                        kv.second.io = true;
                        changed = true;
                        break;
                    }
                }
            }
        }
        out = "-- Generated by LFortran (`lfortran --backend=lean " + filename
            + "`). Do not edit.\nimport LFortran.Exec\n\n";
        for (ASR::Module_t *m : mods) note_symtab(m->m_symtab);
        for (ASR::Function_t *fn : top) note_symtab(fn->m_symtab);
        note_symtab(prog->m_symtab);
        {
            std::vector<ASR::Struct_t*> roots(struct_roots.begin(),
                struct_roots.end());
            for (ASR::Struct_t *st : roots) emit_struct_decl(st);
        }
        for (ASR::Module_t *m : mods) {
            for (ASR::symbol_t *s : exec_symbols_in_order(m->m_symtab)) {
                if (ASR::is_a<ASR::Enum_t>(*s) || ASR::is_a<ASR::Union_t>(*s)) {
                    unsupported("an enum or union is", s->base.loc);
                }
            }
            std::vector<ASR::Function_t*> fns;
            collect_fns(m->m_symtab, fns);
            emit_group(fns, lean_name(m->m_name, m->base.base.loc));
        }
        if (!top.empty()) emit_group(top, "");
        std::vector<ASR::Function_t*> contained;
        collect_fns(prog->m_symtab, contained);
        emit_group(contained, "");
        scope = prog->m_symtab;
        proc = nullptr;
        indent_level = 0;
        out += "def main : IO UInt32 := do\n";
        indent_level = 1;
        scope = nullptr;
        for (ASR::Module_t *m : mods) {
            for (ASR::symbol_t *s : exec_symbols_in_order(m->m_symtab)) {
                if (!ASR::is_a<ASR::Variable_t>(*s)) continue;
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
                if (!is_param(v)) emit_var(v, false, false);
            }
        }
        scope = prog->m_symtab;
        emit_locals(prog->m_symtab, nullptr);
        emit_body(prog->m_body, prog->n_body);
        out += indent() + "return 0\n";
    }
};

} // namespace

Result<std::string> asr_to_lean_exec(ASR::TranslationUnit_t &asr,
        diag::Diagnostics &diagnostics, const std::string &filename)
{
    ASRToLeanExec v;
    v.filename = filename;
    try {
        v.translate(asr);
    } catch (const ExecError &e) {
        diagnostics.diagnostics.push_back(e.d);
        return Error();
    } catch (const LCompilersException &e) {
        diagnostics.diagnostics.push_back(diag::Diagnostic(
            std::string("Lean backend: ") + e.what(),
            diag::Level::Error, diag::Stage::CodeGen,
            {diag::Label("", {asr.base.base.loc})}));
        return Error();
    }
    return v.out;
}

