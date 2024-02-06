import sys
import os

intrinsic_funcs_args = {
    "Kind": [
        {
            "args": [("int",), ("real",), ("bool",), ("char",)],
            "return": "int32"
        },
    ],
    "FlipSign": [
        {
            "args": [("int", "real")],
            "ret_type_arg_idx": 1
        }
    ],
    "FloorDiv": [
        {
            "args": [("int", "int"), ("uint", "uint"), ("real", "real"), ("bool", "bool")],
            "ret_type_arg_idx": 0
        },
    ],
    "Mod": [
        {
            "args": [("int", "int"), ("real", "real")],
            "ret_type_arg_idx": 0
        },
    ],
    "Trailz": [
        {
            "args": [("int",)],
            "ret_type_arg_idx": 0
        },
    ],
    "Hypot": [
        {
            "args": [("real", "real")],
            "ret_type_arg_idx": 0
        }
    ],
    "Digits": [
        {
            "args": [("int",), ("real",)],
            "return": "int32"
        },
    ],
    "Repeat": [
        {
            "args": [("char", "int")],
            "ret_type_arg_idx": 0
        }
    ],
    "MinExponent": [
        {
            "args": [("real",)],
            "return": "int32"
        }
    ],
    "MaxExponent": [
        {
            "args": [("real",)],
            "return": "int32"
        }
    ],
    "Partition": [
        {
            "args": [("char", "char")],
            "ret_type_arg_idx": 0
        }
    ],
    "ListReverse": [
        {
            "args": [("list",)],
            "return": "nullptr"
        }
    ],
    "Reserve": [
        {
            "args": [("list", "int")],
            "return": "nullptr"
        }
    ],
    "Sign": [
        {
            "args": [("int", "int"), ("real", "real")],
            "ret_type_arg_idx": 0
        },
    ],
    "Radix": [
        {
            "args": [("int",), ("real",)],
            "return": "int32"
        },
    ],
    "Aint": [
        {
            "args": [("real",)],
        }
    ],
    "nint": [
        {
            "args": [("real",)]
        }
    ],
    "Anint": [
        {
            "args": [("real",)],
        }
    ],
    "Floor": [
        {
            "args": [("real",)],
        }
    ],
    "Ceiling": [
        {
            "args": [("real",)],
        }
    ],
    "Sqrt": [
        {
            "args": [("real",), ("complex",)],
            "ret_type_arg_idx": 0
        },
    ],
    "Sngl": [
        {
            "args": [("real",)],
            "return": "real32"
        }
    ],
    "SignFromValue": [
        {
            "args": [("int", "int"), ("real", "real")],
            "ret_type_arg_idx": 0
        },
    ],
    "Ishft": [
        {
            "args": [("int", "int")],
            "ret_type_arg_idx": 0
        },
    ],
    "Aimag": [
        {
            "args": [("complex",)],
        },
    ],
}

skip_create_func = ["Aint", "Anint", "Nint", "Partition", "Floor", "Ceiling", "Aimag"]

type_to_asr_type_check = {
    "int": "is_integer",
    "uint": "is_unsigned_integer",
    "real": "is_real",
    "bool": "is_logical",
    "char": "is_character",
    "complex": "is_complex",
    "dict": "ASR::is_a<ASR::Dict_t>",
    "list": "ASR::is_a<ASR::List_t>",
    "tuple": "ASR::is_a<ASR::Tuple_t>"
}

intrinsic_funcs_ret_type = {
    "Kind": ["int"],
    "Partition": ["tuple"],
    "ListReverse": ["null"],
    "Reserve": [ "null"],
    "Radix": ["int"],
}

src = ""
indent = "    "

def add_verify_arg_type_src(func_name):
    global src
    arg_infos = intrinsic_funcs_args[func_name]
    no_of_args_msg = ""
    for i, arg_info in enumerate(arg_infos):
        condition = ""
        cond_in_msg = ""
        args_lists = arg_info["args"]
        no_of_args = len(args_lists[0])
        else_if = "else if" if i > 0 else "if"
        src += 2 * indent + f"{else_if} (x.n_args == {no_of_args}) " + " {\n"
        if i > 0:
            no_of_args_msg += " or "
        no_of_args_msg += f"{no_of_args}"
        src += 3 * indent + f'ASRUtils::require_impl(x.m_overload_id == {i}, "Overload Id for {func_name} expected to be {i}, found " + std::to_string(x.m_overload_id), x.base.base.loc, diagnostics);\n'
        for _i in range(no_of_args):
            src += 3 * indent + f"ASR::ttype_t *arg_type{_i} = ASRUtils::type_get_past_const(ASRUtils::expr_type(x.m_args[{_i}]));\n"
        for j, arg_list in enumerate(args_lists):
            subcond = ""
            subcond_in_msg = ""
            for _j in range(no_of_args):
                arg = arg_list[_j]
                subcond += f"{type_to_asr_type_check[arg]}(*arg_type{_j})"
                subcond_in_msg += arg
                if _j < no_of_args - 1:
                    subcond += " && "
                    subcond_in_msg += ", "
            condition += f"({subcond})"
            cond_in_msg += f"({subcond_in_msg})"
            if j < len(args_lists) - 1:
                condition += " || "
                cond_in_msg += " or "
        src += 3 * indent + f'ASRUtils::require_impl({condition}, "Unexpected args, {func_name} expects {cond_in_msg} as arguments", x.base.base.loc, diagnostics);\n'
        src += 2 * indent + "}"
    src += " else {\n"
    src += 3 * indent + f'ASRUtils::require_impl(false, "Unexpected number of args, {func_name} takes {no_of_args_msg} arguments, found " + std::to_string(x.n_args), x.base.base.loc, diagnostics);\n'
    src += 2 * indent + "}\n"

def add_verify_return_type_src(func_name):
    if func_name not in intrinsic_funcs_ret_type.keys():
        return ""
    global src
    ret_type_cond = ""
    ret_type_cond_in_msg = ""
    for i, ret_type in enumerate(intrinsic_funcs_ret_type[func_name]):
        if ret_type == "null":
            ret_type_cond += f"x.m_type == nullptr"
        else:
            ret_type_cond += f"{type_to_asr_type_check[ret_type]}(*x.m_type)"
        ret_type_cond_in_msg += f"{ret_type}"
        if i < len(intrinsic_funcs_ret_type[func_name]) - 1:
            ret_type_cond += " || "
            ret_type_cond_in_msg += " or "
    src += 2 * indent + f'ASRUtils::require_impl({ret_type_cond}, "Unexpected return type, {func_name} expects `{ret_type_cond_in_msg}` as return type", x.base.base.loc, diagnostics);\n'

def add_create_func_arg_type_src(func_name):
    global src
    arg_infos = intrinsic_funcs_args[func_name]
    no_of_args_msg = ""
    for i, arg_info in enumerate(arg_infos):
        condition = ""
        cond_in_msg = ""
        args_lists = arg_info["args"]
        no_of_args = len(args_lists[0])
        else_if = "else if" if i > 0 else "if"
        src += 2 * indent + f"{else_if} (args.size() == {no_of_args}) " + " {\n"
        if i > 0:
            no_of_args_msg += " or "
        no_of_args_msg += f"{no_of_args}"
        for _i in range(no_of_args):
            src += 3 * indent + f"ASR::ttype_t *arg_type{_i} = ASRUtils::type_get_past_const(ASRUtils::expr_type(args[{_i}]));\n"
        for j, arg_list in enumerate(args_lists):
            subcond = ""
            subcond_in_msg = ""
            for _j in range(no_of_args):
                arg = arg_list[_j]
                subcond += f"{type_to_asr_type_check[arg]}(*arg_type{_j})"
                subcond_in_msg += arg
                if _j < no_of_args - 1:
                    subcond += " && "
                    subcond_in_msg += ", "
            condition += f"({subcond})"
            cond_in_msg += f"({subcond_in_msg})"
            if j < len(args_lists) - 1:
                condition += " || "
                cond_in_msg += " or "
        src += 3 * indent + f'if(!({condition}))' + ' {\n'
        src += 4 * indent + f'append_error(diag, "Unexpected args, {func_name} expects {cond_in_msg} as arguments", loc);\n'
        src += 4 * indent + f'return nullptr;\n'
        src += 3 * indent + '}\n'
        src += 2 * indent + "}"
    src += " else {\n"
    src += 3 * indent + f'append_error(diag, "Unexpected number of args, {func_name} takes {no_of_args_msg} arguments, found " + std::to_string(args.size()), loc);\n'
    src += 3 * indent + f'return nullptr;\n'
    src += 2 * indent + "}\n"


def add_create_func_return_src(func_name):
    global src, indent
    arg_infos = intrinsic_funcs_args[func_name]
    args_lists = arg_infos[0]["args"]
    ret_type_arg_idx = arg_infos[0]["ret_type_arg_idx"]
    no_of_args = len(args_lists[0])

    ret_type = f"expr_type(args[{ret_type_arg_idx}])"
    src += indent * 2 + "ASR::expr_t *m_value = nullptr;\n"
    src += indent * 2 + "if (all_args_evaluated(args)) {\n"
    src += indent * 3 + f"Vec<ASR::expr_t*> arg_values; arg_values.reserve(al, {no_of_args});\n"
    for _i in range(no_of_args):
        src += indent * 3 + f"arg_values.push_back(al, expr_value(args[{_i}]));\n"
    src += indent * 3 + f"m_value = eval_{func_name}(al, loc, {ret_type}, arg_values);\n"
    src += indent * 2 + "}\n"
    src += indent * 2 + f"ASR::make_IntrinsicScalarFunction_t(al, loc, static_cast<int64_t>(IntrinsicScalarFunctions::{func_name}), args.p, args.n, 0, {ret_type}, m_value);\n"

def get_registry_funcs_src():
    global src
    for func_name in intrinsic_funcs_args.keys():
        src += f"namespace {func_name}" + " {\n\n"
        src += indent + R"static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x, diag::Diagnostics& diagnostics) {" + "\n"
        add_verify_arg_type_src(func_name)
        add_verify_return_type_src(func_name)
        src += indent + "}\n\n"

        if func_name not in skip_create_func:
            src += indent + Rf"static inline void create_{func_name}(Allocator& al, const Location& loc, Vec<ASR::expr_t*>& args, diag::Diagnostics& diag) " + "{\n"
            add_create_func_arg_type_src(func_name)
            add_create_func_return_src(func_name)
            src += indent + "}\n"
        src += "}\n\n"
    return src


HEAD = """#ifndef LIBASR_PASS_INTRINSIC_FUNC_REG_UTIL_H
#define LIBASR_PASS_INTRINSIC_FUNC_REG_UTIL_H

#include <libasr/asr.h>
#include <libasr/asr_utils.h>

namespace LCompilers {

namespace ASRUtils {

"""

FOOT = """
} // namespace ASRUtil

} // namespace LCompilers

#endif // LIBASR_PASS_INTRINSIC_FUNC_REG_UTIL_H
"""
def main(argv):
    if len(argv) == 2:
        out_file = argv[1]
    elif len(argv) == 1:
        print("Assuming default values of intrinsic_function_registry_util.h")
        here = os.path.dirname(__file__)
        pass_dir = os.path.join(here, "pass")
        out_file = os.path.join(pass_dir, "intrinsic_function_registry_util.h")
    else:
        print("invalid arguments")
        return 2
    fp = open(out_file, "w", encoding="utf-8")
    try:
        fp.write(HEAD)
        fp.write(get_registry_funcs_src())
        fp.write(FOOT)
    finally:
        fp.close()

if __name__ == "__main__":
    sys.exit(main(sys.argv))
