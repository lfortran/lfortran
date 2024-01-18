import sys
import os

intrinsic_funcs_args = {
    "Kind": [
        {
            "args": [("int",), ("real",), ("bool",), ("char",)],
        },
    ],
    "FlipSign": [
        {
            "args": [("int", "real")]
        }
    ],
    "FloorDiv": [
        {
            "args": [("int", "int"), ("uint", "uint"), ("real", "real"), ("bool", "bool")],
        },
    ],
    "Mod": [
        {
            "args": [("int", "int"), ("real", "real")]
        },
    ],
    "Trailz": [
        {
            "args": [("int",)]
        },
    ],
    "Hypot": [
        {
            "args": [("real", "real")]
        }
    ],
    "Digits": [
        {
            "args": [("int",), ("real",)]
        },
    ],
    "Repeat": [
        {
            "args": [("char", "int")]
        }
    ],
    "MinExponent": [
        {
            "args": [("real",)]
        }
    ],
    "MaxExponent": [
        {
            "args": [("real",)]
        }
    ],
    "Partition": [
        {
            "args": [("char", "char")],
        }
    ],
    "ListReverse": [
        {
            "args": [("list",)],
        }
    ],
    "Reserve": [
        {
            "args": [("list", "int")],
        }
    ],
    "Sign": [
        {
            "args": [("int", "int"), ("real", "real")]
        },
    ],
    "Radix": [
        {
            "args": [("int",), ("real",)],
        },
    ],
    "Aint": [
        {
            "args": [("real",)]
        },
        {
            "args": [("real", "int")]
        }
    ],
    "Anint": [
        {
            "args": [("real",)]
        },
        {
            "args": [("real", "int")]
        }
    ],
    "Sqrt": [
        {
            "args": [("real",), ("complex",)]
        },
    ],
    "Sngl": [
        {
            "args": [("real",)]
        }
    ],
    "SignFromValue": [
        {
            "args": [("int", "int"), ("real", "real")]
        },
    ]
}

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

def add_arg_type_src(func_name):
    global src
    arg_infos = intrinsic_funcs_args[func_name]
    for i, arg_info in enumerate(arg_infos):
        condition = ""
        cond_in_msg = ""
        args_lists = arg_info["args"]
        no_of_args = len(args_lists[0])
        src += 2 * indent + f"if (x.n_args == {no_of_args}) " + " {\n"
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
        src += 2 * indent + "}\n"

def add_return_type_src(func_name):
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
    src += 2 * indent + f'ASRUtils::require_impl({ret_type_cond}, "Unexpected return type, {func_name} expects {ret_type_cond_in_msg} as return types", x.base.base.loc, diagnostics);\n'

def get_registry_funcs_src():
    global src
    for func_name in intrinsic_funcs_args.keys():
        src += f"namespace {func_name}" + " {\n\n"
        src += indent + R"static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x, diag::Diagnostics& diagnostics) {" + "\n"
        add_arg_type_src(func_name)
        add_return_type_src(func_name)
        src += indent + "}\n\n"
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
