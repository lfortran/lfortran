#!/usr/bin/env python

import os
import subprocess as sp
import sys
from typing import Dict

ROOT_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__)))
sys.path.append(os.path.join(ROOT_DIR, "src", "libasr"))

from compiler_tester.tester import color, fg, log, run_test, style, tester_main

def run_cmd(cmd, cwd=None):
    print(f"+ {cmd}")
    process = sp.run(cmd, shell=True, cwd=cwd)
    if process.returncode != 0:
        print("Command failed.")
        exit(1)

def single_test(test: Dict, verbose: bool, no_llvm: bool, skip_run_with_dbg: bool,
                update_reference: bool, no_color: bool, specific_backends=None,
                excluded_backends=None) -> None:
    def is_included(backend):
        return test.get(backend, False) \
            and (specific_backends is None or backend in specific_backends) \
            and (excluded_backends is None or backend not in excluded_backends)

    filename = test["filename"]
    show_verbose = "" if not verbose else "-v"
    tokens = is_included("tokens")
    ast = is_included("ast")
    ast_indent = is_included("ast_indent")
    ast_no_prescan = is_included("ast_no_prescan")
    ast_f90 = is_included("ast_f90")
    ast_cpp = is_included("ast_cpp")
    ast_cpp_hip = is_included("ast_cpp_hip")
    ast_openmp = is_included("ast_openmp")
    asr = is_included("asr")
    asr_implicit_typing = is_included("asr_implicit_typing")
    asr_implicit_interface = is_included("asr_implicit_interface")
    asr_implicit_interface_and_typing = is_included("asr_implicit_interface_and_typing")
    asr_implicit_interface_and_typing_with_llvm = is_included("asr_implicit_interface_and_typing_with_llvm")
    asr_preprocess = is_included("asr_preprocess")
    asr_indent = is_included("asr_indent")
    mod_to_asr = is_included("mod_to_asr")
    llvm = is_included("llvm")
    cpp = is_included("cpp")
    c = is_included("c")
    julia = is_included("julia")
    wat = is_included("wat")
    obj = is_included("obj")
    x86 = is_included("x86")
    bin_ = is_included("bin")
    pass_ = test.get("pass", None)
    extrafiles = test.get("extrafiles", "").split(",")
    optimization_passes = ["flip_sign", "div_to_mul", "fma", "sign_from_value",
                           "inline_function_calls", "loop_unroll",
                           "dead_code_removal"]

    if pass_ and (pass_ not in ["do_loops", "global_stmts",
                                "transform_optional_argument_functions",
                                "array_op"] and
                  pass_ not in optimization_passes):
        raise Exception(f"Unknown pass: {pass_}")
    log.debug(f"{color(style.bold)} START TEST: {color(style.reset)} {filename}")

    extra_args = f"--no-error-banner {show_verbose}"

    if tokens:
        run_test(
            filename,
            "tokens",
            "lfortran --no-color --show-tokens {infile} -o {outfile}",
            filename,
            update_reference,
            extra_args)
    if ast:
        if filename.endswith(".f"):
            # Use fixed form
            run_test(
                filename,
                "ast",
                "lfortran --indent --fixed-form --show-ast --no-color {infile} -o {outfile}",
                filename,
                update_reference,
                extra_args)
        else:
            # Use free form
            run_test(
                filename,
                "ast",
                "lfortran --indent --show-ast --no-color {infile} -o {outfile}",
                filename,
                update_reference,
                extra_args)
    if ast_indent:
        run_test(
            filename,
            "ast_indent",
            "lfortran --show-ast --indent --no-color {infile} -o {outfile}",
            filename,
            update_reference,
            extra_args)

    if ast_no_prescan:
        # Use free form with prescan disabled
        run_test(
            filename,
            "ast_no_prescan",
            "lfortran --indent --no-prescan --show-ast --no-color {infile} -o {outfile}",
            filename,
            update_reference,
            extra_args)

    if ast_f90:
        if filename.endswith(".f"):
            # Use fixed form
            run_test(
                filename,
                "ast_f90",
                "lfortran --fixed-form --show-ast-f90 --no-color {infile}",
                filename,
                update_reference,
                extra_args)
        else:
            # Use free form
            run_test(
                filename,
                "ast_f90",
                "lfortran --show-ast-f90 --no-color {infile}",
                filename,
                update_reference,
                extra_args)

    if ast_openmp:
        run_test(
            filename,
            "ast_openmp",
            "cpptranslate --show-ast-openmp {infile}",
            filename,
            update_reference)

    if asr:
        # run fixed form
        if filename.endswith(".f"):
            run_test(
                filename,
                "asr",
                "lfortran --indent --fixed-form --show-asr --no-color {infile} -o {outfile}",
                filename,
                update_reference,
                extra_args)
        else:
            skip_test = False
            for extrafile in extrafiles:
                extrafile_ = extrafile.rstrip().lstrip()

                if no_llvm and len(extrafile_) > 0:
                    log.info(f"{filename} * asr   SKIPPED because LLVM is not enabled")
                    skip_test = True
                    break

                if len(extrafile_) > 0:
                    extrafile_ = os.path.join("tests", extrafile_)
                    modfile = extrafile_[:-4] + ".mod"
                    if not os.path.exists(modfile):
                        run_cmd("lfortran -c {}".format(extrafile_))

            if not skip_test:
                run_test(
                    filename,
                    "asr",
                    "lfortran --indent --show-asr --no-color {infile} -o {outfile}",
                    filename,
                    update_reference,
                    extra_args)

    if asr_implicit_interface_and_typing:
        run_test(
            filename,
            "asr",
            "lfortran --indent --show-asr --implicit-typing --implicit-interface --no-color {infile} -o {outfile}",
            filename,
            update_reference,
            extra_args)

    if asr_implicit_interface_and_typing_with_llvm:
        if no_llvm:
            log.info(f"{filename} * llvm   SKIPPED as requested")
        else:
            run_test(
                filename,
                "llvm",
                "lfortran --show-llvm --implicit-typing --implicit-interface {infile} -o {outfile}",
                filename,
                update_reference,
                extra_args)

    if asr_implicit_typing:
        run_test(
            filename,
            "asr",
            "lfortran --indent --show-asr --implicit-typing --no-color {infile} -o {outfile}",
            filename,
            update_reference,
            extra_args)

    if asr_implicit_interface:
        if filename.endswith(".f"):
            run_test(
                filename,
                "asr",
                "lfortran --indent --fixed-form --implicit-interface --show-asr --no-color {infile} -o {outfile}",
                filename,
                update_reference,
                extra_args)
        else:
            run_test(
                filename,
                "asr",
                "lfortran --indent --show-asr --implicit-interface --no-color {infile} -o {outfile}",
                filename,
                update_reference,
                extra_args)

    if asr_preprocess:
        run_test(
            filename,
            "asr_preprocess",
            "lfortran --indent --cpp --show-asr --no-color {infile} -o {outfile}",
            filename,
            update_reference,
            extra_args)

    if asr_indent:
        run_test(
            filename,
            "asr_indent",
            "lfortran --indent --show-asr --indent --no-color {infile} -o {outfile}",
            filename,
            update_reference,
            extra_args)

    if mod_to_asr:
        run_test(
            filename,
            "mod_to_asr",
            "lfortran mod --show-asr --no-color {infile}",
            filename,
            update_reference)

    if pass_ is not None:
        cmd = "lfortran --pass=" + pass_ + \
            " --indent --show-asr --no-color {infile} -o {outfile}"
        run_test(filename, "pass_{}".format(pass_), cmd,
                 filename, update_reference, extra_args)
    if llvm:
        if no_llvm:
            log.info(f"{filename} * llvm   SKIPPED as requested")
        else:
            run_test(
                filename,
                "llvm",
                "lfortran --no-color --show-llvm {infile} -o {outfile}",
                filename,
                update_reference,
                extra_args)

    if cpp:
        run_test(filename, "cpp", "lfortran --no-color --show-cpp {infile}",
                 filename, update_reference, extra_args)

    if obj:
        if no_llvm:
            log.info(f"{filename} * obj    SKIPPED as requested")
        else:
            run_test(
                filename,
                "obj",
                "lfortran --no-color -c {infile} -o output.o",
                filename,
                update_reference,
                extra_args)

    if c:
        run_test(filename, "c", "lfortran --no-color --show-c {infile}",
                 filename, update_reference, extra_args)

    if julia:
        run_test(filename, "julia", "lfortran --no-color --show-julia {infile}",
                 filename, update_reference, extra_args)

    if wat:
        run_test(filename, "wat", "lfortran --no-color --show-wat {infile}",
                 filename, update_reference, extra_args)

    if x86:
        run_test(
            filename,
            "x86",
            "lfortran --no-color --backend=x86 {infile} -o output",
            filename,
            update_reference,
            extra_args)

    if bin_:
        run_test(filename, "bin", "lfortran --no-color {infile} -o {outfile}",
                 filename, update_reference, extra_args)


if __name__ == "__main__":
    tester_main("LFortran", single_test)
