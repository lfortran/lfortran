#!/usr/bin/env python

import sys
import os

ROOT_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__)))
sys.path.append(os.path.join(ROOT_DIR, "src", "libasr"))

from compiler_tester.tester import color, fg, log, run_test, style, tester_main


def single_test(test, specific_test, verbose, no_llvm, update_reference):
    filename = test["filename"]
    if specific_test and specific_test not in filename:
        return
    show_verbose = "" if not verbose else "-v"
    tokens = test.get("tokens", False)
    ast = test.get("ast", False)
    ast_indent = test.get("ast_indent", False)
    ast_f90 = test.get("ast_f90", False)
    ast_cpp = test.get("ast_cpp", False)
    ast_cpp_hip = test.get("ast_cpp_hip", False)
    ast_openmp = test.get("ast_openmp", False)
    asr = test.get("asr", False)
    asr_implicit_typing = test.get("asr_implicit_typing", False)
    asr_preprocess = test.get("asr_preprocess", False)
    asr_indent = test.get("asr_indent", False)
    mod_to_asr = test.get("mod_to_asr", False)
    llvm = test.get("llvm", False)
    cpp = test.get("cpp", False)
    c = test.get("c", False)
    wat = test.get("wat", False)
    obj = test.get("obj", False)
    x86 = test.get("x86", False)
    bin_ = test.get("bin", False)
    pass_ = test.get("pass", None)
    optimization_passes = ["flip_sign", "div_to_mul", "fma", "sign_from_value",
                           "inline_function_calls", "loop_unroll",
                           "dead_code_removal"]

    if pass_ and (pass_ not in ["do_loops", "global_stmts"] and
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
                "lfortran --fixed-form --show-ast --no-color {infile} -o {outfile}",
                filename,
                update_reference,
                extra_args)
        else:
            # Use free form
            run_test(
                filename,
                "ast",
                "lfortran --show-ast --no-color {infile} -o {outfile}",
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
                "lfortran --fixed-form --show-asr --no-color {infile} -o {outfile}",
                filename,
                update_reference,
                extra_args)
        else:
            run_test(
                filename,
                "asr",
                "lfortran --show-asr --no-color {infile} -o {outfile}",
                filename,
                update_reference,
                extra_args)

    if asr_implicit_typing:
        run_test(
            filename,
            "asr",
            "lfortran --show-asr --implicit-typing --no-color {infile} -o {outfile}",
            filename,
            update_reference,
            extra_args)

    if asr_preprocess:
        run_test(
            filename,
            "asr_preprocess",
            "lfortran --cpp --show-asr --no-color {infile} -o {outfile}",
            filename,
            update_reference,
            extra_args)

    if asr_indent:
        run_test(
            filename,
            "asr_indent",
            "lfortran --show-asr --indent --no-color {infile} -o {outfile}",
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
            " --show-asr --no-color {infile} -o {outfile}"
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
