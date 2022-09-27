#!/usr/bin/env python

import sys
import os

ROOT_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__)))
sys.path.append(os.path.join(ROOT_DIR, "src", "libasr"))

from compiler_tester.tester import color, fg, log, run_test, style, tester_main


def single_test(test, verbose, no_llvm, update_reference, specific_backend = ""):
    def need_backend(backend, default=False):
        exist = test.get(backend, default)
        specified = specific_backend == "" or specific_backend == backend
        if specified and exist:
            return True
        return default
    
    filename = test["filename"]
    show_verbose = "" if not verbose else "-v"
    tokens = need_backend("tokens")
    ast = need_backend("ast")
    ast_indent = need_backend("ast_indent")
    ast_f90 = need_backend("ast_f90")
    ast_cpp = need_backend("ast_cpp")
    ast_cpp_hip = need_backend("ast_cpp_hip")
    ast_openmp = need_backend("ast_openmp")
    asr = need_backend("asr")
    asr_implicit_typing = need_backend("asr_implicit_typing")
    asr_implicit_interface = need_backend("ast_implicit_interface")
    asr_preprocess = need_backend("ast_preprocess")
    asr_indent = need_backend("asr_indent")
    mod_to_asr = need_backend("mod_to_asr")
    llvm = need_backend("llvm")
    cpp = need_backend("cpp")
    c = need_backend("c")
    julia = need_backend("julia")
    wat = need_backend("wat")
    obj = need_backend("obj")
    x86 = need_backend("x86")
    bin_ = need_backend("bin")
    pass_ = need_backend("pass", None)
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
            run_test(
                filename,
                "asr",
                "lfortran --indent --show-asr --no-color {infile} -o {outfile}",
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
                "lfortran --indent --fixed-form --allow-implicit-interface --show-asr --no-color {infile} -o {outfile}",
                filename,
                update_reference,
                extra_args)
        else:
            run_test(
                filename,
                "asr",
                "lfortran --indent --show-asr --allow-implicit-interface --no-color {infile} -o {outfile}",
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
