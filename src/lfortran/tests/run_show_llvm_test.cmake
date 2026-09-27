# Builds programs from the LLVM IR that `lfortran --show-llvm` prints, which
# has the startup initialization records in their form independent of the
# object format, and runs them:
#
# - global_init_16 (two modules with records, each in an object file of its
#   own) compiled by LLC and linked by the C compiler CC in both link orders:
#   nothing but the IR's own constructors and destructors registers the
#   records;
# - the same compiled by LFORTRAN, with the records in the form of the
#   object format, and linked the same way;
# - global_init_16_a compiled by LFORTRAN and linked into test_init_ctor.c
#   (in CTOR_MAIN), whose constructor has to have run before main;
# - the same IR joined into one module by LLVM_LINK, if given, and passed to
#   LFORTRAN, which lowers a module with several tables for the target;
# - global_init_01 passed to LFORTRAN as IR.
#
# Every link is made with the default linker and, for each of the ELF
# linkers in LINKERS (e.g. lld), with `-fuse-ld=`. SRC is the directory of
# the integration tests, RUNTIME that of the Fortran runtime library and WORK
# a scratch directory.

file(REMOVE_RECURSE ${WORK})
file(MAKE_DIRECTORY ${WORK})

function(run)
    execute_process(COMMAND ${ARGN} WORKING_DIRECTORY ${WORK}
        RESULT_VARIABLE status OUTPUT_VARIABLE out ERROR_VARIABLE err)
    if (NOT status EQUAL 0)
        message(FATAL_ERROR "failed (${status}): ${ARGN}\n${out}${err}")
    endif()
    set(out "${out}" PARENT_SCOPE)
endfunction()

function(show_llvm name)
    execute_process(COMMAND ${LFORTRAN} --separate-compilation --show-llvm
            ${SRC}/${name}.f90
        WORKING_DIRECTORY ${WORK} RESULT_VARIABLE status
        OUTPUT_FILE ${WORK}/${name}.ll ERROR_VARIABLE err)
    if (NOT status EQUAL 0)
        message(FATAL_ERROR "--show-llvm failed for ${name}: ${err}")
    endif()
endfunction()

function(expect program text)
    run(${WORK}/${program})
    if (NOT out MATCHES "${text}")
        message(FATAL_ERROR "${program} printed:\n${out}\nexpected: ${text}")
    endif()
endfunction()

set(units global_init_16_a global_init_16_b global_init_16_s global_init_16)
# The module files the IR of the modules' users is compiled against.
set(lowered "")
foreach(u ${units})
    run(${LFORTRAN} -c --separate-compilation ${SRC}/${u}.f90 -o ${u}_lf.o)
    list(APPEND lowered ${u}_lf.o)
endforeach()
set(objects "")
foreach(u ${units})
    show_llvm(${u})
    run(${LLC} -filetype=obj -relocation-model=pic ${u}.ll -o ${u}.o)
    list(APPEND objects ${u}.o)
endforeach()
set(libs -L${RUNTIME} -Wl,-rpath,${RUNTIME} -llfortran_runtime -lm)
foreach(linker default ${LINKERS})
    set(flags "")
    if (NOT linker STREQUAL "default")
        set(flags -fuse-ld=${linker})
    endif()
    run(${CC} ${flags} ${CTOR_MAIN} global_init_16_a_lf.o -o ctor_${linker} ${libs})
    expect(ctor_${linker} "constructor ran")
    foreach(kind objects lowered)
        set(list ${${kind}})
        run(${CC} ${flags} ${list} -o ${kind}_forward_${linker} ${libs})
        expect(${kind}_forward_${linker} "ok")
        list(REVERSE list)
        run(${CC} ${flags} ${list} -o ${kind}_backward_${linker} ${libs})
        expect(${kind}_backward_${linker} "ok")
    endforeach()
endforeach()

if (LLVM_LINK)
    run(${LLVM_LINK} -S global_init_16_a.ll global_init_16_b.ll
        global_init_16_s.ll global_init_16.ll -o joined.ll)
    run(${LFORTRAN} joined.ll -o joined)
    expect(joined "ok")
endif()

show_llvm(global_init_01)
run(${LFORTRAN} global_init_01.ll -o from_ir)
expect(from_ir "ok")
