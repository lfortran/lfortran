# Compiles the C that `lfortran --show-c` prints for two modules, each with
# startup initialization records, with every compiler of COMPILERS, links the
# object files into test_init_c_note.c with the default linker and, for each
# of the ELF linkers in LINKERS (e.g. lld), with `-fuse-ld=`, and checks that
# the engine finds the notes of both: the note has to be a read-only, 4-byte
# aligned SHT_NOTE whatever compiler emits it, which an assembler warning
# about its section attributes also shows is not. SRC is the directory of
# the sources, INCLUDE that of the runtime's headers, RUNTIME that of the
# runtime library and WORK a scratch directory.

file(REMOVE_RECURSE ${WORK})
file(MAKE_DIRECTORY ${WORK})

function(run)
    execute_process(COMMAND ${ARGN} WORKING_DIRECTORY ${WORK}
        RESULT_VARIABLE status OUTPUT_VARIABLE out ERROR_VARIABLE err)
    if (NOT status EQUAL 0)
        message(FATAL_ERROR "failed (${status}): ${ARGN}\n${out}${err}")
    endif()
    set(out "${out}" PARENT_SCOPE)
    set(err "${err}" PARENT_SCOPE)
endfunction()

set(modules test_init_c_note_a test_init_c_note_b)
foreach(m ${modules})
    run(${LFORTRAN} -c --separate-compilation ${SRC}/${m}.f90 -o ${m}_lf.o)
    execute_process(COMMAND ${LFORTRAN} --separate-compilation --show-c
            ${SRC}/${m}.f90
        WORKING_DIRECTORY ${WORK} RESULT_VARIABLE status
        OUTPUT_FILE ${WORK}/${m}.c ERROR_VARIABLE err)
    if (NOT status EQUAL 0)
        message(FATAL_ERROR "--show-c failed for ${m}: ${err}")
    endif()
endforeach()

set(libs -L${RUNTIME} -Wl,-rpath,${RUNTIME} -llfortran_runtime -lm)
set(n 0)
foreach(cc ${COMPILERS})
    math(EXPR n "${n} + 1")
    set(objects "")
    foreach(m ${modules})
        run(${cc} -fPIC -c -I${INCLUDE} ${m}.c -o ${m}_${n}.o)
        if (err MATCHES "[Ww]arning")
            message(FATAL_ERROR "${cc} warned about ${m}.c:\n${err}")
        endif()
        list(APPEND objects ${m}_${n}.o)
    endforeach()
    foreach(linker default ${LINKERS})
        set(flags "")
        if (NOT linker STREQUAL "default")
            set(flags -fuse-ld=${linker})
        endif()
        run(${cc} ${flags} ${SRC}/test_init_c_note.c ${objects}
            -o main_${n}_${linker} ${libs})
        run(${WORK}/main_${n}_${linker})
        message("${cc}, ${linker} linker: ${out}")
    endforeach()
endforeach()
