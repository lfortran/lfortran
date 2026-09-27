# A bind(c) entry point called from a C constructor of its own shared
# library, while the library is loaded and before its Fortran constructors
# have run, has to dispatch in full: the module state it reaches only
# through an external procedure is initialized by nothing else. The program
# that loads the library has records of its own, whose dispatch in its
# constructor completed before the load, or has none.
#
# The Fortran sources in SRC are compiled by LFORTRAN; the C sources there
# by the C compiler CC, which links the libraries (the constructor of
# priority 101, and of default priority linked ahead of the Fortran objects)
# and the programs, with the default linker and, for each of the ELF linkers
# in LINKERS (e.g. lld), with `-fuse-ld=`. RUNTIME is the directory of the
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
endfunction()

# The module first, whose module file its user is compiled against.
foreach(u m foo e x)
    run(${LFORTRAN} -c --separate-compilation --implicit-interface
        ${SRC}/test_init_entry_load_${u}.f90 -o ${u}.o)
endforeach()

set(libs -L${RUNTIME} -Wl,-rpath,${RUNTIME} -llfortran_runtime -lm)
foreach(linker default ${LINKERS})
    set(flags "")
    if (NOT linker STREQUAL "default")
        set(flags -fuse-ld=${linker})
    endif()
    foreach(priority early default)
        set(defines "")
        if (priority STREQUAL "default")
            set(defines -DDEFAULT_PRIORITY)
        endif()
        run(${CC} -fPIC ${defines} -c ${SRC}/test_init_entry_load_lib.c
            -o ctor_${priority}.o)
        run(${CC} ${flags} -shared -o libentry_${priority}_${linker}.so
            ctor_${priority}.o m.o foo.o e.o ${libs})
    endforeach()
    run(${CC} ${flags} ${SRC}/test_init_entry_load.c x.o
        -o main_records_${linker} -ldl ${libs})
    run(${CC} ${flags} ${SRC}/test_init_entry_load.c
        -o main_plain_${linker} -ldl ${libs})
    foreach(main main_records main_plain)
        foreach(priority early default)
            run(${WORK}/${main}_${linker}
                ${WORK}/libentry_${priority}_${linker}.so)
            message("${main}, constructor ${priority}, ${linker} linker: ${out}")
        endforeach()
    endforeach()
endforeach()
