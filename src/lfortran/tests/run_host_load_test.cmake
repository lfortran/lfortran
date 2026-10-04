# The host startup contract of LFortran's ISO_Fortran_binding.h with a
# Fortran library loaded by dlopen. A C program calls lfortran_initialize()
# first, loads the library and calls a bind(c) procedure of it that reaches
# module state only through an external procedure. The library is built in
# three variants:
#
#   early    a C constructor of priority 101, which runs ahead of every
#            constructor of default priority, calls lfortran_initialize()
#            again and then the procedure, while the library is loaded and
#            before its Fortran constructors have run;
#   default  the same with a constructor of default priority linked ahead
#            of the Fortran object files;
#   plain    no C constructor: the library's own constructors initialize it.
#
# Every call of the procedure has to find the state initialized. The
# program has records of its own, which its first startup initializes, or
# has none.
#
# The Fortran sources in SRC are compiled by LFORTRAN; the C sources there
# by the C compiler CC, which links the libraries and the programs, with
# the default linker and, for each of the ELF linkers in LINKERS (e.g. lld),
# with `-fuse-ld=`. INCLUDE is the directory of ISO_Fortran_binding.h,
# RUNTIME that of the runtime library and WORK a scratch directory.

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
        ${SRC}/test_init_host_load_${u}.f90 -o ${u}.o)
endforeach()

set(libs -L${RUNTIME} -Wl,-rpath,${RUNTIME} -llfortran_runtime -lm)
foreach(linker default ${LINKERS})
    set(flags "")
    if (NOT linker STREQUAL "default")
        set(flags -fuse-ld=${linker})
    endif()
    foreach(variant early default plain)
        set(defines "")
        if (variant STREQUAL "default")
            set(defines -DDEFAULT_PRIORITY)
        elseif (variant STREQUAL "plain")
            set(defines -DNO_CONSTRUCTOR)
        endif()
        run(${CC} -fPIC -I${INCLUDE} ${defines}
            -c ${SRC}/test_init_host_load_lib.c -o lib_${variant}.o)
        run(${CC} ${flags} -shared -o libhost_${variant}_${linker}.so
            lib_${variant}.o m.o foo.o e.o ${libs})
    endforeach()
    run(${CC} ${flags} -I${INCLUDE} ${SRC}/test_init_host_load.c x.o
        -o main_records_${linker} -ldl ${libs})
    run(${CC} ${flags} -I${INCLUDE} ${SRC}/test_init_host_load.c
        -o main_plain_${linker} -ldl ${libs})
    foreach(main main_records main_plain)
        foreach(variant early default plain)
            run(${WORK}/${main}_${linker}
                ${WORK}/libhost_${variant}_${linker}.so)
            message("${main}, library ${variant}, ${linker} linker: ${out}")
        endforeach()
    endforeach()
endforeach()
