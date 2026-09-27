# Native link and load variants of module initialization. RUN builds one
# executable from sources, so these targets are spelled out: global_init_20_a
# and global_init_20_b (and global_init_20_c, which is only ever loaded with
# dlopen) are built once, as object files, and linked
#
#   global_init_20     from a static archive into a Fortran program,
#   global_init_21_ab  as two shared libraries into a C program, in both
#   global_init_21_ba  orders,
#   global_init_22     as one shared library, behind an early C constructor,
#   global_init_23     as in global_init_21_ab, with global_init_20_c loaded,
#                      unloaded and loaded again by the program,
#   global_init_27     global_init_27_p and global_init_27_q as plugins that a
#                      C program linked with nothing but the dynamic loader
#                      loads, closes and loads again,
#
# every executable and library with dead stripping. No program uses a module:
# each drives them through C-callable procedures. The sources are next to the
# other tests, in the parent directory.

set(src ${CMAKE_CURRENT_SOURCE_DIR}/..)
set(mod ${CMAKE_CURRENT_BINARY_DIR}/mod)
# The C programs start the runtime as ISO_Fortran_binding.h describes.
if (ISO_FORTRAN_BINDING_INCLUDE_DIR)
    include_directories(${ISO_FORTRAN_BINDING_INCLUDE_DIR})
endif()

if (APPLE)
    set(strip -Wl,-dead_strip)
else()
    set(strip -Wl,--gc-sections)
endif()
set(fflags "")
if (LFORTRAN_BACKEND)
    set(label llvm)
    # A module's procedures are only emitted in the object file that defines
    # the module with --separate-compilation.
    list(APPEND fflags --separate-compilation)
    if (FAST)
        list(APPEND fflags --fast)
    endif()
    if (DETECT_LEAKS)
        list(APPEND fflags --detect-leaks)
    endif()
    # CMake knows neither how LFortran is told where to write module files
    # nor that it makes a shared library with --shared and takes the install
    # name or soname only as a linker flag. These apply to this directory
    # alone.
    set(CMAKE_Fortran_MODDIR_FLAG -J)
    if (APPLE)
        set(CMAKE_Fortran_CREATE_SHARED_LIBRARY "<CMAKE_Fortran_COMPILER> --shared <LINK_FLAGS> -o <TARGET> -Wl,-install_name,<TARGET_INSTALLNAME_DIR><TARGET_SONAME> <OBJECTS> <LINK_LIBRARIES>")
    else()
        set(CMAKE_Fortran_CREATE_SHARED_LIBRARY "<CMAKE_Fortran_COMPILER> --shared <LINK_FLAGS> -o <TARGET> -Wl,-soname,<TARGET_SONAME> <OBJECTS> <LINK_LIBRARIES>")
    endif()
else()
    set(label gfortran)
endif()

foreach(m 20_a 20_b 20_c 27_p 27_q)
    add_library(global_init_${m}_obj OBJECT ${src}/global_init_${m}.f90)
    set_target_properties(global_init_${m}_obj PROPERTIES
        POSITION_INDEPENDENT_CODE ON Fortran_MODULE_DIRECTORY ${mod})
    target_include_directories(global_init_${m}_obj PRIVATE ${mod})
    target_compile_options(global_init_${m}_obj PRIVATE ${fflags})
endforeach()
# For the module file of global_init_20_a, which the Ninja generator only
# orders before its users through a link dependency; linking one object
# library to another adds no object files. Like list(TRANSFORM) in RUN, this
# needs CMake 3.12.
target_link_libraries(global_init_20_b_obj PRIVATE global_init_20_a_obj)
target_link_libraries(global_init_20_c_obj PRIVATE global_init_20_a_obj)

add_library(global_init_20_static STATIC
    $<TARGET_OBJECTS:global_init_20_b_obj> $<TARGET_OBJECTS:global_init_20_a_obj>)
add_library(global_init_20_shared_a SHARED $<TARGET_OBJECTS:global_init_20_a_obj>)
# Not linked with global_init_20_shared_a, which the executables load next to
# it instead, so that the link order of the two decides the order the loader
# starts them in.
add_library(global_init_20_shared_b SHARED $<TARGET_OBJECTS:global_init_20_b_obj>)
if (APPLE)
    target_link_libraries(global_init_20_shared_b PRIVATE -Wl,-undefined,dynamic_lookup)
endif()
add_library(global_init_20_shared_c SHARED $<TARGET_OBJECTS:global_init_20_c_obj>)
target_link_libraries(global_init_20_shared_c PRIVATE global_init_20_shared_a)
# The constructor's object file has to come first; see global_init_22c.c.
add_library(global_init_22c_obj OBJECT ${src}/global_init_22c.c)
set_target_properties(global_init_22c_obj PROPERTIES POSITION_INDEPENDENT_CODE ON)
add_library(global_init_22_shared SHARED $<TARGET_OBJECTS:global_init_22c_obj>
    $<TARGET_OBJECTS:global_init_20_b_obj> $<TARGET_OBJECTS:global_init_20_a_obj>)
foreach(t global_init_20_static global_init_20_shared_a global_init_20_shared_b
        global_init_20_shared_c global_init_22_shared)
    set_target_properties(${t} PROPERTIES LINKER_LANGUAGE Fortran)
endforeach()
foreach(t global_init_20_shared_a global_init_20_shared_b global_init_20_shared_c
        global_init_22_shared)
    target_link_libraries(${t} PRIVATE ${strip})
endforeach()

add_executable(global_init_20 ${src}/global_init_20.f90)
target_compile_options(global_init_20 PRIVATE ${fflags})
target_link_libraries(global_init_20 PRIVATE global_init_20_static)
add_executable(global_init_21_ab ${src}/global_init_21c.c)
target_link_libraries(global_init_21_ab PRIVATE global_init_20_shared_a global_init_20_shared_b)
add_executable(global_init_21_ba ${src}/global_init_21c.c)
target_link_libraries(global_init_21_ba PRIVATE global_init_20_shared_b global_init_20_shared_a)
add_executable(global_init_22 ${src}/global_init_22.c)
target_link_libraries(global_init_22 PRIVATE global_init_22_shared)
add_executable(global_init_23 ${src}/global_init_23c.c)
target_link_libraries(global_init_23 PRIVATE global_init_20_shared_a global_init_20_shared_b ${CMAKE_DL_LIBS})
add_dependencies(global_init_23 global_init_20_shared_c)
foreach(t global_init_20 global_init_21_ab global_init_21_ba global_init_22 global_init_23)
    set_target_properties(${t} PROPERTIES LINKER_LANGUAGE Fortran)
    target_link_libraries(${t} PRIVATE ${strip})
endforeach()

# The plugins bring the Fortran runtime with them; the program does not link
# it, being linked by the C compiler with the dynamic loader alone.
foreach(m p q)
    add_library(global_init_27_${m} SHARED $<TARGET_OBJECTS:global_init_27_${m}_obj>)
    set_target_properties(global_init_27_${m} PROPERTIES LINKER_LANGUAGE Fortran)
    target_link_libraries(global_init_27_${m} PRIVATE ${strip})
endforeach()
add_executable(global_init_27 ${src}/global_init_27c.c)
set_target_properties(global_init_27 PROPERTIES LINKER_LANGUAGE C)
target_link_libraries(global_init_27 PRIVATE ${CMAKE_DL_LIBS})
add_dependencies(global_init_27 global_init_27_p global_init_27_q)

add_test(NAME global_init_20 COMMAND global_init_20)
add_test(NAME global_init_21_ab COMMAND global_init_21_ab)
add_test(NAME global_init_21_ba COMMAND global_init_21_ba)
add_test(NAME global_init_22 COMMAND global_init_22)
add_test(NAME global_init_23 COMMAND global_init_23 $<TARGET_FILE:global_init_20_shared_c>)
add_test(NAME global_init_27 COMMAND global_init_27
    $<TARGET_FILE:global_init_27_p> $<TARGET_FILE:global_init_27_q>)
set_tests_properties(global_init_20 global_init_21_ab global_init_21_ba global_init_22
    global_init_23 global_init_27 PROPERTIES LABELS ${label})
