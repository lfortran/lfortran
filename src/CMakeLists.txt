# create all targets for AST, ASR
# putting them here as they are common
find_package(Python)
add_custom_command(
    OUTPUT ${LFORTRAN_GENERATED_FILES_PATH}/lfortran/ast.h
    COMMAND ${Python_EXECUTABLE} ${CMAKE_SOURCE_DIR}/src/libasr/asdl_cpp.py ${CMAKE_SOURCE_DIR}/grammar/AST.asdl ${LFORTRAN_GENERATED_FILES_PATH}/lfortran/ast.h
    DEPENDS ${CMAKE_SOURCE_DIR}/grammar/AST.asdl 
    VERBATIM
)
add_custom_command(
    OUTPUT ${LFORTRAN_GENERATED_FILES_PATH}/libasr/asr.h
    COMMAND ${Python_EXECUTABLE} ${CMAKE_SOURCE_DIR}/src/libasr/asdl_cpp.py ${CMAKE_SOURCE_DIR}/src/libasr/ASR.asdl ${LFORTRAN_GENERATED_FILES_PATH}/libasr/asr.h
    DEPENDS ${CMAKE_SOURCE_DIR}/src/libasr/ASR.asdl 
    VERBATIM
)

#if (LFORTRAN_BUILD_TO_WASM) Are we always generating wasm code?
# Generate a wasm_visitor.h from src/libasr/wasm_instructions.txt (C++)
add_custom_command(
    OUTPUT ${LFORTRAN_GENERATED_FILES_PATH}/libasr/wasm_visitor.h
    COMMAND ${Python_EXECUTABLE} ${CMAKE_SOURCE_DIR}/src/libasr/wasm_instructions_visitor.py
    DEPENDS ${CMAKE_SOURCE_DIR}/src/libasr/wasm_instructions_visitor.py 
    WORKING_DIRECTORY ${LFORTRAN_GENERATED_FILES_PATH}/libasr
    VERBATIM
)
#endif()

# Generate the intrinsic_function_registry_util.h (C++)
add_custom_command(
    OUTPUT ${LFORTRAN_GENERATED_FILES_PATH}/libasr/intrinsic_function_registry_util.h
    COMMAND ${Python_EXECUTABLE} ${CMAKE_SOURCE_DIR}/src/libasr/intrinsic_func_registry_util_gen.py
    DEPENDS ${CMAKE_SOURCE_DIR}/src/libasr/intrinsic_func_registry_util_gen.py 
    VERBATIM
)

add_custom_target(lfortran_generated_files
    DEPENDS ${LFORTRAN_GENERATED_FILES_PATH}/lfortran/ast.h
            ${LFORTRAN_GENERATED_FILES_PATH}/libasr/asr.h
            ${LFORTRAN_GENERATED_FILES_PATH}/libasr/intrinsic_function_registry_util.h
            ${LFORTRAN_GENERATED_FILES_PATH}/libasr/wasm_visitor.h
)

add_library(lfortran_base INTERFACE)
add_dependencies(lfortran_base lfortran_generated_files)
add_library(lf::base ALIAS lfortran_base)
target_include_directories(lfortran_base INTERFACE 
    ${LFORTRAN_GENERATED_FILES_PATH}
    ${CMAKE_SOURCE_DIR}/src)

# check for single quote (linux only?)

add_subdirectory(libasr)
add_subdirectory(tests)
add_subdirectory(lfortran)
add_subdirectory(bin)
add_subdirectory(runtime/legacy)
