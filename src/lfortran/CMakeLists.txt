# Specific files generated for lfortran

# tokenizer
add_custom_command(
    OUTPUT ${LFORTRAN_GENERATED_FILES_PATH}/lfortran/tokenizer.cpp
    COMMAND re2c -W -b ${CMAKE_SOURCE_DIR}/src/lfortran/parser/tokenizer.re -o ${LFORTRAN_GENERATED_FILES_PATH}/lfortran/tokenizer.cpp
    DEPENDS ${CMAKE_SOURCE_DIR}/src/lfortran/parser/tokenizer.re
    VERBATIM
)
# preproc
add_custom_command(
    OUTPUT ${LFORTRAN_GENERATED_FILES_PATH}/lfortran/preprocessor.cpp
    COMMAND re2c -W -b ${CMAKE_SOURCE_DIR}/src/lfortran/parser/preprocessor.re -o ${LFORTRAN_GENERATED_FILES_PATH}/lfortran/preprocessor.cpp
    DEPENDS ${CMAKE_SOURCE_DIR}/src/lfortran/parser/preprocessor.re
    VERBATIM
)
# parser
add_custom_command(
    OUTPUT ${LFORTRAN_GENERATED_FILES_PATH}/lfortran/parser/parser.tab.cc
    BYPRODUCTS ${LFORTRAN_GENERATED_FILES_PATH}/lfortran/parser/parser.output ${LFORTRAN_GENERATED_FILES_PATH}/lfortran/parser/parser.tab.hh
    COMMAND bison -Wall -d -r all ${CMAKE_SOURCE_DIR}/src/lfortran/parser/parser.yy
    DEPENDS ${CMAKE_SOURCE_DIR}/src/lfortran/parser/parser.yy
    WORKING_DIRECTORY ${LFORTRAN_GENERATED_FILES_PATH}/lfortran/parser
    VERBATIM
)

add_library(lfortran_lib STATIC
    ${LFORTRAN_GENERATED_FILES_PATH}/lfortran/preprocessor.cpp
    ${LFORTRAN_GENERATED_FILES_PATH}/lfortran/tokenizer.cpp
    ${LFORTRAN_GENERATED_FILES_PATH}/lfortran/parser/parser.tab.cc
    parser/parser.cpp
    parser/fixedform_tokenizer.cpp

    semantics/ast_symboltable_visitor.cpp
    semantics/ast_body_visitor.cpp
    semantics/ast_to_asr.cpp

    fortran_evaluator.cpp

    pickle.cpp
    ast_serialization.cpp
    cwrapper.cpp

    ast_to_src.cpp
    ast_to_openmp.cpp

    mod_to_asr.cpp

    utils.cpp
)

if (WITH_WHEREAMI)
    target_sources(lfortran_lib PUBLIC ../bin/tpl/whereami/whereami.cpp)
endif()

if (WITH_XEUS)
    target_sources(lfortran_lib PUBLIC fortran_kernel.cpp)
endif()
if (WITH_JSON)
    target_sources(lfortran_lib PUBLIC ast_to_json.cpp)
endif()
target_include_directories(lfortran_lib PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>)

configure_file(config.h.cmakein ${LFORTRAN_GENERATED_FILES_PATH}/config.h @ONLY)
target_link_libraries(lfortran_lib asr lfortran_runtime_static)


if (WITH_ZLIB)
    target_link_libraries(lfortran_lib ZLIB::ZLIB)
endif()

target_include_directories(lfortran_lib BEFORE PUBLIC ${lfortran_SOURCE_DIR}/src)
target_include_directories(lfortran_lib BEFORE PUBLIC ${lfortran_BINARY_DIR}/src)
if (WITH_LIBUNWIND)
    target_link_libraries(lfortran_lib p::libunwind)
endif()
if (WITH_XEUS)
    target_link_libraries(lfortran_lib xeus xeus-zmq)
endif()
if (WITH_JSON)
    target_link_libraries(lfortran_lib p::rapidjson)
endif()
if (WITH_BFD)
    target_link_libraries(lfortran_lib p::bfd)
endif()
if (WITH_LINK)
    target_link_libraries(lfortran_lib p::link)
endif()
if (WITH_EXECINFO)
    target_link_libraries(lfortran_lib p::execinfo)
endif()
if (WITH_LLVM)
    if (WITH_MLIR)
        target_link_libraries(lfortran_lib p::mlir)
    endif()
    target_link_libraries(lfortran_lib p::llvm)
endif()
#install(TARGETS lfortran_lib
#    RUNTIME DESTINATION bin
#    ARCHIVE DESTINATION lib
#    LIBRARY DESTINATION lib
#)

add_subdirectory(tests)
