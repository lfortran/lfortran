set(SRC
    parser/preprocessor.cpp
    parser/tokenizer.cpp
    parser/parser.tab.cc
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
    set(SRC ${SRC} ../bin/tpl/whereami/whereami.cpp)
endif()

if (WITH_XEUS)
    if (NOT LFORTRAN_BUILD_TO_WASM)
        set(SRC ${SRC}
            fortran_kernel.cpp
        )
    endif()
endif()
if (WITH_JSON)
    set(SRC ${SRC}
        ast_to_json.cpp
    )
endif()
add_library(lfortran_lib ${SRC})
target_link_libraries(lfortran_lib asr lfortran_runtime_static)

if (WITH_ZLIB)
    target_link_libraries(lfortran_lib ZLIB::ZLIB)
endif()

target_include_directories(lfortran_lib BEFORE PUBLIC ${lfortran_SOURCE_DIR}/src)
target_include_directories(lfortran_lib BEFORE PUBLIC ${lfortran_BINARY_DIR}/src)

if (WITH_XEUS AND NOT LFORTRAN_BUILD_TO_WASM)
    target_link_libraries(lfortran_lib xeus)
elseif(WITH_XEUS)
    find_library(XEUS_LIBRARY xeus)
    find_path(XEUS_INCLUDE_DIR xeus/xkernel.hpp)
    target_link_libraries(lfortran_lib ${XEUS_LIBRARY})
    target_include_directories(lfortran_lib BEFORE PUBLIC ${XEUS_INCLUDE_DIR})
    message("Found xeus ${XEUS_LIBRARY} and ${XEUS_INCLUDE_DIR}")
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
    target_link_libraries(lfortran_lib p::llvm)
endif()

if (LFORTRAN_BUILD_TO_WASM AND WITH_XEUS)
    # manually search for those for now because the CMake exports
    # too many emscripten flags
    find_library(XEUS_LIBRARY xeus)
    find_path(XEUS_INCLUDE_DIR xeus/xkernel.hpp)
    include_directories(${XEUS_INCLUDE_DIR})
    target_link_libraries(lfortran_lib ${XEUS_LIBRARY})

    message("Found xeus ${XEUS_LIBRARY} and ${XEUS_INCLUDE_DIR}")

    add_executable(lfortran_wasm_kernel fortran_wasm_kernel.cpp)

    target_link_libraries(lfortran_wasm_kernel lfortran_lib)

    target_compile_options(lfortran_wasm_kernel
      PRIVATE
        -fwasm-exceptions
    )

    target_link_options(lfortran_wasm_kernel
         PRIVATE
            --bind
            -fwasm-exceptions
         PUBLIC
            -sMODULARIZE=1
            -sEXPORT_NAME="createXeusModule"
            -sEXPORT_ES6=0
            -sUSE_ES6_IMPORT_META=0
            -sDEMANGLE_SUPPORT=0
            -sASSERTIONS=0
            -sALLOW_MEMORY_GROWTH=1
            -sINITIAL_MEMORY=200MB
            -sEXIT_RUNTIME=1
            -sWASM=1
            -sUSE_PTHREADS=0
            -sTOTAL_STACK=128mb
            -sENVIRONMENT="worker"
            --pre-js=${CMAKE_CURRENT_SOURCE_DIR}/pre.js
    )

    add_executable(mini_wasm_test mini_wasm_test.cpp)
    target_link_libraries(mini_wasm_test lfortran_lib)

    target_compile_options(mini_wasm_test PRIVATE
        -fexceptions
        -sASSERTIONS=2
        -sSAFE_HEAP=1
        -fsanitize=address
        -g
    )

    target_link_options(mini_wasm_test
      PRIVATE
        -fexceptions
        -sENVIRONMENT='node'
        -fsanitize=address
    )

    install(TARGETS lfortran_wasm_kernel
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
        ARCHIVE DESTINATION share/lfortran/lib
        LIBRARY DESTINATION share/lfortran/lib
    )

    install(FILES
            "$<TARGET_FILE_DIR:lfortran_wasm_kernel>/lfortran_wasm_kernel.wasm"
            DESTINATION ${CMAKE_INSTALL_BINDIR})
endif ()
#install(TARGETS lfortran_lib
#    RUNTIME DESTINATION bin
#    ARCHIVE DESTINATION lib
#    LIBRARY DESTINATION lib
#)

add_subdirectory(tests)
