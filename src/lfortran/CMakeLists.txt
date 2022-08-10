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
    add_executable(mini_wasm_test mini_wasm_test.cpp)
    target_link_libraries(lfortran_wasm_kernel lfortran_lib)

    target_compile_options(lfortran_wasm_kernel PRIVATE -fwasm-exceptions)
    target_link_options(lfortran_wasm_kernel
        PRIVATE --bind -fwasm-exceptions)

    target_link_options(lfortran_wasm_kernel
         PUBLIC "SHELL: -s MODULARIZE=1"
         PUBLIC "SHELL: -s EXPORT_NAME=\"createXeusModule\""
         PUBLIC "SHELL: -s EXPORT_ES6=0"
         PUBLIC "SHELL: -s USE_ES6_IMPORT_META=0"
         PUBLIC "SHELL: -s DEMANGLE_SUPPORT=0"
         PUBLIC "SHELL: -s ASSERTIONS=0"
         PUBLIC "SHELL: -s ALLOW_MEMORY_GROWTH=1"
         PUBLIC "SHELL: -s INITIAL_MEMORY=200MB"
         PUBLIC "SHELL: -s EXIT_RUNTIME=1"
         PUBLIC "SHELL: -s WASM=1"
         PUBLIC "SHELL: -s USE_PTHREADS=0"
         PUBLIC "SHELL: -s TOTAL_STACK=128mb"
         PUBLIC "SHELL: -s ENVIRONMENT='worker'"
         PUBLIC "SHELL: --pre-js ${CMAKE_CURRENT_SOURCE_DIR}/pre.js"
    )

    target_compile_options(mini_wasm_test PRIVATE
        "-fwasm-exceptions"
        "SHELL: -s ENVIRONMENT='node'"
        "SHELL: -s ASSERTIONS=2"
        # "SHELL: -s SAFE_HEAP=1"
        "-fsanitize=address"
        "-g"
    )
    target_link_libraries(mini_wasm_test lfortran_lib)
    target_link_options(mini_wasm_test PRIVATE -fwasm-exceptions)

    target_link_options(mini_wasm_test PRIVATE
        "SHELL: -s ENVIRONMENT='node'"
        "SHELL: -s ASSERTIONS=2"
        # "SHELL: -s SAFE_HEAP=1"
        "-fsanitize=address"
    )
    target_link_options(mini_wasm_test
         PUBLIC "SHELL: -s MODULARIZE=1"
         PUBLIC "SHELL: -s EXPORT_NAME=\"createXeusModule\""
         PUBLIC "SHELL: -s EXPORT_ES6=0"
         PUBLIC "SHELL: -s USE_ES6_IMPORT_META=0"
         PUBLIC "SHELL: -s DEMANGLE_SUPPORT=0"
         PUBLIC "SHELL: -s ASSERTIONS=0"
         PUBLIC "SHELL: -s ALLOW_MEMORY_GROWTH=1"
         PUBLIC "SHELL: -s INITIAL_MEMORY=200MB"
         PUBLIC "SHELL: -s EXIT_RUNTIME=1"
         PUBLIC "SHELL: -s WASM=1"
         PUBLIC "SHELL: -s USE_PTHREADS=0"
         PUBLIC "SHELL: -s TOTAL_STACK=128mb"
         PUBLIC "SHELL: -s ENVIRONMENT='node'"
         PUBLIC "SHELL: --pre-js ${CMAKE_CURRENT_SOURCE_DIR}/pre.js"
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
