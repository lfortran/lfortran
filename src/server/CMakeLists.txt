cmake_minimum_required(VERSION 3.10)

project(llanguage-server)

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  add_compile_definitions(DEBUG_MODE)
elseif(CMAKE_BUILD_TYPE STREQUAL "Release")
  add_compile_definitions(NDEBUG)
endif()

set(LLANGUAGE_SERVER_SRC
  lsp_specification.cpp
  lsp_exception.cpp
  lsp_json_parser.cpp
  lsp_json_serializer.cpp
  logger.cpp
  queue.hpp
  lsp_text_document.cpp
  thread_pool.cpp
  lsp_transformer.cpp
  language_server.cpp
  lsp_language_server.cpp
  base_lsp_language_server.cpp
  message_stream.cpp
  lsp_message_stream.cpp
  communication_protocol.cpp
)

add_library(llanguage_server STATIC ${LLANGUAGE_SERVER_SRC})
target_include_directories(llanguage_server BEFORE PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/..)
target_include_directories(llanguage_server BEFORE PUBLIC ${CMAKE_CURRENT_BINARY_DIR}/..)
