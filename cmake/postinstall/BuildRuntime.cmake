execute_process(
  COMMAND "${CMAKE_COMMAND}" --install "${CMAKE_BINARY_DIR}/src/runtime"
  WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/src/runtime"
  RESULT_VARIABLE STATUS_BUILD
  COMMAND_ECHO STDOUT)

if (STATUS_BUILD AND NOT STATUS_BUILD EQUAL 0)
  message(FATAL_ERROR "cmake failed with status: ${STATUS_BUILD}")
else()
  message(STATUS "Done.")
endif()
