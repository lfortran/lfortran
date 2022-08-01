if (NOT WITH_RUNTIME_LIBRARY)
  add_subdirectory(libasr)
  add_subdirectory(tests)
  add_subdirectory(lfortran)
  add_subdirectory(bin)
elseif(WITH_RUNTIME_LIBRARY_ONLY)
  add_subdirectory(runtime)
  add_subdirectory(runtime/legacy)
else()
  add_subdirectory(libasr)
  add_subdirectory(tests)
  add_subdirectory(lfortran)
  add_subdirectory(bin)
  # If we want to compile the compiler and the runtime we need
  # to use the add_custom_command in bin/CMakeLists.txt otherwise the
  # runtime gets built before lfortran.
  # Unfortunately simply calling lfortran in a command doesn't do well
  # with cross compilation thus runtime/CMakeLists.txt uses targets.
  # This should probably be solved by splitting the compiler and
  # runtime in two different projects like flang does.
  add_subdirectory(runtime)
  add_subdirectory(runtime/legacy)
endif()
