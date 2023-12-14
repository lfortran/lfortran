# in tarball
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/lfortran.1)
  install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/lfortran.1 DESTINATION ${CMAKE_INSTALL_MANDIR}/man1)
# in git repo
elseif(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/lfortran.md)
  find_program(PANDOC_EXECUTABLE NAMES pandoc)
  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(PANDOC REQUIRED_VARS PANDOC_EXECUTABLE)
  if(PANDOC_FOUND)
    add_custom_command(OUTPUT lfortran.1
      COMMAND ${PANDOC_EXECUTABLE} --standalone --to man lfortran.md -o lfortran.1
      COMMENT "Building lfortran manpage"
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/lfortran.md)
    add_custom_target(manpage ALL DEPENDS lfortran.1)
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/lfortran.1 DESTINATION ${CMAKE_INSTALL_MANDIR}/man1)
  endif()
endif()
