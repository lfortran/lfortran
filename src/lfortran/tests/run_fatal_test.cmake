# Runs PROGRAM with the argument SCENARIO and passes only if it exits, rather
# than being killed by a signal, with a nonzero status and its output matches
# EXPECT. An exit status alone would also accept a crash, and a regular
# expression alone would also accept a process that reported the error but
# then exited with 0.
execute_process(COMMAND ${PROGRAM} ${SCENARIO}
    RESULT_VARIABLE status OUTPUT_VARIABLE out ERROR_VARIABLE err)
message("${out}${err}")
if (NOT status MATCHES "^[1-9][0-9]*$")
    message(FATAL_ERROR "expected a nonzero exit status, got: ${status}")
endif()
if (NOT "${out}${err}" MATCHES "${EXPECT}")
    message(FATAL_ERROR "the output does not match: ${EXPECT}")
endif()
