PROGRAM intrinsics_367
CHARACTER(len=255) :: test_env_var
integer :: stat

CALL get_environment_variable("LFORTRAN_TEST_ENV_VAR", test_env_var)
WRITE (*,*) TRIM(test_env_var)
if (trim(test_env_var) /= "STATUS OK!") error stop
test_env_var = ""

call get_environment_variable("LFORTRAN_TEST_ENV_VAR", test_env_var, status=stat)
print *, trim(test_env_var)
if (stat /= 0) error stop
if (trim(test_env_var) /= "STATUS OK!") error stop
test_env_var = ""

call get_environment_variable("LFORTRAN_TEST_ENV_VAR_MISSING", test_env_var, status=stat)
if (stat == 0) error stop
if (trim(test_env_var) /= "") error stop
END PROGRAM
