PROGRAM intrinsics_367
CHARACTER(len=255) :: test_env_var
character(len=5) :: short_env_var
integer :: stat
integer :: len

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

! Test with only name and status (no value parameter)
call get_environment_variable("LFORTRAN_TEST_ENV_VAR", status=stat)
if (stat /= 0) error stop

call get_environment_variable("LFORTRAN_TEST_ENV_VAR_MISSING", status=stat)
if (stat == 0) error stop

short_env_var = ""
call get_environment_variable("LFORTRAN_TEST_ENV_VAR", short_env_var, length=len, status=stat)
if (short_env_var /= "STATU") error stop
if (len /= 10) error stop
if (stat /= -1) error stop

END PROGRAM
