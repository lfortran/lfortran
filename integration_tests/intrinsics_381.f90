PROGRAM intrinsics_381
CHARACTER(len=255) :: test_env_var
integer :: len
CALL get_environment_variable("LFORTRAN_TEST_ENV_VAR", test_env_var, len)
WRITE (*,*) TRIM(test_env_var)
if (trim(test_env_var) /= "STATUS OK!") error stop
print *, len
if (len /= 10) error stop
END PROGRAM
