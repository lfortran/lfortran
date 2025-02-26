PROGRAM intrinsics_367
CHARACTER(len=255) :: test_env_var
CALL get_environment_variable("TEST_ENV_VAR", test_env_var)
WRITE (*,*) TRIM(test_env_var)
if (trim(test_env_var) /= "STATUS OK!") error stop
END PROGRAM
