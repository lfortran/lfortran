PROGRAM test_getenv
    CHARACTER(len=255) :: homedir
    CHARACTER(len=255) :: env_var
    INTEGER :: a
    integer :: b
    logical :: c
    CALL get_environment_variable("HOME", homedir, a, b, c)
    WRITE (*,*) TRIM(homedir)
    print *, homedir
    print *, a
    print *, b
    print *, c

  END PROGRAM