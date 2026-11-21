program compiler_options_02
    ! A PARAMETER statement with compiler_options() for an implicitly
    ! typed named constant
    use, intrinsic :: iso_fortran_env, only: compiler_options
    parameter (m = len(compiler_options()))
    if (m /= len(compiler_options())) error stop
    print *, m >= 0
end program
