program compiler_options
    use iso_fortran_env, only: compiler_options
    implicit none
    print "(A)", 'compiler options = '//compiler_options()
end program 