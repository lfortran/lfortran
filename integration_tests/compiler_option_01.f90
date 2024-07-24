program compiler_option_01
    use iso_fortran_env, only: compiler_options
    print "(A)", 'compiler options = '//compiler_options()
end program 