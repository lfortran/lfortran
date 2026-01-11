program intrinsics_400    
    use, intrinsic :: iso_fortran_env, only : compiler_version, compiler_options
    implicit none
    character(len=:), allocatable :: cv, co
    
    cv = compiler_version()
    co = compiler_options()
    
    print *, "Fortran Compiler Version: ", cv
    print *, "Fortran Compiler Options: ", co
    
    if (len(cv) == 0) error stop
    ! if (len(co) == 0) error stop ! The output of compiler options can be empty (gfortran outputs platform dependent options)
    
end program intrinsics_400