module compiler_options_01_mod
    use, intrinsic :: iso_fortran_env, only: compiler_options
    implicit none
    character(len=*), parameter :: compile_options = compiler_options()
end module

program compiler_options_01
    ! Named constants initialized with compiler_options()
    use, intrinsic :: iso_fortran_env, only: compiler_options
    use compiler_options_01_mod, only: compile_options
    implicit none
    character(len=*), parameter :: co = compiler_options()
    character(len=500), parameter :: co500 = compiler_options()

    if (len(co) /= len(compiler_options())) error stop
    if (co /= compiler_options()) error stop
    if (len(co500) /= 500) error stop
    if (co500 /= compiler_options()) error stop
    if (len(compile_options) /= len(co)) error stop
    print *, len(co) >= 0
end program
