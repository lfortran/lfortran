! A Fortran main program that calls a C library which starts the runtime
! itself with lfortran_initialize() (global_init_34c.c), as a library that C
! programs use too may do. The runtime is started already, so the call
! neither restarts the random_number stream nor replaces the program's
! command line.
program global_init_34
    implicit none
    interface
        subroutine lib_start() bind(c, name="global_init_34_lib_start")
        end subroutine
    end interface
    real :: a, b
    integer :: n
    character(len=256) :: arg0, arg0_after
    n = command_argument_count()
    call get_command_argument(0, arg0)
    call random_number(a)
    call lib_start()
    call random_number(b)
    if (a == b) error stop 1
    if (command_argument_count() /= n) error stop 2
    call get_command_argument(0, arg0_after)
    if (arg0_after /= arg0) error stop 3
    print *, "ok"
end program global_init_34
