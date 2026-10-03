! An external bind(c) procedure (global_init_33_e.f90) whose automatic array
! takes its extent from module state and which contains an internal
! procedure using it, called by a program that does not use the module: the
! program's startup initializes every module linked into it, whether the
! program uses it or not.
program global_init_33
    use iso_c_binding, only: c_int
    implicit none
    interface
        integer(c_int) function global_init_33_external() bind(c)
            import :: c_int
        end function global_init_33_external
    end interface
    if (global_init_33_external() /= 0) error stop 1
    print *, "ok"
end program global_init_33
