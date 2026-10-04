! Calls global_init_31_m's bind(c) procedure through its binding label; see
! global_init_31_m.f90.
program global_init_31
    use iso_c_binding, only: c_int
    use global_init_31_m, only: settings
    implicit none
    type, bind(c) :: loc
        integer(c_int) :: value
    end type
    interface
        subroutine call_p(x) bind(c, name="global_init_31_p")
            import :: loc
            type(loc), intent(inout) :: x
        end subroutine call_p
    end interface
    type(loc) :: x
    x%value = 2
    call call_p(x)
    if (x%value /= 9) error stop 1
    settings(1)%count = 5
    x%value = 1
    call call_p(x)
    if (x%value /= 10) error stop 2
    print *, "ok"
end program global_init_31
