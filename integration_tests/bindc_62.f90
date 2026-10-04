! A bind(c) procedure whose specification part has an interface block (for a
! dummy procedure or for an external procedure) must keep its C ABI and
! binding label. Visiting the interface body used to reset the ABI of the
! enclosing procedure to Source, so the C caller failed to link against it.
subroutine bindc_62_helper(x, r)
    implicit none
    integer, intent(in) :: x
    integer, intent(out) :: r
    r = 2 * x
end subroutine

integer function bindc_62_triple(x)
    implicit none
    integer, intent(in) :: x
    bindc_62_triple = 3 * x
end function

module bindc_62_mod
    use iso_c_binding, only: c_int
    implicit none
contains
    ! Interface block for a bind(c) dummy procedure, in a module procedure
    subroutine bindc_62_mcall(f, k, r) bind(c, name="bindc_62_cmcall")
        interface
            subroutine f() bind(c)
            end subroutine
        end interface
        integer(c_int), value :: k
        integer(c_int), intent(out) :: r
        call f()
        r = k
    end subroutine
end module

! Interface block for a bind(c) dummy procedure
subroutine bindc_62_apply(f, k, r) bind(c, name="bindc_62_capply")
    use iso_c_binding, only: c_int
    implicit none
    interface
        function f(i) bind(c) result(j)
            import :: c_int
            integer(c_int), value :: i
            integer(c_int) :: j
        end function
    end interface
    integer(c_int), value :: k
    integer(c_int), intent(out) :: r
    r = f(k)
end subroutine

! Interface block for an external procedure
subroutine bindc_62_twice(x, r) bind(c, name="bindc_62_ctwice")
    use iso_c_binding, only: c_int
    implicit none
    integer(c_int), value :: x
    integer(c_int), intent(out) :: r
    interface
        subroutine bindc_62_helper(x, r)
            integer, intent(in) :: x
            integer, intent(out) :: r
        end subroutine
    end interface
    call bindc_62_helper(x, r)
end subroutine

! Interface block for an external function, in a bind(c) function
function bindc_62_thrice(x) bind(c, name="bindc_62_cthrice") result(r)
    use iso_c_binding, only: c_int
    implicit none
    interface
        integer function bindc_62_triple(x)
            integer, intent(in) :: x
        end function
    end interface
    integer(c_int), value :: x
    integer(c_int) :: r
    r = bindc_62_triple(x)
end function

program bindc_62
    use iso_c_binding, only: c_int
    implicit none
    interface
        function call_from_c() bind(c, name="bindc_62_call_from_c") result(r)
            import :: c_int
            integer(c_int) :: r
        end function
    end interface
    integer(c_int) :: r
    r = call_from_c()
    print *, r
    if (r /= 5 + 1 + 2 * 21 + 3 * 4 + 7 + 100) error stop 1
    print *, "OK"
end program
