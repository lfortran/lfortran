! Test: procedure pointer assignment with matching function interface.
! Regression test for ICE in continue-compilation mode where visit_Associate
! left tmp as a stale expression when types didn't fully match.
module associate_38_mod
    implicit none

    abstract interface
        function func_iface(x) result(r)
            integer, intent(in) :: x
            integer :: r
        end function
    end interface

contains

    function double_it(x) result(r)
        integer, intent(in) :: x
        integer :: r
        r = x * 2
    end function

    function triple_it(x) result(r)
        integer, intent(in) :: x
        integer :: r
        r = x * 3
    end function

end module

program associate_38
    use associate_38_mod
    implicit none
    procedure(func_iface), pointer :: fptr
    integer :: res

    fptr => double_it
    res = fptr(5)
    if (res /= 10) error stop

    fptr => triple_it
    res = fptr(4)
    if (res /= 12) error stop

    print *, "PASS"
end program
