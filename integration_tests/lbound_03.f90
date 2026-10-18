module lbound_03_m
    implicit none
    abstract interface
        function lbound_03_iface(n) result(r)
            integer, intent(in) :: n
            integer :: r(n:n+4)
        end function
    end interface
contains
    function lbound_03_h(n) result(r)
        integer, intent(in) :: n
        integer :: r(n:n+4)
        r = n
    end function
end module

program lbound_03
    ! A function reference is not a whole array, so lbound gives 1 and
    ! ubound gives the extent, whatever bounds the result variable has.
    use lbound_03_m
    implicit none
    procedure(lbound_03_iface), pointer :: fp
    integer :: n
    n = 3
    if (lbound(f(), 1) /= 1) error stop 1
    if (ubound(f(), 1) /= 5) error stop 2
    if (any(lbound(f()) /= [1])) error stop 3
    if (any(ubound(f()) /= [5])) error stop 4
    if (lbound(g(n), 1) /= 1) error stop 5
    if (any(ubound(g(n)) /= [5])) error stop 6
    fp => lbound_03_h
    if (lbound(fp(n), 1) /= 1) error stop 7
    if (any(ubound(fp(n)) /= [5])) error stop 8
    print *, "PASS"
contains
    function f() result(r)
        integer :: r(6:10)
        r = 1
    end function
    function g(n) result(r)
        integer, intent(in) :: n
        integer :: r(2*n:10)
        r = 1
    end function
end program
