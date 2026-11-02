! A procedure value of an internal function with a pointer result that refers
! to a host variable: the thunk associates its result with the result of
! the internal function, rather than assigning to it
module host_assoc_26_m
implicit none
abstract interface
    function getter() result(p)
        integer, pointer :: p
    end function
end interface
contains
subroutine use_getter(f, expected)
    procedure(getter) :: f
    integer, intent(in) :: expected
    integer, pointer :: p
    p => f()
    if (.not. associated(p)) error stop 1
    if (p /= expected) error stop 2
    p = p + 1
end subroutine

subroutine host(n)
    integer, intent(in) :: n
    integer, target :: k
    k = n
    call use_getter(get, n)
    if (k /= n + 1) error stop 3
contains
    function get() result(p)
        integer, pointer :: p
        p => k
    end function
end subroutine
end module host_assoc_26_m

program host_assoc_26
use host_assoc_26_m
implicit none
call host(5)
call host(42)
print *, "ok"
end program host_assoc_26
