module kwarg_gp
    interface mergegp
        procedure :: mergei32, merger32, merger64
    end interface

contains

    function mergei32(a, b, m) result(r)
        integer :: a(:), b(:)
        integer, optional :: m(:)
        integer :: r
    end function

    function merger32(a, b, m) result(r)
        real :: a(:), b(:)
        real, optional :: m(:)
        real :: r
    end function

    function merger64(a, b, m) result(r)
        real(8) :: a(:), b(:)
        real(8), optional :: m(:)
        real(8) :: r
    end function

end module

program kwarg_use
use kwarg_gp
implicit none

integer :: a1(4), b1(4), m1(4)
real :: a2(5), b2(5), m2(5)

print *, mergegp(a1, b1)
print *, mergegp(a1, b1, m=m1)
print *, mergegp(a1, b1, m1)
print *, mergegp(a2, b2)
print *, mergegp(a2, b2, m=m2)
print *, mergegp(a2, b2, m2)

end program