! Implicit-interface calls inside BLOCK and ASSOCIATE constructs of a
! procedure with an assumed-shape array dummy.
module implicit_interface_103_m
implicit none
contains
subroutine work(a)
    real(8), intent(inout) :: a(:)
    external implicit_interface_103_inc
    block
        external implicit_interface_103_dbl
        call implicit_interface_103_dbl(a(1))
    end block
    associate (y => a(2))
        call implicit_interface_103_inc(y)
    end associate
end subroutine
end module

subroutine implicit_interface_103_dbl(x)
real(8), intent(inout) :: x
x = 2*x
end subroutine

subroutine implicit_interface_103_inc(x)
real(8), intent(inout) :: x
x = x + 1
end subroutine

program implicit_interface_103
use implicit_interface_103_m
implicit none
real(8) :: a(2) = [3d0, 4d0]
call work(a)
print *, a
if (abs(a(1) - 6d0) > 1d-12) error stop
if (abs(a(2) - 5d0) > 1d-12) error stop
end program
