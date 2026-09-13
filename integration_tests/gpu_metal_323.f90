! An array whose extent is an expression of its own, reached through an
! ASSOCIATE construct: the associate name carries the extent `2*n` in its
! own type rather than as a size argument of the routine. That extent is
! the stride the second subscript is counted by, so the kernel has to
! evaluate the expression; striding by anything else reads a different
! column of the array.
module gpu_metal_323_mod
implicit none
contains
subroutine work(n, m, a, r)
    integer, intent(in) :: n, m
    real, intent(in) :: a(2*n, m)
    real, intent(out) :: r(3)
    integer :: i
    associate (b => a)
        do concurrent (i = 1:3)
            r(i) = b(i, 2)
        end do
    end associate
end subroutine
end module

program gpu_metal_323
use gpu_metal_323_mod
implicit none
real :: a(4,3), r(3)
integer :: i, j

do j = 1, 3
    do i = 1, 4
        a(i,j) = real(i) + 10.0*real(j)
    end do
end do

call work(2, 3, a, r)

do i = 1, 3
    if (abs(r(i) - (real(i) + 20.0)) > 1.0e-5) error stop
end do

print *, "PASS"
end program
