! With --legacy-array-sections, a module procedure passes an array element
! to an array dummy of an external procedure defined later in the same file
! (sequence association of the element and the elements that follow it).
module external_33_mod
implicit none
contains
    subroutine double_column(n, m, c, k)
    integer, intent(in) :: n, m, k
    real, intent(inout) :: c(n,m)
    call external_33_double(n, c(1,k))
    end subroutine
end module

program external_33
use external_33_mod
implicit none
real :: c(2,3)
c = 1
call double_column(2, 3, c, 2)
print '(6F4.1)', c
if (any(abs(reshape(c, [6]) - [1, 1, 2, 2, 1, 1]) > 1e-6)) error stop 1
end program

subroutine external_33_double(n, x)
implicit none
integer, intent(in) :: n
real, intent(inout) :: x(n)
x = 2*x
end subroutine
