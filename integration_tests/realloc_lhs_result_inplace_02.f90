! The explicit-shape result of a function is not computed into the
! allocatable target it is assigned to when the function can reach the
! target, through an internal procedure passed to it or as a module
! variable: the expression is evaluated before the target is defined (F2018
! 10.2.1.3), and the function reads the target as it was. GFortran 15
! computes the result into the target in both cases and gets other values,
! so the test is not run with it.
module realloc_lhs_result_inplace_02_m
implicit none
real, allocatable :: mc(:)
abstract interface
    function reader_i(i) result(r)
        integer, intent(in) :: i
        real :: r
    end function
end interface
contains
! The elements the reader gives, in reverse order
function through(n, reader) result(r)
    integer, intent(in) :: n
    procedure(reader_i) :: reader
    real :: r(n)
    integer :: i
    do i = 1, n
        r(i) = reader(n + 1 - i)
    end do
end function
! The module variable, shifted, in reverse order
function shifted(n) result(r)
    integer, intent(in) :: n
    real :: r(n)
    integer :: i
    do i = 1, n
        r(i) = mc(n + 1 - i) + 1
    end do
end function
end module

program realloc_lhs_result_inplace_02
use realloc_lhs_result_inplace_02_m
implicit none
real, allocatable :: c(:)
c = [10., 20., 30.]
c = through(3, old_c)
if (size(c) /= 3 .or. any(c /= [30., 20., 10.])) error stop 1
c = [10., 20., 30., 40.]
c = through(3, old_c)
if (size(c) /= 3 .or. any(c /= [30., 20., 10.])) error stop 2
mc = [1., 2.]
mc = shifted(2)
if (any(mc /= [3., 2.])) error stop 3
print *, "ok"
contains
    function old_c(i) result(r)
        integer, intent(in) :: i
        real :: r
        r = c(i)
    end function
end program
