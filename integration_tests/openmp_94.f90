program openmp_94
! Whole-array assignments to explicit-shape dummy arguments inside OpenMP
! regions: the dimensions of the dummy keep referring to its own procedure
implicit none
integer :: v(8), w(3, 0:4)
v = 0
call fill(v, 8)
if (v(1) /= 5 .or. v(8) /= 5) error stop "whole-array assignment to explicit-shape dummy"
call add_one(v, 8)
if (v(1) /= 6 .or. v(8) /= 6) error stop "whole-array expression on explicit-shape dummy"
w = 0
call fill_2d(w, 3, 4)
if (w(1, 0) /= 7 .or. w(3, 4) /= 7) error stop "whole-array assignment to 2d explicit-shape dummy"
print *, v(1), w(3, 4)

contains

subroutine fill(a, n)
    integer, intent(in) :: n
    integer, intent(inout) :: a(n)
    !$omp parallel
    a = 5
    !$omp end parallel
end subroutine

subroutine add_one(a, n)
    integer, intent(in) :: n
    integer, intent(inout) :: a(n)
    integer :: i
    !$omp parallel do
    do i = 1, 1
        a = a + 1
    end do
    !$omp end parallel do
end subroutine

subroutine fill_2d(a, n, m)
    integer, intent(in) :: n, m
    integer, intent(inout) :: a(n, 0:m)
    !$omp parallel
    a = 7
    !$omp end parallel
end subroutine

end program
