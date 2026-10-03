! A per-thread workspace sized from a fixed-size array component of an
! array of derived type, indexed by the loop variable.
!
! `twice(c(j)%m)` returns an allocatable array, so the later passes give
! the loop body a temporary holding the result and allocate it with
! `size(c(j)%m)`.  The workspace analysis read that extent as "the size of
! a component of struct element `j`", and `j` has no value on the host, so
! the host could not build the extent and the launch aborted.
!
! `m` is declared `real :: m(2)`, though: its shape is in the type, so
! `size(c(j)%m)` is 2 for every `j` and the extent is a compile-time
! constant that never needs the index at all.
program gpu_metal_314
implicit none

type :: t
    real :: m(2)
end type

type(t) :: c(2)
real :: o(2, 2)
integer :: i, j

do j = 1, 2
    do i = 1, 2
        c(j)%m(i) = real(10 * j + i)
    end do
end do

o = 0.0
do concurrent (j = 1:2)
    o(:, j) = twice(c(j)%m)
end do

print *, o
do j = 1, 2
    do i = 1, 2
        if (abs(o(i, j) - 2.0 * real(10 * j + i)) > 1.0e-5) error stop
    end do
end do
print *, "PASS"

contains

    pure function twice(v) result(r)
        real, intent(in) :: v(:)
        real, allocatable :: r(:)
        allocate(r(size(v)))
        r = 2.0 * v
    end function

end program
