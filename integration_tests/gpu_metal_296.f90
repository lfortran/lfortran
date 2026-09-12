! A function result copied from the second allocatable member of a
! struct-array kernel argument. The workspace used to be sized from the
! first allocatable member of the first such argument. `a_` is longer
! than `b_`, so guessing `a_` for a copy of `b_` is a wrong extent.
program gpu_metal_296
implicit none

type :: pair_t
    real, allocatable :: a_(:)
    real, allocatable :: b_(:)
end type

type(pair_t) :: c(4)
real :: o(2, 4)
integer :: i, j

do j = 1, 4
    allocate(c(j)%a_(5))
    allocate(c(j)%b_(2))
    c(j)%a_ = 100.0
    do i = 1, 2
        c(j)%b_(i) = real(10 * j + i)
    end do
end do

o = 0.0
call run_from_b(o, c, 4)

do j = 1, 4
    do i = 1, 2
        if (abs(o(i, j) - 2.0 * real(10 * j + i)) > 1.0e-4) error stop "b"
    end do
end do
print *, "PASS"

contains

    pure function twice(v) result(res)
        real, intent(in) :: v(:)
        real, allocatable :: res(:)
        integer :: k
        allocate(res(size(v)))
        do k = 1, size(v)
            res(k) = 2.0 * v(k)
        end do
    end function

    subroutine run_from_b(out, s, nn)
        real, intent(out) :: out(:,:)
        type(pair_t), intent(in) :: s(:)
        integer, intent(in) :: nn
        integer :: jj
        do concurrent (jj = 1:nn)
            out(:, jj) = twice(s(jj)%b_)
        end do
    end subroutine

end program
