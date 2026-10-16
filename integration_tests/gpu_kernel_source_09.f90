module gpu_kernel_source_09_m
implicit none
type :: point
    real :: a, b, c
end type
type :: aggregate
    real :: a(120000)
end type
contains
    pure subroutine update(q, j)
        type(aggregate), intent(inout) :: q
        integer, intent(in) :: j
        q%a = j
    end subroutine
    pure real function read_value(q)
        type(aggregate), intent(in) :: q
        read_value = q%a(1) + q%a(120000)
    end function
end module

program gpu_kernel_source_09
use gpu_kernel_source_09_m
implicit none
integer, parameter :: n = 4
real :: small(6000), large(20000), y(n)
type(aggregate) :: shared(n)
integer :: j, k

! The derived-type array fits Metal's stack alone, but not alongside small.
do concurrent (j = 1:n)
    do k = 1, 6000
        small(k) = k + j
    end do
    block
        type(point) :: q(4000)
        q%a = j
        q%b = 1
        q%c = 2
        y(j) = small(6000) + q(4000)%a + q(1)%c
    end block
end do
do j = 1, n
    if (y(j) /= 6002 + 2 * j) error stop 1
end do

! A scalar aggregate's fixed array also occupies stack storage.
! This combination exceeds CUDA's stack budget: large must spill.
do concurrent (j = 1:n)
    do k = 1, 20000
        large(k) = k + j
    end do
    block
        type(aggregate) :: q
        q%a = j
        y(j) = large(20000) + q%a(1) + q%a(120000)
    end block
end do
do j = 1, n
    if (y(j) /= 20000 + 3 * j) error stop 2
end do

! An updating aggregate actual is materialized before storage placement,
! not afterward by address-space specialization.
do concurrent (j = 1:n)
    do k = 1, 20000
        large(k) = k + j
    end do
    call update(shared(j), j)
    y(j) = large(20000) + shared(j)%a(1)
end do
do j = 1, n
    if (y(j) /= 20000 + 2 * j) error stop 3
end do
do concurrent (j = 1:n)
    do k = 1, 20000
        large(k) = k + j
    end do
    y(j) = large(20000) + read_value(shared(j))
end do
do j = 1, n
    if (y(j) /= 20000 + 3 * j) error stop 4
end do
print *, "ok"
end program
