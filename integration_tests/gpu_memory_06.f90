module gpu_memory_06_m
implicit none
contains
    ! The temporaries of a procedure that launches kernels are freed when it
    ! returns, while the kernels may still run: the memory goes back to a
    ! cache of blocks, and a later allocation reuses it once the host has
    ! waited for the device.
    subroutine scale_sum(x, total)
        real, intent(in) :: x(:)
        real, intent(out) :: total
        real :: y(size(x)), z(size(x))
        integer :: i

        do concurrent (i = 1:size(x))
            y(i) = 2*x(i)
        end do
        do concurrent (i = 1:size(x))
            z(i) = y(i) + 1
        end do
        total = sum(z)
    end subroutine
end module

program gpu_memory_06
! Arrays of a few kilobytes that are allocated, written by a kernel and freed
! in a loop, in the managed memory model: the host reads what the kernel
! wrote right after the kernel, and again after the memory was freed and
! allocated again.
use gpu_memory_06_m
implicit none
integer, parameter :: n = 2048
real, allocatable :: a(:), b(:)
real :: x(n), total
integer :: i, it

do i = 1, n
    x(i) = i
end do
do it = 1, 200
    call scale_sum(x, total)
    if (total /= real(n)*(n + 1) + n) error stop "scale_sum"
    allocate(a(n), b(n))
    do concurrent (i = 1:n)
        a(i) = it + i
    end do
    do concurrent (i = 1:n)
        b(i) = a(i) + a(n - i + 1)
    end do
    if (b(3) /= 2*it + 3 + n - 2 .or. a(n) /= it + n) error stop "kernel result"
    do i = 1, n
        a(i) = -1
    end do
    do concurrent (i = 1:n)
        b(i) = a(i)
    end do
    if (b(n) /= -1) error stop "host write"
    deallocate(a, b)
end do
print *, "ok"
end program
