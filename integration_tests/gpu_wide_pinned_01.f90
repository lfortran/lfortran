module gpu_wide_pinned_01_m
use iso_fortran_env, only: int16, int32, real64
implicit none
contains
    ! Matrix-vector products whose lanes share the loop over the columns of
    ! a row: the vectors of 16 bytes of the matrix, which the kernel only
    ! reads, are read in the order of the statements on an NVIDIA GPU. A
    ! leading dimension that is not a multiple of the vector reads them
    ! element by element instead.
    subroutine mv4(a, x, y)
        integer(int32), intent(in) :: a(:, :)
        real, intent(in) :: x(:)
        real, intent(inout) :: y(:)
        integer :: i
        do concurrent (i = 1:size(a, 2))
            block
                real :: t
                integer :: c
                t = 0
                do c = 1, size(a, 1)/4
                    t = t + a(4*c-3, i)*x(4*c-3) + a(4*c-2, i)*x(4*c-2) &
                          + a(4*c-1, i)*x(4*c-1) + a(4*c, i)*x(4*c)
                end do
                y(i) = t
            end block
        end do
    end subroutine

    subroutine mv8(a, x, y)
        integer(int16), intent(in) :: a(:, :)
        real, intent(in) :: x(:)
        real, intent(inout) :: y(:)
        integer :: i
        do concurrent (i = 1:size(a, 2))
            block
                real :: t
                integer :: c
                t = 0
                do c = 1, size(a, 1)/8
                    t = t + a(8*c-7, i)*x(8*c-7) + a(8*c-6, i)*x(8*c-6) + a(8*c-5, i)*x(8*c-5) &
                          + a(8*c-4, i)*x(8*c-4) + a(8*c-3, i)*x(8*c-3) + a(8*c-2, i)*x(8*c-2) &
                          + a(8*c-1, i)*x(8*c-1) + a(8*c, i)*x(8*c)
                end do
                y(i) = t
            end block
        end do
    end subroutine

    subroutine mv2(a, x, y)
        real(real64), intent(in) :: a(:, :)
        real(real64), intent(in) :: x(:)
        real(real64), intent(inout) :: y(:)
        integer :: i
        do concurrent (i = 1:size(a, 2))
            block
                real(real64) :: t
                integer :: c
                t = 0
                do c = 1, size(a, 1)/2
                    t = t + a(2*c-1, i)*x(2*c-1) + a(2*c, i)*x(2*c)
                end do
                y(i) = t
            end block
        end do
    end subroutine
end module

program gpu_wide_pinned_01
! Every value is a small integer, so the sums are exact in any order.
use gpu_wide_pinned_01_m
implicit none
integer :: lead(2), k, n, i, j
integer(int32), allocatable :: a4(:, :)
integer(int16), allocatable :: a8(:, :)
real(real64), allocatable :: a2(:, :), x2(:), y2(:)
real, allocatable :: x(:), y(:)
lead = [1024, 1026]
n = 300
do k = 1, size(lead)
    allocate(a4(lead(k), n), a8(lead(k), n), a2(lead(k), n), x(lead(k)), x2(lead(k)), &
        y(n), y2(n))
    do j = 1, n
        do i = 1, lead(k)
            a4(i, j) = mod(3*i + 7*j, 11) - 5
            a8(i, j) = int(mod(5*i + j, 9) - 4, int16)
            a2(i, j) = real(mod(i + 2*j, 7) - 3, real64)
        end do
    end do
    do i = 1, lead(k)
        x(i) = real(mod(i, 5) - 2)
        x2(i) = real(mod(i, 3) - 1, real64)
    end do
    call mv4(a4, x, y)
    do j = 1, n
        if (y(j) /= sum(a4(1:4*(lead(k)/4), j)*x(1:4*(lead(k)/4)))) error stop 1
    end do
    call mv8(a8, x, y)
    do j = 1, n
        if (y(j) /= sum(a8(1:8*(lead(k)/8), j)*x(1:8*(lead(k)/8)))) error stop 2
    end do
    call mv2(a2, x2, y2)
    do j = 1, n
        if (y2(j) /= sum(a2(1:2*(lead(k)/2), j)*x2(1:2*(lead(k)/2)))) error stop 3
    end do
    deallocate(a4, a8, a2, x, x2, y, y2)
end do
print *, "ok"
end program
