! Index-set splitting of boundary stencils whose loops do not run over every
! element of the array they assign: the interior and the frame together
! cover only the loop's iterations, so the launches must still copy the
! array to the device, and the elements outside keep the host's values.
module gpu_metal_528_m
implicit none
contains

    pure real function diff(a, i, lo, hi) result(r)
        integer, intent(in) :: i, lo, hi
        real, intent(in) :: a(:)
        if (i == lo) then
            r = a(i + 1) - a(i)
        else if (i == hi) then
            r = a(i) - a(i - 1)
        else
            r = 0.5 * (a(i + 1) - a(i - 1))
        end if
    end function

    pure real function edge(b, i, j, lo, hi, m) result(r)
        integer, intent(in) :: i, j, lo, hi, m
        real, intent(in) :: b(:,:)
        if (j == 1 .or. j == m .or. i == lo .or. i == hi) then
            r = b(i, j)
        else
            r = b(i - 1, j) + b(i + 1, j) - 2 * b(i, j)
        end if
    end function

    ! The loop runs over 2:n - 1 of an array of n elements.
    subroutine inner(a, d, n)
        integer, intent(in) :: n
        real, intent(in) :: a(:)
        real, intent(inout) :: d(:)
        integer :: i
        do concurrent (i = 2:n - 1)
            d(i) = diff(a, i, 2, n - 1)
        end do
    end subroutine

    ! The loop runs over the first k rows of an array of n rows.
    subroutine rows(b, e, k, m)
        integer, intent(in) :: k, m
        real, intent(in) :: b(:,:)
        real, intent(inout) :: e(:,:)
        integer :: i, j
        do concurrent (j = 1:m, i = 1:k)
            e(i, j) = edge(b, i, j, 1, k, m)
        end do
    end subroutine

end module

program gpu_metal_528
use gpu_metal_528_m, only: inner, rows
implicit none
integer, parameter :: n = 40, m = 24, k = 30
real, allocatable :: a(:), d(:), b(:,:), e(:,:)
integer :: i, j
allocate(a(n), d(n), b(n, m), e(n, m))
do i = 1, n
    a(i) = real(i * i)
end do
do j = 1, m
    do i = 1, n
        b(i, j) = real(i * i + j)
    end do
end do
d = -100
e = -100
call inner(a, d, n)
call rows(b, e, k, m)
if (d(1) /= -100 .or. d(n) /= -100) error stop 1
if (d(2) /= 5) error stop 2
if (d(n - 1) /= real(2 * n - 3)) error stop 3
do i = 3, n - 2
    if (d(i) /= real(2 * i)) error stop 4
end do
do j = 1, m
    do i = 1, n
        if (i > k) then
            if (e(i, j) /= -100) error stop 5
        else if (j == 1 .or. j == m .or. i == 1 .or. i == k) then
            if (e(i, j) /= b(i, j)) error stop 6
        else
            if (e(i, j) /= 2) error stop 7
        end if
    end do
end do
print *, "ok"
end program
