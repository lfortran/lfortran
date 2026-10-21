! Index-set splitting runs a boundary stencil as an interior and a frame
! whose indices the body computes, which hides from the offload that the
! iterations assign every element of `d` and `e`. The splitting records it
! from the loops as written, so the launches still say when the arrays need
! not reach the device (see the ASR reference). The values the host put in
! them before must then not show in the results.
module gpu_metal_527_m
implicit none
contains

    pure real function diff(a, i, n) result(r)
        integer, intent(in) :: i, n
        real, intent(in) :: a(:)
        if (i == 1) then
            r = a(2) - a(1)
        else if (i == n) then
            r = a(n) - a(n - 1)
        else
            r = 0.5 * (a(i + 1) - a(i - 1))
        end if
    end function

    pure real function lap(b, i, j, n, m) result(r)
        integer, intent(in) :: i, j, n, m
        real, intent(in) :: b(:,:)
        if (j == 1 .or. j == m .or. i == 1 .or. i == n) then
            r = b(i, j)
        else
            r = b(i - 1, j) + b(i + 1, j) + b(i, j - 1) + b(i, j + 1) &
                - 4 * b(i, j)
        end if
    end function

end module

program gpu_metal_527
use gpu_metal_527_m, only: diff, lap
implicit none
integer, parameter :: n = 40, m = 24
real, allocatable :: a(:), d(:), b(:,:), e(:,:)
integer :: i, j
allocate(a(n), d(n), b(n, m), e(n, m))
do i = 1, n
    a(i) = real(i * i)
end do
do j = 1, m
    do i = 1, n
        b(i, j) = real(i + 3 * j * j)
    end do
end do
d = -100
e = -100
do concurrent (i = 1:n)
    d(i) = diff(a, i, n)
end do
do concurrent (j = 1:m, i = 1:n)
    e(i, j) = lap(b, i, j, n, m)
end do
if (d(1) /= 3) error stop 1
if (d(n) /= real(2 * n - 1)) error stop 2
do i = 2, n - 1
    if (d(i) /= real(2 * i)) error stop 3
end do
do j = 1, m
    do i = 1, n
        if (j == 1 .or. j == m .or. i == 1 .or. i == n) then
            if (e(i, j) /= b(i, j)) error stop 4
        else
            if (e(i, j) /= 6) error stop 5
        end if
    end do
end do
print *, "ok"
end program
