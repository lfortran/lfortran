! A kernel that assigns every element of an array before it reads any need
! not have the array copied to the device first. Each loop below either does
! that, so the copy may be skipped, or leaves some element unassigned or
! reads one first, so it may not: the elements the loop does not assign
! must keep their values, and the values it reads must be the ones the host
! put there.
module gpu_metal_505_m
implicit none
contains

    ! Every element, through the loop variables.
    subroutine whole(a, b, n, m)
        integer, intent(in) :: n, m
        real, intent(inout) :: a(:,:)
        real, intent(in) :: b(:,:)
        integer :: i, j
        do concurrent (j = 1:m, i = 1:n)
            a(i, j) = b(i, j) + 1
        end do
    end subroutine

    ! The interior only: the edges keep their values.
    subroutine interior(a, n, m)
        integer, intent(in) :: n, m
        real, intent(inout) :: a(:,:)
        integer :: i, j
        do concurrent (j = 2:m - 1, i = 2:n - 1)
            a(i, j) = -1
        end do
    end subroutine

    ! An element is assigned only when the data says so.
    subroutine where_positive(a, b, n, m)
        integer, intent(in) :: n, m
        real, intent(inout) :: a(:,:)
        real, intent(in) :: b(:,:)
        integer :: i, j
        do concurrent (j = 1:m, i = 1:n)
            if (b(i, j) > 0) a(i, j) = 2
        end do
    end subroutine

    ! Each element is read before it is assigned.
    subroutine increment(a, n, m)
        integer, intent(in) :: n, m
        real, intent(inout) :: a(:,:)
        integer :: i, j
        real :: t
        do concurrent (j = 1:m, i = 1:n) local(t)
            t = a(i, j)
            a(i, j) = t + 1
        end do
    end subroutine

    ! A guard that leaves the last row out.
    subroutine guarded_short(a, n, m)
        integer, intent(in) :: n, m
        real, intent(inout) :: a(:,:)
        integer :: i, j
        do concurrent (j = 1:m, i = 1:n)
            if (i <= n - 1) a(i, j) = 3
        end do
    end subroutine

    ! A guard that cuts a longer loop down to the array: every element.
    subroutine guarded_whole(a, n, m)
        integer, intent(in) :: n, m
        real, intent(inout) :: a(:,:)
        integer :: i, j
        do concurrent (j = 1:m, i = 1:n + 1)
            if (i <= n) a(i, j) = 4
        end do
    end subroutine

    ! A loop variable that runs over one dimension only.
    subroutine one_column(a, n)
        integer, intent(in) :: n
        real, intent(inout) :: a(:,:)
        integer :: i
        do concurrent (i = 1:n)
            a(i, 2) = 6
        end do
    end subroutine

end module

program gpu_metal_505
use gpu_metal_505_m
implicit none
integer, parameter :: n = 6, m = 5
real, allocatable :: a(:,:), b(:,:), c(:,:)
integer :: i, j, k

allocate(a(n, m), b(n, m), c(n + 1, m))
do j = 1, m
    do i = 1, n
        b(i, j) = merge(1.0, -1.0, mod(i + j, 2) == 0)
    end do
end do

do k = 1, 2
    a = 7
    call whole(a, b, n, m)
    if (any(a /= b + 1)) error stop 1

    a = 7
    call interior(a, n, m)
    if (any(a(2:n-1, 2:m-1) /= -1)) error stop 2
    if (any(a(1, :) /= 7) .or. any(a(n, :) /= 7)) error stop 3
    if (any(a(:, 1) /= 7) .or. any(a(:, m) /= 7)) error stop 4

    a = 7
    call where_positive(a, b, n, m)
    if (any(a /= merge(2.0, 7.0, b > 0))) error stop 5

    a = 7
    call increment(a, n, m)
    if (any(a /= 8)) error stop 6

    a = 7
    call guarded_short(a, n, m)
    if (any(a(1:n-1, :) /= 3)) error stop 7
    if (any(a(n, :) /= 7)) error stop 8

    a = 7
    call guarded_whole(a, n, m)
    if (any(a /= 4)) error stop 9

    a = 7
    call one_column(a, n)
    if (any(a(:, 2) /= 6)) error stop 11
    if (any(a(:, 1) /= 7) .or. any(a(:, 3:) /= 7)) error stop 12

    ! An array whose first dimension is longer than the loop.
    c = 7
    call guarded_whole(c, n, m)
    if (any(c(1:n, :) /= 4)) error stop 13
    if (any(c(n + 1, :) /= 7)) error stop 14
end do
print *, "ok"
end program
