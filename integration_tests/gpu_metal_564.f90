! A `do concurrent` nested in an IF, DO, DO WHILE, SELECT CASE or BLOCK
! of a pure procedure called from an offloaded `do concurrent` is run by
! the device copy of that procedure; the enclosing statement must be kept.
module gpu_metal_564_m
implicit none
contains

    pure subroutine in_if(flag, d)
        logical, intent(in) :: flag
        real, intent(out) :: d(3)
        integer :: i
        if (flag) then
            do concurrent (i = 1:3)
                d(i) = i
            end do
        else
            do concurrent (i = 1:3)
                d(i) = -i
            end do
        end if
    end subroutine

    pure subroutine in_do(d)
        real, intent(out) :: d(3)
        integer :: i, k
        d = 0
        do k = 1, 2
            do concurrent (i = 1:3)
                d(i) = d(i) + k * i
            end do
        end do
    end subroutine

    pure subroutine in_while(d)
        real, intent(out) :: d(3)
        integer :: i, k
        d = 0
        k = 0
        do while (k < 3)
            k = k + 1
            do concurrent (i = 1:3)
                d(i) = d(i) + i
            end do
        end do
    end subroutine

    pure subroutine in_select(n, d)
        integer, intent(in) :: n
        real, intent(out) :: d(3)
        integer :: i
        select case (n)
        case (1)
            do concurrent (i = 1:3)
                d(i) = 10 * i
            end do
        case default
            do concurrent (i = 1:3)
                d(i) = 20 * i
            end do
        end select
    end subroutine

    pure subroutine in_block(d)
        real, intent(out) :: d(3)
        block
            integer :: i
            do concurrent (i = 1:3)
                d(i) = 7 * i
            end do
        end block
    end subroutine

end module

program gpu_metal_564
use gpu_metal_564_m, only: in_if, in_do, in_while, in_select, in_block
implicit none
real :: a(3, 4), b(3, 4), c(3, 4), e(3, 4), f(3, 4)
integer :: i, j

a = 0
b = 0
c = 0
e = 0
f = 0
do concurrent (j = 1:4)
    call in_if(mod(j, 2) == 1, a(:, j))
    call in_do(b(:, j))
    call in_while(c(:, j))
    call in_select(j, e(:, j))
    call in_block(f(:, j))
end do

do j = 1, 4
    do i = 1, 3
        if (mod(j, 2) == 1) then
            if (a(i, j) /= i) error stop 1
        else
            if (a(i, j) /= -i) error stop 2
        end if
        if (b(i, j) /= 3 * i) error stop 3
        if (c(i, j) /= 3 * i) error stop 4
        if (j == 1) then
            if (e(i, j) /= 10 * i) error stop 5
        else
            if (e(i, j) /= 20 * i) error stop 6
        end if
        if (f(i, j) /= 7 * i) error stop 7
    end do
end do
print *, "ok"
end program
