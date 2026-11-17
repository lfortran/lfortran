! Bound expressions in `do concurrent` must be visible inside the OpenMP outline.
! A bare bound already is; `n-1` / `n+2` are not, unless every variable they use
! is recorded the same way.
program openmp_120
implicit none
integer, parameter :: np = 5
integer :: n, i
integer :: a(8)

n = 4

a = 0
! program variable, upper-bound expression
do concurrent (i = 1:n - 1)
    a(i) = i
end do
if (a(1) /= 1 .or. a(2) /= 2 .or. a(3) /= 3 .or. a(4) /= 0) error stop 1

a = 0
! both bounds are expressions, so `n` is not captured by a bare Var bound
do concurrent (i = n - 1:n + 2)
    a(i) = i
end do
if (a(1) /= 0 .or. a(2) /= 0 .or. a(3) /= 3 .or. a(4) /= 4) error stop 2
if (a(5) /= 5 .or. a(6) /= 6 .or. a(7) /= 0) error stop 3

a = 0
! bare upper bound
do concurrent (i = 2:n)
    a(i) = i
end do
if (a(1) /= 0 .or. a(2) /= 2 .or. a(3) /= 3 .or. a(4) /= 4) error stop 4
if (a(5) /= 0) error stop 5

a = 0
! expression only in the lower bound; the upper bound is a bare Var
do concurrent (i = n - 1:n)
    a(i) = i
end do
if (a(1) /= 0 .or. a(2) /= 0 .or. a(3) /= 3 .or. a(4) /= 4) error stop 6
if (a(5) /= 0) error stop 7

a = 0
! parameter
do concurrent (i = 1:np - 1)
    a(i) = i
end do
if (a(1) /= 1 .or. a(2) /= 2 .or. a(3) /= 3 .or. a(4) /= 4) error stop 8
if (a(5) /= 0) error stop 9

a = 0
do concurrent (i = 1:n + 1)
    a(i) = i
end do
if (a(5) /= 5 .or. a(6) /= 0) error stop 10

a = 0
do concurrent (i = 1:2 * n)
    a(i) = i
end do
if (a(1) /= 1 .or. a(8) /= 8) error stop 11

call intent_in_upper(a, n)
call plain_dummy_bounds(a, n)
contains
    subroutine intent_in_upper(b, m)
        integer, intent(in) :: m
        integer :: b(8)
        integer :: k
        b = 0
        do concurrent (k = 1:m - 1)
            b(k) = 10 + k
        end do
        if (b(1) /= 11 .or. b(2) /= 12 .or. b(3) /= 13 .or. b(4) /= 0) error stop 12
    end subroutine intent_in_upper

    subroutine plain_dummy_bounds(b, m)
        integer :: m
        integer :: b(8)
        integer :: k, p
        b = 0
        do concurrent (k = 1:m - 1)
            b(k) = 20 + k
        end do
        if (b(1) /= 21 .or. b(2) /= 22 .or. b(3) /= 23 .or. b(4) /= 0) error stop 13

        ! lower-bound expression; the upper bound does not name `m`
        do concurrent (k = m - 1:8)
            b(k) = 30 + k
        end do
        if (b(1) /= 21 .or. b(2) /= 22 .or. b(3) /= 33 .or. b(8) /= 38) error stop 14

        b = 0
        do concurrent (k = m - 1:m + 2)
            b(k) = k
        end do
        if (b(1) /= 0 .or. b(2) /= 0 .or. b(3) /= 3 .or. b(4) /= 4) error stop 15
        if (b(5) /= 5 .or. b(6) /= 6 .or. b(7) /= 0) error stop 16

        do concurrent (k = 2:m)
            b(k) = 40 + k
        end do
        if (b(1) /= 0 .or. b(2) /= 42 .or. b(3) /= 43 .or. b(4) /= 44) error stop 17
        if (b(5) /= 5) error stop 18

        ! local, not a dummy, used only by an upper-bound expression
        p = m
        b = 0
        do concurrent (k = 1:p - 1)
            b(k) = k
        end do
        if (b(1) /= 1 .or. b(2) /= 2 .or. b(3) /= 3 .or. b(4) /= 0) error stop 19
    end subroutine plain_dummy_bounds
end program openmp_120
