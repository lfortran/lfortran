module assumed_rank_30_m
    ! Regression for #17116 and #17117: size() of an assumed-rank entity in
    ! the bound of a dummy, substituted at a call, is the size of its actual
    ! argument, not 1.
    implicit none
contains
    subroutine h2(a, n1, n2, total)
        integer, intent(in) :: n1, n2, a(n1, n2)
        integer, intent(out) :: total
        total = sum(a)
    end subroutine

    subroutine h1(a, n1, first)
        integer, intent(in) :: n1, a(n1, *)
        integer, intent(out) :: first(:)
        first = a(1:n1, 1)
    end subroutine

    subroutine by_dim(x, total, first)
        integer, intent(in) :: x(..)
        integer, intent(out) :: total, first(:)
        select rank (x)
        rank (2)
            call h2(x, size(x, 1), size(x, 2), total)
            call h1(x, size(x, 1), first)
        rank default
            error stop "by_dim: rank"
        end select
    end subroutine

    subroutine g(n, e, total)
        integer, intent(in) :: n, e(n)
        integer, intent(out) :: total
        total = sum(e)
    end subroutine

    subroutine whole(x, e, n, total)
        integer, intent(in) :: x(..), e(:)
        integer, intent(out) :: n, total
        select rank (x)
        rank (1)
            n = size(x)
            call g(size(x), e, total)
        rank default
            error stop "whole: rank"
        end select
    end subroutine

    subroutine fill(src, dst)
        real, intent(in) :: src(..)
        real, intent(out) :: dst(size(src))
        if (size(dst) /= size(src)) error stop "fill: wrong size"
        dst = 2.5
    end subroutine

    function make(src) result(r)
        real, intent(in) :: src(..)
        real :: r(size(src))
        r = 3.0
    end function

    ! Forward an assumed-rank dummy to assumed-rank dummies of other
    ! procedures whose sizes bound a dummy and a function result.
    subroutine forward(a, d, n)
        real, intent(in) :: a(..)
        real, intent(out) :: d(:)
        integer, intent(out) :: n
        call fill(a, d)
        n = size(make(a))
    end subroutine
end module

program assumed_rank_30
    use assumed_rank_30_m
    implicit none
    integer :: m(2, 2) = reshape([1, 2, 3, 4], [2, 2])
    integer :: v(3), w(0), first(2), n, total
    real :: a(2, 3), d6(6), s, d1(1)

    call by_dim(m, total, first)
    if (total /= 10) error stop 1
    if (any(first /= [1, 2])) error stop 2

    v = [1, 2, 3]
    call whole(v, v, n, total)
    if (n /= 3 .or. total /= 6) error stop 3
    call whole(v(1:0), w, n, total)
    if (n /= 0 .or. total /= 0) error stop 4

    a = 0.0
    d6 = 0.0
    call forward(a, d6, n)
    if (any(d6 /= 2.5)) error stop 5
    if (n /= 6) error stop 6

    s = 1.0
    d1 = 0.0
    call forward(s, d1, n)
    if (any(d1 /= 2.5)) error stop 7
    if (n /= 1) error stop 8
    print *, "OK"
end program
