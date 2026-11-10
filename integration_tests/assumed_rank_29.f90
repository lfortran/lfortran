module assumed_rank_29_m
    ! Regression for #17223: size() of an assumed-rank dummy in a
    ! specification expression is the size of the actual argument, not 1.
    implicit none
contains
    subroutine fill(src, dst)
        real, intent(in) :: src(..)
        real, intent(out) :: dst(size(src))
        if (size(dst) /= size(src)) error stop "fill: wrong size"
        dst = 2.5
    end subroutine

    subroutine fill_dim(src, dst)
        real, intent(in) :: src(..)
        real, intent(out) :: dst(size(src, 2))
        if (size(dst) /= size(src, 2)) error stop "fill_dim: wrong size"
        dst = 1.5
    end subroutine

    integer function count_local(src) result(n)
        real, intent(in) :: src(..)
        real :: tmp(size(src))
        tmp = 1.0
        n = size(tmp)
    end function

    function make(src) result(r)
        real, intent(in) :: src(..)
        real :: r(size(src))
        r = 3.0
    end function
end module

program assumed_rank_29
    use assumed_rank_29_m
    implicit none
    real :: m(2, 3), flat(6), v(4), dv(4), d3(3), c(2, 3, 2), dc(12)
    m = 0.0
    flat = 0.0
    call fill(m, flat)
    if (any(flat /= 2.5)) error stop 1

    v = 0.0
    dv = 0.0
    call fill(v, dv)
    if (any(dv /= 2.5)) error stop 2

    c = 0.0
    dc = 0.0
    call fill(c, dc)
    if (any(dc /= 2.5)) error stop 3

    d3 = 0.0
    call fill_dim(m, d3)
    if (any(d3 /= 1.5)) error stop 4

    if (count_local(m) /= 6) error stop 5
    if (count_local(c) /= 12) error stop 6

    if (size(make(m)) /= 6) error stop 7
    if (any(make(c) /= 3.0)) error stop 8
    print *, "OK"
end program
