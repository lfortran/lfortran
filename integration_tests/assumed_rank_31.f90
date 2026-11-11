module assumed_rank_31_m
    ! size() of an assumed-rank dummy associated with a scalar is 1, also
    ! inside a larger bound of another dummy or of the function result, and
    ! as a character length.
    implicit none
contains
    subroutine fill(src, dst)
        real, intent(in) :: src(..)
        real, intent(out) :: dst(2*size(src) + 1)
        if (size(dst) /= 2*size(src) + 1) error stop "fill: wrong size"
        dst = 2.5
    end subroutine

    subroutine fill_str(src, dst)
        real, intent(in) :: src(..)
        character(len=size(src)), intent(out) :: dst
        if (len(dst) /= size(src)) error stop "fill_str: wrong length"
        dst = repeat("x", len(dst))
    end subroutine

    function make(src) result(r)
        real, intent(in) :: src(..)
        real :: r(2*size(src) + 1)
        r = 3.0
    end function

    function make_str(src) result(r)
        real, intent(in) :: src(..)
        character(len=size(src)) :: r
        r = repeat("y", len(r))
    end function

    real function g(x)
        real, intent(in) :: x
        g = x + 1.0
    end function
end module

program assumed_rank_31
    use assumed_rank_31_m
    implicit none
    real :: s, m(2, 3), d3(3), d13(13)
    character(len=1) :: c1
    character(len=6) :: c6
    s = 1.0
    m = 0.0

    d3 = 0.0
    call fill(s, d3)
    if (any(d3 /= 2.5)) error stop 1
    d3 = 0.0
    call fill(m(1, 1), d3)
    if (any(d3 /= 2.5)) error stop 2
    d3 = 0.0
    call fill(g(s), d3)
    if (any(d3 /= 2.5)) error stop 3
    d3 = 0.0
    call fill(sum(m), d3)
    if (any(d3 /= 2.5)) error stop 4
    d13 = 0.0
    call fill(m, d13)
    if (any(d13 /= 2.5)) error stop 5

    d3 = 0.0
    d3 = make(s)
    if (any(d3 /= 3.0)) error stop 6
    d3 = 0.0
    d3 = make(m(1, 1))
    if (any(d3 /= 3.0)) error stop 7
    d3 = 0.0
    d3 = make(g(s))
    if (any(d3 /= 3.0)) error stop 8
    d3 = 0.0
    d3 = make(sum(m))
    if (any(d3 /= 3.0)) error stop 9
    if (size(make(m)) /= 13) error stop 10

    c1 = ""
    call fill_str(s, c1)
    if (c1 /= "x") error stop 11
    c1 = ""
    call fill_str(m(1, 1), c1)
    if (c1 /= "x") error stop 12
    c6 = ""
    call fill_str(m, c6)
    if (c6 /= "xxxxxx") error stop 13

    c6 = ""
    c6 = make_str(m)
    if (c6 /= "yyyyyy") error stop 14
    if (len(make_str(m)) /= 6) error stop 15
    print *, "OK"
end program
