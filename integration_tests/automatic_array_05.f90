! Explicit-shape extents are zero when the upper bound precedes the lower.
program automatic_array_05
    implicit none
    integer :: n, lo, hi, i, input(2, 3)
    if (size(seq(-2)) /= 0) error stop 1
    if (size(seq(0)) /= 0) error stop 2
    if (any(seq(3) /= [1, 2, 3])) error stop 3
    call check_bounds(-3, -5)
    call check_bounds(-3, -1)
    input = 1
    do n = -2, 3
        if (size(trimmed(input, n)) /= max(0, 2-n)) error stop 16
        if (any(trimmed(input, n) /= 7)) error stop 17
        call check_local(n)
        call check_wide(int(n, 8))
        if (size(seq(n)) /= max(0, n)) error stop 13
        call check_assumed(seq(n), max(0, n))
        lo = 4
        hi = lo + n - 1
        call check_bounds(lo, hi)
        if (size(bounded(lo, hi)) /= max(0, n)) error stop 4
        if (n > 0) then
            if (any(bounded(lo, hi) /= [(i, i=lo,lo+n-1)])) error stop 5
        end if
    end do
contains
    pure function trimmed(a, k) result(r)
        integer, intent(in) :: a(:,:), k
        integer :: r(minval(shape(a)) - k)
        r = 7
    end function
    function seq(n) result(a)
        integer, intent(in) :: n
        integer :: a(n), i
        a = [(i, i=1,n)]
    end function
    function bounded(lo, hi) result(a)
        integer, intent(in) :: lo, hi
        integer :: a(lo:hi), i
        a = [(i, i=lo,hi)]
    end function
    subroutine check_local(n)
        integer, intent(in) :: n
        integer :: a(n), i
        a = [(i, i=1,n)]
        if (size(a) /= max(0, n)) error stop 6
        if (any(shape(a) /= [max(0, n)])) error stop 18
        block
            integer :: b(n)
            if (size(b) /= max(0, n) .or. ubound(b, 1) /= max(0, n)) error stop 19
        end block
        if (n > 0) then
            if (any(a /= [(i, i=1,n)])) error stop 7
        end if
        call check_dummy(n, a)
        call check_assumed(a, max(0, n))
        call check_assumed(a + 1, max(0, n))
        call check_char(n)
    end subroutine
    subroutine check_dummy(n, a)
        integer, intent(in) :: n, a(n)
        if (size(a) /= max(0, n)) error stop 8
        if (any(shape(a) /= [max(0, n)])) error stop 20
        call check_assumed(a, max(0, n))
    end subroutine
    subroutine check_char(n)
        integer, intent(in) :: n
        character(len=2) :: c(n)
        c = "ab"
        call check_assumed_char(c, max(0, n))
    end subroutine
    subroutine check_assumed(b, ext)
        integer, intent(in) :: b(:), ext
        if (size(b) /= ext) error stop 21
        if (lbound(b, 1) /= 1 .or. ubound(b, 1) /= ext) error stop 22
        if (any(lbound(b) /= [1]) .or. any(ubound(b) /= [ext])) error stop 23
    end subroutine
    subroutine check_assumed_char(c, ext)
        character(len=*), intent(in) :: c(:)
        integer, intent(in) :: ext
        if (size(c) /= ext .or. ubound(c, 1) /= ext) error stop 24
        if (ext > 0) then
            if (any(c /= "ab")) error stop 25
        end if
    end subroutine
    ! Bounds of a dimension of zero extent are 1 and 0.
    integer function lb(lo, hi)
        integer, intent(in) :: lo, hi
        lb = merge(lo, 1, hi >= lo)
    end function
    integer function ub(lo, hi)
        integer, intent(in) :: lo, hi
        ub = merge(hi, 0, hi >= lo)
    end function
    subroutine check_bounds(lo, hi)
        integer, intent(in) :: lo, hi
        integer :: a(lo:hi), m(2, lo:hi), i
        a = [(i, i=lo,hi)]
        m = 0
        if (size(a) /= max(0, hi-lo+1)) error stop 9
        if (lbound(a, 1) /= lb(lo, hi) .or. ubound(a, 1) /= ub(lo, hi)) error stop 10
        if (any(lbound(m) /= [1, lb(lo, hi)])) error stop 26
        if (any(ubound(m) /= [2, ub(lo, hi)])) error stop 27
        do i = lo, hi
            if (a(i) /= i) error stop 11
        end do
        call check_bound_dummy(lo, hi, a, m)
        call check_assumed(a, max(0, hi-lo+1))
    end subroutine
    subroutine check_wide(n)
        integer(8), intent(in) :: n
        integer :: a(2_8:n+1_8)
        a = 42
        if (size(a, kind=8) /= max(0_8, n)) error stop 14
        if (n > 0) then
            if (a(2) /= 42 .or. a(n+1) /= 42) error stop 15
        end if
    end subroutine
    subroutine check_bound_dummy(lo, hi, a, m)
        integer, intent(in) :: lo, hi, a(lo:hi), m(2, lo:hi)
        if (size(a) /= max(0, hi-lo+1)) error stop 12
        if (lbound(a, 1) /= lb(lo, hi) .or. ubound(a, 1) /= ub(lo, hi)) error stop 28
        if (any(lbound(a) /= [lb(lo, hi)])) error stop 29
        if (any(ubound(a) /= [ub(lo, hi)])) error stop 30
        if (any(lbound(m) /= [1, lb(lo, hi)])) error stop 31
        if (any(ubound(m) /= [2, ub(lo, hi)])) error stop 32
        if (lbound(m, 2) /= lb(lo, hi) .or. ubound(m, 2) /= ub(lo, hi)) error stop 33
        call check_assumed(a, max(0, hi-lo+1))
    end subroutine
end program
