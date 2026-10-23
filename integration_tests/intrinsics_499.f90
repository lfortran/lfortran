program intrinsics_499
    ! `spread` of a scalar source, also through the Fortran backend, which
    ! has to print the scalar (not `[source]`) as the `source` argument.
    implicit none
    integer :: a, r(3), s(2)
    real :: x, y(2)
    logical :: l, m(2)
    character(len=2) :: c, d(2)
    a = 5
    x = 1.5
    l = .true.
    c = "ab"
    r = spread(a, 1, 3)
    print *, r
    if (any(r /= 5)) error stop
    s = spread(7, 1, 2)
    print *, s
    if (any(s /= 7)) error stop
    y = spread(x, 1, 2)
    print *, y
    if (any(abs(y - 1.5) > 1e-6)) error stop
    m = spread(l, 1, 2)
    print *, m
    if (.not. all(m)) error stop
    d = spread(c, 1, 2)
    print *, d
    if (any(d /= "ab")) error stop
    if (size(spread(a, 1, 4)) /= 4) error stop
end program
