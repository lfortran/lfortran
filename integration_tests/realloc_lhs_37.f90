! An assignment to an allocated array of the same shape keeps its bounds,
! and one of another shape allocates it again (F2018 10.2.1.3), also on
! the C backend.
program realloc_lhs_37
    implicit none
    integer, allocatable :: w(:)
    allocate(w(-2:2))
    w = [1, 2, 3, 4, 5]
    if (lbound(w, 1) /= -2 .or. ubound(w, 1) /= 2) error stop 1
    if (w(-2) /= 1 .or. w(2) /= 5) error stop 2
    w = [7, 8, 9]
    if (lbound(w, 1) /= 1 .or. ubound(w, 1) /= 3) error stop 3
    if (w(1) /= 7 .or. w(3) /= 9) error stop 4
    print *, w
end program realloc_lhs_37
