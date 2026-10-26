module array_bound_14_m
    implicit none
    integer, allocatable :: q
contains
    subroutine s(e)
        integer :: e(q)
        integer :: d(q:10)
        if (size(e) /= 7) error stop
        if (lbound(d, 1) /= 7 .or. size(d) /= 4) error stop
    end subroutine

    subroutine t(n)
        integer, allocatable, intent(in) :: n
        integer :: d(n:10)
        print *, lbound(d, 1), ubound(d, 1)
        if (lbound(d, 1) /= 7 .or. ubound(d, 1) /= 10) error stop
        if (size(d) /= 4) error stop
    end subroutine
end module

program array_bound_14
    use array_bound_14_m
    implicit none
    integer :: x(7)
    allocate(q)
    q = 7
    call s(x)
    call t(q)
end program
