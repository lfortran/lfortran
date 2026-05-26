program intrinsics_467
    implicit none

    type :: point
        real(8) :: x(2)
    end type

    type(point), allocatable :: top(:)
    integer :: loc(1)

    allocate(top(3))
    top(1)%x = [0.0d0, 0.0d0]
    top(2)%x = [1.0d0, 0.0d0]
    top(3)%x = [2.0d0, 0.0d0]

    loc = maxloc(top(:)%x(1), top(:)%x(1) < 1.5d0)
    if (loc(1) /= 2) error stop
end program intrinsics_467
