program intrinsics_336
    implicit none
    complex, dimension(2) :: dc = [(1.4, 1.1), (2.3, 2.2)]
    complex :: x = (1.4, 1.1)
    print *, findloc(dc, x)
    if (any(findloc(dc, x) /= 1)) error stop
    print*, findloc([(1.0, 2.0), (3.0, 4.0)], (3.0, 4.0))
    if (any(findloc([(1.0, 2.0), (3.0, 4.0)], (3.0, 4.0)) /= 2)) error stop
    print*, findloc([(1.0, 2.0), (3.0, 4.0)], (1.0, 2.0), mask = [.true., .true.])
    if (any(findloc([(1.0, 2.0), (3.0, 4.0)], (1.0, 2.0), mask = [.true., .true.]) /= 1)) error stop
    print*, findloc([(1.0, 2.0), (3.0, 4.0)], (3.0, 4.0), mask=[.true., .true.], kind = 8)
    if (any(findloc([(1.0, 2.0), (3.0, 4.0)], (3.0, 4.0), mask = [.true., .true.], kind = 8) /= 2)) error stop
    print*, findloc([(1.0, 2.0), (3.0, 4.0)], (3.0, 4.0), mask=[.false., .true.], dim=1)
    if (findloc([(1.0, 2.0), (3.0, 4.0)], (3.0, 4.0), mask = [.false., .true.], dim = 1) /= 2) error stop
    ! print*, findloc([(1.0, 2.0), (3.0, 4.0)], (3.0, 4.0), mask=[.true., .false.], dim=1)
    ! if (findloc([(1.0, 2.0), (3.0, 4.0)], (3.0, 4.0), mask = [.true., .false.], dim = 1) /= 0) error stop
end program