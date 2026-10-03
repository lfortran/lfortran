program intrinsics_435
    implicit none
    integer :: input(2, 2), res(2)
    logical :: mask(2, 2)

    input = reshape([11, 2, 3, 4], [2, 2])
    mask = reshape([.true., .false., .false., .false.], [2, 2])

    res = minloc(input, 1, mask)
    print *, res
    if (res(1) /= 1) error stop
    if (res(2) /= 0) error stop

    res = maxloc(input, 1, mask)
    print *, res
    if (res(1) /= 1) error stop
    if (res(2) /= 0) error stop

    mask = reshape([.true., .true., .false., .false.], [2, 2])
    res = minloc(input, 1, mask)
    print *, res
    if (res(1) /= 2) error stop
    if (res(2) /= 0) error stop

    res = maxloc(input, 1, mask)
    print *, res
    if (res(1) /= 1) error stop
    if (res(2) /= 0) error stop

    mask = reshape([.true., .true., .true., .true.], [2, 2])
    res = minloc(input, 1, mask)
    print *, res
    if (res(1) /= 2) error stop
    if (res(2) /= 1) error stop

    res = maxloc(input, 1, mask)
    print *, res
    if (res(1) /= 1) error stop
    if (res(2) /= 2) error stop
end program intrinsics_435
