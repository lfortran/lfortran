program intrinsics_311
    implicit none
    integer, dimension(5) :: array = [3, 1, 4, 1, 5]
    logical, dimension(5) :: mask = [.true., .false., .true., .false., .true.]
    integer, dimension(:), allocatable :: loc

    ! Find the location of the minimum value considering only elements where mask is true
    loc = minloc(array, mask = mask)
    print *, "Minloc with MASK:", loc
    if (any(loc /= 1)) error stop

    ! Find the location of the minimum value searching backwards
    loc = minloc(array, back = .true.)
    print *, "Minloc with BACK:", loc
    if (any(loc /= 4)) error stop

    loc = minloc(array)
    print *, loc
    if (any(loc /= 2)) error stop
    loc = minloc(array, mask = mask)
    print *, loc
    if (any(loc /= 1)) error stop
    loc = minloc(array, back = .true.)
    print *, loc
    if (any(loc /= 4)) error stop
    loc = minloc(array, back = .false.)
    print *, loc
    if (any(loc /= 2)) error stop
    loc = minloc(array, back = .true.)
    print *, loc
    if (any(loc /= 4)) error stop
    loc = minloc(array, back = .false.)
    print *, loc
    if (any(loc /= 2)) error stop
    loc = minloc(array, back = .true.)
    print *, loc
    if (any(loc /= 4)) error stop

 end program
