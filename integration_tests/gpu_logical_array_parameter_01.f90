program gpu_logical_array_parameter_01
    implicit none
    logical, parameter :: m(2) = [.true., .false.]
    logical, parameter :: opposite(2) = [.false., .true.]
    integer :: o(2), i

    o = -1
    do concurrent (i = 1:2)
        o(i) = 0
        if (m(i)) o(i) = o(i) + 1
        if (opposite(i)) o(i) = o(i) + 2
    end do

    if (any(o /= [1, 2])) error stop
    print *, "ok"
end program
