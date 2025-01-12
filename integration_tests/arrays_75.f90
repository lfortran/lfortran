program arrays_75
    integer :: score(4) = [-1,1,3,2]

    print *, [score, 1] > 0
    if (ALL(([score, 1] > 0) .NEQV. [.false., .true., .true., .true., .true.])) then
        error stop
    end if

    print *, 0 < [score, -1]
    if (ALL((0 < [score, -1]) .NEQV. [.false., .true., .true., .true., .false.])) then
        error stop
    end if

    print *, [score, 1] <= [score, -1]
    if (ALL(([score, 1] <= [score, -1]) .NEQV. [.true., .true., .true., .true., .false.])) then
        error stop
    end if
end program