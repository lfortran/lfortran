program data_17
    implicit none

    character(len=1) :: facts(3)

    data facts / 'F', 'N', 'E' /

    if (facts(1) /= 'F') error stop
    if (facts(2) /= 'N') error stop
    if (facts(3) /= 'E') error stop
end program data_17
