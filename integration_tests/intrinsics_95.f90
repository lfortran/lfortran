program intrinsics_95
    implicit none
    if (sign(1.0, -2.0) /= -1.0) error stop
end program
