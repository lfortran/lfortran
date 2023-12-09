program intrinsics_83
    implicit none

    complex(kind=kind(1.0D0)) :: val

    val = (1.0, -3.0)

    print *, val

    if (abs(val - (1.0, -3.0)) > 1e-5) error stop
 end program intrinsics_83
