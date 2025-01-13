program intrinsics_341
    print *, trailz(0_8)
    if (trailz(0_8) /= 64) error stop
    if (kind(trailz(0_8)) /= 4) error stop

    print *, trailz(5_8)
    if (trailz(5_8) /= 0) error stop
    if (kind(trailz(5_8)) /= 4) error stop

end program 