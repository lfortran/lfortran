program intrinsics_340
    print *, leadz(0_8)
    if (leadz(0_8) /= 64) error stop
    if (kind(leadz(0_8)) /= 4) error stop 

    print *, leadz(5_8)
    if (leadz(5_8) /= 61) error stop
    if (kind(leadz(5_8)) /= 4) error stop

end program intrinsics_340