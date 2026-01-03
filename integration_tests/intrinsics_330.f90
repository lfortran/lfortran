program intrinsics_330
    print *, amax0(1,2,3)
    if (amax0(1, 2, 3) /= 3) error stop
    print *, max1(1.0,2.0,3.0)
    if (max1(1.0,2.0,3.0) /= 3.0) error stop
end program