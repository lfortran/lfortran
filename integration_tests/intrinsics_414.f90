program intrinsics_414
    logical(8) :: ar1(3) = logical([.true., .false., .true.], 8)
    print *, ar1
    if (ar1(1) .neqv. .true.) error stop
    if (ar1(2) .neqv. .false.) error stop
    if (ar1(3) .neqv. .true.) error stop
end program intrinsics_414