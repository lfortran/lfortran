program intrinsics_154
    integer(8) :: n = 921092378411_8
    print *, mod(n, 10_8)

    if (mod(n, 10_8) /= 1) error stop
end program
