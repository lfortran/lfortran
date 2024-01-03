program intrinsics_100
    implicit none
    integer(1) :: ix_8, iy_8
    integer(2) :: ix_16, iy_16
    integer    :: iresult
    integer(8) :: ix_64, iy_64

    ix_8 = 127_1
    iy_8 = 23_1
    iresult = mod(ix_8, iy_8)
    print *, "Test 1: mod( ", ix_8, ",", iy_8, ") = ", iresult
    if (iresult /= 12) error stop "Test 1 failed"

    ix_16 = -32767_2
    iy_16 = 17_2
    iresult = mod(ix_16, iy_16)
    print *, "Test 2: mod( ", ix_16, ",", iy_16, ") = ", iresult
    if (iresult /= -8) error stop "Test 2 failed"

    ix_64 = 43256_8
    iy_64 = 53_8
    iresult = mod(ix_64, iy_64)
    print *, "Test 3: mod( ", ix_64, ",", iy_64, ") = ", iresult
    if (iresult /= 8) error stop "Test 3 failed"
end program intrinsics_100
