program intrinsics_78
    implicit none
    integer :: ix, iy, iresult
    integer(8) :: j
    integer(4) :: i
    real(8) :: a
    real(4) :: b
    real :: rx, ry, rresult
    double precision :: dx, dy, dresult

    ! Test integer values
    ix = 10
    iy = 3
    iresult = mod(ix, iy)
    print *, "Test 1: mod(", ix, ",", iy, ") = ", iresult
    if (iresult /= 1) error stop "Test 1 failed"

    ix = 10
    iy = 5
    iresult = mod(ix, iy)
    print *, "Test 2: mod(", ix, ",", iy, ") = ", iresult
    if (iresult /= 0) error stop "Test 2 failed"

    ix = -10
    iy = 3
    iresult = mod(ix, iy)
    print *, "Test 3: mod(", ix, ",", iy, ") = ", iresult
    if (iresult /= -1) error stop "Test 3 failed"

    ! Test real values
    rx = 10.0
    ry = 3.0
    rresult = mod(rx, ry)
    print *, "Test 4: mod(", rx, ",", ry, ") = ", rresult
    if (abs(rresult - 1.0) > 1e-9) error stop "Test 4 failed"

    rx = 10.0
    ry = 5.0
    rresult = mod(rx, ry)
    print *, "Test 5: mod(", rx, ",", ry, ") = ", rresult
    if (abs(rresult - 0.0) > 1e-9) error stop "Test 5 failed"

    rx = -10.0
    ry = 3.0
    rresult = mod(rx, ry)
    print *, "Test 6: mod(", rx, ",", ry, ") = ", rresult
    if (abs(rresult - (-1.0)) > 1e-9) error stop "Test 6 failed"

    rx = 12.98
    ry = 3.0
    rresult = mod(rx, ry)
    print *, "Test 7: mod(", rx, ",", ry, ") = ", rresult
    if (abs(rresult - 0.98) > 1e-6) error stop "Test 7 failed"

    rx = 12.98
    ry = 13.0
    rresult = mod(rx, ry)
    print *, "Test 8: mod(", rx, ",", ry, ") = ", rresult
    if (abs(rresult - 12.98) > 1e-9) error stop "Test 8 failed"

    ! Test double precision values
    dx = 10.0D0
    dy = 3.0D0
    dresult = mod(dx, dy)
    print *, "Test 9: mod(", dx, ",", dy, ") = ", dresult
    if (abs(dresult - 1.0D0) > 1d-9) error stop "Test 9 failed"

    dx = 12.98D0
    dy = 3.0D0
    dresult = mod(dx, dy)
    print *, "Test 10: mod(", dx, ",", dy, ") = ", dresult
    if (abs(dresult - 0.98D0) > 1d-9) error stop "Test 10 failed"

    dx = 12.98D0
    dy = 13.0D0
    dresult = mod(dx, dy)
    print *, "Test 11: mod(", dx, ",", dy, ") = ", dresult
    if (abs(dresult - 12.98D0) > 1d-9) error stop "Test 11 failed"

    dx = -12.98D0
    dy = 13.0D0
    dresult = mod(dx, dy)
    print *, "Test 12: mod(", dx, ",", dy, ") = ", dresult
    if (abs(dresult - (-12.98D0)) > 1d-9) error stop "Test 12 failed"

    dx = -3.14D0
    dy = -2.0D0
    dresult = mod(dx, dy)
    print *, "Test 13: mod(", dx, ",", dy, ") = ", dresult
    if (abs(dresult - (-1.14D0)) > 1d-9) error stop "Test 13 failed"

    ! Test integer values with different kinds
    i = 121
    j = 121
    iresult = kind(mod(j,i))
    print *, "Test 14: kind(mod(", i, ",", j, ")) = ", iresult 
    if(iresult /= 8) error stop "Test 14 failed"

    ! Test real values with different kinds
    a = 121.882
    b = 121.882
    iresult = kind(mod(a,b))
    print *, "Test 15: kind(mod(", a, ",", b, ")) = ", iresult 
    if(iresult /= 8) error stop "Test 15 failed"
end program intrinsics_78
