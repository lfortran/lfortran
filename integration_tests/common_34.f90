! Test: block data with two common blocks where the second has an array
! This verifies that implicit typing is preserved when processing
! multiple common blocks in a block data unit (previously caused a
! segfault during compilation).
block data b
    real :: x
    real :: y(1)
    common /c1/ x
    common /c2/ y
    data x /3.14/
end

program common_34
    implicit none
    real :: x
    real :: y(1)
    common /c1/ x
    common /c2/ y
    if (abs(x - 3.14) > 1e-5) error stop
    y(1) = 2.71
    if (abs(y(1) - 2.71) > 1e-5) error stop
    print *, x, y(1)
end program common_34
