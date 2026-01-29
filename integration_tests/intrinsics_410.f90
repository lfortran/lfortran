! Test maxloc/minloc/shape return correct kind with -fdefault-integer-8
program intrinsics_410
    implicit none
    integer :: arr(5) = [1, 3, 2, 5, 4]
    integer :: loc(1)
    integer :: shp(1)

    ! maxloc should return integer of default kind
    loc = maxloc(arr)
    if (loc(1) /= 4) error stop "maxloc failed"

    ! minloc should return integer of default kind
    loc = minloc(arr)
    if (loc(1) /= 1) error stop "minloc failed"

    ! shape should return integer of default kind
    shp = shape(arr)
    if (shp(1) /= 5) error stop "shape failed"

    print *, "PASS"
end program intrinsics_410
