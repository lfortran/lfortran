program intrinsics_30
    use iso_fortran_env, only : int8, int16
    integer(4) :: i1
    integer(8) :: i2
    integer(int8) :: i3
    integer(int16) :: i4
    real(4) :: r1
    real(8) :: r2
    complex(4) :: c1
    complex(8) :: c2
    integer, parameter :: ri3 = range(i3)
    integer, parameter :: ri4 = range(i4)

    print *, range(i1), range(i2), ri3, ri4
    print *, range(r1), range(r2)
    print *, range(c1), range(c2)

    if (ri3 /= 2) error stop
    if (ri4 /= 4) error stop
    if (range(i1) /= 9) error stop
    if (range(i2) /= 18) error stop
    if (range(r1) /= 37) error stop
    if (range(r2) /= 307) error stop
    if (range(c1) /= 37) error stop
    if (range(c2) /= 307) error stop
end program
