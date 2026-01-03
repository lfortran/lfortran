program intrinsics_310
    use iso_fortran_env, only: int8, int16, int32, int64, real32, real64
    implicit none
    integer(int8), parameter :: i1 = digits(127_int8)
    integer(int16), parameter :: i2 = digits(32_int16)
    integer(int32), parameter :: i4 = digits(21_int32)
    integer(int64), parameter :: i8 = digits(9223_int64)
    integer, parameter :: r4 = digits(1.0_real32)
    integer, parameter :: r8 = digits(1.0_real64)
  
    print *, i1
    if (i1 /= 7) error stop
    print *, i2
    if (i2 /= 15) error stop
    print *, i4
    if (i4 /= 31) error stop
    print *, i8
    if (i8 /= 63) error stop
    print *, r4
    if (r4 /= 24) error stop
    print *, r8
    if (r8 /= 53) error stop
end program
  