program intrinsics_93
    use iso_fortran_env, only: dp => real64, sp => real32
    implicit none

    integer, parameter :: i1 = digits(31)
    integer(8), parameter :: i2 = digits(63)
    integer, parameter :: i3 = digits(24.5_sp)
    integer(8), parameter :: i4 = digits(53.5_dp)
    ! integer, parameter :: ar1 = digits([1, 33, 56]) ! Does not work #4368
    ! integer, parameter :: ar2 = digits([1.0_sp, 33.0_sp, 56.0_sp])
    integer :: i 
    integer(8) :: j
    real :: x 
    real(8) :: y
    integer :: arr1(3) = [1, 33, 56]
    real(8) :: arr2(3) = [1.0_dp, 33.0_dp, 56.0_dp]

    print *, i1
    if (i1 /= 31) error stop
    print *, i2
    if (i2 /= 31) error stop
    print *, i3
    if (i3 /= 24) error stop
    print *, i4
    if (i4 /= 53) error stop
    ! print *, ar1
    ! if (ar1 /= 31) error stop
    ! print *, ar2
    ! if (ar2 /= 24) error stop
    
    print *, digits(i)
    if (digits(i) /= 31) error stop
    print *, digits(j)
    if (digits(j) /= 63) error stop
    print *, digits(x)
    if (digits(x) /= 24) error stop
    print *, digits(y)
    if (digits(y) /= 53) error stop

    print *, digits(arr1)
    if (digits(arr1) /= 31) error stop
    print *, digits(arr2)
    if (digits(arr2) /= 53) error stop

end program