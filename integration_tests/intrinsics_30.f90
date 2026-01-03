program intrinsics_30
    use iso_fortran_env, only : int8, int16
    implicit none
    integer, parameter :: a1 = range(11)
    integer, parameter :: a2 = range(22.90)
    integer, parameter :: a3 = range((1,2))
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
    integer, parameter :: ar1 = range([1, 21, 13])
    integer, parameter :: ar2 = range([1.0, 21.0, 13.0])
    integer, parameter :: ar3 = range([(1, 2), (31, 4), (51, 62)])
    integer :: arr1(3) = [1, 21, 13]
    real(8) :: arr2(3) = [1.0, 21.0, 13.0]
    complex :: arr3(3) = [(1, 2), (31, 4), (51, 62)]

    print *, a1
    if (a1 /= 9) error stop
    print *, a2
    if (a2 /= 37) error stop
    print *, a3
    if (a3 /= 37) error stop

    print *, ar1
    if (ar1 /= 9) error stop
    print *, ar2
    if (ar2 /= 37) error stop
    print *, ar3
    if (ar3 /= 37) error stop

    print *, range(ri3)
    if (ri3 /= 2) error stop
    print *, range(ri4)
    if (ri4 /= 4) error stop
    print *, range(i1)
    if (range(i1) /= 9) error stop
    print *, range(i2)
    if (range(i2) /= 18) error stop
    print *, range(r1)
    if (range(r1) /= 37) error stop
    print *, range(r2)
    if (range(r2) /= 307) error stop
    print *, range(c1)
    if (range(c1) /= 37) error stop
    print *, range(c2)
    if (range(c2) /= 307) error stop

    print *, range(arr1)
    if (range(arr1) /= 9) error stop
    print *, range(arr2)
    if (range(arr2) /= 307) error stop
    print *, range(arr3)
    if (range(arr3) /= 37) error stop
end program
