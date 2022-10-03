program int_03
    implicit none
    integer, parameter :: dp = kind(0.d0)

    integer(8) :: ans(5)
    integer :: i4
    integer(8) :: i8
    complex(dp) :: w8
    complex :: w4
    ans(1) = 8524933037632333570_8
    ans(2) = -1069250973542918798_8
    ans(3) = -5123867065024149335_8
    ans(4) = 7303655603304982073_8
    ans(5) = 5108441843522503546_8
    print *, ans

    w8 = (7.7_8, 5.0_8)
    i4 = w8
    i8 = w8
    print *, i4, i8
    if (i4 /= 7) error stop
    if (i8 /= 7) error stop

    w4 = (7.7, 5.0)
    i4 = w4
    i8 = w4
    print *, i4, i8
    if (i4 /= 7) error stop
    if (i8 /= 7) error stop
end program
