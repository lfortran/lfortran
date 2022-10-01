program int_03
    implicit none

    integer(8) :: ans(5)
    ans(1) = 8524933037632333570_8
    ans(2) = -1069250973542918798_8
    ans(3) = -5123867065024149335_8
    ans(4) = 7303655603304982073_8
    ans(5) = 5108441843522503546_8
    print *, ans

    integer :: i
    complex*16 w
    w = (7.7, 5.0)
    i = w
    print *, i
    if (i /= 7) error stop
end program
