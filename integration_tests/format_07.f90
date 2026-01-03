program format_07
    implicit none
    integer, parameter :: dp=kind(0d0)
    real(dp) :: x(3)
    real :: z(3)
    integer :: y(4)
    x = [1._dp,1._dp,1._dp]
    y = [117,123,124,126]
    z = [1.0,2.0,3.0]
  10 format(3d12.5)
  20 format(4i10)
  30 format(3d12.5)
    print 10, x
    print 20, y
    print 30, z
    write (*,40) x
    write (*,50) y
    write (*,60) z
  40 format(3d12.5)
  50 format(4i10)
  60 format(3d12.5)
end program