program format_09
    implicit none
    integer :: a(5),b(5),c(10)
    integer, parameter :: dp=kind(0d0)
    real(dp) :: d(3)
    a = [117,123,124,126,129]
    b = [1,2,3,4,5]
    c = 1
    d = [1._dp,1._dp,1._dp]
    print '(6(i6))', c
    print 10, "hello", a ,"world", b
  10  format(a,i10,i4,i5,i6,i7)
    print 20, 'String:', 3.1415926d0, &
    'Integer:', 42, &
    'Array:', d
  20 format(5x,a,d15.7//,5x,a,16x,i10//,5x,a//1(5x,3d15.7))
  end program
  