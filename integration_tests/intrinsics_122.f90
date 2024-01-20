program intrinsic_122
    implicit none
    integer :: a(2,3), b(3,2), c(2), d
    real, allocatable :: e(:,:)
  
    a = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    b = reshape([10, 20, 30, 40, 50, 60], [3, 2])
  
    print*, rank(1)
    if ( rank(1) /= 0 ) error stop
  
    print*, rank([.true., .false.])
    if ( rank([.true., .false.]) /= 1 ) error stop
  
    print*, rank(a)
    if ( rank(a) /= 2 ) error stop
  
    print*, rank(b)
    if ( rank(b) /= 2 ) error stop
  
    print*, rank(c)
    if ( rank(c) /= 1 ) error stop
  
    print*, rank(d)
    if ( rank(d) /= 0 ) error stop
  
    print*, rank(e)
    if ( rank(e) /= 2 ) error stop
  
  end