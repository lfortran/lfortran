program intrinsics_377
    implicit none
 
    integer, allocatable :: a(:)
    integer :: idx, n
 
    n = 50
    allocate(a(n))
    a = -1
    a(2:) = 1
 
    idx = minval(a, dim=1)
    print *, idx
    if ( idx /= -1 ) error stop
    idx = minval(a, dim=1, mask=a>0)
    print *, idx
    if ( idx /= 1 ) error stop
    idx = maxval(a, dim=1)
    print *, idx
    if ( idx /= 1 ) error stop
    idx = maxval(a, dim=1, mask=a>0)
    print *, idx
    if ( idx /= 1 ) error stop
 end program
 