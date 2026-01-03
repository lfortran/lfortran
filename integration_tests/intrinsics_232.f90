program intrinsics_232
    implicit none
    
    integer, parameter :: i1 = iparity([1,2,3,4,5,6,7,8,9,10,11])
    integer(8), parameter :: i2 = product([1, 2, 3], [.true., .true., .true.])
    integer(4), parameter :: i3 = product([11, 2, 5], 1, mask = [.true., .false., .true.])
    integer :: i(10) = [1,2,3,4,5,6,7,8,9,10]
    integer :: x(5) = [ 1, 2, 3, 4, 5 ]
    logical :: mask(5) = [ .TRUE., .FALSE., .TRUE., .FALSE., .TRUE. ]
    integer :: dim = 1

    print *, i1
    if ( i1 /= 0 ) error stop

    print *, i2
    if ( i2 /= 6 ) error stop

    print *, i3
    if ( i3 /= 55 ) error stop

    print *, iparity( array = x, mask = mask )
    if ( .not. iparity( array = x, mask = mask ) == 7 ) error stop

    print *, iparity( x, mask = mask)
    if ( .not. iparity( x, mask = mask ) == 7 ) error stop

    print *, iparity( x, mask )
    if ( .not. iparity( x, mask ) == 7 ) error stop

    print *, iparity( x, mask = mask, dim = dim )
    if ( .not. iparity( x, mask = mask, dim = dim ) == 7 ) error stop

    print *, iparity([1,2,4,5,6,8,10,11])
    if ( iparity([1,2,4,5,6,8,10,11]) /= 13 ) error stop

    print *, iparity(i)
    if ( iparity(i) /= 11 ) error stop

    print *, iparity(i, [.true., .false., .true., .false., .true., .false., .true., .false., .true., .false.])
    if ( iparity(i, [.true., .false., .true., .false., .true., .false., .true., .false., .true., .false.]) /= 9 ) error stop
    
end program