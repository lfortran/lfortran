program unsigned_test

    implicit none

    ! 4 byte unsigned int 
    type(unsigned) :: x
    integer(kind=4)    :: y
    integer :: i
    x = 1
    y = 1

    do i = 1, 30
        x = x * _lfortran_unsigned(2)
        y = y * 2
        if ( x < _lfortran_unsigned(0) ) error stop 
        if ( y < 0 )  error stop 
    end do

    x = x * _lfortran_unsigned(2)
    y = y * 2
    if ( x < _lfortran_unsigned(0) ) error stop 
    if ( y > 0 )  error stop 


    x = x * _lfortran_unsigned(2)
    y = y * 2
    if ( x /= _lfortran_unsigned(0) ) error stop 
    if ( y /= 0 )  error stop 

    

end program unsigned_test

