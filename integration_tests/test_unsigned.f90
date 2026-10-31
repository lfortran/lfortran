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
        if ( x /= _lfortran_unsigned(y) ) error stop
        if ( y < 0 ) error stop
    end do

    ! Cross the signed range using only unsigned arithmetic.
    x = x * _lfortran_unsigned(2)
    if ( x <= _lfortran_unsigned(y) ) error stop
    if ( x / _lfortran_unsigned(2) /= _lfortran_unsigned(y) ) error stop

    x = x * _lfortran_unsigned(2)
    if ( x /= _lfortran_unsigned(0) ) error stop

    ! Subtraction and addition also wrap modulo 2**32.
    x = x - _lfortran_unsigned(1)
    if ( x <= _lfortran_unsigned(y) ) error stop
    x = x + _lfortran_unsigned(1)
    if ( x /= _lfortran_unsigned(0) ) error stop
end program unsigned_test
