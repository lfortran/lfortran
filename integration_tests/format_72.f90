program format_72
    implicit none
    character(15) :: line
    integer :: i1
    real :: a, b

    line = '2345 1 34512 45'
    read(line, '(BZ, I5, F5.0, BN, F5.2)') i1, a, b

    if (i1 /= 23450) error stop
    if (abs(a - 10345.0) > 0.0001) error stop
    if (abs(b - 12.45) > 0.0001) error stop

    print *, 'BN/BZ read test passed.'
end program format_72
