program transfer_02
    integer :: result
    double precision :: d
    complex :: result1
    real :: r

    d = 3.14D0
    result = transfer(d, result)
    print *, result
    if (result /= 1374389535) error stop

    r = 1.0
    result1 = transfer(r, result1)
    print *, result1
    if( real(result1) /= 1.0 ) error stop
end program
