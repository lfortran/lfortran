program transfer_01
    implicit none
    integer :: i, integer_result, y(3, 2)
    real :: r, result, x(5, 4)
    character(len=*), parameter :: chr = 'A'
    r = 2.987654
    i = 123456
    x = 3.19
    y = 4

    result = transfer(i, r)
    print *, result
    if (abs(result - 1.72998703e-40) > 1e-6) error stop

    integer_result = transfer(r, i)
    print *, integer_result
    if (integer_result /= 1077884345) error stop

    integer_result = transfer(x(5,2), i)
    print *, integer_result
    if (integer_result /= 1078733046) error stop

    result = transfer(y(1,1), x(5,2))
    print *, result
    if (abs(result - 5.60519386E-45) > 1e-6) error stop

    print *, transfer(65, chr)
    if (transfer(65, chr) /= 'A') error stop
    print *, transfer(90, 'A')
    if (transfer(90, 'A') /= 'Z') error stop
end program transfer_01
