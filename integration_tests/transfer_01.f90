program transfer_01
    implicit none
    integer :: i, integer_result, y(3, 2)
    real :: r, result, x2, x(5, 4)
    real, parameter :: x1 = transfer(666,1.0)
    character(len=*), parameter :: chr = 'A'
    character(len=*), parameter :: chr2 = transfer(65, 'A')
    character(len=2), parameter :: chr3 = transfer(16961, 'AB')
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

    if (transfer(x1, 1) /= 666) error stop
    x2 = transfer(666,1.0)
    if (transfer(x2, 1) /= 666) error stop
    print *, transfer(65, chr)
    if (transfer(65, chr) /= 'A') error stop
    print *, transfer(90, 'A')
    if (transfer(90, 'A') /= 'Z') error stop
    if (chr2 /= 'A') error stop
    if (chr3 /= 'AB') error stop
end program transfer_01
