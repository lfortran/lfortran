program read_09
    ! Test variable-bounds implied-do in READ
    implicit none
    integer :: vals(5), i, n

    open(10, file='read_09_input.txt', status='replace')
    write(10, '(5I6)') 10, 20, 30, 40, 50
    close(10)

    open(10, file='read_09_input.txt', status='old')
    n = 5
    read(10, *) (vals(i), i=1, n)
    close(10, status='delete')

    if (vals(1) /= 10) error stop
    if (vals(2) /= 20) error stop
    if (vals(3) /= 30) error stop
    if (vals(4) /= 40) error stop
    if (vals(5) /= 50) error stop

    print *, "All tests passed!"
end program
