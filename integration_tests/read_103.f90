program read_103
    ! READ, WRITE and INQUIRE in long loops must not grow the stack
    implicit none
    integer, parameter :: n = 500000
    integer :: i, k, c, u
    integer(8) :: s
    logical :: op
    character(len=20) :: buf

    open(newunit=u, file='read_103.txt', status='replace')
    c = 0
    do i = 1, n
        inquire(unit=u, opened=op)
        if (op) c = c + 1
    end do
    if (c /= n) error stop 1

    do i = 1, n
        write(u, *) i
    end do
    rewind(u)
    s = 0
    do i = 1, n
        read(u, *) k
        s = s + k
    end do
    close(u, status='delete')
    if (s /= 125000250000_8) error stop 2

    s = 0
    do i = 1, n
        write(buf, '(i0)') 2*i
        read(buf, *) k
        s = s + k
    end do
    if (s /= 250000500000_8) error stop 3
    print *, s
end program
