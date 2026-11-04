! READ, WRITE and INQUIRE inside a loop must not grow the stack: the
! temporaries they need are allocated once, in the entry block (see the LLVM
! reference output, which has no `alloca` in the loop body). To reproduce the
! original stack overflow at run time, set n = 1000000.
program read_119
    implicit none
    integer, parameter :: n = 3
    integer, parameter :: u = 10
    integer :: i, k, c
    integer(8) :: s
    logical :: op
    character(len=20) :: buf

    open(unit=u, status='scratch')
    c = 0
    s = 0
    do i = 1, n
        inquire(unit=u, opened=op)
        if (op) c = c + 1
        write(u, *) i
        write(buf, '(i0)') 2*i
        read(buf, *) k
        s = s + k
    end do
    if (c /= n) error stop 1
    if (s /= int(n, 8)*(n + 1)) error stop 2

    rewind(u)
    s = 0
    do i = 1, n
        read(u, *) k
        s = s + k
    end do
    close(u)
    if (s /= int(n, 8)*(n + 1)/2) error stop 3
    print *, s
end program
