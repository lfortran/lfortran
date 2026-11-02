! INQUIRE by a file name with trailing blanks, as in a fixed-length
! character variable: the trailing blanks are not significant
program inquire_17
    implicit none
    character(len=30) :: f
    logical :: e, op
    integer :: u, n
    f = 'inquire_17.txt'
    open(newunit=u, file=f)
    inquire(file=f, exist=e, opened=op, number=n)
    if (.not. e) error stop 1
    if (.not. op) error stop 2
    if (n /= u) error stop 3
    close(u)
    inquire(file=f, exist=e, opened=op)
    if (.not. e) error stop 4
    if (op) error stop 5
    open(newunit=u, file=f)
    close(u, status='delete')
    inquire(file=f, exist=e)
    if (e) error stop 6
end program
