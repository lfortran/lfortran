program read_116
    ! Whole arrays before and after the nested implied-do loops of a
    ! formatted READ
    implicit none
    integer :: a(2,2), b(3), i, j, n
    integer, allocatable :: c(:)
    real :: r(2)
    character(len=2) :: cs(2)
    character(len=60) :: s
    n = 2
    allocate(c(2))
    s = ' 1 2 3 4 5 6 7 8 9 1.5 2.5abcd'
    read(s, '(9I2,2F4.1,2A2)') b, ((a(i,j), j=1,n), i=1,n), c, r, cs
    if (any(b /= [1, 2, 3])) error stop 1
    if (a(1,1) /= 4 .or. a(1,2) /= 5 .or. a(2,1) /= 6 .or. a(2,2) /= 7) error stop 2
    if (any(c /= [8, 9])) error stop 3
    if (abs(r(1) - 1.5) > 1e-6 .or. abs(r(2) - 2.5) > 1e-6) error stop 4
    if (cs(1) /= 'ab' .or. cs(2) /= 'cd') error stop 5
    if (i /= 3 .or. j /= 3) error stop 6
    print *, b, a, c, r, cs
end program
