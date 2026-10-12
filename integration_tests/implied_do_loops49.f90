program implied_do_loops49
    ! Implied-do loops in output lists whose bounds are compile-time
    ! constants that are not literals (size() of a fixed size array, a
    ! named constant, a negative step) must keep every trim() value at its
    ! own length and write all items into one record.
    implicit none
    integer, parameter :: n = 3
    integer :: i, u, ios
    character(5) :: s(2) = ['ab   ', 'cde  ']
    character(5) :: names(n) = [character(5) :: 'five', 'six', 'seven']
    real :: r(3) = [1.0, 2.0, 3.0]
    character(40) :: line

    open(newunit=u, status='scratch', action='readwrite')
    write(u, '(*(1x,a))') (trim(s(i)), i=1,size(s)), 'z'
    write(u, *) (trim(s(i)), i=1,size(s)), 'z'
    write(u, '(*(1x,a))') (trim(names(i)), i=1,n)
    write(u, '(3(i0,1x,f3.1,1x))') (i, r(i), i=3,1,-1)
    rewind(u)
    read(u, '(a)') line
    print '(a)', trim(line)
    if (line /= ' ab cde z') error stop 1
    read(u, '(a)') line
    print '(a)', trim(line)
    if (adjustl(line) /= 'abcdez') error stop 2
    read(u, '(a)') line
    print '(a)', trim(line)
    if (line /= ' five six seven') error stop 3
    read(u, '(a)') line
    print '(a)', trim(line)
    if (line /= '3 3.0 2 2.0 1 1.0') error stop 4
    read(u, '(a)', iostat=ios) line
    if (ios == 0) error stop 5
    close(u)

    write(line, '(*(1x,a))') (trim(s(i)), i=1,size(s)), 'z'
    print '(a)', trim(line)
    if (line /= ' ab cde z') error stop 6
end program
