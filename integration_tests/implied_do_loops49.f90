program implied_do_loops49
    ! Implied-do loops in output lists whose bounds are compile-time
    ! constants that are not literals (size() of a fixed size array, a
    ! named constant, a negative step) must keep every trim() value at its
    ! own length and write all items into one record. A nested loop sees
    ! the outer loop's value, a run-time step is honoured, and a large
    ! constant trip count is not expanded item by item.
    implicit none
    integer, parameter :: n = 3, m = 5000
    integer :: i, j, k, u, ios, st
    character(5) :: s(2) = ['ab   ', 'cde  ']
    character(5) :: names(n) = [character(5) :: 'five', 'six', 'seven']
    real :: r(3) = [1.0, 2.0, 3.0]
    character(4) :: t(2,2)
    real :: x(m)
    character(40) :: line, expected

    t(1,1) = 'a'; t(2,1) = 'bb'; t(1,2) = 'ccc'; t(2,2) = 'dddd'
    x = 1.5
    st = 2

    open(newunit=u, status='scratch', action='readwrite')
    write(u, '(*(1x,a))') (trim(s(i)), i=1,size(s)), 'z'
    write(u, *) (trim(s(i)), i=1,size(s)), 'z'
    write(u, '(*(1x,a))') (trim(names(i)), i=1,n)
    write(u, '(3(i0,1x,f3.1,1x))') (i, r(i), i=3,1,-1)
    write(u, '(*(a,"|"))') ((trim(t(i,j)), i=1,size(t,1)), j=1,size(t,2))
    j = 1
    write(u, '(*(a,"|"))') ((trim(t(i,j)), i=1,2), j=1,2)
    write(u, '(*(i0,1x,f3.1,1x))') ((i, k=1,1), r(i), i=1,2)
    write(u, '(i0,1x,f3.1)') (i, r(i), i=1,3,st)
    write(u, '(i0,1x,f3.1)') (i, x(i), i=1,m)
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
    read(u, '(a)') line
    print '(a)', trim(line)
    if (line /= 'a|bb|ccc|dddd|') error stop 5
    read(u, '(a)') line
    print '(a)', trim(line)
    if (line /= 'a|bb|ccc|dddd|') error stop 6
    read(u, '(a)') line
    print '(a)', trim(line)
    if (line /= '1 1.0 2 2.0') error stop 7
    read(u, '(a)') line
    print '(a)', trim(line)
    if (line /= '1 1.0') error stop 8
    read(u, '(a)') line
    print '(a)', trim(line)
    if (line /= '3 3.0') error stop 9
    do k = 1, m
        read(u, '(a)') line
        write(expected, '(i0,a)') k, ' 1.5'
        if (line /= expected) error stop 10
    end do
    read(u, '(a)', iostat=ios) line
    if (ios == 0) error stop 11
    close(u)

    write(line, '(*(1x,a))') (trim(s(i)), i=1,size(s)), 'z'
    print '(a)', trim(line)
    if (line /= ' ab cde z') error stop 12

    ! Not conforming (the inner loop reuses the outer loop's variable) but
    ! accepted, so it must compile. Only the first record is checked, as
    ! the rest depends on the processor.
    open(newunit=u, status='scratch', action='readwrite')
    write(u, '(a,"|")') ((trim(s(i)), i=1,size(s)), i=1,2)
    rewind(u)
    read(u, '(a)') line
    print '(a)', trim(line)
    if (line /= 'ab|') error stop 13
    rewind(u)
    write(u, '(i0,1x,f3.1)') ((i, r(i), i=1,2), i=1,2)
    rewind(u)
    read(u, '(a)') line
    print '(a)', trim(line)
    if (line /= '1 1.0') error stop 14
    close(u)
end program
