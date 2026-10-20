program read_112
    ! Items of nested implied-do loops in a formatted READ are read like
    ! the same items written out as scalars
    implicit none
    integer :: i, j, m, n
    integer :: ka(2,2), kt(3,3)
    integer(8) :: qa(2,2)
    real, target :: ra(2,2)
    character(len=3) :: c(2,2)
    character(len=4) :: cs(2,2)
    complex :: z(2,2)
    character(len=40) :: s

    m = 3
    n = 2

    ! G edits into integer items
    s = '  1  2  3  4'
    ka = 0
    read(s, '(4G3.0)') ((ka(i,j), j=1,n), i=1,n)
    if (any(ka(1,:) /= [1, 2]) .or. any(ka(2,:) /= [3, 4])) error stop 1
    s = '  11  22  33  44'
    qa = 0
    read(s, '(4G4.0)') ((qa(i,j), j=1,n), i=1,n)
    if (any(qa(1,:) /= [11, 22]) .or. any(qa(2,:) /= [33, 44])) error stop 2

    ! An inner bound that depends on the outer loop variable
    s = ' 1 2 3 4 5 6'
    kt = 0
    read(s, '(6I2)') ((kt(i,j), j=1,i), i=1,m)
    if (any(kt(1,:) /= [1, 0, 0])) error stop 3
    if (any(kt(2,:) /= [2, 3, 0])) error stop 4
    if (any(kt(3,:) /= [4, 5, 6])) error stop 5
    kt = 0
    read(s, '(6I2)') ((kt(i,j), j=1,min(i,n)), i=1,m)
    if (any(kt(1,:) /= [1, 0, 0])) error stop 6
    if (any(kt(2,:) /= [2, 3, 0])) error stop 7
    if (any(kt(3,:) /= [4, 5, 0])) error stop 8

    ! Character, substring and complex items
    s = 'abcdefghijkl'
    read(s, '(4A3)') ((c(i,j), j=1,n), i=1,n)
    if (c(1,1) /= 'abc' .or. c(1,2) /= 'def') error stop 9
    if (c(2,1) /= 'ghi' .or. c(2,2) /= 'jkl') error stop 10
    cs = '....'
    read(s, '(4A2)') ((cs(i,j)(2:3), j=1,n), i=1,n)
    if (cs(1,1) /= '.ab.' .or. cs(1,2) /= '.cd.') error stop 11
    if (cs(2,1) /= '.ef.' .or. cs(2,2) /= '.gh.') error stop 12
    s = ' 1. 2. 3. 4. 5. 6. 7. 8.'
    read(s, '(8F3.0)') ((z(i,j), j=1,n), i=1,n)
    if (z(1,1) /= (1., 2.) .or. z(1,2) /= (3., 4.)) error stop 13
    if (z(2,1) /= (5., 6.) .or. z(2,2) /= (7., 8.)) error stop 14

    ! A TARGET item that the loop bounds do not use
    ra = 0
    read(s, '(4F3.0)') ((ra(i,j), j=1,n), i=1,n)
    if (any(ra(1,:) /= [1., 2.]) .or. any(ra(2,:) /= [3., 4.])) error stop 15
    print *, "ok"
end program read_112
