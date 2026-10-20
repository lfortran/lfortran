program read_112
    ! Items of nested implied-do loops in a formatted READ are read like
    ! the same items written out as scalars
    implicit none
    integer :: i, j, m, n, e, k, k2, u
    integer :: kv(2)
    integer :: ka(2,2), kt(3,3)
    integer(8) :: qa(2,2)
    real, target :: ra(2,2)
    character(len=3) :: c(2,2)
    character(len=4) :: cs(2,2)
    complex :: z(2,2)
    character(len=40) :: s
    real :: ca(3,3), cb(2,2)
    integer :: cm, cn, ck
    common /read_112_a/ ca, cm, cn
    common /read_112_b/ cb
    common /read_112_n/ ck

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

    ! Empty lists and zero-size sections at the end of the input list are
    ! no items: the READ ends in its current record
    e = 0
    open(newunit=u, file='read_112.dat', status='replace')
    write(u, '(A)') ' 1', ' 2', ' 3', ' 4', ' 5', ' 6'
    close(u)
    open(newunit=u, file='read_112.dat', status='old')
    ka = -1
    read(u, '(I2)') k, ((ka(i,j), j=1,n), i=1,e)
    read(u, '(I2)') k2
    if (k /= 1 .or. k2 /= 2) error stop 16
    read(u, '(I2)') k, ((ka(i,j), j=1,e), i=1,n)
    read(u, '(I2)') k2
    if (k /= 3 .or. k2 /= 4) error stop 17
    if (any(ka /= -1)) error stop 18
    kv = -1
    read(u, '(I2)') k, kv(1:e)
    read(u, '(I2)') k2
    if (k /= 5 .or. k2 /= 6) error stop 19
    if (any(kv /= -1)) error stop 20
    close(u, status='delete')

    ! An array and its dimensions in COMMON
    cm = 2
    cn = 2
    ck = 2
    ca = 0
    cb = 0
    s = '  1.  2.  3.  4.  5.  6.  7.  8.'
    read(s, '(8F4.0)') ((ca(i,j), j=1,cn), i=1,cm), ((cb(i,j), i=1,ck), j=1,ck)
    if (any(ca(1,1:2) /= [1., 2.]) .or. any(ca(2,1:2) /= [3., 4.])) error stop 21
    if (any(cb(:,1) /= [5., 6.]) .or. any(cb(:,2) /= [7., 8.])) error stop 22
    print *, "ok"
end program read_112
