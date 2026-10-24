program read_111
    ! Formatted READ of nested implied-do loops with run-time bounds
    implicit none
    integer :: i, j, k, m, n, nz
    real :: a(3,2), b(3)
    integer :: c(2,2,2), ia(2,2), ib(2,2)
    real :: x(2)
    character(len=40) :: s
    character(len=6) :: recs(2)

    m = 3
    n = 2
    nz = 0

    ! External unit, an outer item after the inner loop, and format
    ! reversion over several records
    open(10, status='scratch')
    write(10, '(a)') '  1.  2.  3.  4.'
    write(10, '(a)') '  5.  6.  7.  8.'
    write(10, '(a)') '  9.'
    write(10, '(a)') ' 42'
    rewind(10)
    read(10, '(4F4.0)') ((a(i,j), j=1,n), b(i), i=1,m)
    if (any(a(1,:) /= [1., 2.])) error stop 1
    if (any(a(2,:) /= [4., 5.])) error stop 2
    if (any(a(3,:) /= [7., 8.])) error stop 3
    if (any(b /= [3., 6., 9.])) error stop 4
    if (i /= m + 1) error stop 5
    if (j /= n + 1) error stop 6
    read(10, '(I3)') k
    if (k /= 42) error stop 7

    ! Three levels, integer items
    rewind(10)
    write(10, '(a)') ' 1 2 3 4 5 6 7 8'
    rewind(10)
    read(10, '(8I2)') (((c(i,j,k), k=1,n), j=1,n), i=1,n)
    if (any(c(1,1,:) /= [1, 2])) error stop 8
    if (any(c(1,2,:) /= [3, 4])) error stop 9
    if (any(c(2,1,:) /= [5, 6])) error stop 10
    if (any(c(2,2,:) /= [7, 8])) error stop 11
    if (i /= n + 1 .or. j /= n + 1 .or. k /= n + 1) error stop 12

    ! Items of different types, an empty inner loop and a negative step
    rewind(10)
    write(10, '(a)') ' 1 2 1.5 3 4 2.5'
    write(10, '(a)') ' 7 8'
    rewind(10)
    read(10, '(2(2I2,F4.1))') ((ia(i,j), j=1,n), x(i), i=1,n)
    if (any(ia(1,:) /= [1, 2]) .or. any(ia(2,:) /= [3, 4])) error stop 13
    if (any(x /= [1.5, 2.5])) error stop 14
    read(10, '(2I2)') ((ib(i,j), j=1,nz), ib(i,1), i=n,1,-1)
    if (ib(2,1) /= 7 .or. ib(1,1) /= 8) error stop 15
    if (i /= 0 .or. j /= 1) error stop 16
    close(10)

    ! Internal file
    a = 0
    b = 0
    s = ' 11 12 13 14 15 16 17 18 19'
    read(s, '(9F3.0)') ((a(i,j), j=1,n), b(i), i=1,m)
    if (any(a(1,:) /= [11., 12.])) error stop 17
    if (any(a(2,:) /= [14., 15.])) error stop 18
    if (any(a(3,:) /= [17., 18.])) error stop 19
    if (any(b /= [13., 16., 19.])) error stop 20
    if (i /= m + 1 .or. j /= n + 1) error stop 21

    ! Internal file with several records
    recs(1) = ' 1 2 3'
    recs(2) = ' 4 5 6'
    ia = 0
    read(recs, '(3I2)') ((ia(i,j), j=1,n), i=1,n)
    if (any(ia(1,:) /= [1, 2]) .or. any(ia(2,:) /= [3, 4])) error stop 22
    if (i /= n + 1 .or. j /= n + 1) error stop 23
    print *, "ok"
end program read_111
