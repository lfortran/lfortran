! Namelist READ of array sections: subscript triplets (lo:hi[:stride]),
! a bare `:`, and omitted bounds. The section's elements are read in array
! element order; fewer values than elements leave the rest unchanged.
program namelist_44
    implicit none
    real :: a(3,4), b(6), x(2,2,3)
    character(len=4) :: c(3)
    integer :: k(5), m(3,2)
    namelist /g/ a, b, x, c, k, m
    integer :: u, ios

    a = -1
    b = -1
    x = -1
    c = 'zz'
    k = -1
    m = -1
    open(newunit=u, status='scratch')
    write(u, '(a)') "&g"
    write(u, '(a)') " b(2:3)=5,6"
    write(u, '(a)') " a(1,:)=1,2,3,4"
    write(u, '(a)') " a(:,3)=7,8"
    write(u, '(a)') " a(2,2:)=20,21,22"
    write(u, '(a)') " x(1,2,:)=11,12,13 x(:,1,2)=31,32"
    write(u, '(a)') " c(2:)='bb','cc'"
    write(u, '(a)') " k(1:5:2)=1,3,5"
    write(u, '(a)') " m(2:3,1)=4, 5 m(1,:)=2*9"
    write(u, '(a)') "/"
    rewind(u)
    read(u, nml=g, iostat=ios)
    close(u)
    if (ios /= 0) error stop 1

    if (any(b /= [-1., 5., 6., -1., -1., -1.])) error stop 2
    if (any(a(1,:) /= [1., 2., 7., 4.])) error stop 3
    if (any(a(2,:) /= [-1., 20., 21., 22.])) error stop 4
    if (any(a(3,:) /= [-1., -1., -1., -1.])) error stop 5
    if (any(x(1,2,:) /= [11., 12., 13.])) error stop 6
    if (any(x(:,1,2) /= [31., 32.])) error stop 7
    if (count(x /= -1) /= 5) error stop 8
    if (c(1) /= 'zz' .or. c(2) /= 'bb' .or. c(3) /= 'cc') error stop 9
    if (any(k /= [1, -1, 3, -1, 5])) error stop 10
    if (any(m(:,1) /= [9, 4, 5])) error stop 11
    if (any(m(:,2) /= [9, -1, -1])) error stop 12

    print *, "ok"
end program
