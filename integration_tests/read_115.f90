program read_115
    ! Constant-bound implied-do loops of a formatted READ that also has
    ! nested implied-do loops with run-time bounds
    implicit none
    type t
        integer :: a(2,2)
        integer :: m
    end type
    type(t) :: x(3)
    integer :: i, j, k, n, u
    integer :: a(2,2), a3(3,2,3), b(4), kt(3,3)
    character(len=3) :: cs(3)
    character(len=40) :: s

    n = 2

    ! A constant loop over structures around loops with run-time bounds
    s = ' 1 2 3 4 5 6 7 8'
    x(1)%a = 0
    x(2)%a = 0
    i = 7; j = 7; k = 7
    read(s, '(8I2)') (((x(k)%a(i,j), i=1,n), j=1,n), k=1,2)
    if (any(reshape(x(1)%a, [4]) /= [1, 2, 3, 4])) error stop 1
    if (any(reshape(x(2)%a, [4]) /= [5, 6, 7, 8])) error stop 2
    if (i /= 3 .or. j /= 3 .or. k /= 3) error stop 3

    ! An inner bound that is the variable of a constant outer loop
    s = ' 1 2 3 4 5 6 7 8 9101112'
    a3 = 0
    k = 3
    read(s, '(12I2)') (((a3(i,j,k), i=1,k), j=1,n), k=1,3)
    if (any(a3(1:1,:,1) /= reshape([1, 2], [1, 2]))) error stop 4
    if (any(a3(1:2,:,2) /= reshape([3, 4, 5, 6], [2, 2]))) error stop 5
    if (any(a3(:,:,3) /= reshape([7, 8, 9, 10, 11, 12], [3, 2]))) error stop 6
    if (a3(2,1,1) /= 0 .or. a3(3,2,2) /= 0) error stop 7
    if (k /= 4) error stop 8

    ! A constant loop over substrings next to a nested loop
    s = ' 1 2 3 4abcdef'
    a = 0
    cs = '---'
    j = 2
    read(s, '(4I2,3A2)') ((a(i,j), i=1,n), j=1,n), (cs(j)(1:2), j=1,3)
    if (any(reshape(a, [4]) /= [1, 2, 3, 4])) error stop 9
    if (cs(1) /= 'ab-' .or. cs(2) /= 'cd-' .or. cs(3) /= 'ef-') error stop 10
    if (j /= 4) error stop 11

    ! Constant loops over components and expression subscripts next to a
    ! nested loop
    s = ' 1 2 3 4 5 6 7 8 910'
    a = 0
    x%m = 0
    b = 0
    k = 2
    read(s, '(10I2)') ((a(i,j), i=1,n), j=1,n), (x(k)%m, k=1,3), (b(k+1), k=1,3)
    if (any(reshape(a, [4]) /= [1, 2, 3, 4])) error stop 12
    if (any(x%m /= [5, 6, 7])) error stop 13
    if (any(b /= [0, 8, 9, 10])) error stop 14
    if (k /= 4) error stop 15

    ! A constant outer loop around loops with run-time bounds
    s = ' 1 2 3 4 5 6 7 8'
    a3 = 0
    k = 7
    read(s, '(8I2)') (((a3(i,j,k), i=1,n), j=1,n), k=1,2)
    if (any(reshape(a3(1:2,:,1), [4]) /= [1, 2, 3, 4])) error stop 16
    if (any(reshape(a3(1:2,:,2), [4]) /= [5, 6, 7, 8])) error stop 17
    if (i /= 3 .or. j /= 3 .or. k /= 3) error stop 18

    ! An inner bound that is the variable of a constant outer loop, two
    ! levels deep
    s = ' 1 2 3 4 5 6'
    kt = 0
    i = 1
    read(s, '(6I2)') ((kt(i,j), j=1,i), i=1,3)
    if (any(kt(1,:) /= [1, 0, 0])) error stop 19
    if (any(kt(2,:) /= [2, 3, 0])) error stop 20
    if (any(kt(3,:) /= [4, 5, 6])) error stop 21
    if (i /= 4) error stop 22

    ! A constant outer loop around a loop with a run-time bound is still
    ! read when the bound is read earlier in the same statement
    open(newunit=u, file='read_115.dat', status='replace')
    write(u, '(a)') ' 2 1 2 3 4'
    rewind(u)
    a = 0
    n = 0
    read(u, '(5I2)') n, ((a(i,j), i=1,n), j=1,2)
    close(u, status='delete')
    if (n /= 2) error stop 23
    if (any(reshape(a, [4]) /= [1, 2, 3, 4])) error stop 24

    print *, "ok"
end program
