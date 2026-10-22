! Test: nested implied do loops over struct-typed array elements in
! read/write, including a loop that mixes struct and integer items.
program implied_do_loops50
    implicit none

    type :: t
        integer :: k
    end type

    type :: pair_t
        integer :: k, m
    end type

    type, extends(pair_t) :: triple_t
        integer :: z
    end type

    type(t) :: x(2,2)
    type(pair_t) :: p(2,2)
    type(triple_t) :: q(2,2)
    type(pair_t), allocatable :: w(:,:,:)
    integer :: a(2)
    integer :: i, j, l, n, u
    character(len=200) :: s

    n = 2

    ! External unit, formatted (the reproducer from the issue)
    open(newunit=u, status='scratch')
    write(u, '(a)') ' 1 2 3 4'
    rewind(u)
    read(u, '(4I2)') ((x(i,j), i = 1, n), j = 1, n)
    close(u)
    if (any(reshape(x%k, [4]) /= [1, 2, 3, 4])) error stop 1

    ! Internal file, formatted, two components
    s = ' 1 2 3 4 5 6 7 8'
    read(s, '(8I2)') ((p(i,j), i = 1, n), j = 1, n)
    if (any(reshape(p%k, [4]) /= [1, 3, 5, 7])) error stop 2
    if (any(reshape(p%m, [4]) /= [2, 4, 6, 8])) error stop 3

    ! Internal file, list-directed
    s = '8 7 6 5 4 3 2 1'
    read(s, *) ((p(i,j), i = 1, n), j = 1, n)
    if (any(reshape(p%k, [4]) /= [8, 6, 4, 2])) error stop 4
    if (any(reshape(p%m, [4]) /= [7, 5, 3, 1])) error stop 5

    ! Formatted and list-directed write
    write(s, '(8I2)') ((p(i,j), i = 1, n), j = 1, n)
    if (s /= ' 8 7 6 5 4 3 2 1') error stop 6
    p%k = 0
    p%m = 0
    read(s, *) ((p(i,j), i = 1, n), j = 1, n)
    if (any(reshape(p%k, [4]) /= [8, 6, 4, 2])) error stop 7
    write(s, *) ((p(i,j), i = 1, n), j = 1, n)
    p%k = 0
    p%m = 0
    read(s, *) ((p(i,j), i = 1, n), j = 1, n)
    if (any(reshape(p%m, [4]) /= [7, 5, 3, 1])) error stop 8

    ! Extended type: parent components come first
    s = '1 2 3 4 5 6 7 8 9 10 11 12'
    read(s, *) ((q(i,j), i = 1, n), j = 1, n)
    if (any(reshape(q%k, [4]) /= [1, 4, 7, 10])) error stop 9
    if (any(reshape(q%m, [4]) /= [2, 5, 8, 11])) error stop 10
    if (any(reshape(q%z, [4]) /= [3, 6, 9, 12])) error stop 11

    ! Three levels over an allocatable array
    allocate(w(2,2,2))
    s = '1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16'
    read(s, *) (((w(i,j,l), i = 1, n), j = 1, n), l = 1, n)
    if (any(reshape(w%k, [8]) /= [1, 3, 5, 7, 9, 11, 13, 15])) error stop 12
    if (any(reshape(w%m, [8]) /= [2, 4, 6, 8, 10, 12, 14, 16])) error stop 13

    ! A loop whose first item is an integer and whose second is a struct
    s = '1 2 3 4'
    read(s, *) (a(i), x(i,1), i = 1, n)
    if (any(a /= [1, 3])) error stop 14
    if (any(x(:,1)%k /= [2, 4])) error stop 15

    ! Nested loop mixing a component and the whole element
    s = '1 2 3 4 5 6 7 8 9 10 11 12'
    read(s, *) ((p(i,j)%m, p(i,j), i = 1, n), j = 1, n)
    if (any(reshape(p%k, [4]) /= [2, 5, 8, 11])) error stop 16
    if (any(reshape(p%m, [4]) /= [3, 6, 9, 12])) error stop 17

    print *, "ok"
end program
