program read_117
    ! Integer and logical items of every kind in a formatted READ with
    ! nested implied-do loops
    implicit none
    integer(1) :: b(2,2)
    integer(2) :: a(2,2), g(2)
    integer(8) :: k8(2,2)
    logical(1) :: l1(2,2)
    logical(2) :: l2(2)
    logical(8) :: l8(2)
    integer(2) :: before
    logical(1) :: after
    integer :: i, j, n
    character(len=80) :: s
    n = 2
    g = -7
    s = ' 9 1 2 3 4 5 6 7 8 1 2 3 4 T F F T F T T F T'
    read(s, '(13I2,9L2)') before, ((a(i,j), j=1,n), i=1,n), &
        ((b(i,j), j=1,n), i=1,n), ((k8(i,j), j=1,n), i=1,n), &
        ((l1(i,j), j=1,n), i=1,n), (l2(i), i=1,n), (l8(i), i=1,n), after
    if (before /= 9) error stop 1
    if (a(1,1) /= 1 .or. a(1,2) /= 2 .or. a(2,1) /= 3 .or. a(2,2) /= 4) error stop 2
    if (b(1,1) /= 5 .or. b(1,2) /= 6 .or. b(2,1) /= 7 .or. b(2,2) /= 8) error stop 3
    if (k8(1,1) /= 1 .or. k8(1,2) /= 2 .or. k8(2,1) /= 3 .or. k8(2,2) /= 4) error stop 4
    if (.not. l1(1,1) .or. l1(1,2) .or. l1(2,1) .or. .not. l1(2,2)) error stop 5
    if (l2(1) .or. .not. l2(2)) error stop 6
    if (.not. l8(1) .or. l8(2)) error stop 7
    if (.not. after) error stop 8
    if (any(g /= -7)) error stop 9
    print *, before, a, b, k8, l1, l2, l8, after
end program
