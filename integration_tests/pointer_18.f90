program pointer_18
    implicit none
    integer, pointer :: t(:), u(:)
    integer :: i

    allocate(t(-1:10))
    do i = -1, 10
        t(i) = i*10
    end do

    u(0:) => t(0:5)
    if (lbound(u, 1) /= 0) error stop 1
    if (ubound(u, 1) /= 5) error stop 2
    if (size(u) /= 6) error stop 3
    if (u(0) /= 0) error stop 4
    if (u(3) /= 30) error stop 5
    if (u(5) /= 50) error stop 6

    u(2:) => t(3:)
    if (lbound(u, 1) /= 2) error stop 7
    if (size(u) /= 8) error stop 8
    if (u(2) /= 30) error stop 9
    if (u(9) /= 100) error stop 10

    print *, lbound(u, 1), ubound(u, 1), size(u)
end program
