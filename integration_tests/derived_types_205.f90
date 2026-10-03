program derived_types_205
    ! An element of an array component selected out of an array section, or
    ! out of an allocatable, pointer or assumed-shape array, as in `w(:)%u(1)`,
    ! is an array shaped like its base.
    implicit none
    type :: n_t
        integer :: v(3) = [1, 2, 3]
    end type
    type :: t
        integer :: u(2) = [9, 9]
        real :: r(3) = 0
        type(n_t) :: nest
    end type
    type(t), target :: w(6)
    type(t), allocatable :: a(:)
    type(t), allocatable :: m(:, :)
    type(t), pointer :: p(:)
    integer :: k, i, j, cnt
    integer :: res(3)

    allocate(a(2))
    if (.not. all(a(:)%u(1) == 9)) error stop 1
    if (any(a(:)%u(1) /= 9)) error stop 2
    if (any(a%u(2) /= 9)) error stop 3

    do k = 1, 6
        w(k)%u = [k, 10*k]
        w(k)%r = real(k)
        w(k)%nest%v = [k, 2*k, 3*k]
    end do
    if (.not. all(w(:)%u(1) == [1, 2, 3, 4, 5, 6])) error stop 4
    if (any(w(2:4:2)%u(2) /= [20, 40])) error stop 5
    if (sum(w(4:)%u(2)) /= 150) error stop 6
    if (size(w(1:5:2)%u(1)) /= 3) error stop 7
    if (count(w(:)%u(1) > 2) /= 4) error stop 8
    if (abs(sum(w(1:3)%r(2) * 2.0) - 12.0) > 1e-6) error stop 9
    res = w(1:5:2)%u(1)
    if (any(res /= [1, 3, 5])) error stop 10
    if (any(w(2:4)%nest%v(3) /= [6, 9, 12])) error stop 11

    w(1:5:2)%u(2) = 0
    if (any(w%u(2) /= [0, 20, 0, 40, 0, 60])) error stop 12
    w(5:6)%nest%v(1) = -1
    if (any(w%nest%v(1) /= [1, 2, 3, 4, -1, -1])) error stop 13
    w(2:6)%u(1) = w(1:5)%u(1)
    if (any(w%u(1) /= [1, 1, 2, 3, 4, 5])) error stop 14
    w(:)%u(2) = w(:)%u(1) + 1
    if (any(w%u(2) /= [2, 2, 3, 4, 5, 6])) error stop 15

    p => w(2:6:2)
    if (any(p(:)%u(1) /= [1, 3, 5])) error stop 16
    if (any(p%u(2) /= [2, 4, 6])) error stop 17
    p(2:3)%u(1) = -1
    if (any(w%u(1) /= [1, 1, 2, -1, 4, -1])) error stop 18

    deallocate(a)
    allocate(a(4))
    do k = 1, 4
        a(k)%u = [k, -k]
    end do
    if (any(a(2:3)%u(2) /= [-2, -3])) error stop 19
    a(1:4:3)%u(1) = 7
    if (any(a%u(1) /= [7, 2, 3, 7])) error stop 20

    allocate(m(3, 2))
    do i = 1, 3
        do j = 1, 2
            m(i, j)%u = [i + 10*j, 0]
        end do
    end do
    if (any(m(:, 2)%u(1) /= [21, 22, 23])) error stop 21
    if (any(m(2, :)%u(1) /= [12, 22])) error stop 22
    if (maxval(m(1:3:2, 1)%u(1)) /= 13) error stop 23
    m(1, :)%u(2) = 5
    if (sum(m%u(2)) /= 10) error stop 24

    cnt = 0
    do while (any(w(1:2)%u(2) < 5))
        w(1:2)%u(2) = w(1:2)%u(2) + 1
        cnt = cnt + 1
    end do
    if (cnt /= 3) error stop 25

    call check(w(1:3))
    print *, "ok"
contains
    subroutine check(x)
        type(t), intent(in) :: x(:)
        if (any(x(:)%u(1) /= [1, 1, 2])) error stop 26
        if (any(x(2:)%u(1) /= [1, 2])) error stop 27
        if (any(x%u(1) /= [1, 1, 2])) error stop 28
    end subroutine
end program
