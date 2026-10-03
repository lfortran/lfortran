program derived_types_207
    ! An element of an array component of an array that is itself a
    ! component of a scalar, as in `s%w%u(1)`, is an array shaped like `s%w`,
    ! whether `s%w` is allocatable, a pointer or of fixed size.
    implicit none
    type :: t
        integer :: u(2) = [6, 7]
    end type
    type :: h
        type(t), allocatable :: w(:)
        type(t), pointer :: p(:) => null()
        type(t) :: f(3)
    end type
    type(h) :: s
    type(h), allocatable :: hs(:)
    integer :: k

    allocate(s%w(2))
    s%w%u(1) = 9
    if (s%w(1)%u(1) /= 9 .or. s%w(2)%u(1) /= 9) error stop 1
    if (any(s%w%u(2) /= 7)) error stop 2

    s%f%u(2) = 4
    if (any(s%f%u(2) /= 4) .or. any(s%f%u(1) /= 6)) error stop 3
    s%f%u(1) = s%f%u(2) + [1, 2, 3]
    if (any(s%f%u(1) /= [5, 6, 7])) error stop 4
    if (sum(s%f%u(1)) /= 18 .or. size(s%f%u(1)) /= 3) error stop 5

    allocate(s%p(4))
    do k = 1, 4
        s%p(k)%u = [k, -k]
    end do
    s%p%u(2) = s%p%u(1) * 10
    if (any(s%p%u(2) /= [10, 20, 30, 40])) error stop 6
    if (any(s%p(2:4:2)%u(2) /= [20, 40])) error stop 7
    s%w(2:2)%u(2) = 0
    if (any(s%w%u(2) /= [7, 0])) error stop 8

    allocate(hs(2))
    allocate(hs(2)%w(3))
    hs(2)%w%u(2) = 1
    if (any(hs(2)%w%u(2) /= 1) .or. any(hs(2)%w%u(1) /= 6)) error stop 9
    deallocate(s%p)
    print *, "ok"
end program
