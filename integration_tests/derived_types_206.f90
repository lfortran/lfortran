program derived_types_206
    ! Assigning to an element of an array component of a whole array, as in
    ! `w%u(1) = 9`, when `w` is allocatable, a pointer, an assumed-shape
    ! dummy or an associate name, sets that element in every element of `w`.
    implicit none
    type :: t
        integer :: u(2) = [6, 7]
        character(len=3) :: c(2) = ['abc', 'def']
        real :: r(2) = 1.0
    end type
    type(t), allocatable :: w(:)
    type(t), allocatable :: m(:, :)
    type(t), allocatable, target :: tw(:)
    type(t), pointer :: p(:)
    integer :: k

    allocate(w(2))
    w%u(1) = 9
    if (w(1)%u(1) /= 9 .or. w(2)%u(1) /= 9) error stop 1
    if (w(1)%u(2) /= 7 .or. w(2)%u(2) /= 7) error stop 2

    deallocate(w)
    allocate(w(0:3))
    if (lbound(w%u(1), 1) /= 1 .or. ubound(w%u(1), 1) /= 4) error stop 3
    do k = 0, 3
        w(k)%u = [10*k + 1, 10*k + 2]
    end do
    w%u(2) = w%u(1) + 100
    if (any(w%u(2) /= [101, 111, 121, 131])) error stop 4
    if (sum(w%u(1)) /= 64 .or. size(w%u(2)) /= 4) error stop 5
    w%c(2) = 'xyz'
    if (any(w%c(2) /= 'xyz') .or. any(w%c(1) /= 'abc')) error stop 6
    w%r(2) = w%r(1) * 2 + real(w%u(1))
    if (any(abs(w%r(2) - [3.0, 13.0, 23.0, 33.0]) > 1e-6)) error stop 7

    allocate(m(2, 3))
    m%u(2) = 3
    m(2, 3)%u(2) = 4
    if (sum(m%u(2)) /= 19) error stop 8
    if (m(1, 1)%u(1) /= 6) error stop 9

    allocate(tw(3))
    p => tw
    p%u(1) = 5
    if (any(tw%u(1) /= 5) .or. any(tw%u(2) /= 7)) error stop 10

    call set_first(w)
    if (any(w%u(1) /= [-1, -2, -3, -4])) error stop 11
    if (any(w%u(2) /= [101, 111, 121, 131])) error stop 12

    associate (x => w)
        x%u(2) = 0
    end associate
    if (any(w%u(2) /= 0)) error stop 13
    print *, "ok"
contains
    subroutine set_first(y)
        type(t), intent(inout) :: y(:)
        y%u(1) = [(-k, k = 1, size(y))]
    end subroutine
end program
