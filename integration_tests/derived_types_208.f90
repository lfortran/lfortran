program derived_types_208
    ! An intrinsic subroutine that defines an element of an array component
    ! of an array, or a component of an array, has to define it in every
    ! element of the array.
    implicit none
    type :: t
        real :: y = 5
        real :: r(2) = 5
        integer :: u(2) = 0
    end type
    type(t), allocatable :: a(:)
    type(t), target :: w(6)
    type(t), pointer :: q(:)

    allocate(a(4))
    call random_number(a%r(1))
    if (any(a%r(1) >= 1) .or. any(a%r(1) < 0)) error stop
    if (any(a%r(2) /= 5)) error stop

    call random_number(a(2:4:2)%r(2))
    if (any(a(2:4:2)%r(2) >= 1)) error stop
    if (any(a(1:3:2)%r(2) /= 5)) error stop

    call random_number(a(1:3)%y)
    if (any(a(1:3)%y >= 1) .or. a(4)%y /= 5) error stop

    q => w(1:6:2)
    call random_number(q%r(2))
    if (any(w(1:6:2)%r(2) >= 1) .or. any(w(2:6:2)%r(2) /= 5)) error stop

    a%u(1) = [1, 2, 3, 4]
    a%u(2) = 0
    call mvbits(a%u(1), 0, 1, a%u(2), 0)
    print *, a%u(2)
    if (any(a%u(2) /= [1, 0, 1, 0])) error stop
    if (any(a%u(1) /= [1, 2, 3, 4])) error stop

    a%u(1) = [5, 6, 7, 8]
    call mvbits(a%u(1), 0, 1, a%u(1), 1)
    print *, a%u(1)
    if (any(a%u(1) /= [7, 4, 7, 8])) error stop

    a%u(2) = 0
    call mvbits(a(2:4)%u(1), 0, 2, a(2:4)%u(2), 0)
    print *, a%u(2)
    if (any(a%u(2) /= [0, 0, 3, 0])) error stop

    call sub(a)
contains
    subroutine sub(x)
        type(t), intent(inout) :: x(:)
        x%r(2) = 7
        call random_number(x(::2)%r(2))
        if (any(x(::2)%r(2) >= 1) .or. any(x(2::2)%r(2) /= 7)) error stop
    end subroutine
end program
