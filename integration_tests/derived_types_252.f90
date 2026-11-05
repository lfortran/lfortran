program derived_types_252
    ! An element of an array component of each element of a derived-type
    ! array, as in a%x(2)%u(2), read and stored (#13629).
    implicit none
    type :: in
        integer :: u(2) = [1, 2]
    end type
    type :: t
        type(in) :: x(3)
    end type
    type(t) :: a(3)
    integer :: r(3)

    ! Read element 2 of u out of element 2 of x from every element of a.
    r = a%x(2)%u(2)
    if (r(1) /= 2 .or. r(2) /= 2 .or. r(3) /= 2) error stop
    print *, r

    ! Store through the same kind of reference.
    a%x(1)%u(1) = 7
    if (a(1)%x(1)%u(1) /= 7 .or. a(2)%x(1)%u(1) /= 7 &
        .or. a(3)%x(1)%u(1) /= 7) error stop
    if (a(1)%x(2)%u(1) /= 1 .or. a(1)%x(1)%u(2) /= 2) error stop
    print *, a(1)%x(1)%u(1), a(2)%x(2)%u(1), a(3)%x(1)%u(1)

    ! Element of an array component of a single element still works.
    r(1) = a(1)%x(2)%u(2)
    if (r(1) /= 2) error stop
end program
