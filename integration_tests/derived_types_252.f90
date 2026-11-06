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
    integer :: i

    ! Give every base element a distinct value, so reading from the wrong
    ! element of a cannot pass the checks below.
    do i = 1, 3
        a(i)%x(2)%u = [10*i, 20*i]
    end do

    ! Read element 2 of u out of element 2 of x from every element of a.
    r = a%x(2)%u(2)
    if (r(1) /= 20 .or. r(2) /= 40 .or. r(3) /= 60) error stop
    print *, r

    ! Store through the same kind of reference, on every element of a.
    a%x(1)%u(1) = 7
    if (a(1)%x(1)%u(1) /= 7 .or. a(2)%x(1)%u(1) /= 7 &
        .or. a(3)%x(1)%u(1) /= 7) error stop
    if (a(1)%x(2)%u(1) /= 10 .or. a(1)%x(1)%u(2) /= 2) error stop
    print *, a(1)%x(1)%u(1), a(2)%x(2)%u(1), a(3)%x(1)%u(1)

    ! Element of an array component of a single element still works.
    r(1) = a(1)%x(2)%u(2)
    if (r(1) /= 20) error stop
end program
