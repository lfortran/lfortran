program derived_types_253
    ! An element of an array component of each element of a derived-type
    ! array selecting a scalar component, as in a%x(2)%u (#13629).
    implicit none
    type :: tscalar
        integer :: u = 1
    end type
    type :: tscalar_wrap
        type(tscalar) :: x(3)
    end type
    type(tscalar_wrap) :: b(3)
    integer :: r(3)
    integer :: i

    ! Give every base element a distinct value, so reading from the wrong
    ! element of b cannot pass the checks below.
    do i = 1, 3
        b(i)%x(2)%u = 10*i
    end do

    r = b%x(2)%u
    if (r(1) /= 10 .or. r(2) /= 20 .or. r(3) /= 30) error stop
    print *, r

    ! Store through the same kind of reference, on every element of b.
    b%x(1)%u = 5
    if (b(1)%x(1)%u /= 5 .or. b(2)%x(1)%u /= 5 .or. b(3)%x(1)%u /= 5) error stop
    if (b(1)%x(2)%u /= 10 .or. b(3)%x(2)%u /= 30) error stop
    print *, b(1)%x(1)%u, b(2)%x(2)%u, b(3)%x(1)%u

    ! Scalar store to a scalar component of a single base element.
    b(2)%x(1)%u = 7
    if (b(2)%x(1)%u /= 7) error stop
    if (b(1)%x(1)%u /= 5 .or. b(3)%x(1)%u /= 5) error stop
end program
