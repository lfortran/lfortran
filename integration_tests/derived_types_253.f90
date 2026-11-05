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

    r = b%x(2)%u
    if (r(1) /= 1 .or. r(2) /= 1 .or. r(3) /= 1) error stop
    print *, r
end program
