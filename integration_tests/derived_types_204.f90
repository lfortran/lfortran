program derived_types_204
    implicit none

    type :: t
        integer :: u(2) = [6, 7]
    end type

    type(t), allocatable :: w(:)

    allocate(w(2))
    w%u(1) = 9

    if (w(1)%u(1) /= 9 .or. w(2)%u(1) /= 9) error stop
    
end program derived_types_204