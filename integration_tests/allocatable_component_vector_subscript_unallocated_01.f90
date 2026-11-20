program allocatable_component_vector_subscript_unallocated_01
    implicit none

    type :: t
        integer, allocatable :: d(:)
    end type t

    type(t) :: r(2)
    integer :: a(4) = [10, 20, 30, 40]

    r(1)%d = a([2, 4])

    if (.not. allocated(r(1)%d)) error stop
    if (size(r(1)%d) /= 2) error stop
    if (any(r(1)%d /= [20, 40])) error stop
end program allocatable_component_vector_subscript_unallocated_01
