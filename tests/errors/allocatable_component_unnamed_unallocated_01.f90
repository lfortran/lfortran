program allocatable_component_unnamed_unallocated_01
    implicit none

    type :: t
        integer, allocatable :: arr(:)
    end type t

    type(t) :: x(1)

    x(1)%arr = [1, 2, 3]
end program allocatable_component_unnamed_unallocated_01
