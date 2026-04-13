program allocatable_component_unnamed_realloc_01
    implicit none

    type :: t
        integer, allocatable :: arr(:)
    end type t

    type(t) :: x(1)

    x(1)%arr = [1, 2, 3]

    if (.not. allocated(x(1)%arr)) error stop
    if (size(x(1)%arr) /= 3) error stop
    if (.not. all(x(1)%arr == [1, 2, 3])) error stop
end program allocatable_component_unnamed_realloc_01
