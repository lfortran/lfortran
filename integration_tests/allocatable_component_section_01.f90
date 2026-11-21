program allocatable_component_section_01
    implicit none
    type :: inner_t
        integer, allocatable :: d(:)
    end type inner_t
    type :: outer_t
        type(inner_t) :: inner
    end type outer_t

    type(inner_t) :: s, r(2)
    type(outer_t) :: u

    allocate(s%d(3), r(1)%d(4), u%inner%d(3))
    s%d(:) = 5
    r(1)%d(:) = 7
    r(1)%d(1:2) = 9
    u%inner%d(:) = 11

    if (any(s%d /= [5, 5, 5])) error stop 1
    if (any(r(1)%d /= [9, 9, 7, 7])) error stop 2
    if (any(u%inner%d /= [11, 11, 11])) error stop 3
end program allocatable_component_section_01
