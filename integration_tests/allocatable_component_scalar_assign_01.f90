program allocatable_component_scalar_assign_01
    implicit none

    type :: inner_t
        integer, allocatable :: d(:)
    end type inner_t

    type :: t
        integer, allocatable :: d(:)
        type(inner_t) :: in
    end type t

    type(t) :: r(2), r2(2, 2)

    allocate(r(1)%d(3))
    r(1)%d = 5
    if (size(r(1)%d) /= 3) error stop
    if (any(r(1)%d /= 5)) error stop

    r(1)%d(:) = 7
    if (any(r(1)%d /= 7)) error stop

    allocate(r(1)%in%d(2))
    r(1)%in%d = 9
    if (size(r(1)%in%d) /= 2) error stop
    if (any(r(1)%in%d /= 9)) error stop

    allocate(r2(1, 2)%d(4))
    r2(1, 2)%d = 11
    if (size(r2(1, 2)%d) /= 4) error stop
    if (any(r2(1, 2)%d /= 11)) error stop
end program allocatable_component_scalar_assign_01
