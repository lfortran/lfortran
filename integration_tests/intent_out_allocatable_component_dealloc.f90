module intent_out_allocatable_component_dealloc_m
    implicit none

    type :: container_t
        integer, allocatable :: a(:)
    end type container_t
contains
    subroutine set_zero_len(x)
        integer, allocatable, intent(out) :: x(:)
        if (allocated(x)) error stop 1
        allocate(x(0))
    end subroutine set_zero_len
end module intent_out_allocatable_component_dealloc_m

program intent_out_allocatable_component_dealloc
    use intent_out_allocatable_component_dealloc_m, only: container_t, set_zero_len
    implicit none

    type(container_t) :: c

    allocate(c%a(2))
    c%a = [1, 2]

    call set_zero_len(c%a)

    if (.not. allocated(c%a)) error stop 2
    if (size(c%a) /= 0) error stop 3
end program intent_out_allocatable_component_dealloc

