module select_type_51_mod
    implicit none
    type :: base_type
        integer :: id = 0
    end type
    type, extends(base_type) :: child_type
        real :: val = 0.0
    end type
    type :: wrapper_type
        class(base_type), allocatable :: items(:)
    contains
        procedure :: process
    end type
contains
    subroutine process(self)
        class(wrapper_type), intent(inout) :: self
        select type(elem => self%items(1))
        type is(child_type)
            elem%val = 42.0
        end select
    end subroutine
end module

program select_type_51
    use select_type_51_mod
    implicit none
    type(wrapper_type) :: w
    allocate(child_type :: w%items(2))
    call w%process()
    select type(p => w%items(1))
    type is(child_type)
        if (abs(p%val - 42.0) > 1e-5) error stop
    end select
    print *, "PASS"
end program
