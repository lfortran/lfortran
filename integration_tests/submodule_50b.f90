module submodule_50_mod
    implicit none

    type :: container_type
        integer :: rank = 2
    contains
        procedure :: setup => setup_impl
    end type

    interface
        module subroutine setup_impl(this, lengths)
            class(container_type), intent(inout) :: this
            integer, dimension(this%rank), intent(in) :: lengths
        end subroutine
    end interface

    type :: wrapper_type
        type(container_type), dimension(:), allocatable :: containers
    end type

end module
