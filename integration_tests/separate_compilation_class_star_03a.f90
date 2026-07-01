module separate_compilation_class_star_03_base
    implicit none
    private
    type, public :: arg_base
        class(*), allocatable :: value
    end type
end module

module separate_compilation_class_star_03_arg
    use separate_compilation_class_star_03_base
    implicit none
    private
    type, extends(arg_base), public :: arg
    end type
end module

module separate_compilation_class_star_03_method
    use separate_compilation_class_star_03_arg
    implicit none
    private
    type, public :: method
        type(arg), allocatable, public :: args(:)
    contains
        procedure :: invoke => invoke_impl
    end type
contains
    subroutine invoke_impl(this)
        class(method), intent(inout) :: this
        if (allocated(this%args)) then
            associate(a => this%args(1)%value)
                ! use a
            end associate
        end if
    end subroutine
end module
