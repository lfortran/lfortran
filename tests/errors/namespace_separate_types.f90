! Derived types and procedures with dummy arguments of them, for
! namespace_separate_component.f90 and namespace_separate_submodule.f90.
module nssep_types
    implicit none
    type :: t
        integer :: k = 1
    end type
    type :: u
        integer :: k = 2
    end type
contains
    subroutine take(v)
        type(t), intent(in) :: v
    end subroutine
end module
