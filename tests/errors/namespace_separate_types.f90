! Derived types and procedures with dummy arguments of them, for cc_64 and
! cc_65 of namespace_continue_compilation.f90.
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
