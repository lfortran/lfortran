! A derived type with a component declared through a module entity, for
! cc_64 of namespace_continue_compilation.f90, and a module with a variable
! declared through a module entity and a separate module procedure, for its
! submodule cc_65.
module nssep_holder
    use, namespace :: l => nssep_types
    implicit none
    type :: holder
        type(l%u) :: c
    end type
end module

module nssep_parent
    use, namespace :: l => nssep_types
    implicit none
    type(l%u) :: pv
    interface
        module subroutine run()
        end subroutine
    end interface
end module
