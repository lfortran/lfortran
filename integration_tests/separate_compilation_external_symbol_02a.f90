! Modules that reproduce an ICE during deserialization when a procedure
! pointer member in a derived type references an implicit-interface
! ExternalSymbol from a foreign module.  The body visitor must preserve
! the ExternalSymbol wrapper on m_type_declaration so that the .mod
! file serialises the correct (local) symbol-table ID.

module sep_ext02_mod_method
    implicit none

    type :: method
        procedure(), nopass, pointer :: caller => null()
    end type method

contains

    subroutine caller(x)
        integer, intent(inout) :: x
        x = x + 1
    end subroutine

end module sep_ext02_mod_method

module sep_ext02_mod_runner
    use sep_ext02_mod_method, only: caller
    implicit none

    type :: runner
        procedure(), nopass, pointer :: caller => null()
    end type runner

contains

    subroutine set_caller(this)
        type(runner), intent(inout) :: this
        this%caller => caller
    end subroutine

end module sep_ext02_mod_runner
