module traits_runtime_combination_01_contracts_m
    use iso_c_binding, only: c_int, c_ptr
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    abstract interface :: ILabel
        integer function label()
        end function
    end interface
    abstract interface :: IScale
        integer function scaled(factor)
            integer, intent(in) :: factor
        end function
    end interface
    abstract interface, extends(IValue) :: ILeft
    end interface
    abstract interface, extends(IValue) :: IRight
    end interface
    abstract interface :: IAlias
        integer function value()
        end function
    end interface
    abstract interface, extends(IScale + ILeft + IRight + ILabel + IAlias) :: IRich
    end interface
    type :: PublicBox
        integer(c_int) :: payload = 0
    contains
        final :: finalize_public
    end type
    type(c_ptr) :: expected_address, observed_address
    integer :: finals = 0, final_sum = 0
    interface
        subroutine combination_acquire(choice, n, object)
            import :: IRich
            integer, intent(in) :: choice, n
            class(IRich), pointer, intent(out) :: object
        end subroutine
        subroutine combination_acquire_anonymous(choice, n, object)
            import :: IValue, ILabel, IScale
            integer, intent(in) :: choice, n
            class(IScale + IValue + ILabel), pointer, intent(out) :: object
        end subroutine
        subroutine combination_update(choice, n)
            integer, intent(in) :: choice, n
        end subroutine
        subroutine combination_release()
        end subroutine
        subroutine combination_alternative(object, n, tag)
            import :: IRich
            class(IRich), pointer, intent(in) :: object
            integer, intent(in) :: n, tag
        end subroutine
    end interface
contains
    subroutine finalize_public(self)
        type(PublicBox), intent(inout) :: self
        finals = finals + 1
        final_sum = final_sum + self%payload
        self%payload = -777
    end subroutine
end module
