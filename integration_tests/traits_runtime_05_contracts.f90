module traits_runtime_05_base_m
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
    type :: Box
        integer(c_int) :: payload
    end type
    type(c_ptr) :: expected_address
end module

module traits_runtime_05_contracts_m
    use traits_runtime_05_base_m, only: IValue, ILabel, Box, expected_address
    implicit none
    abstract interface, extends(IValue) :: IChild
        integer function double_value()
        end function
    end interface
    abstract interface, extends(IChild + ILabel) :: ICombined
    end interface
    interface
        subroutine r3_acquire(n, object)
            import :: ICombined
            integer, intent(in) :: n
            class(ICombined), pointer, intent(out) :: object
        end subroutine
        subroutine r3_update(n)
            integer, intent(in) :: n
        end subroutine
        subroutine r3_check_parent(object, n)
            import :: IValue
            class(IValue), target, intent(in) :: object
            integer, intent(in) :: n
        end subroutine
        subroutine r3_check_readonly(object, n)
            import :: IValue
            class(IValue), pointer, intent(in) :: object
            integer, intent(in) :: n
        end subroutine
        subroutine r3_check_combined(object, n)
            import :: ICombined
            class(ICombined), pointer, intent(in) :: object
            integer, intent(in) :: n
        end subroutine
        subroutine r3_check_alternative_scope(object, n)
            import :: ICombined
            class(ICombined), pointer, intent(in) :: object
            integer, intent(in) :: n
        end subroutine
    end interface
end module
