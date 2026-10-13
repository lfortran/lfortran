module traits_runtime_pointer_separate_01_consumer
    use traits_runtime_pointer_separate_01_contracts
    implicit none
contains
    subroutine observe(object, expected)
        class(IValue), pointer, intent(in) :: object
        integer, intent(in) :: expected
        class(IValue), pointer :: alias
        integer :: result
        alias => object
        call alias%read_into(result)
        if (result /= expected .or. alias%value() /= expected) error stop 1
        if (borrow(alias) /= expected) error stop 2
        if (.not. associated(alias, object)) error stop 3
        nullify(alias)
        if (.not. associated(object)) error stop 4
    end subroutine
    integer function borrow(object)
        class(IValue), intent(in) :: object
        borrow = object%value()
    end function
    subroutine forward(source, target)
        class(IValue), pointer, intent(in) :: source
        class(IValue), pointer, intent(out) :: target
        target => source
    end subroutine
end module
