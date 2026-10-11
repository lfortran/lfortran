module traits_runtime_08_m
    use iso_c_binding, only: c_int, c_ptr, c_loc, c_associated
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function value
    end interface IValue
    abstract interface :: ILabel
        function label() result(r)
            integer :: r
        end function label
    end interface ILabel
    type :: Box
        integer(c_int) :: payload
    end type Box
    type(c_ptr) :: expected_address
    implements IValue + ILabel :: Box
        procedure, nopass :: label => box_label
        procedure, pass :: value => box_value
    end implements Box
contains
    function box_value(self) result(r)
        type(Box), target, intent(in) :: self
        type(c_ptr) :: actual_address
        integer :: r
        actual_address = c_loc(self%payload)
        if (.not. c_associated(actual_address, expected_address)) error stop 1201
        r = self%payload
    end function box_value
    function box_label() result(r)
        integer :: r
        r = 101
    end function box_label
    function reverse_order(object) result(r)
        class(ILabel + IValue), intent(in) :: object
        integer :: r
        r = object%label() + object%value()
    end function reverse_order
    function forward_order(object, n) result(r)
        class(IValue + ILabel), intent(in) :: object
        integer, intent(in) :: n
        integer :: r
        if (object%value() /= n) error stop 1202
        if (object%label() /= 101) error stop 1203
        r = reverse_order(object)
    end function forward_order
    subroutine remember(object, retrieve, result_view)
        class(IValue + ILabel), pointer, intent(in) :: object
        logical, intent(in) :: retrieve
        class(ILabel + IValue), pointer, intent(out) :: result_view
        class(IValue + ILabel), pointer, save :: saved => null()
        if (retrieve) then
            result_view => saved
            nullify(saved)
        else
            saved => object
            nullify(result_view)
        end if
    end subroutine remember
    subroutine readonly(object, expected)
        class(ILabel + IValue), pointer, intent(in) :: object
        logical, intent(in) :: expected
        if (associated(object) .neqv. expected) error stop 1212
    end subroutine readonly
end module traits_runtime_08_m

program traits_runtime_08
    use traits_runtime_08_m
    implicit none
    type(Box), target :: object
    class(IValue + ILabel), pointer :: forward => null()
    class(ILabel + IValue), pointer :: reverse => null()
    class(IValue + ILabel + IValue), pointer :: repeated => null()
    class(IValue), pointer :: value1 => null(), value2 => null()
    class(ILabel), pointer :: label => null()
    object%payload = 7
    expected_address = c_loc(object%payload)
    forward => object
    reverse => forward
    repeated => reverse
    value1 => forward
    value2 => reverse
    label => reverse
    if (.not. associated(value1, value2)) error stop 1204
    if (forward_order(forward, 7) /= 108) error stop 1205
    if (forward_order(reverse, 7) /= 108) error stop 1206
    if (forward_order(object, 7) /= 108) error stop 1213
    if (value1%value() /= 7 .or. label%label() /= 101) error stop 1207
    call remember(repeated, .false., forward)
    if (associated(forward)) error stop 1214
    call readonly(null(), .false.)
    call readonly(forward, .false.)
    call readonly(object, .true.)
    nullify(reverse, repeated)
    call remember(forward, .true., reverse)
    object%payload = 13
    forward => reverse
    if (forward_order(forward, 13) /= 114) error stop 1208
    if (reverse_order(reverse) /= 114 .or. value2%value() /= 13) error stop 1209
    nullify(forward, value1)
    if (reverse%value() /= 13 .or. label%label() /= 101) error stop 1210
    nullify(reverse, value2, label)
    if (object%payload /= 13) error stop 1211
end program traits_runtime_08
