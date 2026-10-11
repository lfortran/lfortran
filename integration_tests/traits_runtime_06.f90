module traits_runtime_06_m
    use iso_c_binding, only: c_int, c_ptr, c_loc, c_associated
    implicit none
    abstract interface :: IBase
        integer function value()
        end function
    end interface
    abstract interface, extends(IBase) :: ILeft
        integer function left_value()
        end function
    end interface
    abstract interface, extends(IBase) :: IRight
        integer function right_value()
        end function
    end interface
    abstract interface, extends(ILeft + IRight) :: IDiamond
    end interface
    abstract interface :: IAlias
        integer function value()
        end function
    end interface
    abstract interface, extends(IDiamond + IAlias) :: IAgreement
    end interface
    type :: Box
        integer(c_int) :: n
    end type
    type(c_ptr) :: expected_address
    implements IAgreement :: Box
        procedure, pass :: right_value => box_right
        procedure, pass :: value => box_value
        procedure, pass :: left_value => box_left
    end implements
contains
    integer function box_value(self)
        type(Box), target, intent(in) :: self
        type(c_ptr) :: actual_address
        actual_address = c_loc(self%n)
        if (.not. c_associated(actual_address, expected_address)) error stop 1
        box_value = self%n
    end function
    integer function box_left(self)
        type(Box), target, intent(in) :: self
        type(c_ptr) :: actual_address
        actual_address = c_loc(self%n)
        if (.not. c_associated(actual_address, expected_address)) error stop 2
        box_left = self%n + 10
    end function
    integer function box_right(self)
        type(Box), target, intent(in) :: self
        type(c_ptr) :: actual_address
        actual_address = c_loc(self%n)
        if (.not. c_associated(actual_address, expected_address)) error stop 3
        box_right = self%n + 100
    end function
    integer function base_value(object)
        class(IBase), intent(in) :: object
        base_value = object%value()
    end function
    subroutine check_left(object, n)
        class(ILeft), intent(in) :: object
        integer, intent(in) :: n
        if (object%left_value() /= n + 10) error stop 4
        if (base_value(object) /= n) error stop 5
    end subroutine
    subroutine check_right(object, n)
        class(IRight), intent(in) :: object
        integer, intent(in) :: n
        if (object%right_value() /= n + 100) error stop 6
        if (base_value(object) /= n) error stop 7
    end subroutine
    subroutine check_agreement(object, n)
        class(IAgreement), pointer, intent(in) :: object
        integer, intent(in) :: n
        class(IDiamond), pointer :: diamond
        class(ILeft), pointer :: left
        class(IRight), pointer :: right
        class(IBase), pointer :: left_base, right_base
        class(IAlias), pointer :: alias
        diamond => object
        left => diamond
        right => diamond
        left_base => left
        right_base => right
        alias => object
        if (.not. associated(left_base, right_base)) error stop 8
        call check_left(left, n)
        call check_right(right, n)
        call check_left(diamond, n)
        call check_right(diamond, n)
        if (base_value(left_base) /= n .or. base_value(right_base) /= n) error stop 9
        if (alias%value() /= n .or. object%value() /= n) error stop 10
        nullify(left, left_base, diamond)
        if (right_base%value() /= n .or. alias%value() /= n) error stop 11
        nullify(right, right_base, alias)
    end subroutine
end module

program traits_runtime_06
    use traits_runtime_06_m
    implicit none
    type(Box), target :: object
    class(IAgreement), pointer :: view => null()
    class(IBase), pointer :: base => null()
    base => view
    if (associated(base)) error stop 12
    object%n = 7
    expected_address = c_loc(object%n)
    view => object
    call check_agreement(view, 7)
    base => object
    if (base_value(base) /= 7) error stop 13
    object%n = 23
    call check_agreement(view, 23)
    nullify(view)
    if (base%value() /= 23) error stop 14
    nullify(base)
    if (object%n /= 23) error stop 15
end program
