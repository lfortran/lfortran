module derived_type_operator_02_mod
    implicit none
    type :: base
        integer :: i = 0
    contains
        procedure :: cat
        generic :: operator(//) => cat
    end type

    type, extends(base) :: child
    end type

contains
    integer function cat(a, b)
        class(base), intent(in) :: a
        character(*), intent(in) :: b
        cat = a%i + len(b)
    end function
end module derived_type_operator_02_mod

program derived_type_operator_02
    use derived_type_operator_02_mod
    implicit none
    type(child) :: c
    c%i = 7
    print *, c // "x"
    if ((c // "x") /= 8) error stop
end program derived_type_operator_02