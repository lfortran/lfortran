module derived_type_operator_01_mod
    implicit none
    type :: base
        integer :: i = 0
    contains
        procedure :: land
        generic :: operator(.and.) => land
    end type

    type, extends(base) :: child
    end type

contains
    integer function land(a, b)
        class(base), intent(in) :: a
        logical, intent(in) :: b
        land = 1
        if (b) land = a%i
    end function
end module derived_type_operator_01_mod

program derived_type_operator_01
    use derived_type_operator_01_mod
    implicit none
    type(child) :: c
    c%i = 7
    print *, c .and. .true.
    if ((c .and. .true.) /= 7) error stop
end program derived_type_operator_01x