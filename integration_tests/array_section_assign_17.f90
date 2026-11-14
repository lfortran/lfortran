! An assignment to a substring of each element of a character array section,
! `names(:)(2:3) = value`, in a procedure that has a variable named `value`:
! the temporary that the compiler creates for the value is not that variable.
module array_section_assign_17_mod
    implicit none
contains
    subroutine set_scalar(names, value)
        character(len=*), intent(inout) :: names(:)
        character(len=*), intent(in) :: value
        names(:)(2:3) = value
    end subroutine

    subroutine set_array(names, value)
        character(len=*), intent(inout) :: names(:)
        character(len=*), intent(in) :: value(:)
        names(:)(2:3) = value
        names(:)(4:4) = value(size(value):1:-1)(1:1)
    end subroutine

    ! The dummy argument `value` is the target
    subroutine shift(value)
        character(len=4), intent(inout) :: value(:)
        value(:)(2:3) = value(:)(1:2)
    end subroutine

    subroutine spread_first(value)
        character(len=4), intent(inout) :: value(:)
        value(:)(4:4) = value(1)(1:1)
    end subroutine
end module

program array_section_assign_17
    use array_section_assign_17_mod
    implicit none
    character(len=4) :: names(3)
    character(len=2) :: value(3)

    names = ['abcd', 'efgh', 'ijkl']
    call set_scalar(names, 'xy')
    if (any(names /= ['axyd', 'exyh', 'ixyl'])) error stop 1

    names = ['abcd', 'efgh', 'ijkl']
    call set_array(names, ['xy', 'zw', 'uv'])
    if (any(names /= ['axyu', 'ezwz', 'iuvx'])) error stop 2

    names = ['abcd', 'efgh', 'ijkl']
    call shift(names)
    if (any(names /= ['aabd', 'eefh', 'iijl'])) error stop 3

    call spread_first(names)
    if (any(names /= ['aaba', 'eefa', 'iija'])) error stop 4

    ! A local variable `value`, also used by an internal procedure, and an
    ! internal procedure with a dummy argument `value`
    value = ['xy', 'zw', 'uv']
    names = ['abcd', 'efgh', 'ijkl']
    names(:)(2:3) = value
    if (any(names /= ['axyd', 'ezwh', 'iuvl'])) error stop 5

    names(:)(1:2) = value(3:1:-1)
    if (any(names /= ['uvyd', 'zwwh', 'xyvl'])) error stop 6

    call host()
    if (any(names /= ['xyxy', 'xyzw', 'xyuv'])) error stop 8

    call internal(names)
    if (any(names /= ['xxyx', 'xxyx', 'xxyx'])) error stop 10

contains

    subroutine host()
        names(:)(3:4) = value
        if (any(names /= ['uvxy', 'zwzw', 'xyuv'])) error stop 7
        names(:)(1:2) = value(1)
    end subroutine

    subroutine internal(value)
        character(len=4), intent(inout) :: value(:)
        value(:)(2:3) = value(:)(1:2)
        if (any(value /= ['xxyy', 'xxyw', 'xxyv'])) error stop 9
        value(:)(4:4) = value(1)(1:1)
    end subroutine

end program
