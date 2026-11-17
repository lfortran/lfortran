module c_reset_character_component_01_m
    implicit none
    type :: item
        character(len=4) :: value = 'ab'
    end type
    character(len=4), save :: shared = 'ab'
contains
    subroutine unused(x)
        class(item), intent(inout) :: x
    end subroutine

    subroutine overwrite(s)
        character(len=4), intent(inout) :: s
        s = 'q'
    end subroutine

    function result_text() result(s)
        character(len=4) :: s
        s = 'wxyz'
    end function

    subroutine change_length(n)
        integer, intent(inout) :: n
        character(len=n) :: automatic
        automatic = 'a'
        n = 8
        automatic = 'xy'
        if (automatic /= 'xy  ') error stop 11
        if (automatic /= 'xy') error stop 16
    end subroutine
end module

program c_reset_character_component_01
    use c_reset_character_component_01_m
    implicit none
    type(item) :: x
    character(len=4) :: local
    character(len=0) :: empty
    integer :: n
    x%value = 'a'
    if (x%value /= 'a   ') error stop 1
    x%value = 'abcdef'
    if (x%value /= 'abcd') error stop 2
    x%value = 'xy'
    if (x%value /= 'xy  ') error stop 3
    if (x%value /= 'xy') error stop 14
    x%value = x%value
    if (x%value /= 'xy  ') error stop 4
    call overwrite(x%value)
    if (x%value /= 'q   ') error stop 5
    local = 'ab'
    local = local
    if (local /= 'ab  ') error stop 6
    if (local /= 'ab') error stop 15
    call overwrite(local)
    if (local /= 'q   ') error stop 7
    local = result_text()
    if (local /= 'wxyz') error stop 8
    if (shared /= 'ab  ') error stop 9
    if (shared /= 'ab') error stop 13
    shared = 'xy'
    if (shared /= 'xy  ') error stop 10
    empty = ''
    if (empty /= '') error stop 12
    n = 4
    call change_length(n)
end program
