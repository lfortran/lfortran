module c_reset_character_component_01_m
    implicit none
    type :: item
        character(len=4) :: value = 'ab'
    end type
contains
    subroutine unused(x)
        class(item), intent(inout) :: x
    end subroutine
end module

program c_reset_character_component_01
    use c_reset_character_component_01_m
    implicit none
    type(item) :: x
    x%value = 'a'
    if (x%value /= 'a   ') error stop 1
    x%value = 'abcdef'
    if (x%value /= 'abcd') error stop 2
    x%value = 'xy'
    if (x%value /= 'xy  ') error stop 3
end program
