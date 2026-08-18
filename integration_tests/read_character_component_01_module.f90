module read_character_component_m
    implicit none
    type :: record
        character(len=2) :: name
    end type
contains
    subroutine parse(line, value)
        character(len=*), intent(in) :: line
        type(record), intent(out) :: value
        read(line, *) value%name
    end subroutine
end module
