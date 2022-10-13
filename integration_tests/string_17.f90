module string_module
implicit none
    type :: string_type
        sequence
        private
        character(len=:), allocatable :: raw
    end type string_type

    interface len
        module procedure :: len_string
    end interface

contains

    elemental function len_string(string) result(length)
            type(string_type), intent(in) :: string
            integer :: length

            if (allocated(string%raw)) then
                length = len(string%raw)
            else
                length = 0
            end if

    end function len_string

    pure function maybe(string) result(maybe_string)
            type(string_type), intent(in) :: string
            character(len=len(string)) :: maybe_string
            if (allocated(string%raw)) then
                maybe_string = string%raw
            else
                maybe_string = ''
            end if
    end function maybe

end module