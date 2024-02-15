module string_module
    implicit none
        type :: string_type
        sequence
        private
        character(len=:), allocatable :: raw
        end type string_type
    contains
        elemental subroutine assign_char_to_string(lhs, rhs)
            type(string_type), intent(inout) :: lhs
            character(len=*), intent(in) :: rhs
            lhs%raw = rhs
        end subroutine assign_char_to_string

        elemental function len_string(string3) result(length)
            type(string_type), intent(in) :: string3
            integer :: length
            if (allocated(string3%raw)) then
                length = len(string3%raw)
            else
                length = 0
            end if
        end function len_string

        pure function maybe(string2) result(maybe_string)
        type(string_type), intent(in) :: string2
        character(len=len_string(string2)) :: maybe_string         
        ! error in above line
        if (allocated(string2%raw)) then
            maybe_string = string2%raw
        else
            maybe_string = ''
        end if
        end function maybe

        elemental function adjustl_string(string1) result(adjusted_string)
            type(string_type), intent(in) :: string1
            type(string_type) :: adjusted_string
            adjusted_string%raw = adjustl(maybe(string1))
        end function adjustl_string

        


    end module
    
    program main_program
        use string_module
        implicit none
        
        type(string_type) :: my_string
        
        ! Assigning a string to the user-defined type using assign_char_to_string subroutine
        call assign_char_to_string(my_string, "      Hello, Fortran!")
        
        ! Accessing the string content using the adjustl_string function
        print *, "Length of the string:", len_string(my_string)
        print *, "String content:", maybe(my_string)
        print *, "String content:", adjustl_string(my_string)
        
    end program main_program
    
