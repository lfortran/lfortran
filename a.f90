module string_module
    contains
        pure function len_string(string3) result(length)
            character(len=:), allocatable, intent(in) :: string3
            integer :: length
        end function len_string

        function maybe(string2) result(maybe_string)
        character(len=:), allocatable, intent(in) :: string2
        ! character(len=:), allocatable :: maybe_string
        character(len=len_string(string2)) :: maybe_string
        end function maybe

        function adjustl_string(string1) result(adjusted_string)
            character(len=:), allocatable, intent(in) :: string1
            character(len=:) , allocatable:: adjusted_string
            print*, adjustl(maybe(string1))
        end function adjustl_string
end module
program module_23
    use string_module
    implicit none
end program
