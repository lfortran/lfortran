module interface5_module
    implicit none

    type string_t
        character(len=:), allocatable :: s
    end type

    interface operator(.in.)
        module procedure string_array_contains
    end interface

contains
    logical function string_array_contains(search_string, array)
        character(*), intent(in) :: search_string
        type(string_t), intent(in) :: array(:)
    end function string_array_contains

end module interface5_module

program interface5
    use interface5_module, only: operator(.in.)
    implicit none
end program interface5

