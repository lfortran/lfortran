module separate_compilation_29b_module
    implicit none

    type :: t
        integer :: x
    end type t

    type(t) :: targets

contains

    subroutine sb()
        targets%x = 2
    end subroutine sb

    integer function get_b()
        get_b = targets%x
    end function get_b

end module separate_compilation_29b_module
