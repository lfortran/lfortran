module separate_compilation_29a_module
    implicit none

    type :: t
        integer :: x
    end type t

    type(t) :: targets

contains

    subroutine sa()
        targets%x = 1
    end subroutine sa

    integer function get_a()
        get_a = targets%x
    end function get_a

end module separate_compilation_29a_module
