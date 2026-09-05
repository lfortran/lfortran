program derived_types_156
    implicit none
    type settings
        integer :: w
    end type settings
    type(settings) :: se
    se%w = 10
    call derived_types_156_sub(se)

contains

    subroutine derived_types_156_sub(se)
        type(settings), intent(in) :: se
        character(se%w) :: stmin(10)
    end subroutine derived_types_156_sub

end program derived_types_156