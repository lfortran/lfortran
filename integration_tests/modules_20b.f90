module modules_20b
    implicit none
    integer, public, parameter :: tfc = selected_char_kind('DEFAULT')
    type t
        character(len=1, kind=tfc) :: newline = achar(10, kind=tfc)
    end type t
contains

    subroutine trim2(x)
        character(len=*),intent(in) :: x
        integer :: len_trim
        len_trim = 1
        print *, len_trim, trim(x)
    end subroutine

end module
