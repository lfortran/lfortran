program select_type_17
    implicit none

    call print_any("  Hello World  ")
    contains
    subroutine print_any(generic)
        class(*), intent(in) :: generic
        character(len = 15) :: a
        select type (generic)
        type is (character(len=*))
            print *, generic
        end select
    end subroutine print_any

end program select_type_17
