program select_type_char_mre
    implicit none

    call print_any("  Hello World  ")
    print *, "test passed"
contains

    subroutine print_any(generic)
        class(*), intent(in) :: generic
        character(len=:), allocatable :: out

        select type (generic)
        type is (character(len=*))
            out = trim(generic)
        class default
            stop 1
        end select

        if (out /= "  Hello World  ") stop 2
    end subroutine print_any

end program select_type_char_mre