program intrinsics_405
    implicit none

    call print_any("  Hello World  ")
    contains
    subroutine print_any(generic)
        class(*), intent(in) :: generic
        character(len = 15) :: a
        select type (generic)
        type is (character(len=*))
            a = trim(generic)
            print *, trim(generic)
        end select
        if (a /= trim("  Hello World  ")) error stop
    end subroutine print_any

end program intrinsics_405