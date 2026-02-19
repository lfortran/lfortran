program len_01
    implicit none
    call print_any("Hello World")
contains
    subroutine print_any(generic)
        class(*), intent(in) :: generic
        print *, len(generic)
    end subroutine print_any
end program len_01
