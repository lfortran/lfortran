program main
    implicit none
    integer :: info_original
    contains

    subroutine compare_solutions(x)

        implicit none
        integer :: y
        real, intent(in) :: x(:)

        y = info_original
    end subroutine

end program main
