program array_bounds_check_02
    integer :: x
    x = 4

    call temp(x)
contains
    subroutine temp(x)
        integer, intent(in) :: x
        integer :: y(x)

        y = [1, 2, 3]
    end subroutine
end program
