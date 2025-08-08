program array_bounds_check_05
    integer, allocatable :: x(:)
    allocate(x(3))

    x = [1, 2, 3]

    call my(x, 5)
    contains

        subroutine my(x, i)
            integer, intent(in) :: x(i)
            integer, intent(in) :: i
        end subroutine
end program
