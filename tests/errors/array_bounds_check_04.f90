program array_bounds_check_04
    integer, allocatable :: x(:,:)
    allocate(x(3, 4))

    call my(x)

    contains

        subroutine my(x)
            integer, intent(in) :: x(3, 1)
        end subroutine
end program
