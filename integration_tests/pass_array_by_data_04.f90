module pass_array_by_data_04_find_fit_module

contains

    subroutine find_fit(data_x)
        real, intent(in) :: data_x(:)
        real :: y
        y = 0
        call fcn(data_x, y)
        print *, y
        if (abs(y - 6.0) > 1e-6) error stop

    contains

        subroutine fcn(data_x, y)
            real, intent(in) :: data_x(:)
            real, intent(out) :: y
            integer :: i
            do i = 1, size(data_x)
                y = y + data_x(i)
            end do
        end subroutine fcn

end subroutine find_fit

end module

program main
use pass_array_by_data_04_find_fit_module, only: find_fit
implicit none

    real :: data_x(3)
    integer :: i

    do i = 1, 3
        data_x(i) = i
    end do

    call find_fit(data_x)

end program main
