program associate2
    implicit none
    real :: x(3)
    x = [1.0, 2.0, 3.0]
    call s(x)
contains
    subroutine s(input_data)
        real, intent(in) :: input_data(*)
        associate(a => input_data)
            print *, a(1)
        end associate
    end subroutine s
end program associate2
