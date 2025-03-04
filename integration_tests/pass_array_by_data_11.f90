program pass_array_by_data_11

    implicit none
    real :: y0(5)
    if (AB5(func, y0(:)) /= 1.0) error stop 

contains

    real function AB5(f, y0)
        interface
            real function f(y)
                real, intent(in) :: y(:)
            end function
        end interface
        real, intent(in) :: y0(:)
        AB5 = 1.0
    end function

    real function func(y)
        real, intent(in) :: y(:)
    end function

end program