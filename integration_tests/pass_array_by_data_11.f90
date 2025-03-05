program pass_array_by_data_11

    implicit none
    real :: y0(5)
    y0 = AB5(func, y0(:))
    if (any(y0 /= [1.0,1.0,1.0,1.0,0.0])) error stop 

contains

    function AB5(f, y0)
        interface
            real function f(y)
                real, intent(in) :: y(:)
            end function
        end interface
        real, intent(in) :: y0(:)
        real, allocatable :: AB5(:)
        allocate(AB5(size(y0)))
        AB5(1:size(y0)-1) = func(y0(1:3))
        AB5(size(y0)) = 0.0 
    end function

    real function func(y)
        real, intent(in) :: y(:)
        func = 1.0
    end function

end program