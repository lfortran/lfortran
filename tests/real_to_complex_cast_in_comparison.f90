program expr2
    implicit none
    integer, parameter :: dp = kind(0.d0)
    real(dp) :: zero
    zero = 0.0_dp
 
    contains
    elemental complex(dp) function dabs(x) result(r)
        complex(dp), intent(in) :: x
        if (x /= zero) then
            r = x
        else
            r = 0 - x
        end if
    end function dabs
end program
