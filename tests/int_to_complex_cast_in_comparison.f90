program expr2
    implicit none
    integer, parameter :: dp = kind(0.d0)

    contains
    elemental complex(dp) function dabs(x) result(r)
        complex(dp), intent(in) :: x
        if (x /= 0) then
            r = x
        else
            r = 0 - x
        end if
    end function dabs
end program
