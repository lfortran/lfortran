program expr2
implicit none
integer(4), parameter :: dp = Kind(0.0000000000000000e+00_8)

contains

elemental real(8) function dabs(x) result(r)
    real(8), intent(in) :: x
    if (x > real(0, kind=8)) then
        r = x
    else
        r = real(0, kind=8) - x
    end if
end function dabs

end program expr2
