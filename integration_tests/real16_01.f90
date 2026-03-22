program real16_01
    implicit none
    real(16) :: x

    x = 1.0_16 + 2.0_16

    if (x /= 3.0_16) then
        error stop "real(kind=16) computation failed"
    end if
end program real16_01