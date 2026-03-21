program kinds_03
    call realkinds()
    contains
    subroutine realkinds()
        integer, parameter :: rkarray(6) = [1, 2, 4, 8, 16, 16]
        logical :: mask(5)
        mask = rkarray(1:5) == rkarray(2:6)
        if (count(mask) /= 1) error stop
        if (.not. mask(5)) error stop
        print *, mask
    end subroutine
end program kinds_03
