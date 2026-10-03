program equivalence_48
    implicit none
    call cauplt()
contains
    subroutine cauplt()
        real :: y(7500), w(7500), ws(15000)
        equivalence (y(1), ws(1))
        equivalence (w(1), ws(7501))
        integer :: i
        do i = 1, 15000
            ws(i) = real(i)
        end do
        print *, size([ws, y, w])
        print *, y(1), y(7500), w(1), w(7500)
        if (size([ws, y, w]) /= 30000) error stop
        if (abs(y(1) - 1.0) > 1e-6) error stop
        if (abs(y(7500) - 7500.0) > 1e-6) error stop
        if (abs(w(1) - 7501.0) > 1e-6) error stop
        if (abs(w(7500) - 15000.0) > 1e-6) error stop

        y(10) = -1.0
        w(20) = -2.0
        print *, ws(10), ws(7520)
        if (abs(ws(10) + 1.0) > 1e-6) error stop
        if (abs(ws(7520) + 2.0) > 1e-6) error stop
    end subroutine cauplt
end program equivalence_48
