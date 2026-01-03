subroutine qrfac()
    implicit none

    real:: a(5, 5)

    a = 12.9
    print *, enorm(a(1, 1))
    if ( abs(enorm(a(1,1)) - 832.049927 ) > 1e-8) error stop

    contains
        pure real function enorm(x)

        implicit none
        real, intent(in) :: x(5)
        integer :: i

        enorm = 0.0
        do i = 1, 5
            enorm = enorm + x(i)**2
        end do

        end function enorm
end subroutine qrfac

program legacy_array_sections_05
    implicit none
    call qrfac()
end program
