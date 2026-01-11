program legacy_array_sections_09
    implicit none

    integer :: n, lwa
    real, allocatable :: wa(:)

    n = 3
    lwa = 10
    allocate(wa(lwa))
    wa = 0.0

    call caller(n, lwa, wa)

contains

    subroutine callee(n, diag)
        implicit none

        integer, intent(in) :: n
        real, intent(inout) :: diag(n)

        diag = diag
    end subroutine callee

    subroutine caller(n, lwa, wa)
        implicit none

        integer, intent(in) :: n
        integer, intent(in) :: lwa
        real, intent(inout) :: wa(lwa)

        call callee(n, wa(1))
    end subroutine caller

end program legacy_array_sections_09
