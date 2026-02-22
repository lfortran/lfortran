! Test selected_real_kind chain with constant nkr parameter (issue #4726)
! This is the constant parameter case that triggered the ICE
program intrinsics_418
    implicit none
    integer, parameter :: kr1 = selected_real_kind(1), maxkr = 3
    integer, parameter :: skr2 = selected_real_kind(precision(1.0_kr1) + 1)
    integer, parameter :: kr2 = merge(skr2, kr1, skr2 > 0)
    integer, parameter :: skr3 = selected_real_kind(precision(1.0_kr2) + 1)
    integer, parameter :: kr3 = merge(skr3, kr2, skr3 > 0)
    integer, parameter :: allkr(maxkr + 1) = [kr1, kr2, kr3, kr3]
    integer, parameter :: nkr = minloc(abs(allkr(1:maxkr) - allkr(2:maxkr + 1)), 1)

    call realkinds(nkr)

contains
    subroutine realkinds(nkr)
        integer, intent(in) :: nkr
        integer :: goodkr(nkr)

        goodkr = allkr(1:nkr)
        if (nkr < 2) error stop
    end subroutine realkinds
end program intrinsics_418
