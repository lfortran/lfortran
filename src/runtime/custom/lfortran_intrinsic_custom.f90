module lfortran_intrinsic_custom
implicit none

contains

    subroutine newunit(unit)
        implicit none
        integer, parameter :: LUN_MIN=0, LUN_MAX=1000
        logical :: opened
        integer :: lun
        integer, intent(out) :: unit

        do lun=LUN_MIN,LUN_MAX
            inquire(unit=lun,opened=opened)
            if (.not. opened) then
                unit = lun
                return
            end if
        end do
        print *, "All unit numbers are utilized"
        error stop
    end subroutine newunit

end module
