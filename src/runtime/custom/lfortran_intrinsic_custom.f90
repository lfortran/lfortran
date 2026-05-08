module lfortran_intrinsic_custom
implicit none

interface  newunit
    procedure :: newunit_int_1
    procedure :: newunit_int_2
    procedure :: newunit_int_4
    procedure :: newunit_int_8
end interface 

contains
    function get_valid_newunit() result(unit)
        ! F2023 12.5.6.13: NEWUNIT= must return a negative unit number
        ! that is not -1 and is not currently associated with any unit.
        integer, parameter :: LUN_START=-10, LUN_MIN=-1000
        logical :: opened
        integer(4) :: lun
        integer(4) :: unit
        do lun=LUN_START,LUN_MIN,-1
            inquire(unit=lun,opened=opened)
            if (.not. opened) then
                unit = lun
                return
            end if
        end do
        print *, "All unit numbers are utilized"
        error stop
    end function

    subroutine newunit_int_1(unit)
        implicit none
        integer(1), intent(out) :: unit
        integer(4) :: u
        u = get_valid_newunit()
        if (u < -2**7) then
            print *, "integer(KIND=1) &
            &has small range. Use larger kind for the unit number"
            error stop
        end if
        unit =  INT(u,1)
    end subroutine newunit_int_1

    subroutine newunit_int_2(unit)
        implicit none
        integer(2), intent(out) :: unit
        unit =  INT(get_valid_newunit(),2)
    end subroutine newunit_int_2

    subroutine newunit_int_4(unit)
        implicit none
        integer(4), intent(out) :: unit
        unit =  INT(get_valid_newunit(),4)
    end subroutine newunit_int_4

    subroutine newunit_int_8(unit)
        implicit none
        integer(8), intent(out) :: unit
        unit =  INT(get_valid_newunit(),8)
    end subroutine newunit_int_8

end module
