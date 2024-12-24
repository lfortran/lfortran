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
        integer, parameter :: LUN_MIN=0, LUN_MAX=1000
        logical :: opened
        integer(4) :: lun 
        integer(4) :: unit ! no need for kind 8, we have limit of 1000 for now.
        do lun=LUN_MIN,LUN_MAX
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
        if(get_valid_newunit() >= 2**8) then
            print *, "integer(KIND=1) & 
            &is has small limit. Use larger kind for the unit number"
            error stop
        end if
        unit =  INT(get_valid_newunit(),1)
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
