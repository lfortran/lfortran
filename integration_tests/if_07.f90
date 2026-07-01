module if_07_module
implicit none

contains

subroutine with_side_stepped_code()
    integer(4), parameter :: iflag = 0
    integer(4) :: j
    real(8) :: tmp

    if (iflag == 1) then
        do j = 1, 10
            tmp = real(j) / real(10)
        end do
    else
        tmp = 0.d0
    end if

    if (tmp /= 0.d0) error stop
end subroutine

end module

program if_07
use if_07_module, only: with_side_stepped_code
implicit none

call with_side_stepped_code()
end program
