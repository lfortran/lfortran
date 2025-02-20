subroutine save_sub()
    real :: x = 5
end subroutine

program save4
    if (x .eq. 1) then
        print *, "x is 1"
    end if
end program

real function save_fun()
    implicit none
    real :: x = 5
end function
