program save4
    implicit none
    integer :: y = 1
    if (y .eq. 1) then
        print *, "y is 1"
    end if
    contains
    subroutine f()
        integer :: x = 5
        if (x .eq. 1) then
            print *, "x is 1"
        end if
    end subroutine
end program
