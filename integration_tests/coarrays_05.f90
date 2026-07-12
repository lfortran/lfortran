program coarrays_05
    implicit none
    ! run with 2 images
    integer :: x[*]
    integer :: i = 1
    integer :: y(2) = [20, 42]
    if (this_image() == 1) then
        x[2] = 42
    end if
    if (this_image() == 2) then
        x[1] = 20
    end if
    sync all
    do i = 1, 2
        if (this_image() == i .and. x /= y(i)) then 
            error stop
        end if
    end do
    

end program coarrays_05