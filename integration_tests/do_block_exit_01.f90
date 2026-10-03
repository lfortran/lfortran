program p
    implicit none
    integer :: i

    outer: do i = 1, 5
        block
            if (i == 2) exit outer
        end block
    end do outer
    
    print *, i
    if (i /= 2) error stop
end program
