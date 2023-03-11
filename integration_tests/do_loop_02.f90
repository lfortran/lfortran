program main
    implicit none
    integer :: i, j
    i = 0
    j = 0
    outer : do
        i = i + 1
        inner : do
            j = j + 1
            if (j == 5) then
                print *, j
                exit inner
            end if
        end do inner
        print *, "out of inner loop"
        if ( i == 2 ) then
            exit outer
        end if
    end do outer
    if (i /= 2) error stop
    if (j /= 5) error stop
    print *, "out of outer loop"
end program
