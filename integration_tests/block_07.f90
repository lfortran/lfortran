program main
    implicit none
    integer :: i, j
    i = 0
    j = 0
    test: block
        outer : do
            i = i + 1
            inner : do
                j = j + 1
                if ( j == 3 ) then
                    exit test
                end if
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
        print *, "out of outer loop"
    end block test
    if (i /= 1) error stop
    if (j /= 3) error stop
    print *, "out of test block"
end program
