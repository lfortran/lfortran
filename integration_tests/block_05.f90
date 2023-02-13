subroutine hybrd(n)
    implicit none
    real :: n

    main : block
        if ( n <= 0 ) then
            exit main
        else if ( n >= 2 ) then
            n = n + 1
            exit main
        else 
            n = n + 9
        end if 
        n = n + 2
    end block main
    n = n + 3
    print *, "n = ", n


end subroutine hybrd

program main
    implicit none
    real :: n = 1
    call hybrd(n)
end program main
