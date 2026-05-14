subroutine test()
    implicit none
    real :: tmp, j
    if ( .false. ) then
        tmp = real(j)
    else
        tmp = 0
    end if
end subroutine test

program if_07
    implicit none
    call test()
end program if_07
