program doloop_16
    integer :: i
    do i = 0, -16
    end do 
    print *, i
    if ( i /= 0 ) error stop
end program doloop_16
