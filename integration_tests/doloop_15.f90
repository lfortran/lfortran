program doloop_15
    integer :: i
    do i = 2, 2
    end do 
    print *, i
    if ( i /= 3 ) error stop
end program doloop_15
