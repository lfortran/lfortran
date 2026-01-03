program doloop_14
    integer :: i
    do i = 1, 2, 16
    end do
    print *, i
    if ( i /= 17 ) error stop
end program doloop_14
