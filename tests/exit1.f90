program exit1
integer :: i, N
N = 10
do i = 1, N
    print *, i
    if ( i .eq. 5 ) then
        call exit(1)
    end if
end do
end program
