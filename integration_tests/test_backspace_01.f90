program test_backspace_01
    integer :: u=10, i=0, iostat
    open(file='tests.txt', unit=u)
    do
        read(u, *, end=20) i
    end do
20  backspace(u, iostat=iostat)

    if (iostat == 0) then
        write(u, *), i+1
    end if
end program test_backspace_01
