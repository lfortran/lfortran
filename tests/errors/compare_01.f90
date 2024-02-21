program compare_01
    character(1) :: x
    integer :: i
    x = 'u'
    i = 10
    if (i > x) then
        print *, "Hello World"
    else
        print *, "New world"
    end if
end program
