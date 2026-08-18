program if3
    integer :: i
    i = 1
    if (i > 0) then
        i = 2
        go to 86
        i = 3
86  end if
end program
