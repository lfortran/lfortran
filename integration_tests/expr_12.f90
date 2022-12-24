program expr_12
implicit none

    if ('1'//char(38)//char(35)//'1' /= '1&#1') error stop
    if ('1'//char(38)//char(35)//'1'//char(0) /= '1&#1'//char(0)) error stop

end program expr_12
