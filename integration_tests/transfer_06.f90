program transfer_06
    integer :: i
    character(1) :: res
    i = 321
    res = transfer(i, res)
    print *, res
    if(res /= 'A') error stop
    ! -------------------------------------------- !
    i = 65
    res = transfer(i, res)
    print *, res
    if(res /= 'A') error stop
end program