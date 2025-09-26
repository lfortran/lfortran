program transfer_07
    integer(4), parameter :: scalar = 4
    integer(2), parameter :: mold = 0
    integer(2) :: array(2)
    
    array = transfer( scalar, mold, 2 )

    print *, array(1)
    if (array(1) /= 4) error stop ! TODO : Add test for complete array with error stop after fixing transfer completely
end program