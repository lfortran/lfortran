program character_33
    implicit none
    character(len=10, kind=4) :: wide
    wide = "wrong"
    if (wide /= 4_"wrong     ") error stop 1
    print *, "test passed"
end program
