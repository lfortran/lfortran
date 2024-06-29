program intrinsics2
    real(8) :: x, y, datan2
    x = 2.33D0
    y = 3.41D0
    print *, datan2(x,y)
    if(abs(datan2(x,y) - 0.59941916594660438) > 1d-6) error stop
    
end program