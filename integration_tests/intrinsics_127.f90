program intrinsics_127
    real :: x
    x = 4.23

    print *, ifix(x)
    if( .not. ifix(x) == 4 ) error stop

    print *, ifix(4.23)
    if (ifix(123.41) /= 123 ) error stop

end program
