program binop_03
    integer(8) :: x
    real(8) :: y
    x = 665663010
    x = x**2
    print *, x
    if(x /= 443107242882260100_8) error stop
    
    y = 665663010
    y = y**2
    print *, y
    if(abs(y - 443107242882260100_8) > 10e-20) error stop
end program