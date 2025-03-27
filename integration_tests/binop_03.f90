program test
    integer(8) :: x
    x = 665663010
    x = x**2
    print *, x
    if(x /= 443107242882260100_8) error stop
end program