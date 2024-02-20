program lbound_01
    integer(8) :: x(2)
    integer(8) :: res(1)
    res = lbound(x, kind = 8)
    print *, res
    if (res(1) /= 1) error stop 
end program
