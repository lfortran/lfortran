program infer_walrus_03
    implicit none
    x := 5
    y := 10
    x = x + y
    if (x /= 15) error stop
    y = x * 2
    if (y /= 30) error stop
    print *, "PASSED"
end program
