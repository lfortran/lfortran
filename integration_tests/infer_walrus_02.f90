program infer_walrus_02
    implicit none
    x := 10
    y := 2.5d0
    print *, x + int(y)
    if (x + int(y) /= 12) error stop
    print *, "PASSED"
end program
