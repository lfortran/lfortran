program expr_19
    complex(8), parameter :: x = (5, 6)
    print *, x

    if (abs(x - (5, 6)) > 1e-5) error stop
end program
