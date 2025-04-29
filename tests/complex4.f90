program complex4
    implicit none
    complex(4) :: x_4
    complex(8) :: x_8
    x_4 = (0.5, 0.5)
    x_8 = (0.5, 0.5)
    print *, sin(x_4)
    print *, sin(x_8)
end program complex4
