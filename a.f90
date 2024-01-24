program main
    complex(4) :: x, y(4)
    x = (1.0, 2.0)
    y = (3.0, 4.0)
    ! print*, abs([(1, 2), (3 ,4)])
    print*, abs(y)
    ! print*, sin([(1, 2), (3 ,4)])
end