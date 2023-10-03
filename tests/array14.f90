program array14
    implicit none

    real :: x(5)
    real :: z(3)
    x = [5, -1, 3, 4, 2]
    print *, x

    z = x(1:3)
    print *, z

end program
