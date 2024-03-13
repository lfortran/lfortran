program expr2
    implicit none
    real :: x
    x = 1.2
    print *, helloWorld(5.1, 2)
    !call my_func(5)

contains
    real function helloWorld(x)
        real, intent(in) :: x
        helloWorld = x + 1
    end function helloWorld

end program expr2
