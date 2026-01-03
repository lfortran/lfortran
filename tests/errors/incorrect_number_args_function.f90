program expr2
    implicit none
    print *, helloWorld(5.1, 2)

contains
    real function helloWorld(x)
        real, intent(in) :: x
        helloWorld = x + 1
    end function helloWorld

end program expr2
