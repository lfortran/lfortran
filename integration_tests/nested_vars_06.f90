program nested_vars_06
    implicit none
    real :: xt
    xt = 5.0
    call b1(xt)
    if (abs(xt - 10.0) > 1e-5) error stop 1

contains

    subroutine b1(xt)
        real :: xt
        real :: x, h
        real :: g8

        ! Statement function with function call
        g8(x, h) = bvalu(xt)

        xt = g8(1.2, 3.4)
    end subroutine b1

    real function bvalu(xt)
        real :: xt
        bvalu = xt * 2.0
    end function bvalu

end program nested_vars_06
