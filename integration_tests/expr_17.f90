program expr_17
    implicit none
    type t
        real, dimension(:), pointer :: x
    end type t

    type(t) :: type
    real, target :: r(2)
    real :: ans(2)
    r = 34
    type%x => r

    ans = type%x**2

    print *, ans
    if (abs(ans(1) - 1156.0) > 1e-5) error stop
    if (abs(ans(2) - 1156.0) > 1e-5) error stop
end program
