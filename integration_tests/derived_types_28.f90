program derived_types_28
    implicit none
    type t
        real, dimension(:), pointer :: x, y, z
    end type t

    type(t) :: type
    real, target :: r(1)
    real :: ans
    r = 34
    type%x => r
    type%y => r
    type%z => r

    type%x = type%y + type%z

    ans = type%x(1)
    print *, ans

    if (abs(ans - 68.0) > 1e-5) error stop
end program
