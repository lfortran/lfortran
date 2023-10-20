program bin_op_real_01
    implicit none
    real :: R(4), V(4)
    R = 23
    V = 9
    print *, func(R, V)
    if (func(R, V) /= 39304) error stop
    if (((sum(R) - 9.16E-04) > -1e-8)) error stop
contains
    integer function func(R, V) result(i)
        real, intent(inout) :: R(:), V(:)
        i = (34**3)
        R = -(R-V)/i
    end function
end program bin_op_real_01
