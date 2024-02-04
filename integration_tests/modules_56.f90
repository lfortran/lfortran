module modules_56_module

contains
logical function is_close_rsp(a) result(res)
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan

    real, intent(in) :: a

    res = ieee_is_nan(a)
end function is_close_rsp
end module

program main
    use modules_56_module
    implicit none
    real :: a = 0.0

    print *, is_close_rsp(a)
    if (is_close_rsp(a)) error stop
end program main
