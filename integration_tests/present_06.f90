! Test: procedure dummy passed through to another routine without being called
subroutine inner(func, x, y)
    implicit none
    double precision :: func
    external :: func
    double precision, intent(in) :: x
    double precision, intent(out) :: y
    y = func(x)
end subroutine inner

subroutine outer(f, a, b)
    implicit none
    double precision :: f
    external :: f
    double precision, intent(in) :: a
    double precision, intent(out) :: b
    call inner(f, a, b)
end subroutine outer

double precision function myfunc(x)
    implicit none
    double precision, intent(in) :: x
    myfunc = x * 2.0d0
end function myfunc

program present_06
    implicit none
    double precision :: result
    double precision, external :: myfunc
    call outer(myfunc, 2.0d0, result)
    if (abs(result - 4.0d0) > 1.0d-10) error stop "Expected 4.0"
    print *, "PASS"
end program present_06
