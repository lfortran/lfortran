subroutine foo(x)
    implicit none
    real, intent(inout) :: x(*)
    x(1) = x(1) + 1.0
end subroutine foo

