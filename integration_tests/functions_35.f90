module function_33_mod
contains
subroutine istril(y)
    real(8), intent(inout) :: y(:, :)
    if (y(2,2) /= 0.0) error stop
    if (y(1,2) /= 2.0) error stop
end subroutine
subroutine matprod(y)
    real(8), intent(inout) :: y(:, :)
    y(1,2) = 2
    call istril(y)
end subroutine 
end module

program function_33
    use function_33_mod
    real(8) :: A(5,3)
    A = 1.0_8
    A(2,2) = 0
    call matprod(A(1:2,1:2))
end program 