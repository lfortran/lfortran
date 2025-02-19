module function_32_mod_array_section_04
contains
subroutine istril(y)
    real(8), intent(inout) :: y(:, :)
    print *, y(2, 2)
    if ( abs(y(2,2) - 3.0) > 1e-8 ) error stop
    y(2, 2) = 4.0

end subroutine
subroutine matprod(y)
    real(8), intent(inout) :: y(:, :)
    call istril(y)
end subroutine 
end module


program array_section_04
    use function_32_mod_array_section_04
    real(8) :: A(5, 3)
    A = 1.0_8
    A(1,2) = 0
    A(2,2) = 3.0
    call matprod(A(1:2,1:2))
    print *, A(2, 2)
    if ( abs(A(2,2) - 4.0) > 1e-8 ) error stop
end program

