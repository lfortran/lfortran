subroutine test_complex_param
    ! Test COMPLEX PARAMETER with negative real literal
    ! A negative real assigned to COMPLEX must convert to (real, 0.0)
    implicit none
    complex :: pos_one, neg_one, zero
    parameter (pos_one = 1.0, neg_one = -1.0, zero = 0.0)

    if (real(pos_one) /= 1.0) error stop
    if (aimag(pos_one) /= 0.0) error stop

    if (real(neg_one) /= -1.0) error stop
    if (aimag(neg_one) /= 0.0) error stop

    if (real(zero) /= 0.0) error stop
    if (aimag(zero) /= 0.0) error stop

    print *, "PASS: complex parameter with negative real literal"
end subroutine

program parameter_16
    call test_complex_param
end program
