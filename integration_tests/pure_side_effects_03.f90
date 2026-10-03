pure subroutine test_sp(a, b)
    implicit none
    complex, intent(in) :: a
    real, intent(out) :: b
    complex :: t
    real :: abssq
    abssq(t) = real(t)**2 + aimag(t)**2
    b = abssq(a)
end subroutine test_sp

pure subroutine test_dp(a, b)
    implicit none
    complex(8), intent(in) :: a
    real(8), intent(out) :: b
    complex(8) :: t
    real(8) :: abssq
    abssq(t) = real(t, kind=8)**2 + aimag(t)**2
    b = abssq(a)
end subroutine test_dp

program pure_side_effects_03
    implicit none
    complex :: a_sp
    real :: b_sp
    complex(8) :: a_dp
    real(8) :: b_dp

    a_sp = (3.0, 4.0)
    call test_sp(a_sp, b_sp)
    if (abs(b_sp - 25.0) > 1e-6) error stop

    a_dp = (3.0d0, 4.0d0)
    call test_dp(a_dp, b_dp)
    if (abs(b_dp - 25.0d0) > 1d-12) error stop

    print *, b_sp
    print *, b_dp
end program pure_side_effects_03
