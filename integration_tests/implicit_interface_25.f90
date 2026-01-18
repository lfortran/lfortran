! Test passing procedure variables through implicit interface call chains
! This verifies that Function argument type info propagates to implicit
! interface parameters, enabling correct codegen without bitcast workarounds.
program implicit_interface_25
    implicit none
    real(8) :: result, expected

    ! Test 1: Pass a function through an implicit interface call chain
    expected = 25.0d0
    call test_procedure_pass(square_fn, 5.0d0, result)
    if (abs(result - expected) > 1.0d-10) error stop "Test 1 failed"

    ! Test 2: Different function, same interface
    expected = 8.0d0
    call test_procedure_pass(double_fn, 4.0d0, result)
    if (abs(result - expected) > 1.0d-10) error stop "Test 2 failed"

    ! Test 3: Chain through two implicit interface levels
    expected = 49.0d0
    call outer_chain(square_fn, 7.0d0, result)
    if (abs(result - expected) > 1.0d-10) error stop "Test 3 failed"

    print *, "PASSED"

contains

    real(8) function square_fn(x)
        real(8), intent(in) :: x
        square_fn = x * x
    end function

    real(8) function double_fn(x)
        real(8), intent(in) :: x
        double_fn = x * 2.0d0
    end function

end program

! Subroutine with implicit interface for the procedure argument
subroutine test_procedure_pass(f, x, result)
    implicit none
    real(8), intent(in) :: x
    real(8), intent(out) :: result
    real(8), external :: f
    result = f(x)
end subroutine

! Outer chain: passes procedure through another implicit interface layer
subroutine outer_chain(f, x, result)
    implicit none
    real(8), intent(in) :: x
    real(8), intent(out) :: result
    real(8), external :: f

    ! Call inner routine which also has implicit interface for f
    call inner_apply(f, x, result)
end subroutine

! Inner layer of the chain
subroutine inner_apply(func, val, out)
    implicit none
    real(8), intent(in) :: val
    real(8), intent(out) :: out
    real(8), external :: func
    out = func(val)
end subroutine
