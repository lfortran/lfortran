! Test that call argument alloca reuse works correctly
! This test verifies that expression arguments to implicit interface calls
! are handled correctly when allocas are reused across multiple calls
program implicit_interface_24
    implicit none
    integer :: result, x, y

    x = 10
    y = 20
    result = 0

    ! Test 1: Multiple calls with expression arguments of same type
    ! The allocas for x+1, x+2, x+3 should be reused
    call ext_sub1(x + 1, result)
    if (result /= 11) error stop "Test 1a failed"
    call ext_sub1(x + 2, result)
    if (result /= 12) error stop "Test 1b failed"
    call ext_sub1(x + 3, result)
    if (result /= 13) error stop "Test 1c failed"

    ! Test 2: Call with multiple expression arguments
    ! Should use separate allocas within the same call, but reuse across calls
    call ext_sub2(x + 1, y + 1, result)
    if (result /= 32) error stop "Test 2a failed"
    call ext_sub2(x + 2, y + 2, result)
    if (result /= 34) error stop "Test 2b failed"

    ! Test 3: Mix of variable and expression arguments
    call ext_sub2(x, y + 5, result)
    if (result /= 35) error stop "Test 3a failed"
    call ext_sub2(x + 5, y, result)
    if (result /= 35) error stop "Test 3b failed"

    print *, "All tests passed"
end program

subroutine ext_sub1(a, r)
    integer, intent(in) :: a
    integer, intent(out) :: r
    r = a
end subroutine

subroutine ext_sub2(a, b, r)
    integer, intent(in) :: a, b
    integer, intent(out) :: r
    r = a + b
end subroutine
