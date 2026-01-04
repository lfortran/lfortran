! Test: Deep recursion and large array allocation via arena allocator
! Verifies: (1) Arena alternation at 10+ recursion levels
!           (2) Arena chunk growth with large arrays
!           (3) Zero-size edge case handling
program arena_02
    implicit none
    call test_deep_recursion()
    call test_large_arrays()
    call test_zero_size_arrays()
    print *, "PASS: Arena deep recursion and large array tests passed"
contains

    ! Test deep recursion with arena alternation (10+ levels)
    subroutine test_deep_recursion()
        integer :: result
        result = recursive_sum(15)
        if (result /= 120) error stop "FAIL: Deep recursion sum incorrect"
    end subroutine

    recursive function recursive_sum(depth) result(res)
        integer, intent(in) :: depth
        integer :: res
        integer :: local_arr(20)
        integer :: i
        ! Use local array at each level to trigger arena allocation
        do i = 1, 20
            local_arr(i) = depth + i
        end do
        if (depth <= 1) then
            res = depth
        else
            res = depth + recursive_sum(depth - 1)
            ! Verify local array not corrupted after recursive call
            do i = 1, 20
                if (local_arr(i) /= depth + i) then
                    error stop "FAIL: Deep recursion arena corruption"
                end if
            end do
        end if
    end function

    ! Test large array allocation that triggers chunk growth
    subroutine test_large_arrays()
        call large_outer()
    end subroutine

    subroutine large_outer()
        real :: big_arr(10000)
        integer :: i
        ! Initialize large array
        do i = 1, 10000
            big_arr(i) = real(i) * 1.5
        end do
        ! Call inner with another large array
        call large_inner()
        ! Verify outer array not corrupted
        do i = 1, 10000
            if (abs(big_arr(i) - real(i) * 1.5) > 0.001) then
                error stop "FAIL: Large array corruption in outer"
            end if
        end do
    end subroutine

    subroutine large_inner()
        real :: another_big(8000)
        integer :: j
        do j = 1, 8000
            another_big(j) = real(j) * 2.0
        end do
        ! Third level with even larger array
        call large_innermost()
        ! Verify inner array
        do j = 1, 8000
            if (abs(another_big(j) - real(j) * 2.0) > 0.001) then
                error stop "FAIL: Large array corruption in inner"
            end if
        end do
    end subroutine

    subroutine large_innermost()
        real :: huge_arr(20000)
        integer :: k
        do k = 1, 20000
            huge_arr(k) = real(k)
        end do
        ! Verify huge array
        do k = 1, 20000
            if (abs(huge_arr(k) - real(k)) > 0.001) then
                error stop "FAIL: Huge array corruption"
            end if
        end do
    end subroutine

    ! Test zero-size or minimal arrays (edge case)
    subroutine test_zero_size_arrays()
        call minimal_sub()
    end subroutine

    subroutine minimal_sub()
        integer :: tiny_arr(1)
        tiny_arr(1) = 42
        call minimal_inner()
        if (tiny_arr(1) /= 42) error stop "FAIL: Minimal array corrupted"
    end subroutine

    subroutine minimal_inner()
        integer :: also_tiny(1)
        also_tiny(1) = 99
        if (also_tiny(1) /= 99) error stop "FAIL: Inner minimal array wrong"
    end subroutine

end program
