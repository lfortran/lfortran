! Test: Nested arena allocation with two-arena conflict avoidance
! Verifies that nested/recursive function calls correctly alternate arenas
! to prevent data corruption between caller and callee.
program arena_01
    implicit none
    call test_nested()
    call test_recursive()
    print *, "PASS: Arena allocator tests passed"
contains

    ! Test nested subroutine calls with local arrays
    subroutine test_nested()
        integer :: outer_arr(100)
        integer :: i
        ! Initialize outer array
        do i = 1, 100
            outer_arr(i) = i
        end do
        ! Call inner subroutine (uses different arena)
        call inner_sub()
        ! Verify outer array was not corrupted
        do i = 1, 100
            if (outer_arr(i) /= i) error stop "FAIL: Nested arena corruption"
        end do
    end subroutine

    subroutine inner_sub()
        integer :: inner_arr(200)
        integer :: j
        ! Inner uses different values
        do j = 1, 200
            inner_arr(j) = j * 10
        end do
        ! Third level call (alternates back to first arena)
        call innermost_sub()
    end subroutine

    subroutine innermost_sub()
        integer :: deep_arr(50)
        integer :: k
        do k = 1, 50
            deep_arr(k) = k * 100
        end do
    end subroutine

    ! Test recursive function with local arrays
    subroutine test_recursive()
        integer :: result
        result = factorial(5)
        if (result /= 120) error stop "FAIL: Recursive factorial wrong"
    end subroutine

    recursive function factorial(n) result(res)
        integer, intent(in) :: n
        integer :: res
        integer :: scratch(10)
        integer :: i
        ! Use local array to trigger arena allocation
        do i = 1, 10
            scratch(i) = n * i
        end do
        if (n <= 1) then
            res = 1
        else
            res = n * factorial(n - 1)
            ! Verify scratch was not corrupted after recursive call
            do i = 1, 10
                if (scratch(i) /= n * i) error stop "FAIL: Recursive arena corruption"
            end do
        end if
    end function

end program
