! Test: Stack vs arena allocation threshold behavior
! Verifies: Arrays below threshold stay on stack, larger use arena
! Note: This test validates correct behavior regardless of threshold;
!       the arrays should work correctly whether on stack or arena.
program arena_03
    implicit none
    call test_small_arrays_fast_path()
    call test_mixed_sizes()
    print *, "PASS: Arena threshold behavior tests passed"
contains

    ! Small arrays - should be on stack with --stack-arrays, arena otherwise
    subroutine test_small_arrays_fast_path()
        call small_outer()
    end subroutine

    subroutine small_outer()
        integer :: small_a(10)
        integer :: small_b(20)
        integer :: i
        ! Initialize small arrays
        do i = 1, 10
            small_a(i) = i * 2
        end do
        do i = 1, 20
            small_b(i) = i * 3
        end do
        call small_inner()
        ! Verify not corrupted
        do i = 1, 10
            if (small_a(i) /= i * 2) error stop "FAIL: small_a corrupted"
        end do
        do i = 1, 20
            if (small_b(i) /= i * 3) error stop "FAIL: small_b corrupted"
        end do
    end subroutine

    subroutine small_inner()
        integer :: small_c(15)
        integer :: j
        do j = 1, 15
            small_c(j) = j * 4
        end do
        ! Verify
        do j = 1, 15
            if (small_c(j) /= j * 4) error stop "FAIL: small_c corrupted"
        end do
    end subroutine

    ! Mix of small and large arrays in same call chain
    subroutine test_mixed_sizes()
        call mixed_outer()
    end subroutine

    subroutine mixed_outer()
        integer :: small_arr(50)
        real :: large_arr(5000)
        integer :: i
        ! Small array
        do i = 1, 50
            small_arr(i) = i
        end do
        ! Large array
        do i = 1, 5000
            large_arr(i) = real(i) * 0.5
        end do
        call mixed_inner()
        ! Verify both
        do i = 1, 50
            if (small_arr(i) /= i) error stop "FAIL: mixed small_arr corrupted"
        end do
        do i = 1, 5000
            if (abs(large_arr(i) - real(i) * 0.5) > 0.001) then
                error stop "FAIL: mixed large_arr corrupted"
            end if
        end do
    end subroutine

    subroutine mixed_inner()
        integer :: inner_small(30)
        real :: inner_large(3000)
        integer :: j
        do j = 1, 30
            inner_small(j) = j * 10
        end do
        do j = 1, 3000
            inner_large(j) = real(j) * 1.5
        end do
        call mixed_innermost()
        do j = 1, 30
            if (inner_small(j) /= j * 10) error stop "FAIL: inner_small corrupted"
        end do
        do j = 1, 3000
            if (abs(inner_large(j) - real(j) * 1.5) > 0.001) then
                error stop "FAIL: inner_large corrupted"
            end if
        end do
    end subroutine

    subroutine mixed_innermost()
        integer :: deep_small(25)
        real :: deep_large(4000)
        integer :: k
        do k = 1, 25
            deep_small(k) = k * 5
        end do
        do k = 1, 4000
            deep_large(k) = real(k) * 2.5
        end do
        ! Verify at deepest level
        do k = 1, 25
            if (deep_small(k) /= k * 5) error stop "FAIL: deep_small corrupted"
        end do
        do k = 1, 4000
            if (abs(deep_large(k) - real(k) * 2.5) > 0.001) then
                error stop "FAIL: deep_large corrupted"
            end if
        end do
    end subroutine

end program
