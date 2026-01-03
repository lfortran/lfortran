! Test arena allocation for large fixed-size arrays
! This test verifies that multiple large arrays (12MB total) can be allocated
! without stack overflow, using arena allocation for FixedSizeArray locals.
program arrays_101
    implicit none
    call test_large_fixed_arrays()
    print *, "OK"
contains
    subroutine test_large_fixed_arrays()
        ! Multiple large arrays that would overflow typical 8MB stack
        real :: arr1(1000000)  ! 4MB
        real :: arr2(1000000)  ! 4MB
        real :: arr3(1000000)  ! 4MB
        integer :: i

        ! Initialize and use all arrays to ensure they are allocated
        do i = 1, 1000000
            arr1(i) = real(i)
            arr2(i) = real(i) * 2.0
            arr3(i) = real(i) * 3.0
        end do

        ! Verify values at midpoint
        if (abs(arr1(500000) - 500000.0) > 0.1) error stop
        if (abs(arr2(500000) - 1000000.0) > 0.1) error stop
        if (abs(arr3(500000) - 1500000.0) > 0.1) error stop
    end subroutine
end program
