! Test ILP64 array section with assumed-size arrays
! This tests that array section operations work correctly when
! -fdefault-integer-8 is used (ILP64 mode)

program array_section_ilp64
    implicit none
    integer, parameter :: n = 4
    real :: a(n, n)
    integer :: i, j

    do j = 1, n
        do i = 1, n
            a(i, j) = real(i + j * 10)
        end do
    end do

    call test_section(a, n, 2, 3)
contains
    subroutine test_section(a, lda, ilo, ihi)
        integer, intent(in) :: lda, ilo, ihi
        real, intent(inout) :: a(lda, *)
        real :: sum_val
        integer :: i, j

        sum_val = 0.0
        do j = ilo, ihi
            do i = ilo, ihi
                sum_val = sum_val + a(i, j)
            end do
        end do

        ! Expected: a(2,2) + a(3,2) + a(2,3) + a(3,3)
        !         = 22 + 23 + 32 + 33 = 110
        if (abs(sum_val - 110.0) > 0.001) error stop

        ! Test array section passed to subroutine
        call modify_section(a(ilo:ihi, ilo:ihi), ihi - ilo + 1)

        ! Verify modification: each element should be multiplied by 2
        sum_val = 0.0
        do j = ilo, ihi
            do i = ilo, ihi
                sum_val = sum_val + a(i, j)
            end do
        end do

        ! Expected: 110 * 2 = 220
        if (abs(sum_val - 220.0) > 0.001) error stop
    end subroutine

    subroutine modify_section(b, n)
        integer, intent(in) :: n
        real, intent(inout) :: b(n, n)
        integer :: i, j

        do j = 1, n
            do i = 1, n
                b(i, j) = b(i, j) * 2.0
            end do
        end do
    end subroutine
end program
