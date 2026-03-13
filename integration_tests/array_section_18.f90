! Tests that copy-in/copy-out is skipped for array sections
! with undefined assumed-size bounds (recursive sequence association).
program array_section_18
    implicit none
    real :: a(4, 4)
    integer :: i, j
    do j = 1, 4
        do i = 1, 4
            a(i, j) = real(10 * j + i)
        end do
    end do
    call double_diag(4, a, 4)
    if (abs(a(1,1) - 22.0) > 0.001) error stop
    if (abs(a(2,2) - 44.0) > 0.001) error stop
    if (abs(a(3,3) - 66.0) > 0.001) error stop
    if (abs(a(4,4) - 88.0) > 0.001) error stop
    ! Off-diagonal elements should be unchanged
    if (abs(a(1,2) - 21.0) > 0.001) error stop
    if (abs(a(2,1) - 12.0) > 0.001) error stop
    print *, "All checks passed."
end program

