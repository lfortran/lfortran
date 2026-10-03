module array_section_22_mod
    implicit none
    integer, parameter :: sp = kind(1.0)

contains

    subroutine assign_tau(q, k)
        real(sp), intent(out), contiguous, target :: q(:,:)
        real(sp), pointer :: tau(:)
        integer, intent(in) :: k
        integer :: i, j

        ! Initialize q (required since INTENT(OUT))
        do i = 1, size(q,1)
            do j = 1, size(q,2)
                q(i,j) = i*10 + j
            end do
        end do

        ! Pointer assignment
        tau(1:k) => q(1:k,1)

        ! ===== CHECK 1: pointer must be associated =====
        if (.not. associated(tau)) then
            error stop "FAIL: tau is not associated"
        end if

        ! ===== CHECK 2: values must match q(:,1) slice =====
        do i = 1, k
            if (tau(i) /= q(i,1)) then
                error stop "FAIL: tau values do not match q slice"
            end if
        end do

        ! Modify via pointer
        tau = -5.0_sp

        ! ===== CHECK 3: q must reflect pointer modification =====
        do i = 1, k
            if (q(i,1) /= -5.0_sp) then
                error stop "FAIL: q not updated via tau"
            end if
        end do

        ! ===== CHECK 4: elements outside slice unchanged =====
        do i = k+1, size(q,1)
            if (q(i,1) == -5.0_sp) then
                error stop "FAIL: out-of-slice elements modified"
            end if
        end do

    end subroutine assign_tau

end module array_section_22_mod

program array_section_22
    use array_section_22_mod
    implicit none

    real(sp), target :: A(5,5)
    integer :: k

    k = 3

    call assign_tau(A, k)

    print *, "All checks passed."

end program array_section_22