! gpu decline reason: AliasTemporaryRuntimeSized
! an array assignment whose two sides overlap needs a temporary, and here the
! target is a section of an assumed-shape dummy, so that temporary cannot be
! given compile-time extents.
program gpu_decline_01
    implicit none
    real :: w(3,4)
    integer :: i, j
    do j = 1, 4
        do i = 1, 3
            w(i,j) = real(i + j)
        end do
    end do
    call go(w)
    do j = 1, 4
        do i = 1, 3
            if (abs(w(i,j) - real(2*j + i + 1)) > 1.0e-6) error stop "bad w"
        end do
    end do
contains
    subroutine go(w)
        real, intent(inout) :: w(:,:)
        integer :: i, k
        do concurrent (i = 1:4)
            do k = 1, 1
                w(:,i) = w(:,i) + w(1,i)
            end do
        end do
    end subroutine
end program
