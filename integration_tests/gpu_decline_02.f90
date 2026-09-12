! gpu decline reason: UngatherableStridedSection
! a call in the loop body takes a strided section whose extent is not a
! compile-time constant, so it cannot be gathered into a kernel-local
! temporary.
program gpu_decline_02
    implicit none
    real :: b(8), a(4)
    integer :: i
    do i = 1, 8
        b(i) = real(i)
    end do
    call go(b, a)
    do i = 1, 4
        if (abs(a(i) - real(i) * 16.0) > 1.0e-6) error stop "bad a"
    end do
contains
    subroutine go(b, a)
        real, intent(in) :: b(:)
        real, intent(out) :: a(4)
        integer :: i
        do concurrent (i = 1:4)
            a(i) = real(i) * tot(b(1:size(b):2))
        end do
    end subroutine
    pure real function tot(v)
        real, intent(in) :: v(:)
        integer :: k
        tot = 0.0
        do k = 1, size(v)
            tot = tot + v(k)
        end do
    end function
end program
