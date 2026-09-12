! A contained function with an automatic array, called from a loop.
! --fast used to inline it, allocate tmp(m) at the caller's entry (size 0),
! and return zeros.
program functions_64
implicit none
integer, parameter :: n = 3
real :: a(4)
integer :: i

a = 0.0
call run(a, n)
do i = 1, 4
    if (abs(a(i) - real(i) * real(n)) > 1.0e-5) error stop
end do
print *, "PASS"

contains

    pure function vla_fill(m, v) result(s)
        integer, intent(in) :: m
        real, intent(in) :: v
        real :: s
        real :: tmp(m)
        integer :: k
        do k = 1, m
            tmp(k) = v
        end do
        s = sum(tmp)
    end function

    subroutine run(r, nn)
        real, intent(out) :: r(:)
        integer, intent(in) :: nn
        integer :: i
        do i = 1, size(r)
            r(i) = vla_fill(nn, real(i))
        end do
    end subroutine

end program
