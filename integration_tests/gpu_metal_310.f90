! A device function with a run-time sized local, called from ASSOCIATE
! inside do concurrent, where it cannot be spliced into the kernel. The
! call stays a call, and the local becomes an argument the kernel gives
! the function, so Metal is never given a helper it cannot compile.
program gpu_metal_310
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
        do concurrent (i = 1:size(r))
            associate (t => real(i))
                r(i) = vla_fill(nn, t)
            end associate
        end do
    end subroutine

end program
