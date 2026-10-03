program gpu_metal_340
! An offloaded `do concurrent` with a `shared` locality spec on an
! assumed-shape dummy, compiled with --separate-compilation. The procedure
! is specialised into a copy that takes the array by data, and separate
! compilation keeps the original beside it, so the copy must own its own
! clauses: a `shared` clause borrowed from the original names the
! original's variable from the copy's scope.
implicit none
integer, parameter :: n = 1000
real :: r(n)
integer :: i

do i = 1, n
    r(i) = real(i)
end do
call double_it(r)
do i = 1, n
    if (abs(r(i) - 2.0*real(i)) > 1e-3) error stop
end do
print *, r(1), r(n)

contains

    subroutine double_it(x)
        real, intent(inout) :: x(:)
        integer :: j
        do concurrent (j = 1:size(x)) shared(x)
            x(j) = 2*x(j)
        end do
    end subroutine

end program
