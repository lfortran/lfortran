! A single fixed result allocation remains offloadable even when its data
! are assigned conditionally and the actual argument is device-local.
program gpu_metal_328
implicit none
real :: results(4), reallocated(4)
integer :: i

do concurrent (i = 1:4)
    results(i) = sum(fixed_result(i))
end do
do concurrent (i = 1:4)
    reallocated(i) = sum(reallocated_result(i))
end do

if (any(abs(results - [-2.0, 4.0, -6.0, 8.0]) > 1.0e-5)) error stop
if (any(abs(reallocated - [2.0, 4.0, 6.0, 8.0]) > 1.0e-5)) error stop
print *, "PASS"

contains

pure function fixed_result(n) result(r)
    integer, intent(in) :: n
    integer, parameter :: width = 2
    real, allocatable :: r(:)
    allocate(r(width))
    r = real(n)
    if (mod(n, 2) == 0) then
        r = [real(n), real(n)]
    else
        r = -real(n)
    end if
end function

pure function reallocated_result(n) result(r)
    integer, intent(in) :: n
    real, allocatable :: r(:)
    allocate(r(2))
    r = -real(n)
    deallocate(r)
    allocate(r(2))
    r = real(n)
end function

end program
