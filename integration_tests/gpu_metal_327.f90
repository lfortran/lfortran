! Result allocations must be checked before offload commits, while CPU
! fallback can still preserve the function's run-time result shape.
program gpu_metal_327
implicit none
real :: conditional(4), resized(4), assigned(4), reshaped(1)
real :: implicit_results(4)
real, parameter :: expected(4) = [2.0, 2.0, 6.0, 4.0]
integer :: i

do concurrent (i = 1:4)
    conditional(i) = sum(conditional_result(i))
end do
do concurrent (i = 1:4)
    resized(i) = sum(resized_result(i))
end do
do concurrent (i = 1:4)
    assigned(i) = sum_assigned(i)
end do
do concurrent (i = 1:1)
    reshaped(i) = sum(reshaped_result(i))
end do
do concurrent (i = 1:4)
    implicit_results(i) = sum(fixed_implicit_result(i))
end do

if (any(abs(conditional - expected) > 1.0e-5)) error stop
if (any(abs(resized - expected) > 1.0e-5)) error stop
if (any(abs(assigned - expected) > 1.0e-5)) error stop
if (abs(reshaped(1) - 3.0) > 1.0e-5) error stop
if (any(abs(implicit_results - [-2.0, 4.0, -6.0, 8.0]) > 1.0e-5)) error stop
print *, "PASS"

contains

pure function conditional_result(n) result(r)
    integer, intent(in) :: n
    real, allocatable :: r(:)
    if (mod(n, 2) == 0) then
        allocate(r(1))
    else
        allocate(r(2))
    end if
    r = real(n)
end function

pure function resized_result(n) result(r)
    integer, intent(in) :: n
    real, allocatable :: r(:)
    allocate(r(2))
    r = real(n)
    if (mod(n, 2) /= 0) return
    deallocate(r)
    allocate(r(1))
    r = real(n)
end function

pure function assigned_result(n) result(r)
    integer, intent(in) :: n
    real, allocatable :: r(:)
    if (mod(n, 2) == 0) then
        r = [real(n)]
    else
        r = [real(n), real(n)]
    end if
end function

pure function sum_assigned(n) result(r)
    integer, intent(in) :: n
    real :: r
    r = sum(assigned_result(n))
end function

pure function reshaped_result(n) result(r)
    integer, intent(in) :: n
    real, allocatable :: r(:, :)
    allocate(r(1, 2))
    r = real(n)
    if (mod(n, 2) /= 0) then
        r = reshape([real(n), real(n)], [2, 1])
    end if
    r(1, 1) = real(size(r, 1))
end function

pure function fixed_implicit_result(n) result(r)
    integer, intent(in) :: n
    real, allocatable :: r(:)
    r = [real(n), real(n)]
    if (mod(n, 2) == 0) then
        r = [real(n), real(n)]
    else
        r = -real(n)
    end if
end function

end program
