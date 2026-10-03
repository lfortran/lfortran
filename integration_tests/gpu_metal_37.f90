program gpu_metal_37
! Test: VLA with intrinsic array function (maxval) in block inside do concurrent.
! The dimension expression contains a FunctionCall that must be pre-computed
! on the host side before kernel launch.
implicit none
integer :: x(6), i
integer :: dims(2)
dims(1) = 2
dims(2) = 3
x = 0
call compute(x, dims)
if (x(1) /= 10) error stop
if (x(2) /= 20) error stop
if (x(3) /= 30) error stop
if (x(4) /= 40) error stop
if (x(5) /= 50) error stop
if (x(6) /= 60) error stop
print *, "ok"
contains
    subroutine compute(x, vals)
        integer, intent(inout) :: x(:)
        integer, intent(in) :: vals(:)
        integer :: i
        do concurrent (i = 1:6)
            block
                real :: a(maxval(vals))
                a(1) = real(i * 10)
                x(i) = int(a(1))
            end block
        end do
    end subroutine
end program gpu_metal_37
