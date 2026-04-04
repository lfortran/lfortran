program gpu_metal_48
! Test: function call inside do concurrent (GPU offload)
! Verifies that internal functions defined in `contains` are correctly
! duplicated into the GPU kernel scope.
implicit none
integer :: i
real :: x(4)
integer :: y(4)
do concurrent (i = 1:4)
    x(i) = scale_val(real(i))
    y(i) = double_int(i)
end do
if (abs(x(1) - 2.0) > 1e-6) error stop
if (abs(x(2) - 4.0) > 1e-6) error stop
if (abs(x(3) - 6.0) > 1e-6) error stop
if (abs(x(4) - 8.0) > 1e-6) error stop
if (y(1) /= 2) error stop
if (y(2) /= 4) error stop
if (y(3) /= 6) error stop
if (y(4) /= 8) error stop
print *, "ok"
contains
    pure function scale_val(v) result(r)
        real, intent(in) :: v
        real :: r
        r = v * 2.0
    end function

    pure function double_int(n) result(r)
        integer, intent(in) :: n
        integer :: r
        r = n * 2
    end function
end program
