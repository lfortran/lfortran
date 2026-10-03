program gpu_metal_35
! Test: fixed-size array in block inside do concurrent with GPU offloading
implicit none
integer :: i, x(5)
x = 0

do concurrent (i = 1:5)
    block
        integer :: a(3)
        a(1) = i
        a(2) = i * 2
        a(3) = a(1) + a(2)
        x(i) = a(3)
    end block
end do

if (x(1) /= 3) error stop
if (x(2) /= 6) error stop
if (x(3) /= 9) error stop
if (x(4) /= 12) error stop
if (x(5) /= 15) error stop
print *, "ok"
end program
