program gpu_metal_29
implicit none
integer :: x(5), i
x = 0
do concurrent (i = 1:5)
    block
        integer :: tmp
        tmp = i * 2
        x(i) = tmp
    end block
end do
if (x(1) /= 2) error stop
if (x(2) /= 4) error stop
if (x(3) /= 6) error stop
if (x(4) /= 8) error stop
if (x(5) /= 10) error stop
print *, "ok"
end program
