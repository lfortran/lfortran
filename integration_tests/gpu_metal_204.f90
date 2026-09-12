program gpu_metal_204
! A do concurrent loop that does input or output cannot be offloaded: the
! device has no way to run it. The loop has to stay on the CPU and the
! output has to happen.
implicit none
integer :: i
character(len=4) :: s(4)
real :: a(4)

s = "----"
a = 0.0
do concurrent (i = 1:4)
    a(i) = i
    write(s(i), '(i1)') i
end do

if (abs(a(4) - 4.0) > 1.0e-5) error stop
if (s(1) /= "1") error stop
if (s(4) /= "4") error stop
print *, "PASSED"
end program
