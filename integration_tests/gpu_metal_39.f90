program gpu_metal_39
! Test: associate variable used as do concurrent loop bound
implicit none
integer :: i, m, s
integer :: a(10)
m = 10
s = 0

do i = 1, m
    a(i) = 0
end do

associate(n => m)
    do concurrent(i = 1:n)
        a(i) = i * 3
    end do
end associate

do i = 1, m
    s = s + a(i)
end do

if (s /= 165) error stop
print *, "PASSED"
end program
