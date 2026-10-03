program gpu_metal_40
! Test: do concurrent loop bound from outer nested associate block
implicit none
integer :: i, n, m, s
integer :: a(10)
n = 10
m = 0
s = 0

do i = 1, n
    a(i) = 0
end do

associate(nn => n)
  associate(mm => m)
    do concurrent(i = 1:nn)
        a(i) = i * 2
    end do
  end associate
end associate

do i = 1, n
    s = s + a(i)
end do

if (s /= 110) error stop
print *, "PASSED"
end program
