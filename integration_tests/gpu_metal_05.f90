program gpu_metal_05
! Integer arrays: element-wise addition
implicit none
integer, parameter :: n = 10000
integer :: a(n), b(n), c(n), c_expected(n)
integer :: i

do i = 1, n
    a(i) = i
    b(i) = i * 2
    c(i) = 0
    c_expected(i) = i + i * 2
end do

do concurrent (i = 1:n)
    c(i) = a(i) + b(i)
end do

do i = 1, n
    if (c(i) /= c_expected(i)) error stop
end do

print *, "PASSED"
end program
