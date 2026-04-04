program gpu_metal_13
! Integer(8) (64-bit integer) arrays
implicit none
integer, parameter :: n = 10000
integer(8) :: a(n), b(n), c(n), c_expected(n)
integer :: i

do i = 1, n
    a(i) = int(i, 8) * 100000_8
    b(i) = int(i, 8) * 200000_8
    c(i) = 0_8
    c_expected(i) = int(i, 8) * 300000_8
end do

do concurrent (i = 1:n)
    c(i) = a(i) + b(i)
end do

do i = 1, n
    if (c(i) /= c_expected(i)) error stop
end do

print *, "PASSED"
end program
