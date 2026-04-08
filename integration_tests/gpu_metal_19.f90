program gpu_metal_19
! Logical array operations
implicit none
integer, parameter :: n = 10000
integer :: a(n), b(n), c(n), c_expected(n)
integer :: i

do i = 1, n
    a(i) = i
    b(i) = n - i + 1
    if (a(i) > b(i)) then
        c_expected(i) = 1
    else
        c_expected(i) = 0
    end if
end do

do concurrent (i = 1:n)
    if (a(i) > b(i)) then
        c(i) = 1
    else
        c(i) = 0
    end if
end do

do i = 1, n
    if (c(i) /= c_expected(i)) error stop
end do

print *, "PASSED"
end program
