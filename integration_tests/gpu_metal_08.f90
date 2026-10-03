program gpu_metal_08
! Conditional inside do concurrent
implicit none
integer, parameter :: n = 10000
integer :: a(n), a_expected(n)
integer :: i

do i = 1, n
    if (mod(i, 2) == 0) then
        a_expected(i) = i * 2
    else
        a_expected(i) = i * 3
    end if
end do

do concurrent (i = 1:n)
    if (mod(i, 2) == 0) then
        a(i) = i * 2
    else
        a(i) = i * 3
    end if
end do

do i = 1, n
    if (a(i) /= a_expected(i)) error stop
end do

print *, "PASSED"
end program
