program gpu_kernel_source_08
! Bitwise and power operators inside do concurrent.
! Exercises iand, ior, ieor, ishft, and integer exponentiation.
implicit none
integer, parameter :: n = 8
integer :: a(n), b_and(n), b_or(n), b_xor(n), b_lsh(n), b_rsh(n), b_pow(n)
integer :: i

do i = 1, n
    a(i) = i
end do

b_and = 0; b_or = 0; b_xor = 0
b_lsh = 0; b_rsh = 0; b_pow = 0

do concurrent (i = 1:n)
    b_and(i) = iand(a(i), 3)
    b_or(i)  = ior(a(i), 16)
    b_xor(i) = ieor(a(i), 7)
    b_lsh(i) = ishft(a(i), 2)
    b_rsh(i) = ishft(a(i), -1)
    b_pow(i) = a(i) ** 2
end do

do i = 1, n
    if (b_and(i) /= iand(i, 3))   error stop
    if (b_or(i)  /= ior(i, 16))   error stop
    if (b_xor(i) /= ieor(i, 7))   error stop
    if (b_lsh(i) /= ishft(i, 2))  error stop
    if (b_rsh(i) /= ishft(i, -1)) error stop
    if (b_pow(i) /= i ** 2)       error stop
end do

print *, "PASSED"
end program
