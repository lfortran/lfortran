program gpu_type_inquiry_01
! Type inquiries (huge, tiny, epsilon, digits, ...) in a kernel: constants
! of the type of their argument.
implicit none
integer, parameter :: sp = kind(0.0), dp = kind(0.d0)
integer, parameter :: n = 1000
real(dp) :: x(n), a(n), b(n)
real(sp) :: y(n), c(n)
integer :: k(n), d(n), e(n)
integer :: i

do i = 1, n
    x(i) = i - 500
    y(i) = i - 500
    k(i) = i
end do
do concurrent (i = 1:n)
    a(i) = min(huge(x(i)), x(i)) + tiny(x(i))
    b(i) = x(i)*epsilon(x(i))
    c(i) = max(-huge(y(i)), y(i)) + epsilon(y(i))
    d(i) = min(huge(k(i)), k(i)) + digits(x(i))
    e(i) = k(i) + range(y(i)) + precision(x(i)) + radix(x(i))
end do
do i = 1, n
    if (a(i) /= x(i) + tiny(x(i))) error stop 1
    if (b(i) /= x(i)*epsilon(x(i))) error stop 2
    if (c(i) /= y(i) + epsilon(y(i))) error stop 3
    if (d(i) /= k(i) + 53) error stop 4
    if (e(i) /= k(i) + 37 + 15 + 2) error stop 5
end do
print *, sum(a), sum(c), sum(d), sum(e)
end program
