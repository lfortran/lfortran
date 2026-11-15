program gpu_minmax_args_01
implicit none
integer :: b(4), r(4), s(4), i
integer(8) :: l(4), t(4)
real :: x(4), y(4), z(4)
b = [-16, 1, -1, 64]
l = int(b, 8)
x = real(b)
do concurrent (i = 1:4)
    r(i) = min(b(i), 5, b(i) - 5)
    s(i) = max(b(i), 5, b(i) + 100)
    y(i) = min(x(i), 5.0, x(i) - 5.0, x(i) - 30.0)
    z(i) = max(x(i), 5.0, x(i) - 5.0, 6.0)
    t(i) = max(l(i), 5_8, l(i) + 100_8, 7_8, -1_8)
end do
print *, r
print *, s
print *, y
print *, z
print *, t
if (any(r /= [-21, -4, -6, 5])) error stop 1
if (any(s /= [84, 101, 99, 164])) error stop 2
if (any(y /= [-46., -29., -31., 5.] )) error stop 3
if (any(z /= [6., 6., 6., 64.])) error stop 4
if (any(t /= [84_8, 101_8, 99_8, 164_8])) error stop 5
end program
