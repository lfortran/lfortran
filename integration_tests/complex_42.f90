program complex_42
! assign to the real or imaginary part of a complex array section a value
! with a reduction over the target array
implicit none
type t
    complex :: z(4)
end type
type(t) :: x
complex :: z(4), w(2,3)
integer :: i

z = cmplx([1.0, 2.0, 3.0, 4.0], [5.0, 6.0, 7.0, 8.0])
z(1:2)%re = maxval(z%re)
print *, z
if (any(z%re /= [4.0, 4.0, 3.0, 4.0])) error stop
if (any(z%im /= [5.0, 6.0, 7.0, 8.0])) error stop

z(1:2)%im = minval(z%im)
if (any(z%im /= [5.0, 5.0, 7.0, 8.0])) error stop
if (any(z%re /= [4.0, 4.0, 3.0, 4.0])) error stop

z(2:3)%re = z(1)%re * maxval(z%im)
if (any(z%re /= [4.0, 32.0, 32.0, 4.0])) error stop

z(1:2)%re = maxval(z(3:4)%re)
if (any(z%re /= [32.0, 32.0, 32.0, 4.0])) error stop

z(1:3:2)%im = product(z(2:3)%im)
if (any(z%im /= [35.0, 5.0, 35.0, 8.0])) error stop
if (any(z%re /= [32.0, 32.0, 32.0, 4.0])) error stop

x%z = cmplx([1.0, 2.0, 3.0, 4.0], [5.0, 6.0, 7.0, 8.0])
x%z(2:3)%re = maxval(x%z%re)
if (any(x%z%re /= [1.0, 4.0, 4.0, 4.0])) error stop
x%z(1:2)%im = x%z(3)%im + minval(x%z%im)
if (any(x%z%im /= [12.0, 12.0, 7.0, 8.0])) error stop

w = reshape([(cmplx(i, -i), i = 1, 6)], [2, 3])
w(1, 2:3)%re = maxval(w%re)
if (any(w%re /= reshape([1.0, 2.0, 6.0, 4.0, 6.0, 6.0], [2, 3]))) error stop
w(:, 1)%im = minval(w(2, :)%im)
if (any(w%im /= reshape([-6.0, -6.0, -3.0, -4.0, -5.0, -6.0], [2, 3]))) error stop
end program complex_42
