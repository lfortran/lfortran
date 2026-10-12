program complex_41
! assign to the real or imaginary part of a complex array section
implicit none
complex :: z(4), w(3,3)
complex(8) :: zd(5)
real :: r(4)
integer :: i

z = (5.0, 5.0)
z(2:3)%re = 0.5
print *, z
if (any(z(2:3)%re /= 0.5)) error stop
if (any(z%im /= 5.0)) error stop
if (z(1)%re /= 5.0 .or. z(4)%re /= 5.0) error stop

z(2:3)%im = 1.5
if (any(z(2:3)%im /= 1.5)) error stop
if (z(1)%im /= 5.0 .or. z(4)%im /= 5.0) error stop
if (any(z(2:3)%re /= 0.5)) error stop

r = [1.0, 2.0, 3.0, 4.0]
z(1:4:2)%re = r(1:2)
if (z(1)%re /= 1.0 .or. z(3)%re /= 2.0) error stop
if (z(2)%re /= 0.5 .or. z(4)%re /= 5.0) error stop

! overlapping right hand side
z(2:4)%re = z(1:3)%re
if (z(1)%re /= 1.0 .or. z(2)%re /= 1.0) error stop
if (z(3)%re /= 0.5 .or. z(4)%re /= 2.0) error stop

z(3:1:-1)%im = r(1:3)
if (z(1)%im /= 3.0 .or. z(2)%im /= 2.0 .or. z(3)%im /= 1.0) error stop
if (z(4)%im /= 5.0) error stop

i = 2
z(i:i+1)%re = 9.0
if (z(1)%re /= 1.0 .or. z(2)%re /= 9.0) error stop
if (z(3)%re /= 9.0 .or. z(4)%re /= 2.0) error stop

w = (0.0, 0.0)
w(2,:)%re = 7.0
w(:,3)%im = [1.0, 2.0, 3.0]
if (any(w(2,:)%re /= 7.0)) error stop
if (any(w(1,:)%re /= 0.0) .or. any(w(3,:)%re /= 0.0)) error stop
if (any(w(:,3)%im /= [1.0, 2.0, 3.0])) error stop
if (any(w(:,1:2)%im /= 0.0)) error stop

zd = (1.0d0, 2.0d0)
zd(2:4)%re = 3
if (any(zd(2:4)%re /= 3.0d0)) error stop
if (zd(1)%re /= 1.0d0 .or. zd(5)%re /= 1.0d0) error stop
if (any(zd%im /= 2.0d0)) error stop
end program complex_41
