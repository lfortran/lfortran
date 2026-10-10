program complex_43
! assign an allocatable or pointer value to the real or imaginary part of a
! complex array
implicit none
complex :: z(4)
complex(8) :: zd(4)
real, allocatable :: a(:)
real(8), allocatable :: d(:)
integer, allocatable :: iv(:)
real, pointer :: p(:)
real, target :: t(4)

z = (0.0, 1.0)
zd = (0.0d0, 1.0d0)
allocate(a(4), d(4), iv(4))
a = [1.0, 2.0, 3.0, 4.0]
d = [5.0d0, 6.0d0, 7.0d0, 8.0d0]
iv = [9, 10, 11, 12]
t = [14.0, 15.0, 16.0, 17.0]
p => t

z%re = a
print *, z
if (any(z%re /= [1.0, 2.0, 3.0, 4.0])) error stop
if (any(z%im /= 1.0)) error stop

z(2:3)%im = a(1:2)
if (any(z%im /= [1.0, 1.0, 2.0, 1.0])) error stop

z(1:2)%re = d(3:4)
if (any(z%re /= [7.0, 8.0, 3.0, 4.0])) error stop

z%im = iv
if (any(z%im /= [9.0, 10.0, 11.0, 12.0])) error stop

z(1:3)%im = p(2:4)
if (any(z%im /= [15.0, 16.0, 17.0, 12.0])) error stop

z%re = d
if (any(z%re /= [5.0, 6.0, 7.0, 8.0])) error stop

zd(2:4)%re = a(2:4)
if (any(zd%re /= [0.0d0, 2.0d0, 3.0d0, 4.0d0])) error stop

zd%im = iv
if (any(zd%im /= [9.0d0, 10.0d0, 11.0d0, 12.0d0])) error stop
if (any(zd%re /= [0.0d0, 2.0d0, 3.0d0, 4.0d0])) error stop
end program complex_43
