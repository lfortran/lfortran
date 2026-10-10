program forall_06
implicit none
real :: a(3,5), z(3), w(3,3)
integer :: i, j

a = 1
z = 0
forall (j = 1:3) z(j) = sum(a(j,1:j+2))
if (any(z /= [3.0, 4.0, 5.0])) error stop

z = 0
forall (j = 1:3, j > 1) z(j) = sum(a(j,j:5))
if (any(z /= [0.0, 4.0, 3.0])) error stop

w = 0
forall (i = 1:3, j = 1:3) w(i,j) = sum(a(i,1:i+j-1))
do i = 1, 3
    do j = 1, 3
        if (w(i,j) /= i + j - 1) error stop
    end do
end do

! The forall runs zero times, so a(i,1:2*i) must never be evaluated:
! a(3,1:6) does not exist.
w = 0
do i = 1, 3
    forall (j = 1:i-3) w(i,j) = sum(a(i,1:2*i))
end do
if (any(w /= 0)) error stop

print *, z
end program
