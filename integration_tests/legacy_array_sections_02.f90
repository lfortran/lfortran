subroutine sub(a)
double precision :: a
dimension :: a(16)
print *, a
if (abs(a(1) - 1) > 1d-15) error stop
if (abs(a(2) - 2) > 1d-15) error stop
if (.not. all(abs(a(3:) - 1) < 1d-15)) error stop
end subroutine

program main
double precision :: a(16,3)
integer :: j
j = 3
a = 1
a(2,3) = 2
! call sub(a(1,j))
end program
