subroutine test_real()
implicit none
real :: a(3) = -9.13
double precision :: b(9) = 3.14D0
print *, a
if(any(abs(a - (-9.13) ) > 1e-9)) error stop
print *, b
if(any(abs(b - 3.14D0) > 1d-9)) error stop
end subroutine

program cmake_minimal_test_01
implicit none
integer :: i(3) = 9
print *, i
if(any(i /= 9)) error stop
call test_real()
end program

