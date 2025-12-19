program arrays_62
! Test array section shape conformance checking
implicit none

integer :: a(5), b(10)
integer :: c(3), d(8)
integer :: e(2), f(4)

! Error: shape mismatch - a is (5), b(1:3) is (3)
a = b(1:3)

! Error: shape mismatch - c is (3), d(5:6) is (2)
c = d(5:6)

! Error: shape mismatch - e is (2), f(1:1) is (1)
e = f(1:1)

end program arrays_62
