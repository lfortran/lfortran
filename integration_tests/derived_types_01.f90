module derived_types_01_m_01
implicit none

type :: X
    real :: r
    integer :: i
end type

type :: Y
    complex :: c
    type(X) :: d
end type

contains

subroutine set(a)
type(X), intent(out) :: a
a%i = 1
a%r = 1.5
end subroutine

end module

module derived_types_01_m_02
use derived_types_01_m_01, only: Y
implicit none

type :: Z
    complex :: k
    type(Y) :: l
end type

end module

program derived_types_01
use derived_types_01_m_02, only: Z
use derived_types_01_m_01, only: X, set
implicit none
type(X) :: b
type(Z) :: c
b%i = 5
b%r = 3.5
print *, b%i, b%r
call set(b)
print *, b%i, b%r
c%l%d%r = 2.0
c%l%d%i = 2
c%k = (2.0, 2.0)
print *, c%l%d%r, c%l%d%i, c%k
call set(c%l%d)
print *, c%l%d%r, c%l%d%i, c%k
end
