program array_32
implicit none

type :: der_type
    real :: r
    integer :: i
end type

real, dimension(5) :: a, b
complex, dimension(6) :: x
integer, dimension(3) :: c
logical, dimension(2) :: d
type(der_type), dimension(7) :: p

real, dimension(2,3) :: e
complex, dimension(6,8) :: y
integer, dimension(3,4) :: f
logical, dimension(5,2) :: g
type(der_type), dimension(7,9) :: q

real, dimension(2:3,3:4,4:5) :: h
complex, dimension(6:7,8:10,11:13) :: z
integer, dimension(3:4,4:5,3:4) :: i
logical, dimension(5:6,2:3,2:4) :: j
type(der_type), dimension(7:10,9:15,16:18) :: r

if( lbound(p, 1) /= 1 ) error stop
if( ubound(p, 1) /= 7 ) error stop

if( lbound(q, 1) /= 1 ) error stop
if( ubound(q, 1) /= 7 ) error stop
if( lbound(q, 2) /= 1 ) error stop
if( ubound(q, 2) /= 9 ) error stop

if( lbound(r, 1) /= 7 ) error stop
if( ubound(r, 1) /= 10 ) error stop
if( lbound(r, 2) /= 9 ) error stop
if( ubound(r, 2) /= 15 ) error stop
if( lbound(r, 3) /= 16 ) error stop
if( ubound(r, 3) /= 18 ) error stop

end program
