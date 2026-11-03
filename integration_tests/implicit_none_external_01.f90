! Procedures that are explicitly declared where `implicit none (external)`
! applies: EXTERNAL, interface blocks, module, internal and intrinsic
! procedures, and a procedure pointer. A unit after it is not affected.
module implicit_none_external_01_mod
implicit none (type, external)
contains
subroutine ms(x)
real, intent(inout) :: x
x = x + 1
end subroutine
real function mf(x)
real, intent(in) :: x
mf = 2*x
end function
end module

program implicit_none_external_01
use implicit_none_external_01_mod
implicit none (type, external)
external :: foo
interface
subroutine bar(n)
integer, intent(in) :: n
end subroutine
real function h(x)
real, intent(in) :: x
end function
end interface
procedure(ms), pointer :: p
real :: x, y
integer :: i
x = 1
call ms(x)
call foo()
call bar(3)
call internal(i)
call random_number(y)
y = sin(x) + mf(x) + h(x) + ifn(2)
p => ms
call p(x)
call blk()
if (i /= 5) error stop
if (abs(x - 3) > 1e-6) error stop
print *, 'ok'
contains
subroutine internal(k)
integer, intent(out) :: k
k = 5
end subroutine
integer function ifn(k)
integer, intent(in) :: k
ifn = k
end function
subroutine blk()
block
call foo()
end block
end subroutine
end program

subroutine foo()
print *, 'foo called'
end subroutine
subroutine bar(n)
integer, intent(in) :: n
print *, n
end subroutine
real function h(x)
real, intent(in) :: x
h = x
end function
subroutine notinherited()
call foo()
end subroutine
