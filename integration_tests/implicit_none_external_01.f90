! Procedures that are explicitly declared where `implicit none (external)`
! applies: EXTERNAL, interface blocks, module, internal and intrinsic
! procedures, a procedure pointer, and an external procedure referencing
! itself or its ENTRY recursively. A unit after it is not affected.
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
recursive subroutine countdown(n, total)
integer, intent(in) :: n
integer, intent(inout) :: total
end subroutine
recursive function fact(n) result(r)
integer, intent(in) :: n
integer :: r
end function
recursive subroutine ping(n, total)
integer, intent(in) :: n
integer, intent(inout) :: total
end subroutine
recursive integer function zig(n) result(r)
integer, intent(in) :: n
end function
end interface
procedure(ms), pointer :: p
real :: x, y
integer :: i, total
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
total = 0
call countdown(4, total)
if (total /= 10) error stop
if (fact(5) /= 120) error stop
total = 0
call ping(3, total)
if (total /= 24) error stop
if (zig(4) /= 6) error stop
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
recursive subroutine countdown(n, total)
implicit none (type, external)
integer, intent(in) :: n
integer, intent(inout) :: total
if (n <= 0) return
total = total + n
call countdown(n - 1, total)
end subroutine
recursive function fact(n) result(r)
implicit none (type, external)
integer, intent(in) :: n
integer :: r
if (n <= 1) then
    r = 1
else
    r = n * fact(n - 1)
end if
end function
recursive subroutine ping(n, total)
implicit none (type, external)
integer, intent(in) :: n
integer, intent(inout) :: total
total = total + n
if (n > 0) call pong(n - 1, total)
return
entry pong(n, total)
total = total + 10*n
if (n > 0) call ping(n - 1, total)
end subroutine
recursive integer function zig(n) result(r)
implicit none (type, external)
integer, intent(in) :: n
if (n <= 0) then
    r = 0
else
    r = 1 + zag(n - 1)
end if
return
entry zag(n) result(r)
if (n <= 0) then
    r = 0
else
    r = 2 + zig(n - 1)
end if
end function
