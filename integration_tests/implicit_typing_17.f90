! A PARAMETER or POINTER statement naming an entity that is not declared yet
! applies only to its own scoping unit or BLOCK construct, not to a same-named
! entity of a later program unit, separate module procedure, contained
! procedure or BLOCK construct.
module implicit_typing_17_mod
parameter (mm = 4)
contains
integer function twice_mm()
twice_mm = 2*mm
end function
end module

module implicit_typing_17_mp
interface
    module subroutine mp1(r)
        integer, intent(out) :: r
    end subroutine
    module subroutine mp2(r)
        integer, intent(out) :: r
    end subroutine
end interface
end module

submodule (implicit_typing_17_mp) implicit_typing_17_sm
contains
module procedure mp1
    parameter (k = 3)
    integer k
    r = k
end procedure
module procedure mp2
    integer k
    k = 5
    r = k
end procedure
end submodule

subroutine a1(s)
parameter (m = 10)
integer, intent(out) :: s
integer :: x(m)
x = 1
s = sum(x)
end subroutine

subroutine a2(s)
parameter (m = 20)
integer, intent(out) :: s
s = m
end subroutine

subroutine c1(s)
parameter (n = 5)
integer n
integer, intent(out) :: s
integer :: y(n)
y = 2
s = sum(y)
end subroutine

subroutine p1(s)
pointer p
integer p
integer, target :: t
integer, intent(out) :: s
t = 7
p => t
s = p
end subroutine

integer function f(k)
integer k
integer m, n, p, mm
m = 3
n = 4
p = 5
mm = 6
m = m + k
f = m + n + p + mm
end function

subroutine b(s)
integer, intent(out) :: s
integer m
m = 3
s = m
end subroutine

program implicit_typing_17
use implicit_typing_17_mod, only: twice_mm
use implicit_typing_17_mp, only: mp1, mp2
parameter (mc = 10)
integer f, s
integer :: yc(mc)
call a1(s)
if (s /= 10) error stop
call a2(s)
if (s /= 20) error stop
call c1(s)
if (s /= 10) error stop
call p1(s)
if (s /= 7) error stop
call b(s)
if (s /= 3) error stop
if (f(1) /= 19) error stop
if (twice_mm() /= 8) error stop
block
    parameter (j = 2)
    integer j
    s = j
end block
if (s /= 2) error stop
block
    integer j
    j = 9
    s = j
end block
if (s /= 9) error stop
call mp1(s)
if (s /= 3) error stop
call mp2(s)
if (s /= 5) error stop
yc = 1
call inner(s)
if (s /= 3 .or. sum(yc) /= 10) error stop
print *, "ok"
contains
subroutine inner(r)
    integer, intent(out) :: r
    integer mc
    mc = 3
    r = mc
end subroutine
end program
