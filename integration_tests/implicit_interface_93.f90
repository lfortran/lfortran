! Calls through implicit interfaces declare procedure-pointer temporaries.
! Their names never hide a user variable whose name looks like one (here
! `g_fpcast`), whether the user variable is declared in an enclosing scope
! (BLOCK, statement function, internal procedure, SELECT TYPE, module
! procedure), is use-associated, or is in a COMMON block.
module implicit_interface_93_m
implicit none
real :: g_fpcast = 5.0
real :: f_fpcast = 100.0
contains
    real function use_g(y)
        real, intent(in) :: y
        real, external :: g
        use_g = g(y) + g_fpcast
    end function

    real function apply(f, y)
        real, external :: f
        real, intent(in) :: y
        real :: sf, t
        sf(t) = f(t) + f_fpcast
        apply = sf(y)
    end function
end module

program implicit_interface_93
use implicit_interface_93_m, only: use_g, apply
implicit none
real, external :: g
external :: gsub
procedure(), pointer :: p
real :: g_fpcast, p_fpcast, x
class(*), allocatable :: c
real :: sf, t
sf(t) = g(t) + g_fpcast
g_fpcast = 5.0
block
    x = g(1.0)
    if (abs(x - 2.0) > 1e-6) error stop 1
    if (abs(g_fpcast - 5.0) > 1e-6) error stop 2
end block
if (abs(sf(1.0) - 7.0) > 1e-6) error stop 3
if (abs(h(1.0) - 7.0) > 1e-6) error stop 4
allocate(c, source=1.0)
select type (c)
type is (real)
    if (abs(g(c) + g_fpcast - 7.0) > 1e-6) error stop 5
end select
p_fpcast = 3.0
p => gsub
block
    call p(x)
    if (abs(x - 4.0) > 1e-6) error stop 6
    if (abs(p_fpcast - 3.0) > 1e-6) error stop 7
end block
if (abs(use_g(1.0) - 7.0) > 1e-6) error stop 8
if (abs(apply(g, 1.0) - 102.0) > 1e-6) error stop 9
call use_in_block(x)
if (abs(x - 7.0) > 1e-6) error stop 10
call common_in_block(x)
if (abs(x - 8.0) > 1e-6) error stop 11
print *, "ok"
contains
    real function h(y)
        real, intent(in) :: y
        h = g(y) + g_fpcast
    end function
end program

subroutine use_in_block(r)
use implicit_interface_93_m
implicit none
real, intent(out) :: r
real, external :: g
block
    r = g(1.0) + g_fpcast
end block
end subroutine

subroutine common_in_block(r)
implicit none
real, intent(out) :: r
real :: g_fpcast
common /c93/ g_fpcast
real, external :: g
g_fpcast = 6.0
block
    r = g(1.0) + g_fpcast
end block
end subroutine

subroutine gsub(x)
real :: x
x = 4.0
end subroutine

real function g(y)
real :: y
g = y + 1.0
end function
