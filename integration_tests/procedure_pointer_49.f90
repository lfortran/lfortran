! A typed procedure pointer with an implicit interface passed to an
! external function dummy, directly and through a generic (#16414)
module procedure_pointer_49_m
implicit none
interface g
    module procedure g_int, g_proc
end interface
contains
integer function g_int(x)
    integer, intent(in) :: x
    g_int = -x
end function
integer function g_proc(f)
    integer, external :: f
    g_proc = f(10)
end function
end module

program procedure_pointer_49
use procedure_pointer_49_m
implicit none
procedure(integer), pointer :: p
p => inc
if (run(p) /= 11) error stop 1
if (g_proc(p) /= 11) error stop 2
if (g(p) /= 11) error stop 3
if (g(4) /= -4) error stop 4
print *, "ok"
contains
integer function inc(x)
    integer, intent(in) :: x
    inc = x + 1
end function
integer function run(f)
    integer, external :: f
    run = f(10)
end function
end program
