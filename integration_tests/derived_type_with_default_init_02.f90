module derived_type_with_default_init_02_mod
implicit none

type :: tc
    character(len=5) :: s = "aaaaa"
end type

type :: t
    character(len=5) :: s = "aaaaa"
    real :: x(3) = 0.0
    character(len=2) :: ca(2) = ["xx", "yy"]
    integer :: n = 1
end type

type, extends(tc) :: te
    character(len=3) :: k = "zzz"
end type

type :: uc
    type(tc) :: part = tc("hello")
end type

type :: u
    type(t) :: part = t("hello", [1.0, 2.0, 3.0], ["ab", "cd"], 7)
end type

type :: w
    type(u) :: inner
    type(te) :: ext = te("child", "abc")
end type

contains

subroutine check_uc(a)
    type(uc), intent(in) :: a
    if (a%part%s /= "hello") error stop
end subroutine

subroutine check_u(a)
    type(u), intent(in) :: a
    if (a%part%s /= "hello") error stop
    if (any(a%part%x /= [1.0, 2.0, 3.0])) error stop
    if (a%part%ca(1) /= "ab" .or. a%part%ca(2) /= "cd") error stop
    if (a%part%n /= 7) error stop
end subroutine

subroutine check_w(a)
    type(w), intent(in) :: a
    call check_u(a%inner)
    if (a%ext%s /= "child" .or. a%ext%k /= "abc") error stop
end subroutine

subroutine reset(a)
    type(u), intent(out) :: a
end subroutine

subroutine module_proc_locals()
    type(uc) :: a
    type(u) :: b
    type(w) :: c
    call check_uc(a)
    call check_u(b)
    call check_w(c)
    a%part%s = "bye"
    b%part%s = "bye"
    c%inner%part%s = "bye"
    c%ext%s = "bye"
    if (a%part%s /= "bye" .or. b%part%s /= "bye") error stop
    if (c%inner%part%s /= "bye" .or. c%ext%s /= "bye") error stop
end subroutine

end module

program derived_type_with_default_init_02
use derived_type_with_default_init_02_mod
implicit none
type(uc) :: vc
type(u) :: v
type(w) :: vw
integer :: i

print *, vc%part%s
print *, v%part%s, v%part%x, v%part%ca, v%part%n
print *, vw%inner%part%s, vw%ext%s, vw%ext%k
call check_uc(vc)
call check_u(v)
call check_w(vw)

v%part%s = "bye"
v%part%x = 9.0
call reset(v)
call check_u(v)

do i = 1, 2
    call module_proc_locals()
    call contained_locals()
end do

contains

subroutine contained_locals()
    type(uc) :: a
    type(u) :: b
    type(w) :: c
    call check_uc(a)
    call check_u(b)
    call check_w(c)
end subroutine

end program
