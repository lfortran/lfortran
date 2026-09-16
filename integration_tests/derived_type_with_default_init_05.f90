module derived_type_with_default_init_05_mod
implicit none

type :: t1
    character(len=4) :: b = "bbbb"
    character(len=3) :: s = "t1s"
    integer, pointer :: p => null()
    integer, allocatable :: a(:)
    character(len=2) :: ca(2) = ["aa", "bb"]
end type

type :: t2
    type(t1) :: a = t1("BBBB", "x2a", null(), null(), ["c2", "d2"])
    character(len=4) :: s = "t2ss"
end type

type :: t3
    type(t2) :: b = t2(t1("CCCC", "x3b", null(), null(), ["e3", "f3"]), "t3ss")
end type

type :: t4
    type(t3) :: c
    type(t3) :: d = t3(t2(t1("DDDD", "x4d", null(), null(), ["g4", "h4"]), "t4ss"))
end type

contains

subroutine check_t1(x, b, s, ca)
    type(t1), intent(in) :: x
    character(len=*), intent(in) :: b, s, ca(2)
    if (x%b /= b .or. x%s /= s) error stop 1
    if (x%ca(1) /= ca(1) .or. x%ca(2) /= ca(2)) error stop 2
    if (associated(x%p)) error stop 3
    if (allocated(x%a)) error stop 4
end subroutine

subroutine check(v)
    type(t4), intent(in) :: v
    call check_t1(v%c%b%a, "CCCC", "x3b", ["e3", "f3"])
    if (v%c%b%s /= "t3ss") error stop 5
    call check_t1(v%d%b%a, "DDDD", "x4d", ["g4", "h4"])
    if (v%d%b%s /= "t4ss") error stop 6
end subroutine

subroutine reset(v)
    type(t4), intent(out) :: v
end subroutine

end module

program derived_type_with_default_init_05
use derived_type_with_default_init_05_mod
implicit none
type(t4) :: g
integer :: i

print *, g%c%b%a%b, g%c%b%a%s, g%c%b%a%ca, g%c%b%s
print *, g%d%b%a%b, g%d%b%a%s, g%d%b%a%ca, g%d%b%s
call check(g)
do i = 1, 3
    call local_default()
    g%c%b%a%s = "zzz"
    g%d%b%a%ca = "zz"
    g%d%b%s = "zzzz"
    allocate(g%c%b%a%a(4))
    call reset(g)
    call check(g)
end do

contains

subroutine local_default()
    type(t4) :: v
    type(t2) :: w
    integer :: junk(64)
    junk = -1
    call check(v)
    call check_t1(w%a, "BBBB", "x2a", ["c2", "d2"])
    if (w%s /= "t2ss" .or. junk(1) /= -1) error stop 7
    v%d%b%a%s = "yyy"
    w%a%s = "yyy"
end subroutine

end program
