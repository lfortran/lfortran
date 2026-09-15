module save_20_mod
use iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_loc
implicit none
type :: t
    integer :: h = 0
    integer :: k = 3
end type
type :: a_t
    integer :: x = 1
end type
type :: b_t
    type(a_t) :: a
    integer :: y = 3
end type
type, extends(a_t) :: c_t
    integer :: y = 2
end type
type :: s_t
    character(len=4) :: c = "zz"
    integer :: n = 0
end type
type :: v_t
    integer :: a(3) = 0
end type
type :: u_t
    integer :: h = 0
    real :: x = 0
    integer(8) :: k = 0
    complex :: z = (0, 0)
end type
integer, parameter :: n0 = 4
type(t), parameter :: z = t(9, 30)
integer, target :: tgt = 7
contains
integer function fm() result(r)
    type(t) :: s = z
    s%h = s%h + 1
    r = s%h
end function

integer function f_extends() result(r)
    type(c_t) :: v = c_t(10, 20)
    v%y = v%y + 1
    r = v%x + v%y
end function

logical function f_cptr(k) result(r)
    integer, intent(in) :: k
    type(c_ptr) :: cp = c_null_ptr
    if (k == 1) then
        r = .not. c_associated(cp)
        cp = c_loc(tgt)
    else
        r = c_associated(cp)
    end if
end function

integer function f_nested() result(r)
    type(b_t) :: lb = b_t(a_t(10), 30)
    r = lb%a%x + lb%y
end function

integer function f_char() result(r)
    type(s_t) :: cs = s_t("ab", 5)
    if (cs%c /= "ab") error stop 20
    r = cs%n
end function

integer function f_broadcast() result(r)
    type(v_t) :: w = v_t(5)
    r = sum(w%a)
end function

integer function f_negative() result(r)
    type(t) :: s = t(-9, -30)
    if (s%k /= -30) error stop 21
    s%h = s%h + 1
    r = s%h
end function

integer function f_folded() result(r)
    type(u_t) :: s = u_t(-9 + 18, 1, n0, (1.0, -2.0))
    if (abs(s%x - 1.0) > 1e-6) error stop 22
    if (s%k /= 4_8) error stop 23
    if (abs(s%z - (1.0, -2.0)) > 1e-6) error stop 24
    s%h = s%h + 1
    r = s%h
end function
end module

program save_20
use save_20_mod
implicit none
type(t), parameter :: zp = t(9, 30)
integer :: i

do i = 1, 2
    if (f_param() /= 9 + i) error stop 1
    if (f_literal() /= 9 + i) error stop 2
    if (f_save_literal() /= 9 + i) error stop 3
    if (fm() /= 9 + i) error stop 4
    call s_block(i)
    if (f_extends() /= 30 + i) error stop 7
    if (.not. f_cptr(i)) error stop 8
    if (f_nested() /= 40) error stop 9
    if (f_char() /= 5) error stop 10
    if (f_broadcast() /= 15) error stop 11
    if (f_negative() /= -9 + i) error stop 12
    if (f_folded() /= 9 + i) error stop 13
end do
print *, "ok"

contains

integer function f_param() result(r)
    type(t) :: s = zp
    s%h = s%h + 1
    s%k = s%k + 1
    if (s%k /= 30 + s%h - 9) error stop 5
    r = s%h
end function

integer function f_literal() result(r)
    type(t) :: s = t(9, 30)
    s%h = s%h + 1
    r = s%h
end function

integer function f_save_literal() result(r)
    type(t), save :: s = t(9, 30)
    s%h = s%h + 1
    r = s%h
end function

subroutine s_block(i)
    integer, intent(in) :: i
    block
        type(t) :: sb = zp
        sb%h = sb%h + 1
        if (sb%h /= 9 + i) error stop 6
    end block
end subroutine

end program
