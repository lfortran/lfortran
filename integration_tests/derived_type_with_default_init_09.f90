module derived_type_with_default_init_09_mod
use iso_c_binding, only: c_char, c_int
implicit none

type :: sq
    sequence
    character(len=5) :: s = "aaaaa"
    integer :: n = 1
    character(len=2) :: ca(2) = ["xx", "yy"]
end type

type, bind(c) :: bc
    character(kind=c_char) :: c = 'x'
    integer(c_int) :: n = 2
    character(kind=c_char) :: cs(3) = ['a', 'b', 'c']
end type

type(sq) :: mod_seq
type(bc) :: mod_bindc

contains

subroutine check_seq(v)
    type(sq), intent(in) :: v
    if (v%s /= "aaaaa") error stop 1
    if (v%n /= 1) error stop 2
    if (v%ca(1) /= "xx") error stop 3
    if (v%ca(2) /= "yy") error stop 4
end subroutine

subroutine check_bindc(v)
    type(bc), intent(in) :: v
    if (v%c /= 'x') error stop 5
    if (v%n /= 2) error stop 6
    if (v%cs(1) /= 'a') error stop 7
    if (v%cs(2) /= 'b') error stop 8
    if (v%cs(3) /= 'c') error stop 9
end subroutine

subroutine check_local()
    type(sq) :: v
    type(bc) :: w
    call check_seq(v)
    call check_bindc(w)
end subroutine

subroutine check_saved()
    type(sq), save :: v
    type(bc), save :: w
    call check_seq(v)
    call check_bindc(w)
end subroutine

subroutine reset_seq(v)
    type(sq), intent(out) :: v
end subroutine

subroutine reset_bindc(v)
    type(bc), intent(out) :: v
end subroutine

end module

program derived_type_with_default_init_09
use derived_type_with_default_init_09_mod
implicit none
type(sq) :: v
type(bc) :: w
integer :: i

print *, v%s, v%n, v%ca(1), v%ca(2), w%c, w%n, w%cs(1), w%cs(2), w%cs(3)
call check_seq(v)
call check_bindc(w)
call check_seq(mod_seq)
call check_bindc(mod_bindc)
call check_local()
call check_saved()

do i = 1, 2
    v%s = "bye"
    v%n = 9
    v%ca(1) = "zz"
    v%ca(2) = "zz"
    w%c = 'q'
    w%n = 8
    w%cs(1) = 'q'
    w%cs(3) = 'q'
    call reset_seq(v)
    call reset_bindc(w)
    call check_seq(v)
    call check_bindc(w)
    call check_local()
end do

print *, "ok"
end program
