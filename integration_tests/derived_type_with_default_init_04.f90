module derived_type_with_default_init_04_mod
use iso_c_binding, only: c_char, c_int
implicit none

type :: sq
    sequence
    character(len=5) :: s = "aaaaa"
    integer :: n = 1
end type

type, bind(c) :: bc
    character(kind=c_char) :: c = 'x'
    integer(c_int) :: n = 1
end type

type :: tb
    real :: x(3) = 0.0
    character(len=2) :: ca(2) = ["xx", "yy"]
    integer :: n(2) = 0
end type

type :: ts
    character(len=5) :: s = "aaaaa"
    character(len=3) :: ca(2) = ["xxx", "yyy"]
end type

type :: u
    type(sq) :: seq_part = sq("hello", 3)
    type(bc) :: bindc_part = bc('z', 4)
    type(tb) :: scalar_part = tb(5.0, "ab", 6)
    type(tb) :: array_part = tb([1.0, 2.0, 3.0], ["cd", "ef"], [7, 8])
    type(ts) :: short_part = ts("hi", ["a", "b"])
end type

contains

subroutine check(v)
    type(u), intent(in) :: v
    if (v%seq_part%s /= "hello" .or. v%seq_part%n /= 3) error stop 1
    if (v%bindc_part%c /= 'z' .or. v%bindc_part%n /= 4) error stop 2
    if (any(v%scalar_part%x /= 5.0)) error stop 3
    if (any(v%scalar_part%ca /= "ab") .or. any(v%scalar_part%n /= 6)) error stop 4
    if (any(v%array_part%x /= [1.0, 2.0, 3.0])) error stop 5
    if (v%array_part%ca(1) /= "cd" .or. v%array_part%ca(2) /= "ef") error stop 6
    if (any(v%array_part%n /= [7, 8])) error stop 7
    if (v%short_part%s /= "hi   ") error stop 8
    if (v%short_part%ca(1) /= "a  " .or. v%short_part%ca(2) /= "b  ") error stop 9
end subroutine

subroutine reset(v)
    type(u), intent(out) :: v
end subroutine

end module

program derived_type_with_default_init_04
use derived_type_with_default_init_04_mod
implicit none
type(u) :: g
integer :: i

print *, g%seq_part%s, g%seq_part%n, g%bindc_part%c, g%bindc_part%n
print *, g%scalar_part%x, g%scalar_part%ca, g%scalar_part%n
print *, "[", g%short_part%s, "][", g%short_part%ca(1), "][", g%short_part%ca(2), "]"
call check(g)
do i = 1, 2
    call local_default()
    g%seq_part%s = "bye"
    g%bindc_part%c = 'q'
    g%scalar_part%ca = "zz"
    g%short_part%ca = "zzz"
    call reset(g)
    call check(g)
end do

contains

subroutine local_default()
    type(u) :: v
    call check(v)
end subroutine

end program
