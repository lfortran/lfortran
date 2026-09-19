module array_of_strings_02_mod
use iso_c_binding, only: c_char, c_int
implicit none

! SEQUENCE and bind(C) types store a character component inline, as a flat
! byte blob, instead of behind a string descriptor.
type :: sq
    sequence
    character(len=5) :: s = "aaaaa"
    integer :: n = 1
    character(len=2) :: ca(2) = ["xx", "yy"]
end type

type, bind(c) :: bc
    character(kind=c_char) :: c = 'x'
    integer(c_int) :: n = 1
    character(kind=c_char) :: cs(4) = ['a', 'b', 'c', 'd']
end type

type :: u
    type(sq) :: seq_part = sq("hello", 3, ["ab", "cd"])
    type(bc) :: bindc_part = bc('z', 4, ['p', 'q', 'r', 's'])
    character(len=4) :: tag = "tagg"
end type

contains

subroutine take_ca(x)
    character(len=2), intent(in) :: x(2)
    if (x(1) /= "ab") error stop 10
    if (x(2) /= "cd") error stop 11
end subroutine

subroutine take_cs(x)
    character(kind=c_char), intent(in) :: x(4)
    if (x(1) /= 'p') error stop 12
    if (x(4) /= 's') error stop 13
end subroutine

end module

program array_of_strings_02
use array_of_strings_02_mod
implicit none
type(u) :: v
type(sq) :: w
character(len=20) :: line

print *, v%seq_part%s, v%seq_part%n, v%seq_part%ca
print *, v%bindc_part%c, v%bindc_part%n, v%bindc_part%cs, v%tag
print *, w%s, w%n, w%ca

if (v%seq_part%s /= "hello" .or. v%seq_part%n /= 3) error stop 1
if (v%seq_part%ca(1) /= "ab" .or. v%seq_part%ca(2) /= "cd") error stop 2
if (v%bindc_part%c /= 'z' .or. v%bindc_part%n /= 4) error stop 3
if (any(v%bindc_part%cs /= ['p', 'q', 'r', 's'])) error stop 4
if (v%tag /= "tagg") error stop 5
if (w%s /= "aaaaa" .or. w%n /= 1) error stop 6
if (w%ca(1) /= "xx" .or. w%ca(2) /= "yy") error stop 7

write(line, "(2a2)") v%seq_part%ca
if (line /= "abcd") error stop 8

call take_ca(v%seq_part%ca)
call take_cs(v%bindc_part%cs)

print *, "ok"
end program
