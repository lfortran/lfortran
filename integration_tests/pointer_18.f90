program pointer_18
implicit none

type :: fixed_char_pointer
    character(len=5), pointer :: x(:) => null()
end type

type :: deferred_char_pointer
    character(len=:), pointer :: x(:) => null()
end type

type :: class_pointer
    class(*), pointer :: x(:) => null()
end type

type :: nullified_char_pointer
    character(len=5), pointer :: x(:)
end type

type :: scalar_pointer
    integer, pointer :: x => null()
end type

type(fixed_char_pointer) :: a
type(deferred_char_pointer) :: b
type(class_pointer) :: c
type(nullified_char_pointer) :: d
type(scalar_pointer) :: s
character(len=5), target :: chars(2), other_chars(2)
character(len=5), target :: dchars(2), other_dchars(2)
integer, target :: ints(2), other_ints(2)
integer, target :: i, j

chars = ["abcde", "fghij"]
other_chars = ["klmno", "pqrst"]
dchars = ["uvwxy", "zabcd"]
other_dchars = ["efghi", "jklmn"]
ints = [1, 2]
other_ints = [3, 4]
i = 5
j = 6

if (associated(a%x)) error stop 1
if (associated(b%x)) error stop 2
if (associated(c%x)) error stop 3
if (associated(s%x)) error stop 4

if (associated(a%x, chars)) error stop 5
if (associated(b%x, dchars)) error stop 6
if (associated(c%x, ints)) error stop 7
if (associated(s%x, i)) error stop 8

nullify(d%x)
if (associated(d%x)) error stop 9
if (associated(d%x, chars)) error stop 10

a%x => chars
if (.not. associated(a%x)) error stop 11
if (.not. associated(a%x, chars)) error stop 12
if (associated(a%x, other_chars)) error stop 13
if (a%x(2) /= "fghij") error stop 14

b%x => dchars
if (.not. associated(b%x)) error stop 15
if (.not. associated(b%x, dchars)) error stop 16
if (associated(b%x, other_dchars)) error stop 17

c%x => ints
if (.not. associated(c%x)) error stop 18
if (.not. associated(c%x, ints)) error stop 19
if (associated(c%x, other_ints)) error stop 20

s%x => i
if (.not. associated(s%x)) error stop 21
if (.not. associated(s%x, i)) error stop 22
if (associated(s%x, j)) error stop 23

end program
