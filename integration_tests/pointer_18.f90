module pointer_18_mod
implicit none

character(len=5), pointer :: module_fixed(:) => null()
character(len=:), pointer :: module_deferred(:) => null()
class(*), pointer :: module_class(:) => null()

contains

subroutine check_module_default_null()
character(len=5), target :: chars(2)
character(len=5), target :: dchars(2)
integer, target :: ints(2)

chars = ["abcde", "fghij"]
dchars = ["uvwxy", "zabcd"]
ints = [1, 2]

if (associated(module_fixed)) error stop 24
if (associated(module_deferred)) error stop 25
if (associated(module_class)) error stop 26
if (associated(module_fixed, chars)) error stop 27
if (associated(module_deferred, dchars)) error stop 28
end subroutine

subroutine check_dummy_null(fixed, deferred, unlimited)
character(len=5), pointer :: fixed(:)
character(len=:), pointer :: deferred(:)
class(*), pointer :: unlimited(:)

if (associated(fixed)) error stop 39
if (associated(deferred)) error stop 40
if (associated(unlimited)) error stop 41
end subroutine

end module

program pointer_18
use pointer_18_mod
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
character(len=5), pointer :: local_fixed(:)
character(len=:), pointer :: local_deferred(:)
class(*), pointer :: local_class(:)
character(len=5), target :: chars(2), other_chars(2)
character(len=5), target :: dchars(2), other_dchars(2)
integer, target :: ints(2), other_ints(2)
integer, target :: i, j

call check_module_default_null()
call check_save_default_null()

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

nullify(local_fixed, local_deferred, local_class)
if (associated(local_fixed)) error stop 45
if (associated(local_deferred)) error stop 46
if (associated(local_class)) error stop 47
if (associated(local_fixed, chars)) error stop 48
if (associated(local_deferred, dchars)) error stop 49

call check_dummy_null(local_fixed, local_deferred, local_class)

local_fixed => chars
if (.not. associated(local_fixed)) error stop 51
if (.not. associated(local_fixed, chars)) error stop 52
if (associated(local_fixed, other_chars)) error stop 53

local_deferred => dchars
if (.not. associated(local_deferred)) error stop 54
if (.not. associated(local_deferred, dchars)) error stop 55
if (associated(local_deferred, other_dchars)) error stop 56

local_class => ints
if (.not. associated(local_class)) error stop 57

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

contains

subroutine check_save_default_null()
    character(len=5), pointer, save :: save_fixed(:) => null()
    character(len=:), pointer, save :: save_deferred(:) => null()
    class(*), pointer, save :: save_class(:) => null()
    character(len=5), target :: save_chars(2)
    character(len=5), target :: save_dchars(2)
    integer, target :: save_ints(2)

    save_chars = ["abcde", "fghij"]
    save_dchars = ["uvwxy", "zabcd"]
    save_ints = [1, 2]

    if (associated(save_fixed)) error stop 60
    if (associated(save_deferred)) error stop 61
    if (associated(save_class)) error stop 62
    if (associated(save_fixed, save_chars)) error stop 63
    if (associated(save_deferred, save_dchars)) error stop 64
end subroutine

end program
