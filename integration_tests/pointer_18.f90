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

type(fixed_char_pointer) :: a
type(deferred_char_pointer) :: b
type(class_pointer) :: c
type(nullified_char_pointer) :: d
character(len=5), target :: chars(2)
integer, target :: ints(2)

chars = ["abcde", "fghij"]
ints = [1, 2]

if (associated(a%x)) error stop 1
if (associated(b%x)) error stop 2
if (associated(c%x)) error stop 3

nullify(d%x)
if (associated(d%x)) error stop 4

a%x => chars
if (.not. associated(a%x)) error stop 5
if (a%x(2) /= "fghij") error stop 6

c%x => ints
if (.not. associated(c%x)) error stop 7

end program
