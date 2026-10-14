module pointer_21_mod
implicit none

type :: holder
    character(:), allocatable :: s
    character(:), pointer :: ps
    character(4) :: fs
end type

contains

    subroutine check_dummy(s, q, expected)
    character(*), target, intent(in) :: s
    character(:), pointer, intent(in) :: q
    logical, intent(in) :: expected
    if (associated(q, s) .neqv. expected) error stop 20
    end subroutine

end module

program pointer_21
use pointer_21_mod
implicit none

character(4), target :: f, f2, arr(3)
character(:), allocatable, target :: a, a2
character(:), pointer :: sq, pp
character(4), pointer :: fq
character(2), pointer :: s2
class(*), pointer :: up
type(holder), target :: h, h2

allocate(character(4) :: a, a2, pp, h%s, h%ps, h2%s)

fq => f
if (.not. associated(fq, f)) error stop 1
if (associated(fq, f2)) error stop 2

sq => a
if (.not. associated(sq, a)) error stop 3
if (associated(sq, a2)) error stop 4

sq => pp
if (.not. associated(sq, pp)) error stop 5

fq => arr(2)
if (.not. associated(fq, arr(2))) error stop 6
if (associated(fq, arr(3))) error stop 7

s2 => f(2:3)
if (.not. associated(s2, f(2:3))) error stop 8
if (associated(s2, f(1:2))) error stop 9

fq => h%fs
if (.not. associated(fq, h%fs)) error stop 10

sq => h%s
if (.not. associated(sq, h%s)) error stop 11
if (associated(sq, h2%s)) error stop 12

sq => h%ps
if (.not. associated(sq, h%ps)) error stop 13

sq => f
call check_dummy(f, sq, .true.)
call check_dummy(f2, sq, .false.)

up => f
if (.not. associated(up, f)) error stop 14
up => a
if (.not. associated(up, a)) error stop 15
up => h%s
if (.not. associated(up, h%s)) error stop 16

print *, "ok"
end program
