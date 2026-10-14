module pointer_20_mod
implicit none

type :: t
    integer :: x = 0
end type

type, extends(t) :: t2
    integer :: y = 1
end type

type :: holder
    integer, allocatable :: a
end type

contains

    subroutine check_dummy(ib, iq, other)
    integer, allocatable, target, intent(in) :: ib, other
    integer, pointer, intent(in) :: iq
    if (.not. associated(iq, ib)) error stop 20
    if (associated(iq, other)) error stop 21
    end subroutine

end module

program pointer_20
use pointer_20_mod
implicit none

type(t), allocatable, target :: b, b2
type(t), pointer :: q
integer, allocatable, target :: ib, ib2
integer, pointer :: iq
real(8), allocatable, target :: rb
real(8), pointer :: rq
class(t), allocatable, target :: cb, cb2
class(t), pointer :: cq, cp
class(*), pointer :: up
type(holder), target :: h, h2

allocate(b, b2)
q => b
if (.not. associated(q, b)) error stop 1
if (associated(q, b2)) error stop 2

allocate(ib, ib2)
iq => ib
if (.not. associated(iq, ib)) error stop 3
if (associated(iq, ib2)) error stop 4
call check_dummy(ib, iq, ib2)

allocate(rb)
rq => rb
if (.not. associated(rq, rb)) error stop 5

allocate(t2 :: cb)
allocate(cb2)
cq => cb
if (.not. associated(cq, cb)) error stop 6
if (associated(cq, cb2)) error stop 7
q => cb
if (.not. associated(q, cb)) error stop 8
allocate(t2 :: cp)
cq => cp
if (.not. associated(cq, cp)) error stop 9

allocate(h%a, h2%a)
iq => h%a
if (.not. associated(iq, h%a)) error stop 10
if (associated(iq, h2%a)) error stop 11

up => ib
if (.not. associated(up, ib)) error stop 12
if (associated(up, ib2)) error stop 13
up => cb
if (.not. associated(up, cb)) error stop 14

nullify(iq)
if (associated(iq, ib)) error stop 15

print *, "ok"
end program
