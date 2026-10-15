module derived_types_232_m
implicit none
integer, target :: tgt = 42
integer, target :: tarr(3) = [10, 20, 30]

type :: u
    integer :: x = 5
end type
type(u), target :: uobj

type :: base_t
    integer, pointer :: bp => tarr(2)
end type

type, extends(base_t) :: t
    integer :: k = 7
    integer, pointer :: p => tgt
    integer, pointer :: px => uobj%x
    type(u), pointer :: q => uobj
    integer, pointer :: n => null()
end type

type :: outer
    type(t) :: in
    integer :: j = 3
end type

type(t) :: s
type(t) :: arr(3)
type(outer) :: o
type(outer) :: oarr(2)

contains

subroutine check(a)
    type(t), intent(in) :: a
    if (a%k /= 7) error stop 10
    if (.not. associated(a%p, tgt)) error stop 11
    if (.not. associated(a%bp, tarr(2))) error stop 12
    if (.not. associated(a%px, uobj%x)) error stop 13
    if (.not. associated(a%q, uobj)) error stop 14
    if (associated(a%n)) error stop 15
    if (a%p + a%bp + a%px + a%q%x /= 72) error stop 16
end subroutine

end module

program derived_types_232
use derived_types_232_m
implicit none
type(t) :: loc
integer :: i

call check(s)
do i = 1, size(arr)
    call check(arr(i))
end do
if (o%j /= 3) error stop 1
call check(o%in)
do i = 1, size(oarr)
    if (oarr(i)%j /= 3) error stop 2
    call check(oarr(i)%in)
end do
call check(loc)

arr(1)%p => tarr(3)
if (arr(1)%p /= 30) error stop 3
call check(arr(2))
tgt = 43
if (s%p /= 43 .or. arr(3)%p /= 43) error stop 4
print *, s%k, s%p, arr(3)%bp, o%in%q%x
end program
