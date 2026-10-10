program separate_compilation_58
use separate_compilation_58a
implicit none
type(s), target :: x
type(t) :: w
class(s), allocatable :: cw
integer :: n, tot
tot = 0
allocate(cw, source=w)
do n = 1, 100
    call set_mod(n)
    tot = tot + mq%i
    nullify(mq)
    if (associated(mq)) error stop 1
    call alloc_mod(n)
    tot = tot + mq%i
    deallocate(mq)
    if (associated(mq)) error stop 2
    mq => x
    call clear_mod()
    if (associated(mq)) error stop 3
    call via_dummy(mq, cw)
    tot = tot + mq%i
    call dealloc_mod()
    if (associated(mq)) error stop 4
    allocate(mq, source=cw)
    call dealloc_mod()
    call via_dummy(mq, cw)
    deallocate(mq)
    call set_mod(n)
    mq => null()
end do
print *, tot
if (tot /= 10100) error stop 5
end program
