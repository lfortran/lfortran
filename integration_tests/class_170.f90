! Disassociating SAVE, initialized and module scalar class pointers, directly
! and through pointer dummies, must not free their static class wrappers.
module class_170_mod
implicit none
type :: s
    integer :: i = 0
end type
type :: holder
    class(s), pointer :: p => null()
end type
class(s), pointer :: mq => null()
class(*), pointer :: mu
contains
    subroutine clear(p)
        class(s), pointer, intent(inout) :: p
        nullify(p)
        if (associated(p)) error stop 1
    end subroutine

    subroutine clear_null(p)
        class(s), pointer, intent(inout) :: p
        p => null()
        if (associated(p)) error stop 2
    end subroutine

    subroutine clear_u(p)
        class(*), pointer, intent(inout) :: p
        nullify(p)
        if (associated(p)) error stop 3
    end subroutine

    subroutine reassoc(p, t)
        class(s), pointer, intent(inout) :: p
        type(s), target, intent(in) :: t
        nullify(p)
        p => t
    end subroutine

    subroutine alloc_dealloc(p, n)
        class(s), pointer, intent(inout) :: p
        integer, intent(in) :: n
        allocate(p)
        p%i = n
        if (p%i /= n) error stop 27
        deallocate(p)
        if (associated(p)) error stop 28
    end subroutine

    subroutine save_local(n)
        integer, intent(in) :: n
        type(s), target, save :: y
        class(s), pointer, save :: sq
        integer, target, save :: k
        class(*), pointer, save :: su
        class(s), pointer :: r
        y%i = n
        sq => y
        nullify(sq)
        if (associated(sq)) error stop 4
        sq => y
        if (.not. associated(sq, y)) error stop 5
        call clear(sq)
        if (associated(sq)) error stop 6
        sq => y
        call clear_null(sq)
        if (associated(sq)) error stop 7
        call reassoc(sq, y)
        if (.not. associated(sq, y)) error stop 8
        if (sq%i /= n) error stop 9
        sq => null()
        if (associated(sq)) error stop 10
        allocate(r)
        r%i = n
        sq => r
        if (sq%i /= n) error stop 29
        deallocate(sq)
        if (associated(sq)) error stop 30
        sq => y
        call clear(sq)
        call alloc_dealloc(sq, n)
        allocate(sq)
        sq%i = n
        deallocate(sq)
        if (associated(sq)) error stop 31
        k = n
        su => k
        nullify(su)
        if (associated(su)) error stop 11
        su => k
        call clear_u(su)
        if (associated(su)) error stop 12
    end subroutine

    subroutine heap_local(n)
        integer, intent(in) :: n
        type(s), target :: y, z
        class(s), pointer :: lp
        type(holder) :: h
        y%i = n
        z%i = n + 1
        lp => y
        call clear(lp)
        if (associated(lp)) error stop 13
        call reassoc(lp, z)
        if (.not. associated(lp, z)) error stop 14
        if (lp%i /= n + 1) error stop 15
        call clear_null(lp)
        if (associated(lp)) error stop 16
        lp => y
        if (.not. associated(lp, y)) error stop 17
        h%p => y
        call clear(h%p)
        if (associated(h%p)) error stop 18
        h%p => z
        if (h%p%i /= n + 1) error stop 19
        nullify(h%p)
        lp => y
        call clear(lp)
        call alloc_dealloc(lp, n)
    end subroutine
end module

program class_170
use class_170_mod
implicit none
type(s), target :: x
integer, target :: k
class(s), pointer :: q => null()
integer :: n
x%i = 7
k = 3
do n = 1, 3
    call save_local(n)
    call heap_local(n)
end do
q => x
nullify(q)
if (associated(q)) error stop 20
q => x
call clear(q)
if (associated(q)) error stop 21
call reassoc(q, x)
if (q%i /= 7) error stop 22
mq => x
nullify(mq)
if (associated(mq)) error stop 23
mq => x
call clear(mq)
if (associated(mq)) error stop 24
mq => x
mq => null()
if (associated(mq)) error stop 25
mu => k
call clear_u(mu)
if (associated(mu)) error stop 26
print *, "ok"
end program
