! Disassociating SAVE, initialized and module scalar class pointers, directly
! and through pointer dummies, must not free their static class wrappers.
! A later ALLOCATE (also with SOURCE= or MOLD=) reuses the kept wrapper.
! Allocating and deallocating them only through pointer dummies must not leave
! a heap wrapper in their slots either.
module class_170_mod
implicit none
type :: s
    integer :: i = 0
end type
type, extends(s) :: t
    integer :: j = 5
end type
type :: holder
    class(s), pointer :: p => null()
end type
class(s), pointer :: mq => null()
class(*), pointer :: mu
class(s), pointer :: ms
class(s), pointer :: inst => null()
contains
    integer function dyn(p)
        class(s), intent(in) :: p
        select type (p)
        type is (t)
            dyn = p%j
        type is (s)
            dyn = -1
        class default
            dyn = -2
        end select
    end function

    integer function dyn_u(p)
        class(*), intent(in) :: p
        select type (p)
        type is (t)
            dyn_u = p%j
        type is (s)
            dyn_u = -1
        class default
            dyn_u = -2
        end select
    end function

    subroutine realloc_src(p, src, n)
        class(s), pointer, intent(inout) :: p
        class(s), intent(in) :: src
        integer, intent(in) :: n
        if (associated(p)) deallocate(p)
        if (associated(p)) error stop 32
        allocate(p, source=src)
        if (dyn(p) /= dyn(src)) error stop 33
        p%i = n
    end subroutine

    subroutine realloc_mold(p, src, n)
        class(s), pointer, intent(inout) :: p
        class(s), intent(in) :: src
        integer, intent(in) :: n
        if (associated(p)) deallocate(p)
        nullify(p)
        if (associated(p)) error stop 34
        allocate(p, mold=src)
        select type (p)
        type is (t)
        class default
            error stop 35
        end select
        p%i = n
    end subroutine

    subroutine save_src(n)
        integer, intent(in) :: n
        type(t) :: y
        class(s), allocatable :: cy
        class(s), pointer, save :: sq
        class(*), pointer, save :: su
        y%i = n
        y%j = 2*n
        allocate(cy, source=y)
        allocate(sq, source=y)
        if (sq%i /= n .or. dyn(sq) /= 2*n) error stop 36
        deallocate(sq)
        if (associated(sq)) error stop 37
        allocate(sq, source=cy)
        if (sq%i /= n .or. dyn(sq) /= 2*n) error stop 38
        deallocate(sq)
        allocate(sq, mold=cy)
        if (dyn(sq) < -1) error stop 39
        sq%i = n
        deallocate(sq)
        nullify(sq)
        if (associated(sq)) error stop 40
        allocate(sq, mold=y)
        if (dyn(sq) < -1) error stop 41
        deallocate(sq)
        call realloc_src(sq, cy, n)
        if (sq%i /= n .or. dyn(sq) /= 2*n) error stop 42
        call realloc_mold(sq, cy, n)
        if (sq%i /= n) error stop 43
        deallocate(sq)
        if (associated(sq)) error stop 44
        allocate(su, source=cy)
        if (dyn_u(su) /= 2*n) error stop 45
        deallocate(su)
        if (associated(su)) error stop 46
        allocate(su, source=y)
        if (dyn_u(su) /= 2*n) error stop 47
        deallocate(su)
        if (associated(su)) error stop 48
    end subroutine

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

    subroutine create(p, n)
        class(s), pointer, intent(out) :: p
        integer, intent(in) :: n
        allocate(t :: p)
        p%i = n
    end subroutine

    subroutine create2(p, n)
        class(s), pointer, intent(inout) :: p
        integer, intent(in) :: n
        call create(p, n)
    end subroutine

    subroutine destroy(p)
        class(s), pointer, intent(inout) :: p
        deallocate(p)
    end subroutine

    subroutine destroy2(p)
        class(s), pointer, intent(inout) :: p
        call destroy(p)
    end subroutine

    subroutine ucreate(p, n)
        class(*), pointer, intent(out) :: p
        integer, intent(in) :: n
        allocate(p, source=n)
    end subroutine

    subroutine udestroy(p)
        class(*), pointer, intent(inout) :: p
        deallocate(p)
    end subroutine

    integer function save_dummies(n)
        integer, intent(in) :: n
        class(s), pointer, save :: sp
        class(*), pointer, save :: su
        call create2(sp, n)
        save_dummies = dyn(sp) + sp%i
        call destroy2(sp)
        if (associated(sp)) error stop 60
        call ucreate(su, n)
        select type (su)
        type is (integer)
            save_dummies = save_dummies + su
        end select
        call udestroy(su)
        if (associated(su)) error stop 61
    end function

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
type(s), pointer :: tp
type(t) :: w
class(s), allocatable :: cw
integer, pointer :: ip
class(*), pointer :: u
integer :: n
x%i = 7
k = 3
do n = 1, 3
    call save_local(n)
    call heap_local(n)
    call save_src(n)
end do
w%i = 4
w%j = 9
allocate(cw, source=w)
nullify(ms)
do n = 1, 3
    call realloc_src(q, cw, n)
    if (q%i /= n .or. dyn(q) /= 9) error stop 50
    call realloc_mold(q, w, n)
    if (q%i /= n) error stop 51
    call realloc_src(ms, w, n)
    if (ms%i /= n .or. dyn(ms) /= 9) error stop 52
    call realloc_mold(ms, cw, n)
    deallocate(ms)
    if (associated(ms)) error stop 53
    allocate(ms, source=cw)
    if (dyn(ms) /= 9) error stop 54
    deallocate(ms)
    allocate(ms, mold=cw)
    deallocate(ms)
    nullify(ms)
end do
deallocate(q)
if (associated(q)) error stop 55
tp => null()
q => x
q => tp
if (associated(q)) error stop 56
ip => null()
u => k
u => ip
if (associated(u)) error stop 57
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
do n = 1, 3
    call create(inst, n)
    if (inst%i /= n .or. dyn(inst) /= 5) error stop 62
    call destroy(inst)
    if (associated(inst)) error stop 63
    if (save_dummies(n) /= 5 + 2*n) error stop 64
end do
print *, "ok"
end program
