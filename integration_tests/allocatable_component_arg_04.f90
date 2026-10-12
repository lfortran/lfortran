! Polymorphic pointer components of an allocatable component passed to a
! procedure, which associates, nullifies or re-associates them.
module allocatable_component_arg_04_mod
implicit none
type :: base
    integer :: i = 0
end type
type, extends(base) :: ext
    integer :: j = 0
end type
type :: item_t
    class(base), pointer :: p(:) => null()
    class(base), pointer :: s => null()
    integer :: n = 0
end type
type :: holder_t
    type(item_t), allocatable :: item
    class(base), allocatable :: arr(:)
end type
type(ext), target :: tgt(3), tgt2(2)
type(ext), target :: st
contains
subroutine assoc_none(it)
    type(item_t) :: it
    it%p => tgt
    it%s => st
    it%n = it%n + 1
end subroutine
subroutine nullify_inout(it)
    type(item_t), intent(inout) :: it
    if (.not. associated(it%p)) error stop 1
    nullify(it%p)
    it%s => null()
    it%n = it%n + 10
end subroutine
subroutine reassoc(it)
    type(item_t) :: it
    it%p => tgt2
    it%p => tgt
    it%p => null()
    it%p => tgt2
end subroutine
subroutine ptr_dummy(q)
    class(base), pointer :: q(:)
    if (.not. associated(q)) error stop 2
    q => tgt
    nullify(q)
    q => tgt2
end subroutine
subroutine opt_arr(a, n)
    class(base), optional :: a(:)
    integer, intent(in) :: n
    if (n == 0) then
        if (present(a)) error stop 3
    else
        if (.not. present(a)) error stop 4
        if (size(a) /= n) error stop 5
        select type (a)
        type is (ext)
            a(1)%j = a(1)%j + 7
        end select
    end if
end subroutine
subroutine by_value(it)
    type(item_t), value :: it
    if (.not. associated(it%p)) error stop 6
    it%n = 99
end subroutine
end module

program allocatable_component_arg_04
use allocatable_component_arg_04_mod
implicit none
type(holder_t) :: h
integer :: k
tgt%i = [1, 2, 3]
tgt2%i = [4, 5]
do k = 1, 3
    allocate(h%item)
    call assoc_none(h%item)
    if (.not. associated(h%item%p, tgt)) error stop 10
    if (.not. associated(h%item%s, st)) error stop 11
    if (h%item%n /= 1) error stop 12
    call by_value(h%item)
    if (h%item%n /= 1) error stop 13
    call nullify_inout(h%item)
    if (associated(h%item%p)) error stop 14
    if (associated(h%item%s)) error stop 15
    if (h%item%n /= 11) error stop 16
    call reassoc(h%item)
    if (.not. associated(h%item%p, tgt2)) error stop 17
    if (size(h%item%p) /= 2) error stop 18
    if (h%item%p(2)%i /= 5) error stop 19
    call ptr_dummy(h%item%p)
    if (.not. associated(h%item%p, tgt2)) error stop 20
    allocate(ext :: h%arr(4))
    call opt_arr(h%arr, 4)
    select type (a => h%arr)
    type is (ext)
        if (a(1)%j /= 7) error stop 21
    end select
    nullify(h%item%p)
    deallocate(h%arr)
    deallocate(h%item)
end do
print *, "ok"
end program
