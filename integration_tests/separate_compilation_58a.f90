! An initialized module class pointer has one static class wrapper shared by
! all translation units; disassociating it in one must not free it.
module separate_compilation_58a
implicit none
type :: s
    integer :: i = 0
end type
type, extends(s) :: t
    integer :: j = 5
end type
class(s), pointer :: mq => null()
type(s), target :: my
contains
subroutine set_mod(n)
    integer, intent(in) :: n
    my%i = n
    mq => my
end subroutine
subroutine alloc_mod(n)
    integer, intent(in) :: n
    allocate(t :: mq)
    mq%i = n
end subroutine
subroutine clear_mod()
    nullify(mq)
end subroutine
subroutine dealloc_mod()
    deallocate(mq)
end subroutine
subroutine via_dummy(p, src)
    class(s), pointer, intent(inout) :: p
    class(s), intent(in) :: src
    allocate(p, source=src)
end subroutine
end module
