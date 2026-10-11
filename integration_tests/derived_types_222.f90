module derived_types_222_mod
implicit none
integer :: nfin = 0

type :: comp
    integer :: k = 0
contains
    final :: fin_comp
end type

type :: with_final
    integer :: id = 0
    type(comp) :: d
    type(comp), allocatable :: c
end type

type :: with_strs
    character(len=:), allocatable :: c(:)
end type

type :: s
    integer :: i
end type

type :: inner
    class(s), pointer :: p => null()
end type

type :: outer
    type(inner) :: in
    type(inner) :: fa(3)
    class(*), pointer :: u => null()
end type

type, extends(outer) :: outer_ext
    class(s), pointer :: q => null()
end type

contains

subroutine fin_comp(x)
    type(comp), intent(inout) :: x
    nfin = nfin + 1
end subroutine

subroutine final_by_value(v)
    type(with_final), value :: v
    if (v%id /= 1) error stop
    v%id = 8
end subroutine

subroutine strs_by_value(v)
    type(with_strs), value :: v
    if (v%c(2) /= "qq") error stop
end subroutine

subroutine assign_by_value(v, w, y)
    type(outer_ext), value :: v
    type(outer_ext), intent(in) :: w
    type(s), target, intent(in) :: y
    integer, target, save :: n = 5
    integer :: k
    v%in%p => y
    v%fa(2)%p => y
    v%u => n
    v%q => y
    v = w
    do k = 1, 3
        if (associated(v%fa(k)%p)) error stop
    end do
    if (associated(v%u)) error stop
    v%fa(3)%p => y
end subroutine

subroutine nullify_by_value(v, y)
    type(outer_ext), value :: v
    type(s), target, intent(in) :: y
    v%in%p => y
    v%fa(1)%p => y
    v%q => y
    nullify(v%u)
end subroutine

end module

program derived_types_222
use derived_types_222_mod
implicit none
type(with_final) :: a
type(with_strs) :: b
type(s), target :: x, y
integer, target :: n
type(outer_ext) :: e, w
integer :: k, it

a%id = 1
allocate(a%c)
a%c%k = 2
nfin = 0
call final_by_value(a)
if (nfin /= 0) error stop
if (a%id /= 1 .or. a%c%k /= 2) error stop

allocate(character(len=2) :: b%c(2))
b%c = ["pp", "qq"]
call strs_by_value(b)
call strs_by_value(b)
if (b%c(1) /= "pp" .or. b%c(2) /= "qq") error stop

x%i = 1
y%i = 2
e%in%p => x
do k = 1, 3
    e%fa(k)%p => x
end do
e%u => n
e%q => x
do it = 1, 10
    call assign_by_value(e, w, y)
    call nullify_by_value(e, y)
    if (.not. associated(e%in%p, x)) error stop
    do k = 1, 3
        if (.not. associated(e%fa(k)%p, x)) error stop
    end do
    if (.not. associated(e%u, n)) error stop
    if (.not. associated(e%q, x)) error stop
    call nullify_by_value(w, y)
    if (associated(w%q) .or. associated(w%in%p)) error stop
end do
print *, "ok"
end program
