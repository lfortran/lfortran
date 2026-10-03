! A component default that is itself a nested structure constructor
module derived_type_with_default_init_07_mod
implicit none

type :: inner_t
    character(len=5) :: s = "zzzzz"
    integer, allocatable :: al(:)
    integer :: k = 0
end type

type :: mid_t
    type(inner_t) :: i = inner_t("hello", null(), 3)
    character(len=2) :: c = "aa"
    integer :: n = 1
end type

type :: outer_t
    type(mid_t) :: m = mid_t(inner_t("world", null(), 9), "bb", 4)
    integer :: y = 2
end type

contains

subroutine check_inner(i, s, k)
    type(inner_t), intent(in) :: i
    character(len=*), intent(in) :: s
    integer, intent(in) :: k
    if (i%s /= s) error stop 1
    if (allocated(i%al)) error stop 2
    if (i%k /= k) error stop 3
end subroutine

subroutine check_mid(m)
    type(mid_t), intent(in) :: m
    call check_inner(m%i, "hello", 3)
    if (m%c /= "aa") error stop 4
    if (m%n /= 1) error stop 5
end subroutine

! The nested constructor of `outer_t` replaces every default of `mid_t`,
! including the one `mid_t` gives its own `inner_t` component.
subroutine check_outer(o)
    type(outer_t), intent(in) :: o
    call check_inner(o%m%i, "world", 9)
    if (o%m%c /= "bb") error stop 6
    if (o%m%n /= 4) error stop 7
    if (o%y /= 2) error stop 8
end subroutine

subroutine check_locals()
    type(mid_t) :: lm
    type(outer_t) :: lo
    call check_mid(lm)
    call check_outer(lo)
end subroutine

end module

program derived_type_with_default_init_07
use derived_type_with_default_init_07_mod
implicit none
type(mid_t) :: pm
type(outer_t) :: po
call check_mid(pm)
call check_outer(po)
call check_locals()
print *, "ok"
end program
