! intent(out) dummy array of a derived type with default initialization must be
! default-initialized on entry (F2018 8.5.10), just like the scalar case.

! The type's own module. Its component defaults name a constant declared here
! and build a structure constructor, neither of which is in scope in the module
! below, where the reset statements are emitted.
module intent_out_array_default_init_01_types
implicit none

integer, parameter :: kdef = 5

type :: cfg_t
    integer :: a = 1
    real :: b = 2.0
end type cfg_t

type :: p_t
    integer :: k = kdef
    type(cfg_t) :: cfg = cfg_t(7, 8.0)
end type p_t

end module intent_out_array_default_init_01_types

module intent_out_array_default_init_01_other
use intent_out_array_default_init_01_types
implicit none

contains

    subroutine reset_from_other_module(a)
        type(p_t), intent(out) :: a(:)
        if (any(a%k /= 5)) error stop 19
        if (a(1)%cfg%a /= 7) error stop 20
        if (a(1)%cfg%b /= 8.0) error stop 21
        if (a(2)%cfg%a /= 7) error stop 22
        if (a(2)%cfg%b /= 8.0) error stop 23
    end subroutine reset_from_other_module

end module intent_out_array_default_init_01_other

module intent_out_array_default_init_01_mod
implicit none

type :: inner_t
    integer :: ii = 9
end type inner_t

type :: base_t
    integer :: b = 3
end type base_t

type, extends(base_t) :: t
    integer :: k = 5
    ! No default initializer: its value on entry to an intent(out) dummy is
    ! undefined, so nothing below checks it. It is here so that the reset
    ! leaves it alone instead of zeroing the whole object.
    integer :: nodef
    type(inner_t) :: nest
    integer, pointer :: p => null()
    integer, allocatable :: buf(:)
end type t

contains

    subroutine reset_assumed_shape(a)
        type(t), intent(out) :: a(:)
        if (any(a%k /= 5)) error stop 1
        if (any(a%b /= 3)) error stop 2
        if (a(1)%nest%ii /= 9) error stop 3
        if (a(2)%nest%ii /= 9) error stop 4
        if (associated(a(1)%p)) error stop 5
        if (allocated(a(1)%buf)) error stop 6
        if (allocated(a(2)%buf)) error stop 7
    end subroutine reset_assumed_shape

    subroutine reset_explicit_shape(a)
        type(t), intent(out) :: a(2)
        if (any(a%k /= 5)) error stop 8
        if (any(a%b /= 3)) error stop 9
        if (a(1)%nest%ii /= 9) error stop 10
        if (associated(a(2)%p)) error stop 11
        if (allocated(a(2)%buf)) error stop 12
    end subroutine reset_explicit_shape

    subroutine reset_rank2(a)
        type(t), intent(out) :: a(:,:)
        if (any(a%k /= 5)) error stop 13
        if (a(2,2)%nest%ii /= 9) error stop 14
    end subroutine reset_rank2

    subroutine reset_optional(a)
        type(t), intent(out), optional :: a(:)
        if (present(a)) then
            if (any(a%k /= 5)) error stop 15
        end if
    end subroutine reset_optional

    subroutine reset_scalar(a)
        type(t), intent(out) :: a
        if (a%k /= 5) error stop 16
        if (a%b /= 3) error stop 17
        if (a%nest%ii /= 9) error stop 18
    end subroutine reset_scalar

end module intent_out_array_default_init_01_mod

program intent_out_array_default_init_01
use intent_out_array_default_init_01_mod
use intent_out_array_default_init_01_types
use intent_out_array_default_init_01_other
implicit none

type(t) :: w(2)
type(p_t) :: q(2)
type(t) :: m(2,2)
type(t), target :: s
integer, target :: tg = 1

call dirty(w)
w(1)%p => tg
allocate(w(1)%buf(3), w(2)%buf(3))
call reset_assumed_shape(w)

call dirty(w)
w(2)%p => tg
allocate(w(2)%buf(3))
call reset_explicit_shape(w)

m%k = 1
m(2,2)%nest%ii = 1
call reset_rank2(m)

call dirty(w)
call reset_optional(w)
call reset_optional()

s%k = 1
s%b = 1
s%nest%ii = 1
call reset_scalar(s)

q%k = 1
q(1)%cfg%a = 99
q(1)%cfg%b = 99.0
q(2)%cfg%a = 99
q(2)%cfg%b = 99.0
call reset_from_other_module(q)

print *, "ok"

contains

    subroutine dirty(a)
        type(t), intent(inout) :: a(:)
        integer :: i
        do i = 1, size(a)
            a(i)%k = 1
            a(i)%b = 1
            a(i)%nest%ii = 1
            a(i)%p => null()
            if (allocated(a(i)%buf)) deallocate(a(i)%buf)
        end do
    end subroutine dirty

end program intent_out_array_default_init_01
