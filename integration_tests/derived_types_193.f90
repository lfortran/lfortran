module derived_types_193_mod
implicit none

type :: inner_t
    integer :: n = 11
end type inner_t

type :: base_t
    integer :: i = 5
    real :: r = 2.5
    logical :: l = .true.
    character(len=3) :: c = "abc"
    type(inner_t) :: nested
    integer, allocatable :: av(:)
    integer, pointer :: p => null()
end type base_t

type, extends(base_t) :: ext_t
    integer :: m = 7
end type ext_t

contains

    ! Fortran 2018 8.5.10: every element of an intent(out) dummy array is
    ! default-initialized on entry, also when the dummy is polymorphic.
    subroutine reset_assumed_shape(a)
        class(base_t), intent(out) :: a(:)
        integer :: k
        do k = 1, size(a)
            if (a(k)%i /= 5) error stop 1
            if (a(k)%r /= 2.5) error stop 2
            if (.not. a(k)%l) error stop 3
            if (a(k)%c /= "abc") error stop 4
            if (a(k)%nested%n /= 11) error stop 5
            if (allocated(a(k)%av)) error stop 6
            if (associated(a(k)%p)) error stop 7
        end do
    end subroutine reset_assumed_shape

    subroutine reset_explicit_shape(a)
        class(base_t), intent(out) :: a(2, 2)
        integer :: p, q
        do q = 1, 2
            do p = 1, 2
                if (a(p, q)%i /= 5) error stop 8
                if (a(p, q)%r /= 2.5) error stop 9
                if (a(p, q)%nested%n /= 11) error stop 10
            end do
        end do
    end subroutine reset_explicit_shape

end module derived_types_193_mod

program derived_types_193
use derived_types_193_mod
implicit none
integer, target :: tgt = 3
type(base_t) :: v(3)
type(ext_t) :: w(2)
type(base_t) :: g(2, 2)
integer :: k

do k = 1, 3
    v(k)%i = 1
    v(k)%r = 9.0
    v(k)%l = .false.
    v(k)%c = "xyz"
    v(k)%nested%n = 99
    allocate(v(k)%av(3))
    v(k)%p => tgt
end do
call reset_assumed_shape(v)
if (v(1)%i /= 5) error stop 11
if (v(3)%r /= 2.5) error stop 12
if (v(2)%c /= "abc") error stop 13

! The declared type's defaults apply even when the dynamic type extends it.
do k = 1, 2
    w(k)%i = 1
    w(k)%r = 9.0
    w(k)%nested%n = 99
end do
call reset_assumed_shape(w)
if (w(2)%i /= 5) error stop 14
if (w(1)%nested%n /= 11) error stop 15

g%i = 1
g%r = 9.0
do k = 1, 2
    g(k, 1)%nested%n = 99
    g(k, 2)%nested%n = 99
end do
call reset_explicit_shape(g)
if (g(2, 2)%i /= 5) error stop 16

print *, "ok"
end program derived_types_193
