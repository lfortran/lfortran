! An `intent(out)` dummy array is default-initialized on entry (F2018 8.5.10),
! and that includes the components that are themselves arrays of a derived
! type: one with a default of its own (a single structure constructor, which
! is broadcast to every element) and one that takes the default of its type.
! A constructor that leaves out an allocatable component, or gives it
! `null()`, leaves that component unallocated in every element, and so does
! such a constructor used as the default of a scalar component.
! Allocated subcomponents of the elements of an array component are
! deallocated on entry, for array and scalar dummies alike (F2018 9.7.3.2),
! after the dummy has been finalized (F2018 7.5.6.3); an absent optional
! dummy is left alone.
module intent_out_array_default_init_03_mod
implicit none

type :: t
    integer :: h = 0
    real :: r = 0.0
end type t

type(t), parameter :: t0 = t(6, 3.5)

type :: base
    type(t) :: bp(0:2) = t(3, 1.5)
end type base

type, extends(base) :: u
    type(t) :: parts(2) = t(4, 2.5)
    type(t) :: grid(-1:0, 2) = t0
    type(t) :: plain(3)
    integer :: scal = 77
end type u

type :: w
    type(u) :: inner
    type(u) :: inners(2)
end type w

type :: ta
    integer :: h = 5
    integer, allocatable :: z(:)
end type ta

type :: va
    type(ta) :: c(2) = ta(8)
    type(ta) :: d(2) = ta(9, null())
    type(ta) :: e(2)
    type(ta) :: s = ta(8)
    type(ta) :: r = ta(9, null())
end type va

type :: tb
    type(ta) :: inner(2)
end type tb

type :: sbase
    type(tb) :: outer(2)
end type sbase

type, extends(sbase) :: sext
    integer :: k = 1
end type sext

type :: vf
    type(ta) :: c(2)
contains
    final :: fin_vf
end type vf

integer :: nfin = 0, nfin_dealloc = 0

contains

    subroutine check_u(x, code)
        type(u), intent(in) :: x
        integer, intent(in) :: code
        integer :: i, j
        if (x%scal /= 77) error stop code + 1
        do i = 1, 2
            if (x%parts(i)%h /= 4) error stop code + 2
            if (x%parts(i)%r /= 2.5) error stop code + 3
        end do
        do j = 1, 2
            do i = -1, 0
                if (x%grid(i, j)%h /= 6) error stop code + 4
                if (x%grid(i, j)%r /= 3.5) error stop code + 5
            end do
        end do
        do i = 1, 3
            if (x%plain(i)%h /= 0) error stop code + 6
            if (x%plain(i)%r /= 0.0) error stop code + 7
        end do
        do i = 0, 2
            if (x%bp(i)%h /= 3) error stop code + 8
            if (x%bp(i)%r /= 1.5) error stop code + 9
        end do
    end subroutine check_u

    subroutine spoil_u(x)
        type(u), intent(inout) :: x
        integer :: i, j
        x%scal = 99
        do i = 1, 2
            x%parts(i) = t(99, 99.0)
        end do
        do j = 1, 2
            do i = -1, 0
                x%grid(i, j) = t(99, 99.0)
            end do
        end do
        do i = 1, 3
            x%plain(i) = t(99, 99.0)
        end do
        do i = 0, 2
            x%bp(i) = t(99, 99.0)
        end do
    end subroutine spoil_u

    subroutine reset_assumed(a)
        type(u), intent(out) :: a(:)
        call check_u(a(1), 0)
        call check_u(a(2), 0)
    end subroutine reset_assumed

    subroutine reset_explicit(a)
        type(u), intent(out) :: a(2)
        call check_u(a(1), 10)
        call check_u(a(2), 10)
    end subroutine reset_explicit

    subroutine reset_nested(b)
        type(w), intent(out) :: b(:)
        call check_u(b(2)%inner, 20)
        call check_u(b(2)%inners(1), 30)
        call check_u(b(2)%inners(2), 30)
    end subroutine reset_nested

    subroutine reset_optional(a, code)
        type(u), intent(out), optional :: a(:)
        integer, intent(in) :: code
        if (present(a)) then
            call check_u(a(1), code)
            call check_u(a(2), code)
        end if
    end subroutine reset_optional

    subroutine reset_class(a)
        class(u), intent(out) :: a(:)
        call check_u(a(1), 50)
        call check_u(a(2), 50)
    end subroutine reset_class

    subroutine reset_alloc(a)
        type(va), intent(out) :: a(:)
        integer :: i, k
        do k = 1, size(a)
            do i = 1, 2
                if (a(k)%c(i)%h /= 8) error stop 61
                if (allocated(a(k)%c(i)%z)) error stop 62
                if (a(k)%d(i)%h /= 9) error stop 63
                if (allocated(a(k)%d(i)%z)) error stop 64
                if (a(k)%e(i)%h /= 5) error stop 65
                if (allocated(a(k)%e(i)%z)) error stop 66
            end do
            if (a(k)%s%h /= 8) error stop 67
            if (allocated(a(k)%s%z)) error stop 68
            if (a(k)%r%h /= 9) error stop 69
            if (allocated(a(k)%r%z)) error stop 60
        end do
    end subroutine reset_alloc

    subroutine check_sext(x, code)
        type(sext), intent(in) :: x
        integer, intent(in) :: code
        integer :: i, j
        if (x%k /= 1) error stop code + 1
        do i = 1, 2
            do j = 1, 2
                if (allocated(x%outer(i)%inner(j)%z)) error stop code + 2
            end do
        end do
    end subroutine check_sext

    subroutine reset_scalar(a)
        type(sext), intent(out) :: a
        call check_sext(a, 70)
    end subroutine reset_scalar

    subroutine reset_scalar_optional(a, code)
        type(sext), intent(out), optional :: a
        integer, intent(in) :: code
        if (present(a)) call check_sext(a, code)
    end subroutine reset_scalar_optional

    subroutine fin_vf(x)
        type(vf), intent(inout) :: x
        nfin = nfin + 1
        if (.not. allocated(x%c(1)%z)) nfin_dealloc = nfin_dealloc + 1
        if (.not. allocated(x%c(2)%z)) nfin_dealloc = nfin_dealloc + 1
    end subroutine fin_vf

    subroutine reset_final(a)
        type(vf), intent(out) :: a
        integer :: i
        if (nfin /= 1) error stop 91
        if (nfin_dealloc /= 0) error stop 92
        do i = 1, 2
            if (allocated(a%c(i)%z)) error stop 93
        end do
    end subroutine reset_final

end module intent_out_array_default_init_03_mod

program intent_out_array_default_init_03
use intent_out_array_default_init_03_mod
implicit none
type(u) :: q(2)
type(w) :: ww(2)
type(va) :: vv(2)
type(sext) :: ss
type(vf) :: ff
integer :: i, j, k

call spoil_u(q(1))
call spoil_u(q(2))
call reset_assumed(q)

call spoil_u(q(1))
call spoil_u(q(2))
call reset_explicit(q)

do i = 1, 2
    call spoil_u(ww(i)%inner)
    call spoil_u(ww(i)%inners(1))
    call spoil_u(ww(i)%inners(2))
end do
call reset_nested(ww)

call spoil_u(q(1))
call spoil_u(q(2))
call reset_optional(q, 40)
call reset_optional(code=40)

call spoil_u(q(1))
call spoil_u(q(2))
call reset_class(q)

do k = 1, 2
    do i = 1, 2
        vv(k)%c(i)%h = 99
        vv(k)%d(i)%h = 99
        vv(k)%e(i)%h = 99
        allocate(vv(k)%c(i)%z(3), vv(k)%d(i)%z(3), vv(k)%e(i)%z(3))
    end do
    vv(k)%s%h = 99
    vv(k)%r%h = 99
    allocate(vv(k)%s%z(3), vv(k)%r%z(3))
end do
call reset_alloc(vv)

do i = 1, 2
    do j = 1, 2
        allocate(ss%outer(i)%inner(j)%z(2))
    end do
end do
ss%k = 99
call reset_scalar(ss)
call check_sext(ss, 75)

do i = 1, 2
    do j = 1, 2
        allocate(ss%outer(i)%inner(j)%z(2))
    end do
end do
ss%k = 99
call reset_scalar_optional(ss, 80)
call check_sext(ss, 85)
call reset_scalar_optional(code=80)

do i = 1, 2
    allocate(ff%c(i)%z(3))
end do
call reset_final(ff)
if (nfin /= 1) error stop 94
if (allocated(ff%c(1)%z) .or. allocated(ff%c(2)%z)) error stop 95

print *, "ok"
end program intent_out_array_default_init_03
