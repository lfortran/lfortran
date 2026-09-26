! An `intent(out)` dummy array is default-initialized on entry (F2018 8.5.10),
! and that includes the components that are themselves arrays of a derived
! type: one with a default of its own (a single structure constructor, which
! is broadcast to every element) and one that takes the default of its type.
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

end module intent_out_array_default_init_03_mod

program intent_out_array_default_init_03
use intent_out_array_default_init_03_mod
implicit none
type(u) :: q(2)
type(w) :: ww(2)
integer :: i

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

print *, "ok"
end program intent_out_array_default_init_03
