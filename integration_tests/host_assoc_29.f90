! An internal procedure passes a host explicit-shape or assumed-size dummy
! array to a type-bound procedure that also has an assumed-shape dummy
module host_assoc_29_m
implicit none
type :: t
    real(8) :: s = 0
contains
    procedure :: initialize => t_init
    generic :: init => initialize
end type
contains
    subroutine t_init(me, n, lb, d)
        class(t), intent(inout) :: me
        integer, intent(in) :: n
        real(8), dimension(n), intent(in) :: lb
        real(8), dimension(:), intent(in), optional :: d
        me%s = sum(lb)
        if (present(d)) me%s = me%s + 100 * sum(d)
    end subroutine

    subroutine host(sout, n, lb)
        real(8), intent(out) :: sout
        integer, intent(in) :: n
        real(8), dimension(n), intent(in) :: lb
        type(t) :: w
        call init_it()
        sout = w%s
    contains
        subroutine init_it()
            call w%initialize(n=n, lb=lb, d=[1.0d0])
        end subroutine init_it
    end subroutine host

    subroutine host_generic(sout, n, lb)
        real(8), intent(out) :: sout
        integer, intent(in) :: n
        real(8), dimension(n), intent(in) :: lb
        type(t) :: w
        call init_it()
        sout = w%s
    contains
        subroutine init_it()
            call w%init(n, lb, [1.0d0, 2.0d0])
        end subroutine init_it
    end subroutine host_generic

    subroutine host_assumed_size(sout, n, lb)
        real(8), intent(out) :: sout
        integer, intent(in) :: n
        real(8), intent(in) :: lb(*)
        type(t) :: w
        call init_it()
        sout = w%s
    contains
        subroutine init_it()
            call w%initialize(n, lb, [2.0d0])
        end subroutine init_it
    end subroutine host_assumed_size
end module host_assoc_29_m

program host_assoc_29
use host_assoc_29_m
implicit none
real(8) :: r
call host(r, 3, [1.0d0, 2.0d0, 3.0d0])
if (abs(r - 106.0d0) > 1d-12) error stop 1
call host_generic(r, 2, [4.0d0, 5.0d0])
if (abs(r - 309.0d0) > 1d-12) error stop 2
call host_assumed_size(r, 2, [4.0d0, 5.0d0])
if (abs(r - 209.0d0) > 1d-12) error stop 3
print *, "ok"
end program host_assoc_29
