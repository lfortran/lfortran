! Module variables of derived types whose procedure pointer components have
! default initial targets (Fortran 2018 7.5.4.6): own type, an extended type
! whose parent component carries the default, and a type used from another
! module.
module proc_ptr_37_m1
    implicit none
    abstract interface
        integer function int_fn()
        end function
        subroutine scale_sub(i, r)
            integer, intent(in) :: i
            integer, intent(out) :: r
        end subroutine
    end interface
    type :: t
        integer :: k = 7
        procedure(int_fn), pointer, nopass :: fp => f
        procedure(scale_sub), pointer, nopass :: sp => twice
    end type
    type, extends(t) :: t2
        procedure(int_fn), pointer, nopass :: gp => g
        real :: x = 2.5
    end type
    type(t) :: s
    type(t2) :: s2
contains
    integer function f()
        f = 6
    end function
    integer function g()
        g = 11
    end function
    subroutine twice(i, r)
        integer, intent(in) :: i
        integer, intent(out) :: r
        r = 2*i
    end subroutine
end module

module proc_ptr_37_m2
    use proc_ptr_37_m1, only: t, t2
    implicit none
    type(t) :: u
    type(t2) :: u2
end module

program proc_ptr_37
    use proc_ptr_37_m1
    use proc_ptr_37_m2
    implicit none
    integer :: r

    if (s%k /= 7) error stop 1
    if (.not. associated(s%fp, f)) error stop 2
    if (s%fp() /= 6) error stop 3
    call s%sp(4, r)
    if (r /= 8) error stop 4

    if (s2%k /= 7) error stop 5
    if (s2%fp() /= 6) error stop 6
    if (s2%gp() /= 11) error stop 7
    if (abs(s2%x - 2.5) > 1e-6) error stop 8
    call s2%sp(5, r)
    if (r /= 10) error stop 9

    if (u%fp() /= 6) error stop 10
    call u%sp(3, r)
    if (r /= 6) error stop 11
    if (u2%t%fp() /= 6) error stop 12
    if (u2%gp() /= 11) error stop 13

    s%fp => g
    if (s%fp() /= 11) error stop 14
    if (u%fp() /= 6) error stop 15
    print *, s%fp(), s2%gp(), u%fp(), r
end program
