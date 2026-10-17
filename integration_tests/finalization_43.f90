module finalization_43_m
    implicit none
    integer :: finalized = 0
    integer :: last_value = 0

    type :: inner
        integer :: v = 0
    contains
        final :: finish
    end type

    ! No final subroutine of its own: finalizing it finalizes `in`.
    type :: outer
        type(inner) :: in
    end type

    type, extends(outer) :: outer2
    end type

    type :: holder
        type(inner), allocatable :: t
        class(inner), allocatable :: c
    end type

    type :: type_holder
        type(inner), allocatable :: t
    end type

contains

    subroutine finish(x)
        type(inner), intent(inout) :: x
        finalized = finalized + 1
        last_value = x%v
    end subroutine

    function make_outer(v) result(r)
        integer, intent(in) :: v
        type(outer), allocatable :: r
        allocate(r)
        r%in%v = v
    end function

    function make_outer2(v) result(r)
        integer, intent(in) :: v
        class(outer), allocatable :: r
        allocate(outer2 :: r)
        r%in%v = v
    end function

    function make_inner(v) result(r)
        integer, intent(in) :: v
        type(inner), allocatable :: r
        allocate(r)
        r%v = v
    end function

    function make_class_inner(v) result(r)
        integer, intent(in) :: v
        class(inner), allocatable :: r
        allocate(r)
        r%v = v
    end function

end module

program finalization_43
    use finalization_43_m
    implicit none
    type(outer) :: y
    type(outer), allocatable :: a
    class(outer), allocatable :: c, s
    type(inner) :: z
    type(holder) :: h, h2
    type(type_holder) :: k, k2

    ! F2018 7.5.6.3 p1: an unallocated allocatable variable of intrinsic
    ! assignment is not finalized, also when only a component of its type
    ! is finalizable.
    y%in%v = 1
    a = y
    if (finalized /= 0) error stop 1
    if (a%in%v /= 1) error stop 2
    y%in%v = 2
    a = y
    if (finalized /= 1) error stop 3
    if (last_value /= 1) error stop 4
    if (a%in%v /= 2) error stop 5

    ! The result of a function is finalized after the assignment (p5).
    deallocate(a)
    finalized = 0
    a = make_outer(3)
    if (finalized /= 1) error stop 6
    if (last_value /= 3) error stop 7
    if (a%in%v /= 3) error stop 8
    finalized = 0
    a = make_outer(4)
    if (finalized /= 2) error stop 9
    if (last_value /= 4) error stop 10
    if (a%in%v /= 4) error stop 11

    finalized = 0
    c = y
    if (finalized /= 0) error stop 12
    if (c%in%v /= 2) error stop 13
    deallocate(c)
    finalized = 0
    c = make_outer2(5)
    if (finalized /= 1) error stop 14
    if (last_value /= 5) error stop 15
    finalized = 0
    c = make_outer2(6)
    if (finalized /= 2) error stop 16
    if (last_value /= 6) error stop 17
    if (c%in%v /= 6) error stop 18

    ! A sourced allocation finalizes nothing.
    finalized = 0
    allocate(s, source=y)
    if (finalized /= 0) error stop 19
    if (s%in%v /= 2) error stop 20

    ! An unallocated allocatable component as the variable.
    z%v = 7
    finalized = 0
    h%t = z
    if (finalized /= 0) error stop 21
    if (h%t%v /= 7) error stop 22
    deallocate(h%t)
    finalized = 0
    h%t = make_inner(8)
    if (finalized /= 1) error stop 23
    if (last_value /= 8) error stop 24
    if (h%t%v /= 8) error stop 25
    finalized = 0
    h%c = z
    if (finalized /= 0) error stop 26
    if (h%c%v /= 7) error stop 27
    deallocate(h%c)
    finalized = 0
    h%c = make_class_inner(9)
    if (finalized /= 1) error stop 28
    if (last_value /= 9) error stop 29
    if (h%c%v /= 9) error stop 30

    ! The allocatable components of the variable are finalized only if
    ! they are allocated.
    finalized = 0
    h2 = h
    if (finalized /= 0) error stop 31
    if (h2%t%v /= 8 .or. h2%c%v /= 9) error stop 32
    allocate(k%t)
    k%t%v = 10
    k2 = k
    if (finalized /= 0) error stop 33
    if (k2%t%v /= 10) error stop 34
    k%t%v = 11
    k2 = k
    if (finalized /= 1) error stop 35
    if (last_value /= 10) error stop 36
    if (k2%t%v /= 11) error stop 37

    print *, "ok"
end program
