! An intrinsic assignment to an allocatable polymorphic scalar of a value
! that is not polymorphic gives the variable the type of the value
! (F2018 10.2.1.3 p3). An allocated variable is finalized with its dynamic
! type first (F2018 7.5.6.3 p1), and an unallocated one is not. This holds
! for class(t) and class(*) variables and components, a function result and
! a dummy argument, and for values of intrinsic type.
module finalization_47_m
    implicit none
    integer :: nf = 0
    integer :: nf2 = 0

    type :: t
        integer :: v = 0
    contains
        final :: fin
    end type

    type, extends(t) :: t2
        integer :: w = 0
    contains
        final :: fin2
    end type

    type :: holder
        class(t), allocatable :: c
        class(*), allocatable :: u
    end type
contains
    subroutine fin(x)
        type(t), intent(inout) :: x
        nf = nf + 1
    end subroutine

    subroutine fin2(x)
        type(t2), intent(inout) :: x
        nf2 = nf2 + 1
    end subroutine

    function make(n) result(r)
        integer, intent(in) :: n
        class(t), allocatable :: r
        r = t2(n, n + 1)
    end function

    subroutine set(a, n)
        class(t), allocatable, intent(inout) :: a
        integer, intent(in) :: n
        a = t(n)
    end subroutine
end module

program finalization_47
    use finalization_47_m
    implicit none
    class(t), allocatable :: a
    class(*), allocatable :: u
    character(:), allocatable :: s
    type(holder) :: h

    a = t(1)
    if (nf /= 0 .or. nf2 /= 0) error stop 1
    if (a%v /= 1) error stop 2
    a = t2(2, 3)
    if (nf /= 1 .or. nf2 /= 0) error stop 3
    select type (a)
    type is (t2)
        if (a%v /= 2 .or. a%w /= 3) error stop 4
    class default
        error stop 5
    end select
    a = t(4)
    if (nf /= 2 .or. nf2 /= 1) error stop 6
    select type (a)
    type is (t)
        if (a%v /= 4) error stop 7
    class default
        error stop 8
    end select
    a = t(a%v + 1)
    if (nf /= 3 .or. a%v /= 5) error stop 9

    u = 5
    select type (u)
    type is (integer)
        if (u /= 5) error stop 10
    class default
        error stop 11
    end select
    u = 2.5d0
    select type (u)
    type is (real(8))
        if (u /= 2.5d0) error stop 12
    class default
        error stop 13
    end select
    u = "abc"
    select type (u)
    type is (character(*))
        if (u /= "abc" .or. len(u) /= 3) error stop 14
    class default
        error stop 15
    end select
    s = "hello world"
    u = s
    select type (u)
    type is (character(*))
        if (u /= "hello world" .or. len(u) /= 11) error stop 16
    class default
        error stop 17
    end select
    u = .true.
    select type (u)
    type is (logical)
        if (.not. u) error stop 18
    class default
        error stop 19
    end select
    nf = 0
    nf2 = 0
    u = t2(7, 8)
    if (nf /= 0 .or. nf2 /= 0) error stop 20
    select type (u)
    type is (t2)
        if (u%v /= 7 .or. u%w /= 8) error stop 21
    class default
        error stop 22
    end select
    u = 9
    if (nf /= 1 .or. nf2 /= 1) error stop 23

    h%c = t2(10, 11)
    h%u = 12
    select type (q => h%c)
    type is (t2)
        if (q%v /= 10 .or. q%w /= 11) error stop 24
    class default
        error stop 25
    end select
    select type (q => h%u)
    type is (integer)
        if (q /= 12) error stop 26
    class default
        error stop 27
    end select

    deallocate(a)
    a = make(4)
    select type (a)
    type is (t2)
        if (a%v /= 4 .or. a%w /= 5) error stop 28
    class default
        error stop 29
    end select
    call set(a, 6)
    select type (a)
    type is (t)
        if (a%v /= 6) error stop 30
    class default
        error stop 31
    end select
    print *, "ok"
end program
