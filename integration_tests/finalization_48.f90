! An intrinsic assignment of a polymorphic value to an allocatable
! polymorphic scalar gives the variable the dynamic type of the value
! (F2018 10.2.1.3 p3). An allocated variable of another dynamic type is
! finalized with its dynamic type and deallocated first; one of the same
! dynamic type, as for `a = a`, is finalized and keeps its value; an
! unallocated one is not finalized (F2018 7.5.6.3 p1). This holds when the
! declared type of the value extends that of the variable, for components,
! array elements and class(*) variables.
module finalization_48_m
    implicit none
    integer :: nb = 0
    integer :: nc = 0
    integer :: ng = 0

    type :: base
        integer :: v = 0
    contains
        final :: finb
    end type

    type, extends(base) :: child
        integer :: w = 0
    contains
        final :: finc
    end type

    type, extends(child) :: grand
        integer :: z = 0
    contains
        final :: fing
    end type

    type :: holder
        class(base), allocatable :: c
        class(*), allocatable :: u
    end type
contains
    subroutine finb(x)
        type(base), intent(inout) :: x
        nb = nb + 1
    end subroutine

    subroutine finc(x)
        type(child), intent(inout) :: x
        nc = nc + 1
    end subroutine

    subroutine fing(x)
        type(grand), intent(inout) :: x
        ng = ng + 1
    end subroutine

    subroutine check(x, kind, v, b, c, g, code)
        class(base), intent(in) :: x
        integer, intent(in) :: kind, v, b, c, g, code
        integer :: k
        k = 0
        select type (x)
        type is (base)
            k = 1
        type is (child)
            k = 2
        type is (grand)
            k = 3
        end select
        if (k /= kind .or. x%v /= v) error stop code
        if (nb /= b .or. nc /= c .or. ng /= g) error stop code + 100
    end subroutine
end module

program finalization_48
    use finalization_48_m
    implicit none
    class(base), allocatable :: a, b, arr(:)
    class(child), allocatable :: c
    class(*), allocatable :: u, q
    type(holder) :: h, h2

    allocate(child :: b)
    b%v = 1
    a = b
    call check(a, 2, 1, 0, 0, 0, 1)

    ! The declared type of the value extends that of the variable.
    allocate(grand :: c)
    c%v = 2
    a = c
    call check(a, 3, 2, 1, 1, 0, 2)

    ! The same dynamic type: finalized, not reallocated.
    c%v = 3
    a = c
    call check(a, 3, 3, 2, 2, 1, 3)

    ! The value is the variable.
    a = a
    call check(a, 3, 3, 3, 3, 2, 4)

    deallocate(b)
    allocate(base :: b)
    b%v = 5
    a = b
    call check(a, 1, 5, 5, 5, 3, 5)

    allocate(child :: arr(2))
    arr(2)%v = 6
    a = arr(2)
    call check(a, 2, 6, 6, 5, 3, 6)

    h%c = c
    h2%c = h%c
    call check(h2%c, 3, 3, 6, 5, 3, 7)
    h2%c = h2%c
    select type (p => h2%c)
    type is (grand)
        if (p%v /= 3) error stop 8
    class default
        error stop 22
    end select

    q = 7
    u = q
    select type (u)
    type is (integer)
        if (u /= 7) error stop 9
    class default
        error stop 10
    end select
    q = "abcde"
    u = q
    select type (u)
    type is (character(*))
        if (u /= "abcde" .or. len(u) /= 5) error stop 11
    class default
        error stop 12
    end select
    q = "xy"
    u = q
    select type (u)
    type is (character(*))
        if (u /= "xy" .or. len(u) /= 2) error stop 13
    class default
        error stop 14
    end select
    u = u
    select type (u)
    type is (character(*))
        if (u /= "xy") error stop 15
    class default
        error stop 16
    end select
    u = c
    select type (u)
    type is (grand)
        if (u%v /= 3) error stop 17
    class default
        error stop 18
    end select
    nb = 0
    nc = 0
    ng = 0
    u = q
    if (nb /= 1 .or. nc /= 1 .or. ng /= 1) error stop 19
    h%u = u
    h%u = h%u
    select type (p => h%u)
    type is (character(*))
        if (p /= "xy") error stop 20
    class default
        error stop 21
    end select
    print *, "ok"
end program
