! Reallocation on assignment of an allocatable array that an earlier
! assignment of the same statement sequence gave the shape of the value,
! with nothing in between that can change either, and of arrays that
! something in between can change, which must still be reallocated.
module realloc_lhs_35_mod
    implicit none
    type :: state_t
        real, allocatable :: x(:)
    end type
    real, allocatable :: g(:)
contains
    pure real function scaled(a)
        real, intent(in) :: a
        scaled = 2*a
    end function

    real function grow_g(a)
        real, intent(in) :: a
        g = [g, a]
        grow_g = a
    end function

    subroutine resize(a, n)
        real, allocatable, intent(inout) :: a(:)
        integer, intent(in) :: n
        deallocate(a)
        allocate(a(n))
        a = 9
    end subroutine

    elemental real function opt_add(a, b)
        real, intent(in) :: a
        real, intent(in), optional :: b
        if (present(b)) then
            opt_add = a + b
        else
            opt_add = a
        end if
    end function

    subroutine absent_operand(a1, a2, b)
        real, intent(in) :: a1(:), a2(:)
        real, intent(in), optional :: b(:)
        real, allocatable :: c(:), d(:)
        c = opt_add(a1, b)
        d = opt_add(a2, b)
        c = d
        if (size(c) /= size(a2) .or. any(c /= a2)) error stop 30
    end subroutine
end module

program realloc_lhs_35
    use realloc_lhs_35_mod
    implicit none
    real, allocatable :: u(:), un(:), a(:), b(:), c(:), d(:)
    type(state_t) :: s, t
    real :: r
    integer :: n, k

    ! Straight-line copies: the second assignment keeps the shape
    allocate(u(0:4))
    u = [1., 2., 3., 4., 5.]
    n = 5
    un = u
    un(1:n-2) = u(0:n-3) + 0.5*(u(1:n-2) - u(0:n-3))
    r = scaled(un(0))
    u = un
    if (lbound(u, 1) /= 0 .or. ubound(u, 1) /= 4) error stop 1
    if (any(u /= [1., 1.5, 2.5, 3.5, 5.])) error stop 2
    if (lbound(un, 1) /= 0 .or. r /= 2) error stop 3

    a = [1., 2., 3.]
    b = [4., 5., 6.]
    c = a + b
    d = c
    a = d*2
    if (any(a /= [10., 14., 18.]) .or. size(a) /= 3) error stop 4

    ! A component, through its variable
    s%x = u
    u = s%x + 1
    if (size(u) /= 5 .or. lbound(u, 1) /= 0) error stop 5

    ! The shape of the target changes in between: reallocated
    un = u
    call resize(u, 2)
    u = un
    if (size(u) /= 5 .or. lbound(u, 1) /= 0 .or. u(0) /= 2) error stop 6

    un = u
    u = [u, 7.]
    un = u
    if (size(un) /= 6 .or. un(6) /= 7) error stop 7

    un = u
    deallocate(u)
    allocate(u(2))
    u = 3
    un = u
    if (size(un) /= 2 .or. any(un /= 3)) error stop 8

    ! The shape of the value changes in between
    g = [1., 2.]
    a = g
    r = grow_g(5.)
    a = g
    if (size(a) /= 3 .or. a(3) /= 5) error stop 9

    ! An assignment of the variable of a component in between
    t%x = [1., 2., 3., 4.]
    s%x = a
    s = t
    a = s%x
    if (size(a) /= 4 .or. any(a /= [1., 2., 3., 4.])) error stop 10

    ! A conditional in between
    un = u
    if (size(u) > 1) u = [u, u]
    un = u
    if (size(un) /= 4) error stop 11

    ! A jump back to a label in between
    k = 0
    un = a
10  continue
    a = un
    k = k + 1
    if (size(a) /= size(un)) error stop 12
    if (k < 2) then
        deallocate(un)
        un = [1., 2.]
        goto 10
    end if
    if (size(a) /= 2) error stop 13

    ! Each iteration of a loop starts afresh
    a = [1.]
    do k = 1, 3
        un = a
        a = [a, a]
        a = un
        if (size(a) /= 2**(k - 1)) error stop 14
        a = [a, a]
    end do
    if (size(a) /= 8) error stop 15

    ! An absent optional operand has no shape
    call absent_operand([1., 2., 3.], [4., 5.])

    print *, "ok"
end program
