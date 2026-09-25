module derived_types_199_mod
    implicit none

    integer :: nfin = 0
    integer :: nasg = 0

    type :: a_t
        integer :: x = 1
    contains
        final :: fin
    end type

    type :: asg_t
        integer :: x = 1
    contains
        procedure :: asg
        generic :: assignment(=) => asg
    end type

    type :: b_t
        type(asg_t) :: a
        integer :: y = 3
    end type

    type :: c_t
        type(b_t) :: b
        integer :: z = 4
    end type

    type :: plain_t
        integer :: p = 0
        integer :: q = 0
    end type

contains

    subroutine fin(this)
        type(a_t), intent(inout) :: this
        nfin = nfin + 1
    end subroutine

    subroutine asg(lhs, rhs)
        class(asg_t), intent(out) :: lhs
        type(asg_t), intent(in) :: rhs
        nasg = nasg + 1
        lhs%x = rhs%x + 100
    end subroutine

end module

program derived_types_199
    use derived_types_199_mod
    implicit none

    type(a_t) :: la, lb
    type(a_t), allocatable :: alloc_a, unalloc_a
    type(b_t) :: bc, bd
    type(c_t) :: cc
    type(plain_t) :: ap(2)

    ! F2018 7.5.6.3 p1: an intrinsic assignment finalizes the variable after
    ! the expression is evaluated and before the variable is defined. A
    ! structure constructor on the right-hand side is no exception.
    la%x = 5
    nfin = 0
    la = a_t(10)
    if (nfin /= 1) error stop "constructor assignment did not finalize the target"
    if (la%x /= 10) error stop "constructor assignment gave the wrong value"

    ! The same assignment with a variable on the right-hand side.
    lb%x = 7
    nfin = 0
    la = lb
    if (nfin /= 1) error stop "variable assignment did not finalize the target"
    if (la%x /= 7) error stop "variable assignment gave the wrong value"

    ! The expression is evaluated before the variable is finalized, so the
    ! constructor still reads the value the variable had.
    la%x = 3
    nfin = 0
    la = a_t(la%x + 1)
    if (nfin /= 1) error stop "self referencing constructor did not finalize"
    if (la%x /= 4) error stop "self referencing constructor gave the wrong value"

    ! F2018 10.2.1.3 p13: a nonpointer component of derived type that has a
    ! type-bound defined assignment is assigned through that assignment.
    nasg = 0
    bc = b_t(asg_t(10), 30)
    if (nasg /= 1) error stop "component defined assignment was not used"
    if (bc%a%x /= 110) error stop "component defined assignment gave the wrong value"
    if (bc%y /= 30) error stop "plain component of the constructor is wrong"

    ! The same assignment with a variable on the right-hand side.
    nasg = 0
    bd = bc
    if (nasg /= 1) error stop "component defined assignment not used for a variable"
    if (bd%a%x /= 210) error stop "component defined assignment of a variable is wrong"

    ! A component left out of the constructor takes its default value, and the
    ! components that are given still go through the defined assignment.
    nasg = 0
    bc = b_t(a=asg_t(7))
    if (nasg /= 1) error stop "component defined assignment not used with an omitted component"
    if (bc%a%x /= 107) error stop "omitted component constructor gave the wrong value"
    if (bc%y /= 3) error stop "omitted component did not take its default value"

    ! A nested constructor reaches the same component one level down.
    nasg = 0
    cc = c_t(b_t(asg_t(1), 2), 3)
    if (nasg /= 1) error stop "nested constructor did not use the defined assignment"
    if (cc%b%a%x /= 101) error stop "nested constructor gave the wrong value"
    if (cc%b%y /= 2) error stop "nested constructor plain component is wrong"
    if (cc%z /= 3) error stop "nested constructor outer component is wrong"

    ! An allocated allocatable variable is finalized like any other.
    allocate(alloc_a)
    alloc_a%x = 6
    nfin = 0
    alloc_a = a_t(20)
    if (nfin /= 1) error stop "allocated allocatable target was not finalized"
    if (alloc_a%x /= 20) error stop "allocated allocatable target has the wrong value"

    ! An unallocated allocatable variable is not finalized: it is allocated by
    ! the assignment and takes the value.
    unalloc_a = a_t(21)
    if (unalloc_a%x /= 21) error stop "unallocated allocatable target has the wrong value"

    ! An array variable takes the constructor broadcast over its elements.
    ap = plain_t(8, 9)
    if (ap(1)%p /= 8 .or. ap(1)%q /= 9) error stop "array target element 1 is wrong"
    if (ap(2)%p /= 8 .or. ap(2)%q /= 9) error stop "array target element 2 is wrong"

    print *, "ok"
end program
