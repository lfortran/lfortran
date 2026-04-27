! Regression test: deep-copy of a recursively defined derived type with an
! allocatable component, where the LHS is a sub-component of the RHS
! (e.g., chain2%next = chain2). The intrinsic assignment must behave
! as-if the RHS is evaluated before the assignment, so a temporary copy
! of the RHS is required to avoid clobbering its contents during the
! deep-copy of nested allocatable members.
program allocatable_component_assign_recursive_01
    implicit none

    type recursive
        integer :: chk
        type(recursive), allocatable :: next
    end type recursive

    type(recursive) :: chain1, chain2

    chain1%chk  = 1
    chain2      = chain1
    chain1%chk  = 2
    chain2%next = chain1
    chain1%chk  = 3

    if (chain1%chk      /= 3) error stop
    if (chain2%chk      /= 1) error stop
    if (chain2%next%chk /= 2) error stop

    chain2%next = chain2

    if (chain2%chk           /= 1) error stop
    if (chain2%next%chk      /= 1) error stop
    if (chain2%next%next%chk /= 2) error stop
end program
