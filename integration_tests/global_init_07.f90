! A declaration initializer of an array of a derived type is a constant
! expression, so it is static data rather than executable code. That is what
! lets a specification expression read it: a backend evaluates the bounds of a
! variable while it lays the procedure out, before any statement of the body
! runs, so an initialization turned into a statement would come too late and
! the bound would be evaluated against an uninitialized variable.
!
! F2018 10.1.11 limits which variables a specification expression may read to
! those reached by use or host association, dummy arguments and common blocks,
! so every case below reads the initialized array across a scope boundary. A
! local variable of the same scoping unit could not appear there at all.
module global_init_07_m
    implicit none

    type :: extent
        integer :: n
        integer :: step = 1
    end type

    ! Read by the specification expressions below.
    type(extent) :: shared(2) = extent(3)

contains

    ! Host association: a module procedure reading its module's variable.
    subroutine module_bound()
        integer :: b(shared(1)%n)

        if (size(b) /= 3) error stop 1
        b = 4
        if (sum(b) /= 12) error stop 2
    end subroutine

    ! The value is initialized once and keeps what the previous call left in
    ! it, which is the save attribute of an initialized local.
    subroutine counts(expected)
        integer, intent(in) :: expected
        type(extent), save :: e(1) = extent(10)

        if (e(1)%n /= expected) error stop 3
        e(1)%n = e(1)%n + 1
    end subroutine

end module

! Use association: a procedure outside the module reading the same variable.
subroutine use_bound()
    use global_init_07_m
    implicit none
    integer :: b(shared(2)%n)
    integer :: c(shared(2)%step)

    if (size(b) /= 3) error stop 4
    if (size(c) /= 1) error stop 5
    b = 7
    c = 9
    if (sum(b) /= 21) error stop 6
    if (sum(c) /= 9) error stop 7
end subroutine

program global_init_07
    use global_init_07_m
    implicit none

    interface
        subroutine use_bound()
        end subroutine
    end interface

    ! Read by the internal procedure's specification expression below.
    type(extent) :: owned(1) = extent(5, 2)

    call module_bound()
    call use_bound()
    call host_bound()

    call counts(10)
    call counts(11)
    call counts(12)

    ! Each element is a copy of the broadcast value, not an alias of one.
    if (shared(1)%n /= 3) error stop 8
    if (shared(2)%n /= 3) error stop 9
    if (shared(1)%step /= 1) error stop 10
    shared(1)%n = 99
    if (shared(2)%n /= 3) error stop 11

contains

    ! Host association: an internal procedure reading its program's variable.
    subroutine host_bound()
        integer :: b(owned(1)%n)
        integer :: c(owned(1)%step)

        if (size(b) /= 5) error stop 12
        if (size(c) /= 2) error stop 13
        b = 3
        c = 6
        if (sum(b) /= 15) error stop 14
        if (sum(c) /= 12) error stop 15
    end subroutine

end program
