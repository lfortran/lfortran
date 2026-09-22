! A declaration initializer of an array of a derived type is a constant
! expression, so it is static data rather than executable code. That is what
! lets a specification expression read it: a backend evaluates the bounds of a
! variable while it lays the procedure out, before any statement of the body
! runs, so an initialization turned into a statement would come too late and
! the bound would be evaluated against an uninitialized variable.
module global_init_07_m
    implicit none

    type :: extent
        integer :: n
        integer :: step = 1
    end type

    ! Read by a specification expression of a procedure below.
    type(extent) :: shared(2) = extent(3)

contains

    ! The initialized local is saved, and its value has to be in place before
    ! `b` is laid out, not on the first statement.
    subroutine local_bound()
        type(extent), save :: e(2) = extent(4, 2)
        integer :: b(e(1)%n)
        integer :: c(e(2)%step)

        if (size(b) /= 4) error stop 1
        if (size(c) /= 2) error stop 2
        b = 7
        c = 9
        if (sum(b) /= 28) error stop 3
        if (sum(c) /= 18) error stop 4
    end subroutine

    ! Without an explicit save attribute: an initialized local has it anyway.
    subroutine local_bound_implicit_save()
        type(extent) :: e(1) = extent(5)
        integer :: b(e(1)%n)

        if (size(b) /= 5) error stop 5
        b = 2
        if (sum(b) /= 10) error stop 6
    end subroutine

    ! The same for a module variable, read from a procedure that uses it.
    subroutine module_bound()
        integer :: b(shared(1)%n)

        if (size(b) /= 3) error stop 7
        b = 4
        if (sum(b) /= 12) error stop 8
    end subroutine

    ! The value is still initialized once and keeps what the previous call
    ! left in it, which is the save attribute of an initialized local.
    subroutine counts(expected)
        integer, intent(in) :: expected
        type(extent), save :: e(1) = extent(10)

        if (e(1)%n /= expected) error stop 9
        e(1)%n = e(1)%n + 1
    end subroutine

end module

program global_init_07
    use global_init_07_m
    implicit none

    call local_bound()
    call local_bound_implicit_save()
    call module_bound()

    call counts(10)
    call counts(11)
    call counts(12)

    ! Each element is a copy of the broadcast value, not an alias of one.
    if (shared(1)%n /= 3) error stop 10
    if (shared(2)%n /= 3) error stop 11
    if (shared(1)%step /= 1) error stop 12
    shared(1)%n = 99
    if (shared(2)%n /= 3) error stop 13
end program
