! Constraints on a Fortran 2023 conditional argument (15.5.1 R1526):
!
!     conditional-arg  is  ( scalar-logical-expr ? consequent
!                            [ : scalar-logical-expr ? consequent ]... : consequent )
!     consequent       is  expr  or  variable  or  .NIL.
!
! The consequent that is chosen *is* the actual argument (15.5.2.3), so the
! constraints on a consequent are the constraints on an actual argument, plus
! the ones that come from there being several of them.
module conditional_arg_1
    implicit none

    interface gen
        module procedure gen_allocatable, gen_pointer
    end interface
contains
    subroutine required(x)
        integer, intent(in) :: x
        print *, x
    end subroutine

    subroutine optional_arg(x)
        integer, optional, intent(in) :: x
        print *, present(x)
    end subroutine

    subroutine defined(x)
        integer, intent(out) :: x
        x = 0
    end subroutine

    subroutine gen_allocatable(x)
        integer, allocatable, intent(in) :: x(:)
        print *, size(x)
    end subroutine

    subroutine gen_pointer(x)
        integer, pointer, intent(in) :: x(:)
        print *, size(x)
    end subroutine

    integer function returns_one(x)
        integer, intent(in) :: x
        returns_one = x
    end function

    subroutine two_required(x, y)
        integer, intent(in) :: x, y
        print *, x, y
    end subroutine

    ! C1540: a consequent may be `.NIL.` only when the dummy argument it
    ! corresponds to is optional, since `.NIL.` leaves it not present.
    subroutine nil_for_a_required_dummy()
        implicit none
        integer :: a
        a = 1
        call required( ( .true. ? a : .nil. ) )  ! {Error} `.nil.` is not allowed for the dummy argument `x`, which is not optional
    end subroutine

    ! C1540 again, for a function reference: the expansion is a conditional
    ! expression of the copies rather than an If statement, and the dummy
    ! argument each copy corresponds to is checked the same way.
    subroutine nil_for_a_required_dummy_of_a_function()
        implicit none
        integer :: a, r
        a = 1
        r = returns_one( ( .true. ? a : .nil. ) )  ! {Error} `.nil.` is not allowed for the dummy argument `x`, which is not optional
        print *, r
    end subroutine

    ! C1540 through a keyword actual argument (R1523). The consequent is
    ! supplied, it just leaves the argument with no value, so this is the same
    ! violation as the positional form and not a missing argument.
    subroutine nil_for_a_required_dummy_by_keyword()
        implicit none
        integer :: a
        a = 1
        call two_required(x = a, y = ( .true. ? a : .nil. ))  ! {Error} `.nil.` is not allowed for the dummy argument `y`, which is not optional
    end subroutine

    ! C1540: at least one consequent shall be a consequent-arg, so they
    ! cannot all be `.NIL.`.
    subroutine every_consequent_is_nil()
        implicit none
        call optional_arg( ( .true. ? .nil. : .nil. ) )  ! {Error} every consequent of this conditional argument is `.nil.`
    end subroutine

    ! C1541: when the dummy argument is intent(out) or intent(inout), every
    ! consequent shall be a variable, because the chosen one is defined.
    subroutine expression_consequent_for_intent_out()
        implicit none
        integer :: a
        call defined( ( .true. ? a : 1 ) )  ! {Error} Non-variable expression in variable definition context (actual argument to INTENT = OUT/INOUT)
    end subroutine

    ! C1539: the consequents shall have the same rank.
    subroutine consequents_of_different_rank()
        implicit none
        integer :: a, b(2)
        a = 1; b = 2
        call required( ( .true. ? a : b ) )  ! {Error} the consequents of a conditional argument must have the same rank
    end subroutine

    ! C1538: the consequents shall have the same declared type and kind type
    ! parameters. The length type parameters and the shape may differ, they
    ! come from the consequent that is chosen.
    subroutine consequents_of_different_type()
        implicit none
        integer :: a
        real :: b
        a = 1; b = 2.0
        call required( ( .true. ? a : b ) )  ! {Error} the consequents of a conditional argument must have the same type and kind
    end subroutine

    ! C1545: in a reference to a generic procedure the consequents shall have
    ! the same `allocatable` and `pointer` attributes, because those are what
    ! the generic is resolved against.
    subroutine generic_with_mixed_attributes()
        implicit none
        integer, allocatable :: a(:)
        integer, pointer :: p(:)
        allocate(a(1)); allocate(p(1))
        call gen( ( .true. ? a : p ) )  ! {Error} the consequents of a conditional argument to a generic procedure must have the same `allocatable` and `pointer` attributes
    end subroutine
end module
