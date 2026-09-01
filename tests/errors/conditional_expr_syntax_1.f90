! Syntax of the Fortran 2023 conditional expression (10.1.2.3 R1002):
!
!     conditional-expr  is  ( scalar-logical-expr ? expr
!                             [ : scalar-logical-expr ? expr ]... : expr )
!
! `?` is not an operator (it does not appear in Table 10.1); a conditional
! expression is a primary, and the parentheses are part of its syntax. Each
! subroutine below violates one part of R1002.

subroutine no_outer_parentheses()
    implicit none
    integer :: x
    x = .true. ? 1 : 0  ! {Error} Token '?' is unexpected here
end subroutine

! An inner conditional expression is a primary only when it is parenthesized,
! so it cannot appear bare as the arm that follows `?`.
subroutine unparenthesized_arm()
    implicit none
    integer :: x
    x = ( .true. ? .false. ? 1 : 2 : 3 )  ! {Error} Token '?' is unexpected here
end subroutine

! R1002 always ends in `: expr`, so the default arm cannot be left out.
subroutine missing_default_arm()
    implicit none
    integer :: x
    x = ( .true. ? 1 )  ! {Error} Token ')' is unexpected here
end subroutine

subroutine stray_question_mark()
    implicit none
    integer :: x
    x = ( ? 1 : 0 )  ! {Error} Token '?' is unexpected here
end subroutine

! `.NIL.` is a consequent (R1527), never a condition: the condition of R1526 is
! a scalar-logical-expr, so `.NIL.` in that position is not even syntax.
subroutine nil_as_a_condition()
    implicit none
    integer :: a, x
    a = 1
    x = ( .nil. ? a : a )  ! {Error} Token '.nil.' is unexpected here
end subroutine
