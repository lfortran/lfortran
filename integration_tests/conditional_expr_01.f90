module conditional_expr_01_mod
    implicit none
    interface operator(.dbl.)
        module procedure dbl
    end interface
contains
    integer function dbl(x)
        integer, intent(in) :: x
        dbl = 2*x
    end function
end module

! Fortran 2023 conditional expressions (10.1.2.3 R1002): syntax of the primary.
program conditional_expr_01
    use conditional_expr_01_mod
    implicit none
    integer :: i, x
    character(len=*), parameter :: s = "what?"

    ! Minimal two-arm form
    x = ( .true. ? 1 : 0 )
    if (x /= 1) error stop 1

    ! Multi-arm form: the repeating group of R1002
    i = 0
    x = ( i>0 ? 1 : i<0 ? -1 : 0 )
    if (x /= 0) error stop 2
    i = 2
    x = ( i>0 ? 1 : i<0 ? -1 : 0 )
    if (x /= 1) error stop 3
    i = -3
    x = ( i>0 ? 1 : i<0 ? -1 : 0 )
    if (x /= -1) error stop 4

    ! A conditional expression is a primary, so it can be an arm of another
    ! one when it is parenthesized
    x = ( .true. ? ( .false. ? 1 : 2 ) : 3 )
    if (x /= 2) error stop 5
    x = ( .false. ? ( .true. ? 1 : 2 ) : 3 )
    if (x /= 3) error stop 6

    ! Used as a primary inside a larger expression (10.1.2.2, 10.1.3)
    x = 10 + ( .true. ? 2 : 3 ) * 4
    if (x /= 18) error stop 7
    x = ( ( .false. ? 1 : 2 ) + 3 )
    if (x /= 5) error stop 8

    ! Free form continuation lines inside the conditional expression
    x = ( 1 > 0 &
        ? 42 &
        : 0 )
    if (x /= 42) error stop 9

    ! `?` inside a character literal is not a token (6.1.6, 6.2.1)
    if (s /= "what?") error stop 10

    ! A defined unary operator has the highest precedence (Table 10.1) and
    ! takes the conditional expression as its primary operand
    x = .dbl. ( .true. ? 3 : 4 )
    if (x /= 6) error stop 11

    ! The arms are full expressions (10.1.8)
    x = 1 + ( .true. ? 2 + 3 : 0 )
    if (x /= 6) error stop 12
end program
