! Fortran 2023 conditional expressions: the condition (R1002, C1007) and the
! intrinsic types allowed as arms.
program conditional_expr_03
    implicit none
    integer :: a, b, i, x
    real :: r, ra
    complex :: c
    logical :: l
    logical(kind=kind(.true.)) :: cond
    character(len=:), allocatable :: s
    character(len=8) :: fixed

    ! Both arms have the same declared type, kind and rank (C1004)
    a = 1; b = 2
    x = ( a>0 ? a : b )
    if (x /= 1) error stop 1

    ! Relational and logical expressions as the condition (10.1.2.7, 10.1.2.8)
    i = 4
    x = ( i==4 .and. i>0 ? 1 : 0 )
    if (x /= 1) error stop 2

    ! A logical variable of an explicit kind as the condition
    cond = .false.
    x = ( cond ? 1 : 2 )
    if (x /= 2) error stop 3

    ! Integer arms, including a unary minus (10.1.2.5)
    x = ( .true. ? -3 : 4 )
    if (x /= -3) error stop 4

    ! Real arms
    ra = -1.5
    r = ( ra>0.0 ? ra : 0.0 )
    if (r /= 0.0) error stop 5

    ! Complex arms
    c = ( .true. ? (1.0, 2.0) : (3.0, 4.0) )
    if (c /= (1.0, 2.0)) error stop 6

    ! Logical result
    l = ( 1<2 ? .true. : .false. )
    if (.not. l) error stop 7

    ! Character arms may differ in length; the length of the result is that of
    ! the arm that is chosen (10.1.4 p22). This is the 10.1.2.3 NOTE example.
    s = ( .true. ? "ok" : "did not converge" )
    if (len(s) /= 2) error stop 8
    if (s /= "ok") error stop 9
    s = ( .false. ? "ok" : "did not converge" )
    if (len(s) /= 16) error stop 10
    if (s /= "did not converge") error stop 11

    ! Ordinary character assignment applies to the value (10.2.1.3)
    fixed = ( .true. ? "ok" : "no" )
    if (fixed /= "ok      ") error stop 12

    ! The conditional expression is one operand of a larger expression
    i = 2
    r = ( i>0 ? 1.5 : 0.5 ) + 1.0
    if (abs(r-2.5) > 1.0e-6) error stop 13
end program
