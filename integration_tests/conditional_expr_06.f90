! Fortran 2023 conditional expressions with array results. The arms must agree
! in rank (C1004) but not in shape: the shape of the result is that of the arm
! that is chosen (10.1.4 p22).
!
! This test is not labelled `gfortran`: GFortran 16 rejects an array valued
! conditional expression with "Sorry, array is currently unsupported for
! conditional expressions".
program conditional_expr_06
    implicit none
    integer :: a(3), b(3), x(3)
    integer :: p(2), q(3), empty(0)
    integer :: s(4), sec(2)
    integer :: v(2)
    integer :: n
    logical :: mask(3)
    character(len=3) :: ca(2), cb(2), cx(2)

    ! Whole arrays of the same shape
    a = [1,2,3]; b = [4,5,6]
    x = ( .true. ? a : b )
    if (any(x /= a)) error stop 1
    x = ( .false. ? a : b )
    if (any(x /= b)) error stop 2

    ! The shape comes from the arm that is chosen, even when the arms differ
    p = [1,2]
    q = [7,8,9]
    n = size( ( .true. ? p : q ) )
    if (n /= 2) error stop 3
    n = size( ( .false. ? p : q ) )
    if (n /= 3) error stop 4

    ! A zero sized arm is still an arm of rank 1
    n = size( ( .true. ? empty : p ) )
    if (n /= 0) error stop 5

    ! An array section as an arm (9.5.3)
    s = [10,20,30,40]
    sec = ( .true. ? s(2:3) : s(3:4) )
    if (any(sec /= [20,30])) error stop 6
    sec = ( .false. ? s(2:3) : s(3:4) )
    if (any(sec /= [30,40])) error stop 7

    ! An array constructor as an arm (7.8)
    v = ( .true. ? [1,2] : [3,4] )
    if (any(v /= [1,2])) error stop 8

    ! An elemental operation applies to the result (10.1.10)
    x = ( .true. ? a : b ) + 1
    if (any(x /= [2,3,4])) error stop 9

    ! A logical array result used as a WHERE mask (10.2.3). Both arms have to
    ! be rank 1 here: a scalar arm would violate C1004.
    a = [1, -2, 3]
    mask = ( .true. ? a>0 : a<0 )
    where ( mask )
        a = 0
    end where
    if (any(a /= [0,-2,0])) error stop 10

    ! An array of characters as the arms
    ca = ["abc","def"]
    cb = ["ghi","jkl"]
    cx = ( .false. ? ca : cb )
    if (cx(1) /= "ghi") error stop 13
    if (cx(2) /= "jkl") error stop 14

    ! ASSOCIATE selector (11.1.3)
    associate (y => ( .true. ? p : q ))
        if (size(y) /= 2) error stop 11
        if (any(y /= [1,2])) error stop 12
    end associate
end program
