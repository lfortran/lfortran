! Fortran 2023 conditional expressions used in the language contexts that take
! an expression.
program conditional_expr_04
    implicit none
    integer :: i, n, s, x
    integer :: arr(3), xs(3)
    character(len=5) :: str
    character(len=2) :: t
    character(len=8) :: line

    ! The condition of an IF statement (11.1.8)
    i = 0
    if ( ( .true. ? .true. : .false. ) ) i = 1
    if (i /= 1) error stop 1

    ! DO bounds (11.1.7)
    n = 3
    s = 0
    do i = 1, ( n>0 ? n : 0 )
        s = s + i
    end do
    if (s /= 6) error stop 2

    ! Array subscript (9.5.3)
    arr = [10,20,30]
    i = 2
    x = arr( ( i>0 ? i : 1 ) )
    if (x /= 20) error stop 3

    ! Substring bounds (9.4.1)
    str = "abcde"
    t = str( ( .true. ? 2 : 1 ) : ( .true. ? 3 : 4 ) )
    if (t /= "bc") error stop 4

    ! Actual argument that is an expression (15.5.1 R1524)
    if (twice( ( .true. ? 3 : 4 ) ) /= 6) error stop 5

    ! Internal WRITE (Clause 12)
    write(line, "(I0)") ( .true. ? 42 : 0 )
    if (adjustl(line) /= "42") error stop 6

    ! STOP code (11.4)
    if (n == 0) stop ( .true. ? 1 : 2 )

    ! Implied-DO bound of an array constructor
    n = 3
    xs = [ (i, i=1, ( n>0 ? n : 1 )) ]
    if (any(xs /= [1,2,3])) error stop 7
contains
    integer function twice(k)
        integer, intent(in) :: k
        twice = k*2
    end function
end program
