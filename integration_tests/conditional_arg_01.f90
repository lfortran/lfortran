module conditional_arg_01_mod
    implicit none
    integer :: seq = 0
    type :: holder
        integer :: n = 0
    contains
        procedure :: add
    end type
contains
    logical function mark(v, res)
        integer, intent(in) :: v
        logical, intent(in) :: res
        seq = seq + v
        mark = res
    end function

    subroutine add(self, a)
        class(holder), intent(inout) :: self
        integer, intent(inout) :: a
        a = a + 1
        self%n = self%n + a
    end subroutine
end module

! Fortran 2023 conditional arguments (15.5.1 R1526):
!
!     ( scalar-logical-expr ? consequent
!       [ : scalar-logical-expr ? consequent ]... : consequent )
!
! The consequent that is chosen *is* the actual argument (15.5.2.3), so a
! consequent that is a variable is associated by reference and stays
! definable. This is the part that a conditional expression cannot express:
! that one passes a value.
!
! This test is not labelled `gfortran`: GFortran 16 does not implement
! conditional arguments.
program conditional_arg_01
    use conditional_arg_01_mod
    implicit none
    integer :: x, y, r
    type(holder) :: h

    ! The chosen variable is the actual argument, so intent(inout) writes
    ! back into it and the other consequent is untouched
    x = 4; y = 9
    call five( ( x<5 ? x : y ) )
    if (x /= 5) error stop 1
    if (y /= 9) error stop 2
    call five( ( x<5 ? x : y ) )
    if (x /= 5) error stop 3
    if (y /= 5) error stop 4

    ! The repeating group of R1526, with the conditions evaluated in order
    ! and only up to the first true one
    seq = 0
    x = 1; y = 2
    call five( ( mark(1,.false.) ? x : mark(2,.true.) ? y : x ) )
    if (y /= 5) error stop 5
    if (x /= 1) error stop 6
    if (seq /= 3) error stop 7

    ! A consequent that is an expression is evaluated only when it is chosen
    call take( ( .true. ? 1 : boom() ) )
    call take( ( .false. ? boom() : 2 ) )

    ! A keyword actual argument (R1523) may be a conditional argument
    x = 4
    call five(a = ( x<5 ? x : y ))
    if (x /= 5) error stop 8

    ! A function reference takes a conditional argument the same way
    x = 3; y = 7
    r = twice( ( x>0 ? x : y ) )
    if (r /= 6) error stop 9
    r = twice( ( x>5 ? x : y ) )
    if (r /= 14) error stop 10

    ! Two conditional arguments in one reference, one of them multi-arm
    r = pair( ( x>0 ? x : y ), ( y>9 ? x : x>0 ? y : x ) )
    if (r /= 37) error stop 11

    ! A conditional argument of a type bound procedure reference
    x = 1; y = 5
    call h%add( ( x>0 ? x : y ) )
    if (x /= 2) error stop 12
    if (h%n /= 2) error stop 13
    call h%add( ( x>5 ? x : y ) )
    if (y /= 6) error stop 14
    if (h%n /= 8) error stop 15

    ! A reference with a conditional argument inside the arm of a conditional
    ! expression is only evaluated when that arm is chosen (10.1.4 NOTE 3)
    x = 1
    r = ( x>0 ? 5 : boom_arg( ( x>0 ? x : y ) ) )
    if (r /= 5) error stop 16
contains
    subroutine five(a)
        integer, intent(inout) :: a
        a = 5
    end subroutine

    subroutine take(a)
        integer, intent(in) :: a
        if (a /= 1 .and. a /= 2) error stop 91
    end subroutine

    integer function boom()
        boom = 0
        error stop 92
    end function

    integer function boom_arg(k)
        integer, intent(in) :: k
        boom_arg = k
        error stop 93
    end function

    integer function twice(k)
        integer, intent(in) :: k
        twice = 2*k
    end function

    integer function pair(p, q)
        integer, intent(in) :: p, q
        pair = 10*p + q
    end function
end program
