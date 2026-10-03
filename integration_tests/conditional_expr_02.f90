module conditional_expr_02_mod
    implicit none
    integer :: seq = 0
contains
    logical function mark(v, res)
        integer, intent(in) :: v
        logical, intent(in) :: res
        seq = seq + v
        mark = res
    end function
end module

! Fortran 2023 conditional expressions: only the chosen arm is evaluated and
! conditions are evaluated left to right (10.1.4 p18-24 and NOTE 3).
program conditional_expr_02
    use conditional_expr_02_mod
    implicit none
    integer :: x, z

    ! Only the chosen arm is evaluated
    x = ( .true. ? 1 : boom() )
    if (x /= 1) error stop 1
    x = ( .false. ? boom() : 2 )
    if (x /= 2) error stop 2

    ! A condition that follows a true condition is not evaluated
    x = ( .true. ? 1 : boom_l() ? 2 : 3 )
    if (x /= 1) error stop 3

    ! Conditions are evaluated in order, and stop at the first true one
    seq = 0
    x = ( mark(1,.false.) ? 10 : mark(2,.true.) ? 20 : mark(3,.true.) ? 30 : 40 )
    if (x /= 20) error stop 4
    if (seq /= 3) error stop 5

    ! The last arm is chosen when every condition is false
    x = ( .false. ? 1 : .false. ? 2 : 3 )
    if (x /= 3) error stop 6

    ! An intent(out) dummy in an unchosen arm is never defined
    z = 7
    x = ( .true. ? 1 : side(z) )
    if (x /= 1) error stop 7
    if (z /= 7) error stop 8
contains
    integer function boom()
        boom = 0
        error stop 99
    end function

    logical function boom_l()
        boom_l = .true.
        error stop 99
    end function

    integer function side(a)
        integer, intent(out) :: a
        a = 0
        side = 0
    end function
end program
