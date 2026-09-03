! Fortran 2023 conditional arguments whose consequent is `.NIL.` (R1527).
! `.NIL.` is a token of its own (6.2.1). It is not a named constant and not an
! expression: as the chosen consequent it means that the dummy argument is not
! present (15.5.2.3 p12), so the reference behaves as if the argument had not
! been written at all.
!
! This test is not labelled `gfortran`: GFortran 16 does not implement
! conditional arguments.
program conditional_arg_02
    implicit none
    integer :: a
    real :: x, y, z, edge
    integer :: mode

    ! `.NIL.` leaves the optional dummy argument absent
    a = 4
    call five( ( a<5 ? a : .NIL. ) )
    if (a /= 5) error stop 1
    a = 7
    call five( ( a<5 ? a : .NIL. ) )
    if (a /= 7) error stop 2

    ! The same in a keyword actual argument (R1523)
    a = 1
    call five(x = ( a>0 ? a : .NIL. ))
    if (a /= 5) error stop 3
    a = -1
    call five(x = ( a>0 ? a : .NIL. ))
    if (a /= -1) error stop 4

    ! `present` reports what the chosen consequent decided
    a = 1
    if (.not. is_present( ( .true.  ? a : .NIL. ) )) error stop 5
    if (      is_present( ( .false. ? a : .NIL. ) )) error stop 6

    ! `.NIL.` as one arm of the repeating group
    a = 1
    if (.not. is_present( ( a>5 ? a : a>0 ? a : .NIL. ) )) error stop 7
    a = -1
    if (      is_present( ( a>5 ? a : a>0 ? a : .NIL. ) )) error stop 8

    ! The 15.5.2.3 NOTE example
    x = 1.0; y = -1.0; z = 0.5
    edge = -1.0; mode = 3
    call sub( ( x>0 ? x : y>0 ? y : z ), &
              ( edge>0 ? edge : mode==3 ? 1.0 : .NIL. ) )
    edge = -1.0; mode = 1
    call sub2( ( x>0 ? x : y>0 ? y : z ), &
               ( edge>0 ? edge : mode==3 ? 1.0 : .NIL. ) )
contains
    subroutine five(x)
        integer, optional, intent(inout) :: x
        if (present(x)) x = 5
    end subroutine

    logical function is_present(x)
        integer, optional, intent(in) :: x
        is_present = present(x)
    end function

    subroutine sub(p, q)
        real, intent(in) :: p
        real, intent(in), optional :: q
        if (abs(p-1.0) > 1.0e-6) error stop 9
        if (.not. present(q)) error stop 10
        if (abs(q-1.0) > 1.0e-6) error stop 11
    end subroutine

    subroutine sub2(p, q)
        real, intent(in) :: p
        real, intent(in), optional :: q
        if (abs(p-1.0) > 1.0e-6) error stop 12
        if (present(q)) error stop 13
    end subroutine
end program
