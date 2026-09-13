! Regression test for issue #12713: CHARACTER arrays passed through an implicit
! interface to a separately compiled procedure must reach the callee with the
! right data, element length and bounds, for every form the dummy can take.
! The callees live in implicit_interface_63b.f90, so every call here goes
! through a synthesized implicit interface rather than a real signature.
program implicit_interface_63
    implicit none
    character(len=8) :: w(3)
    character(len=1) :: c(3)
    character(len=16) :: text(4)
    character(len=4) :: grid(2,3)

    w(1) = 'ab'
    w(2) = 'cd'
    w(3) = 'ef'

    ! The five dummy declarations from the issue's table.
    call check_fixed_shape(w)
    call check_star_shape(w)
    call check_fixed_size(w)
    call check_star_size(w)

    c(1) = 'a'
    c(2) = 'c'
    c(3) = 'e'
    call check_char1_size(c)

    ! The MODFLOW-2005 pattern: CHARACTER*16 TEXT(n) with a runtime bound.
    text(1) = 'TEXT1'
    text(2) = 'TEXT2'
    text(3) = 'TEXT3'
    text(4) = 'TEXT4'
    call check_text16(text, 4)

    ! A rank-2 CHARACTER array flattened onto an assumed-size dummy.
    grid(1,1) = 'a11'
    grid(2,1) = 'b21'
    grid(1,2) = 'c12'
    grid(2,2) = 'd22'
    grid(1,3) = 'e13'
    grid(2,3) = 'f23'
    call check_rank2(grid)

    ! An array section of a CHARACTER array.
    call check_section(w(2:3))

    print *, 'OK'
end program implicit_interface_63
