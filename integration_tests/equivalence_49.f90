! With --separate-compilation, the equivalenced module variables are read back
! from the `.mod` file. They are defined in the module's object file, so here
! they are addressed through their equivalence target, not laid out as aliases
! again.
module equivalence_49_user
    use equivalence_49_module, only : matrix, third
    implicit none
contains
    subroutine touch()
        matrix(1,1) = 1.0
        matrix(2,1) = 2.0
        matrix(1,2) = 3.0
        matrix(2,2) = 4.0
        third = 42
    end subroutine
end module

program equivalence_49
    use equivalence_49_module, only : flat, items
    use equivalence_49_user, only : touch, matrix, third
    implicit none
    items = 0
    call touch()
    print *, flat
    print *, items
    if (abs(flat(1) - 1.0) > 1e-6) error stop 1
    if (abs(flat(2) - 2.0) > 1e-6) error stop 2
    if (abs(flat(3) - 3.0) > 1e-6) error stop 3
    if (abs(flat(4) - 4.0) > 1e-6) error stop 4
    if (items(3) /= 42) error stop 5
    if (any(items([1, 2, 4, 5]) /= 0)) error stop 6
    flat(4) = -1.0
    if (abs(matrix(2,2) + 1.0) > 1e-6) error stop 7
    items(3) = 7
    if (third /= 7) error stop 8
end program
