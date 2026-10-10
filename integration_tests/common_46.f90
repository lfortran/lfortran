! A COMMON block declared in a procedure of a module is declared again by
! procedures that use the module.
subroutine common_46_same_names()
    use common_46_mod, only: set_values
    implicit none
    integer :: n, a
    common /common_46_blk/ n, a
    call set_values()
    print *, n, a
    if (n /= 3) error stop
    if (a /= 4) error stop
    a = 6
end subroutine common_46_same_names

subroutine common_46_other_names()
    use common_46_mod, only: set_values
    implicit none
    integer :: i, j
    common /common_46_blk/ i, j
    print *, i, j
    if (i /= 3) error stop
    if (j /= 6) error stop
end subroutine common_46_other_names

! The names of the module procedure at swapped positions: COMMON objects are
! associated by storage position, not by name.
subroutine common_46_swapped_names()
    use common_46_mod, only: set_values
    implicit none
    integer :: a, n
    common /common_46_blk/ a, n
    call set_values()
    print *, a, n
    if (a /= 3) error stop
    if (n /= 4) error stop
end subroutine common_46_swapped_names

program common_46
    implicit none
    call common_46_same_names()
    call common_46_other_names()
    call common_46_swapped_names()
end program common_46
