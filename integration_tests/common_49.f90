! A program unit extends a COMMON block that another unit declared, and the
! new object has the name that the other unit gave to its first object. It is
! associated by storage position: past the end of the other unit's layout.
subroutine common_49_set_blank()
    implicit none
    integer :: a
    common a
    a = 5
end subroutine common_49_set_blank

subroutine common_49_one_statement()
    implicit none
    integer :: x, a
    common x, a
    x = 1
    a = 2
    call common_49_set_blank()
    print *, x, a
    if (x /= 5) error stop
    if (a /= 2) error stop
end subroutine common_49_one_statement

subroutine common_49_two_statements()
    implicit none
    integer :: x, a
    common x
    common a
    x = 1
    a = 3
    call common_49_set_blank()
    print *, x, a
    if (x /= 5) error stop
    if (a /= 3) error stop
end subroutine common_49_two_statements

! The same for a named block whose first declaration was read from a
! modfile. Named blocks of different sizes are an extension, which GFortran
! also accepts.
subroutine common_49_modfile()
    use common_49_mod, only: set_first
    implicit none
    integer :: x, a
    common /common_49_blk/ x, a
    x = 1
    a = 4
    call set_first()
    print *, x, a
    if (x /= 5) error stop
    if (a /= 4) error stop
end subroutine common_49_modfile

program common_49
    implicit none
    call common_49_one_statement()
    call common_49_two_statements()
    call common_49_modfile()
end program common_49
