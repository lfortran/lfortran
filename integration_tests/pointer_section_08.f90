program pointer_section_08
    ! A pointer bounds-spec onto a disassociated data target disassociates
    ! the pointer (F2018 10.2.2.3). GFortran 16 rejects `p(1:) => null()`,
    ! which no rule of the standard forbids, so this test is not run with it.
    implicit none
    integer, target :: a(10)
    integer, pointer :: p(:), q(:)

    a = 1
    p(2:) => a
    if (.not. associated(p)) error stop 1
    p(1:) => null()
    if (associated(p)) error stop 2
    p(2:) => a
    q => null()
    p(1:) => q
    if (associated(p)) error stop 3
    print *, "passed"
end program pointer_section_08
