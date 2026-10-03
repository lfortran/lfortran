program character_27
    ! Test associated() on character (string) array pointers of various ranks.
    character(len=4), pointer :: a1(:)
    character(len=4), pointer :: a2(:,:)
    character(len=4), pointer :: a4(:,:,:,:)

    allocate(a1(3))
    if (.not. associated(a1)) error stop "a1 should be associated"
    deallocate(a1)

    allocate(a2(2,3))
    if (.not. associated(a2)) error stop "a2 should be associated"
    deallocate(a2)

    allocate(a4(3,3,2,2))
    if (.not. associated(a4)) error stop "a4 should be associated"
    if (size(a4) /= 36) error stop "size mismatch"
    deallocate(a4)

    print *, "PASS"
end program
