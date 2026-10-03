
module allocate_63_mod
    implicit none
contains
    subroutine cleanup(errmsg) bind(C)
        character(len=:), intent(out), allocatable :: errmsg
        if (allocated(errmsg)) then
            deallocate(errmsg)
        end if
    end subroutine
end module

program allocate_63
    use allocate_63_mod
    implicit none
    character(len=:), allocatable :: msg

    allocate(character(5) :: msg)
    msg = "hello"
    if (.not. allocated(msg)) error stop "msg should be allocated before cleanup"
    call cleanup(msg)
    if (allocated(msg)) error stop "msg should be deallocated after cleanup"
    print *, "passed"
end program
