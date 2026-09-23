! The only thing that uses the module. The program below reaches it through
! this separately compiled external subroutine, so the program cannot observe
! the module and never calls its initializer.
subroutine global_init_09_s()
    use global_init_09_m
    implicit none
    integer, target, save :: a(3) = [1, 2, 3]
    if (associated(mscal%p)) error stop 1
    mscal%p => a
    if (.not. associated(mscal%p)) error stop 2
    if (size(mscal%p) /= 3) error stop 3
    if (mscal%p(2) /= 2) error stop 4
    print *, "s ok"
end subroutine global_init_09_s
