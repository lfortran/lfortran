! The only user of global_init_14_m.
subroutine global_init_14_s()
    use global_init_14_m
    implicit none
    integer, target, save :: b(3) = [1, 2, 3]
    if (associated(mscal%p)) error stop 1
    if (allocated(a)) error stop 2
    mscal%p => b
    if (size(mscal%p) /= 3 .or. sum(mscal%p) /= 6) error stop 3
    allocate(a(2))
    a = [4, 5]
    if (sum(a) /= 9) error stop 4
    deallocate(a)
    print *, "ok"
end subroutine global_init_14_s
