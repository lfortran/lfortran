program common_40
    implicit none
    call init()
    call check()
end program

subroutine init()
    implicit none
    real, pointer :: p(:)
    common /block/ p
    if (associated(p)) error stop
    allocate(p(3))
    p = [1.0, 2.0, 3.0]
    if (.not. associated(p)) error stop
    if (size(p) /= 3) error stop
    if (abs(sum(p) - 6.0) > 1e-6) error stop
end subroutine init

subroutine check()
    implicit none
    real, pointer :: p(:)
    common /block/ p
    if (.not. associated(p)) error stop
    if (size(p) /= 3) error stop
    if (p(2) /= 2.0) error stop
    deallocate(p)
    if (associated(p)) error stop
end subroutine check
