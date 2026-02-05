program move_alloc_pointer_alias
    implicit none
    real, allocatable, target :: a(:), b(:)
    real, pointer :: c(:)

    allocate(a(10), source=10.0)
    c => a

    call move_alloc(a, b)

    if (.not. associated(c)) error stop
    if (size(b) /= 10) error stop
    if (any(b /= 10.0)) error stop

    c(2) = 2.0
    if (b(2) /= 2.0) error stop
end program
