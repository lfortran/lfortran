program allocate_source_empty_polymorphic_01
    implicit none
    type :: t
        integer :: i = 0
    end type
    class(*), allocatable :: x(:)

    allocate(x, source=[real ::])
    if (.not. allocated(x)) error stop 1
    if (size(x) /= 0) error stop 2
    deallocate(x)
    if (allocated(x)) error stop 3

    allocate(x, source=[integer ::])
    if (.not. allocated(x)) error stop 4
    if (size(x) /= 0) error stop 5
    deallocate(x)

    allocate(x, source=[t ::])
    if (.not. allocated(x)) error stop 9
    if (size(x) /= 0) error stop 10
    deallocate(x)

    allocate(x, source=[t(7)])
    if (.not. allocated(x)) error stop 11
    select type (x)
    type is (t)
        if (x(1)%i /= 7) error stop 12
    class default
        error stop 13
    end select
    deallocate(x)

    allocate(x, source=[1.0, 2.0])
    if (.not. allocated(x)) error stop 6
    select type (x)
    type is (real)
        if (any(x /= [1.0, 2.0])) error stop 7
    class default
        error stop 8
    end select
    deallocate(x)
end program
