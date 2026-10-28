program allocatable_attr_dims_01
    implicit none
    integer :: a
    real :: b, c

    allocatable :: a(:)
    allocatable b(:), c(:,:)

    allocate(a(2), b(3), c(2, 3))
    a = [1, 2]
    b = [1.0, 2.0, 3.0]
    c(2, 3) = 6.0

    if (.not. allocated(a)) error stop
    if (size(a) /= 2 .or. a(2) /= 2) error stop
    if (size(b) /= 3 .or. b(3) /= 3.0) error stop
    if (size(c, 1) /= 2 .or. size(c, 2) /= 3) error stop
    if (c(2, 3) /= 6.0) error stop

    deallocate(a, b, c)
end program allocatable_attr_dims_01
