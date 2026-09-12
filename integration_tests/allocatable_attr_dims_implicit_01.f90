program allocatable_attr_dims_implicit_01
    allocatable x(:)
    allocatable :: m(:,:)

    allocate(x(2), m(2, 3))
    x = [1.0, 2.0]
    m(2, 3) = 6

    if (.not. allocated(x)) error stop
    if (size(x) /= 2 .or. x(2) /= 2.0) error stop
    if (size(m, 1) /= 2 .or. size(m, 2) /= 3) error stop
    if (m(2, 3) /= 6) error stop

    deallocate(x, m)
end program allocatable_attr_dims_implicit_01
