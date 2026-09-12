program allocatable_attr_then_type_decl_01
    allocatable qbh(:)
    integer qbh
    allocatable w(:,:)
    double precision w

    allocate(qbh(2), w(2, 3))
    qbh = [1, 2]
    w(2, 3) = 6.0d0

    if (.not. allocated(qbh)) error stop
    if (size(qbh) /= 2 .or. qbh(2) /= 2) error stop
    if (size(w, 1) /= 2 .or. size(w, 2) /= 3) error stop
    if (w(2, 3) /= 6.0d0) error stop

    deallocate(qbh, w)
end program allocatable_attr_then_type_decl_01
