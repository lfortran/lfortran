! The ALLOCATABLE statement (carrying the array-spec) may precede the
! entity's type declaration; the later declaration only refines the
! element type and must keep the allocatable attribute and the shape.
subroutine allocatable_attr_then_type_decl(n)
    allocatable qbh(:)
    integer qbh
    allocatable w(:,:)
    double precision w
    allocate (qbh(n), w(n,n))
    qbh(1) = 1
    w(1,1) = 2.0d0
    deallocate (qbh, w)
end subroutine allocatable_attr_then_type_decl
