! Under implicit typing the ALLOCATABLE statement may be the entity's only
! declaration, so its array-spec is the only place the shape can come from.
subroutine allocatable_attr_dims_implicit(n)
    allocatable x(:)
    allocatable :: m(:,:)
    allocate (x(n), m(n,n))
    x(1) = 1.0
    m(1,1) = 2
    deallocate (x, m)
end subroutine allocatable_attr_dims_implicit
