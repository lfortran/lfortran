! The array-spec may be given on the ALLOCATABLE statement instead of on the
! type declaration; the entity is then an allocatable array, exactly as if it
! had been declared `integer, allocatable :: a(:)`.
subroutine allocatable_attr_dims(n)
    implicit none
    integer :: n
    integer :: a
    real :: b, c
    allocatable :: a(:)
    allocatable b(:), c(:,:)
    allocate (a(n), b(n), c(n,n))
    a(1) = 1
    b(1) = 1.0
    c(1,1) = 2.0
    deallocate (a, b, c)
end subroutine allocatable_attr_dims
