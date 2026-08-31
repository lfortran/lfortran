! An ALLOCATABLE statement may precede the entity's type declaration
! (allocatable-decl is `object-name [( array-spec )]`, F2018 8.6.1).
! Under IMPLICIT NONE both the attribute and the statement's array-spec
! must survive the later type declaration.
subroutine attr_before_decl(n)
  implicit none
  integer n
  allocatable qbh(:)
  integer qbh
  allocatable r2(:,:)
  real r2
  allocatable :: s
  integer s
  allocate (qbh(n), r2(n,n), s)
  qbh(1) = 1
  r2(1,1) = 2.0
  s = 3
  deallocate (qbh, r2, s)
end subroutine attr_before_decl

! The array-spec may come from a DIMENSION statement instead, with the
! ALLOCATABLE statement bare.
subroutine dim_then_alloc(n)
  implicit none
  integer n
  dimension qbh(:)
  allocatable qbh
  integer qbh
  allocate (qbh(n))
  qbh(1) = 1
  deallocate (qbh)
end subroutine dim_then_alloc

! A character element type keeps its length through the merge.
subroutine char_elem(n)
  implicit none
  integer n
  allocatable c(:)
  character(len=5) c
  allocate (c(n))
  c(1) = 'hello'
  deallocate (c)
end subroutine char_elem

! A same-named variable in a later scope must not inherit anything from
! the consumed declarations above.
subroutine fresh_scope()
  implicit none
  integer qbh
  qbh = 2
end subroutine fresh_scope
