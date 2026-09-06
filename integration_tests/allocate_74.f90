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
  if (.not. allocated(qbh)) error stop
  if (.not. allocated(r2)) error stop
  if (.not. allocated(s)) error stop
  if (size(qbh) /= n) error stop
  if (size(r2, 1) /= n .or. size(r2, 2) /= n) error stop
  qbh(1) = 1
  qbh(n) = 7
  r2(1,1) = 2.0
  r2(n,n) = 5.0
  s = 3
  if (qbh(1) /= 1 .or. qbh(n) /= 7) error stop
  if (abs(r2(1,1) - 2.0) > 1e-6 .or. abs(r2(n,n) - 5.0) > 1e-6) error stop
  if (s /= 3) error stop
  deallocate (qbh, r2, s)
  if (allocated(qbh) .or. allocated(r2) .or. allocated(s)) error stop
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
  if (.not. allocated(qbh)) error stop
  if (size(qbh) /= n) error stop
  qbh(1) = 1
  qbh(n) = 4
  if (qbh(1) /= 1 .or. qbh(n) /= 4) error stop
  deallocate (qbh)
  if (allocated(qbh)) error stop
end subroutine dim_then_alloc

! A character element type keeps its length through the merge.
subroutine char_elem(n)
  implicit none
  integer n
  allocatable c(:)
  character(len=5) c
  allocate (c(n))
  if (.not. allocated(c)) error stop
  if (size(c) /= n) error stop
  if (len(c) /= 5) error stop
  c(1) = 'hello'
  c(n) = 'world'
  if (c(1) /= 'hello' .or. c(n) /= 'world') error stop
  deallocate (c)
end subroutine char_elem

! A same-named variable in a later scope must not inherit anything from
! the consumed declarations above.
subroutine fresh_scope()
  implicit none
  integer qbh
  qbh = 2
  if (qbh /= 2) error stop
end subroutine fresh_scope

program allocate_74
  implicit none
  call attr_before_decl(3)
  call dim_then_alloc(4)
  call char_elem(2)
  call fresh_scope()
  print *, "ok"
end program allocate_74
