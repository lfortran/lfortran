! Free (non-type-bound) non-elemental interface assignment(=) with scalar
! dummies does not match array LHS. Array assignment is intrinsic.

module derived_types_153_mod
   implicit none
   type :: ti
      integer, pointer :: leaf => null()
      logical :: is_temporary = .false.
   end type ti

   interface assignment(=)
      module procedure assign_ti
   end interface assignment(=)

contains

   subroutine assign_ti(lhs, rhs)
      type(ti), intent(out) :: lhs
      type(ti), intent(in)  :: rhs
      lhs%leaf => rhs%leaf
      lhs%is_temporary = .true.
   end subroutine assign_ti

end module derived_types_153_mod

program derived_types_153
   use derived_types_153_mod
   implicit none
   type(ti), dimension(3) :: a, b
   type(ti) :: scalar
   integer :: i

   do i = 1, 3
      allocate(a(i)%leaf)
      a(i)%leaf = i * 11
      a(i)%is_temporary = .false.
      b(i)%is_temporary = .true.
   end do

   b = a
   do i = 1, 3
      if (.not. associated(b(i)%leaf, a(i)%leaf)) error stop 301
      if (b(i)%leaf /= i * 11) error stop 302
      if (b(i)%is_temporary) error stop 303
   end do

   do i = 1, 3
      b(i)%leaf => null()
      b(i)%is_temporary = .true.
   end do
   b(1:3) = a(1:3)
   do i = 1, 3
      if (.not. associated(b(i)%leaf, a(i)%leaf)) error stop 304
      if (b(i)%is_temporary) error stop 305
   end do

   allocate(scalar%leaf)
   scalar%leaf = 55
   scalar%is_temporary = .false.
   b = scalar
   do i = 1, 3
      if (.not. associated(b(i)%leaf, scalar%leaf)) error stop 306
      if (b(i)%leaf /= 55) error stop 307
      if (b(i)%is_temporary) error stop 308
   end do

   ! Scalar free-interface defined assignment still applies
   b(1)%is_temporary = .false.
   b(1) = a(2)
   if (.not. associated(b(1)%leaf, a(2)%leaf)) error stop 309
   if (.not. b(1)%is_temporary) error stop 310

   do i = 1, 3
      deallocate(a(i)%leaf)
   end do
   deallocate(scalar%leaf)
end program derived_types_153
