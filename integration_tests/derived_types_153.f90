! Free (non-type-bound) interface assignment(=) on arrays of derived type.
! Must lower to element-wise calls of assign_ti (not intrinsic copy).

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
      b(i)%is_temporary = .false.
   end do

   ! Whole-array free interface assignment(=)
   b = a
   do i = 1, 3
      if (.not. associated(b(i)%leaf, a(i)%leaf)) error stop 301
      if (b(i)%leaf /= i * 11) error stop 302
      if (.not. b(i)%is_temporary) error stop 303
   end do

   ! Section
   do i = 1, 3
      b(i)%leaf => null()
      b(i)%is_temporary = .false.
   end do
   b(1:3) = a(1:3)
   do i = 1, 3
      if (.not. associated(b(i)%leaf, a(i)%leaf)) error stop 304
      if (.not. b(i)%is_temporary) error stop 305
   end do

   ! Scalar-to-array
   allocate(scalar%leaf)
   scalar%leaf = 55
   scalar%is_temporary = .false.
   b = scalar
   do i = 1, 3
      if (.not. associated(b(i)%leaf, scalar%leaf)) error stop 306
      if (b(i)%leaf /= 55) error stop 307
      if (.not. b(i)%is_temporary) error stop 308
   end do

   do i = 1, 3
      deallocate(a(i)%leaf)
   end do
   deallocate(scalar%leaf)
end program derived_types_153
