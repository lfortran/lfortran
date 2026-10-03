! Free (non-type-bound) non-elemental interface assignment(=) with scalar
! dummies does not match array LHS (F2023 10.2.1.4). Array assignment is
! intrinsic; scalar assignment still uses the free interface.

module defined_assignment_free_interface_01_mod
   implicit none
   type :: ti
      integer, pointer :: leaf => null()
      logical :: is_temporary = .false.
      integer :: v = 0
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
      lhs%v = rhs%v
   end subroutine assign_ti

end module defined_assignment_free_interface_01_mod

program defined_assignment_free_interface_01
   use defined_assignment_free_interface_01_mod
   implicit none
   type(ti), dimension(3) :: a, b
   type(ti) :: scalar
   integer :: i

   do i = 1, 3
      allocate(a(i)%leaf)
      a(i)%leaf = i * 11
      a(i)%v = i * 11
      a(i)%is_temporary = .false.
      b(i)%is_temporary = .true.
   end do

   b = a
   do i = 1, 3
      if (.not. associated(b(i)%leaf, a(i)%leaf)) error stop 301
      if (b(i)%leaf /= i * 11) error stop 302
      if (b(i)%v /= i * 11) error stop 303
      if (b(i)%is_temporary) error stop 304
   end do

   do i = 1, 3
      b(i)%leaf => null()
      b(i)%is_temporary = .true.
   end do
   b(1:3) = a(1:3)
   do i = 1, 3
      if (.not. associated(b(i)%leaf, a(i)%leaf)) error stop 305
      if (b(i)%is_temporary) error stop 306
   end do

   allocate(scalar%leaf)
   scalar%leaf = 55
   scalar%v = 55
   scalar%is_temporary = .false.
   b = scalar
   do i = 1, 3
      if (.not. associated(b(i)%leaf, scalar%leaf)) error stop 307
      if (b(i)%v /= 55) error stop 308
      if (b(i)%is_temporary) error stop 309
   end do

   ! Scalar free-interface defined assignment still applies
   b(1)%is_temporary = .false.
   b(1) = a(2)
   if (.not. associated(b(1)%leaf, a(2)%leaf)) error stop 310
   if (b(1)%v /= 22) error stop 311
   if (.not. b(1)%is_temporary) error stop 312

   do i = 1, 3
      deallocate(a(i)%leaf)
   end do
   deallocate(scalar%leaf)
end program defined_assignment_free_interface_01
