! Intrinsic array assignment of derived type with non-elemental type-bound
! assignment(=) that does not match array ranks (F2023 10.2.1.4).
! Pointer components are associated intrinsically; is_temporary is copied,
! not set by assign_t.

module derived_types_151_mod
   implicit none
   type :: t
      integer, pointer :: leaf => null()
      logical :: is_temporary = .false.
   contains
      procedure :: assign_t
      generic :: assignment(=) => assign_t
   end type t

contains
   subroutine assign_t(lhs, rhs)
      class(t), intent(inout) :: lhs
      class(t), intent(in)    :: rhs
      ! Would mark defined assignment if it ran (it must not for array LHS).
      lhs%leaf => rhs%leaf
      lhs%is_temporary = .true.
   end subroutine assign_t

   function get_data(src) result(out)
      type(t), dimension(:), intent(in) :: src
      type(t), dimension(:), allocatable :: out
      allocate(out(size(src)))
      ! Section assignment: intrinsic for non-elemental scalar assignment(=)
      out(1:size(src)) = src(1:size(src))
   end function get_data
end module derived_types_151_mod

program derived_types_151
   use derived_types_151_mod
   implicit none
   type(t), dimension(:), allocatable :: arr, copy, whole
   type(t) :: scalar
   integer :: i

   allocate(arr(3))
   do i = 1, 3
      allocate(arr(i)%leaf)
      arr(i)%leaf = i * 100
      arr(i)%is_temporary = .false.
   end do

   ! ---- section-to-section via function + allocate source= ----
   allocate(copy, source = get_data(arr))

   if (size(copy) /= 3) error stop
   do i = 1, 3
      if (.not. associated(arr(i)%leaf)) error stop
      if (arr(i)%leaf /= i * 100) error stop
      if (.not. associated(copy(i)%leaf)) error stop
      if (copy(i)%leaf /= i * 100) error stop
      ! Intrinsic assignment of pointer components shares the target
      if (.not. associated(arr(i)%leaf, copy(i)%leaf)) error stop
      ! Defined assignment must not have run
      if (copy(i)%is_temporary) error stop
   end do

   ! ---- whole-array assignment ----
   allocate(whole(3))
   do i = 1, 3
      whole(i)%is_temporary = .true.  ! will be overwritten by intrinsic copy
   end do
   whole = arr
   do i = 1, 3
      if (.not. associated(whole(i)%leaf, arr(i)%leaf)) error stop
      if (whole(i)%is_temporary) error stop  ! copied from arr (.false.)
   end do

   ! ---- scalar-to-array (intrinsic broadcast of components) ----
   allocate(scalar%leaf)
   scalar%leaf = 42
   scalar%is_temporary = .false.
   whole = scalar
   do i = 1, 3
      if (.not. associated(whole(i)%leaf, scalar%leaf)) error stop
      if (whole(i)%leaf /= 42) error stop
      if (whole(i)%is_temporary) error stop
   end do

   ! Scalar defined assignment still applies
   whole(1)%is_temporary = .false.
   whole(1) = arr(2)
   if (.not. associated(whole(1)%leaf, arr(2)%leaf)) error stop
   if (.not. whole(1)%is_temporary) error stop

   do i = 1, 3
      deallocate(arr(i)%leaf)
   end do
   deallocate(scalar%leaf)
   deallocate(copy)
   deallocate(whole)
   deallocate(arr)
end program derived_types_151
