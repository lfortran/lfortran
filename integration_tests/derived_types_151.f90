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
      ! Share the leaf without owning it (the original owns it).
      lhs%leaf => rhs%leaf
      lhs%is_temporary = .true.
   end subroutine assign_t

   function get_data(src) result(out)
      type(t), dimension(:), intent(in) :: src
      type(t), dimension(:), allocatable :: out
      allocate(out(size(src)))
      ! Array slice assignment of derived type with user-defined assignment
      ! must call assign_t element-wise, not perform intrinsic copy.
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

   ! ---- section-to-section (via function return + allocate source=) ----
   allocate(copy, source = get_data(arr))

   if (size(copy) /= 3) error stop
   do i = 1, 3
      if (.not. associated(arr(i)%leaf)) error stop
      if (arr(i)%leaf /= i * 100) error stop
      if (.not. associated(copy(i)%leaf)) error stop
      if (copy(i)%leaf /= i * 100) error stop
      ! Both must point to the same object (shared, not deep copy).
      if (.not. associated(arr(i)%leaf, copy(i)%leaf)) error stop
      if (.not. copy(i)%is_temporary) error stop
   end do

   ! ---- whole-array assignment ----
   allocate(whole(3))
   whole = arr
   do i = 1, 3
      if (.not. associated(whole(i)%leaf, arr(i)%leaf)) error stop
      if (.not. whole(i)%is_temporary) error stop
   end do

   ! ---- scalar-to-array defined assignment ----
   allocate(scalar%leaf)
   scalar%leaf = 42
   scalar%is_temporary = .false.
   whole = scalar
   do i = 1, 3
      if (.not. associated(whole(i)%leaf, scalar%leaf)) error stop
      if (whole(i)%leaf /= 42) error stop
      if (.not. whole(i)%is_temporary) error stop
   end do

   ! Free the heap-allocated leaves owned by arr / scalar (copies share them).
   do i = 1, 3
      deallocate(arr(i)%leaf)
   end do
   deallocate(scalar%leaf)
   deallocate(copy)
   deallocate(whole)
   deallocate(arr)
end program derived_types_151
