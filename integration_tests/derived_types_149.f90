module derived_types_149_mod
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
end module derived_types_149_mod

program derived_types_149
   use derived_types_149_mod
   implicit none
   type(t), dimension(:), allocatable :: arr, copy
   integer :: i

   allocate(arr(3))
   do i = 1, 3
      allocate(arr(i)%leaf)
      arr(i)%leaf = i * 100
      arr(i)%is_temporary = .false.
   end do

   allocate(copy, source = get_data(arr))

   if (size(copy) /= 3) error stop
   do i = 1, 3
      if (.not. associated(arr(i)%leaf)) error stop
      if (arr(i)%leaf /= i * 100) error stop
      if (.not. associated(copy(i)%leaf)) error stop
      if (copy(i)%leaf /= i * 100) error stop
      ! Both must point to the same object (shared, not deep copy).
      if (.not. associated(arr(i)%leaf, copy(i)%leaf)) error stop
   end do

   ! Free the heap-allocated leaves owned by arr (copies share them).
   do i = 1, 3
      deallocate(arr(i)%leaf)
   end do
   deallocate(copy)
   deallocate(arr)
end program derived_types_149
