module intrinsics_shape_ptr_struct_member_01_mod
   implicit none
   type :: array_type
      real, allocatable :: val(:,:)
   end type
   type :: array_ptr_type
      type(array_type), pointer :: array(:,:) => null()
   end type
contains
   subroutine check_shape(arr, ref)
      type(array_ptr_type), dimension(:), allocatable, intent(in) :: arr
      integer, intent(in) :: ref(:)
      integer :: i
      do i = 1, size(arr)
         if (any(shape(arr(i)%array) /= ref)) error stop 1
      end do
   end subroutine

   subroutine check_shape_assoc(arr, n)
      type(array_ptr_type), dimension(:), allocatable, intent(in) :: arr
      integer, intent(in) :: n
      integer :: i, s(2)
      do i = 1, n
         s = shape(arr(i)%array)
         if (s(1) /= 2 .or. s(2) /= 3) error stop 2
      end do
   end subroutine
end module

program intrinsics_shape_ptr_struct_member_01
   use intrinsics_shape_ptr_struct_member_01_mod
   implicit none
   type(array_ptr_type), dimension(:), allocatable :: arr
   type(array_type), target :: t1, t2

   allocate(t1%val(2,3))
   allocate(t2%val(2,3))
   allocate(arr(2))
   allocate(arr(1)%array(2,3))
   allocate(arr(2)%array(2,3))

   call check_shape(arr, [2, 3])
   call check_shape_assoc(arr, 2)

   deallocate(arr(1)%array)
   deallocate(arr(2)%array)
   deallocate(arr)
   deallocate(t1%val)
   deallocate(t2%val)

   print *, "OK"
end program intrinsics_shape_ptr_struct_member_01
