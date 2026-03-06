! Test procedure pointer with parametric return type
! The return type dimension depends on the argument via size(n)
module functions_55_mod
   implicit none

   type :: WrapperType
      procedure(op_interface), pointer, nopass :: op => null()
   end type WrapperType

   interface
      function op_interface(n) result(res)
         integer, intent(in) :: n(:)
         integer :: res(size(n))
      end function op_interface
   end interface

contains

   subroutine apply_op(obj)
      class(WrapperType), intent(in) :: obj
      integer, allocatable :: arr(:)
      allocate(arr(4))
      arr = [1, 2, 3, 4]
      arr = obj%op(arr)
      if (arr(1) /= 2) error stop
      if (arr(2) /= 4) error stop
      if (arr(3) /= 6) error stop
      if (arr(4) /= 8) error stop
   end subroutine apply_op

end module functions_55_mod

program functions_55
   use functions_55_mod, only: WrapperType, apply_op
   implicit none

   type(WrapperType) :: obj
   obj%op => double_array
   call apply_op(obj)
   print *, "ok"

contains

   function double_array(n) result(res)
      integer, intent(in) :: n(:)
      integer :: res(size(n))
      integer :: i
      do i = 1, size(n)
         res(i) = n(i) * 2
      end do
   end function double_array

end program functions_55
