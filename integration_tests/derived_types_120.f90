! Test accessing a procedure pointer member of a class type.
! This verifies that the pass_array_by_data pass correctly
! updates StructInstanceMember references when struct members
! are renamed due to procedure signature changes.
module derived_types_120_mod
   implicit none

   type, abstract :: MyType
      procedure(pintfc), pointer, nopass :: ptr => null()
   end type MyType

   abstract interface
      function pintfc(n) result(res)
         integer(4), dimension(:), intent(in) :: n
         real(8) :: res
      end function pintfc
   end interface

   type, extends(MyType) :: ConcreteType
   end type ConcreteType

contains

   function my_func(n) result(res)
      integer(4), dimension(:), intent(in) :: n
      real(8) :: res
      res = real(sum(n), 8)
   end function my_func

end module derived_types_120_mod

program derived_types_120
   use derived_types_120_mod
   implicit none

   type(ConcreteType), target :: obj
   class(MyType), pointer :: myptr
   procedure(pintfc), pointer :: ptr
   integer :: arr(3)
   real(8) :: res

   obj%ptr => my_func
   myptr => obj
   ptr => myptr%ptr

   arr = [1, 2, 3]
   res = ptr(arr)
   if (abs(res - 6.0d0) > 1.0d-10) error stop

   print *, "PASS"
end program derived_types_120
