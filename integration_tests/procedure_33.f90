module procedure_33_mod
   implicit none

   type :: MyType
      procedure(pintfc), pointer, nopass :: ptr => null()
   end type MyType

   abstract interface
      function pintfc() result(res)
         real(8) :: res
      end function pintfc
   end interface

contains

   function impl() result(res)
      real(8) :: res
      res = 42.0_8
   end function impl

   subroutine pointers()
      class(MyType), pointer :: myptr => null()
      procedure(pintfc), pointer :: ptr => null()
      type(MyType), target :: obj
      real(8) :: val

      ! Associate procedure pointer component
      obj%ptr => impl

      ! Point class pointer to object
      myptr => obj

      ! Assign procedure pointer from component
      ptr => myptr%ptr

      if (.not. associated(ptr)) error stop "ptr not associated"
      if (.not. associated(myptr%ptr)) error stop "component not associated"

      val = ptr()

      if (abs(val - 42.0_8) > 1.0e-12_8) then
         error stop "wrong function result"
      end if
   end subroutine pointers

end module procedure_33_mod


program procedure_33
   use procedure_33_mod
   implicit none
   call pointers()
   print *, "Procedure pointer component procedure_33_mod passed."
end program procedure_33